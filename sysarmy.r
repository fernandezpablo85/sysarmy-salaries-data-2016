library(ggplot2)
df <- read.csv('sueldos.sysarmy.csv')

rename <- function(df, old, wants) {
  names(df)[names(df)==old] <- wants
  return(df)
}

tukey <- function(data) {
  iqr <- IQR(data$Income)
  firstQ <- quantile(data$Income)[2]
  thirdQ <- quantile(data$Income)[4]
  low <- firstQ - (iqr * 1.5)
  high <- thirdQ + (iqr * 1.5)
  data <- subset(data, Income < high)
  data <- subset(data, Income > low)
  return(data)
}

cleanup <- function(data, handleOutliers) {
  # clean gender.
  data$Gender = ifelse(data$Soy == "Hombre", "M", "F")
  data$Gender = as.factor(data$Gender)

  # rename columns.
  data <- rename(data, "Tengo", "Age")
  data <- rename(data, "Argentina", "Region")
  data <- rename(data, "A..os.de.experiencia", "YearsExperience")
  data <- rename(data, "A..os.en.el.puesto.actual", "YearsCurrentJob")
  data <- rename(data, "Trabajo.de", "JobDescription")
  data <- rename(data, "Tipo.de.contrato", "JobType")
  data <- rename(data, "Qu...tan.conforme.est..s.con.tu.sueldo.", "Happiness")
  data <- rename(data, "Cambiaste.de.empresa.en.los...ltimos.6.meses.", "SwitchedJobsLast6Months")

  # fix region names.
  levels(data$Region)[levels(data$Region) == "Entre R\303\255os"] <- "Entre Rios"
  levels(data$Region)[levels(data$Region) == "Ciudad Aut\303\263noma de Buenos Aires"] <- "CABA"
  levels(data$Region)[levels(data$Region) == "C\303\263rdoba"] <- "Cordoba"
  levels(data$Region)[levels(data$Region) == "Neuqu\303\251n"] <- "Neuquen"
  levels(data$Region)[levels(data$Region) == "R\303\255o Negro"] <- "Rio Negro"
  levels(data$Region)[levels(data$Region) == "Tucum\303\241n"] <- "Tucuman"
  levels(data$Region)[levels(data$Region) == "Provincia de Buenos Aires"] <- "GBA"
  
  # fix age.
  levels(data$Age)[levels(data$Age) == "Menos de 18 a\303\261os"] <- "18-"

  # fix salary.
  data <- rename(data, "Salario.mensual..en.tu.moneda.local.", "Income")
  data$Income <- ifelse(data$Bruto.o.neto. == "Bruto", data$Income, data$Income/0.70)
  data$Bruto.o.neto. = NULL
  
  # fix job switch.
  data$SwitchedJobsLast6Months = ifelse(data$SwitchedJobsLast6Months == "No", 0, 1)

  # remove ficticious data.
  data <- subset(data, Income < 200000)
  data <- subset(data, Income > 1000)
  
  # handle outliers.
  data <- handleOutliers(data)
  
  keep <- c("Age", "Region", "YearsExperience", "YearsCurrentJob", "JobDescription",
            "JobType", "Happiness", "Income", "Gender", "SwitchedJobsLast6Months")
  return(data[keep])
}

all.salaries.hist <- function(df) {
  plot <- ggplot(df, aes(x=Income), ylab="") + 
    geom_histogram(binwidth = 1000, fill="#3399FF", alpha=0.9)
  return(plot)
}

all.salaries.hist.median <- function(df) {
  plot <- all.salaries.hist(df) + 
    geom_vline(aes(xintercept = mean(Income)), linetype="longdash", color="red")
  return(plot)
}

all.salaries.gender <- function(df) {
  plot <- ggplot(df, aes(x=Income, fill=Gender), ylab="") + 
    geom_histogram(binwidth = 1000, alpha=0.9)
  return(plot)
}

color.outliers <- function(df) {
  iqr <- IQR(df$Income)
  firstQ <- quantile(df$Income)[2]
  thirdQ <- quantile(df$Income)[4]
  low <- firstQ - (iqr * 1.5)
  high <- thirdQ + (iqr * 1.5)
  df$OutlierTag = "Middle"
  df$OutlierTag[df$Income <= low] = "LowOutliers"
  df$OutlierTag[df$Income >= high] = "HighOutliers"
  plot <- ggplot(df, aes(x=Income, fill=OutlierTag)) +
      geom_histogram(binwidth = 1000) +
      geom_vline(aes(xintercept = high), linetype="longdash", color="red")
  return(plot)
}

normal.distribution.histogram <- function(size, mean, sd) {
  sample.norm <- rnorm(size, mean, sd)
  data <- data.frame(sample.norm)
  plot <- ggplot(data, aes(x=sample.norm)) +
    geom_density(fill="#67ACB3",colour="#67ACB3", alpha=0.5) +
    geom_vline(aes(xintercept = mean), linetype="longdash", color="black")
  return(plot)
}

same.size.distribution <- function(df) {
  women <- subset(df, Gender == "F")
  allMen <- subset(df, Gender == "M")
  men <- allMen[sample(nrow(allMen), nrow(women)), ]
  all <- rbind(women, men)
  return(all)
}

same.sample.size.gender.distribution <- function(df) {
  all <- same.size.distribution(df)
  plot <- ggplot(all, aes(x=Income, fill=Gender), ylab="") + 
    geom_histogram(binwidth = 2000, alpha=0.9, position = "dodge")
  return(plot)
}

all.salaries.gender.distribution <- function(df) {
  plot <- ggplot(df, aes(x=Income, fill=Gender), ylab="") + 
    geom_density(alpha=0.6)
  return(plot)
}

salaries.by.gender.anova <- function(df) {
  model <- lm(Income ~ Gender, data=df)
  return(anova(model))
}

# red #D57668
# green #67ACB3

clean <- cleanup(df, handleOutliers = identity)
write.csv(clean, 'clean.csv', row.names=FALSE)

default.plot <- same.sample.size.gender.distribution
default.plot(clean)
# default.plot(50, 5, 2)


