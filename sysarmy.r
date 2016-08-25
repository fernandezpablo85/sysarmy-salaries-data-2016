library(ggplot2)
df <- read.csv('sueldos.sysarmy.csv')

rename <- function(df, old, wants) {
  names(df)[names(df)==old] <- wants
  return(df)
}

cleanup <- function(data) {
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

  # filter fictitious salaries.
  data <- subset(data, Income < 120000)
  data <- subset(data, Income > 6000)

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

all.salaries.gender = function(df) {
  plot <- ggplot(df, aes(x=Income, fill=Gender), ylab="") + 
    geom_histogram(binwidth = 1000, alpha=0.9)
  return(plot)
}

clean <- cleanup(df)
write.csv(clean, 'clean.csv', row.names=FALSE)

default.plot <- all.salaries.gender
default.plot(clean)
