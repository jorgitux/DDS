library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# App1
app1_report <- read.csv("data-csv/App1 - 2021.csv",header = TRUE, sep = ";")
# App2
app2_report <- read.csv("data-csv/App2 - 2021.csv",header = TRUE, sep = ";")

# Union de reporte
app_report <- rbind(app1_report, app2_report)

# Convierte datos necesarios a datos categóricos
app_report$Application <- as.factor(app_report$Application)
app_report$Language <- as.factor(app_report$Language)
app_report$Category <- as.factor(app_report$Category)
app_report$Criticality <- as.factor(app_report$Criticality)
app_report$Scan.Status <- as.factor(app_report$Scan.Status)
app_report$Impact <- as.factor(app_report$Impact)
app_report$Severity <- as.factor(app_report$Severity)
app_report$Kingdom <- as.factor(app_report$Kingdom)

# Convierte datos necesarios a datos de tipo Date
app_report$Found.Date <- as.Date(app_report$Found.Date,"%d/%m/%Y")
app_report$Removed.Date <- as.Date(app_report$Removed.Date,"%d/%m/%Y")

# Convierte datos necesarios a datos de tipo Lógico
app_report$Is.Reviewed <- as.logical(app_report$Is.Reviewed)
app_report$Has.Correlated.Issues <- as.logical(app_report$Has.Correlated.Issues)
app_report$Has.Comments <- as.logical(app_report$Has.Comments)
app_report$Is.Removed <- as.logical(app_report$Is.Removed)
app_report$Is.Suppressed <- as.logical(app_report$Is.Suppressed)
app_report$Is.Hidden <- as.logical(app_report$Is.Hidden)

# Variables Generales
num_vulns_actually = nrow(app_report)

# Agrupación por Severidad
df <- app_report %>% group_by(Severity) %>% summarise(num = n())


df[,"Percent"] <- df[,"num"] / num_vulns_actually*100


df %>%
  mutate(name = fct_reorder(Severity, Percent)) %>%
  ggplot( aes(x=Severity, y=Percent)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Severity") +
  theme_bw()

# Agrupación por Lenguaje
df_lang <- app_report %>% group_by(Language) %>% summarise(num = n())
df_lang[,"Percent"] <- df_lang[,"num"] / num_vulns_actually*100

df_lang %>%
  mutate(name = fct_reorder(Language, Percent)) %>%
  ggplot( aes(x=Language, y=Percent)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Language") +
  theme_bw()


# Agrupación por Aplicación y Severidad
df_app_severity <- app_report %>% group_by(Application,Severity) %>% summarise(num = n())


df_app_severity %>% ggplot( aes(fill=Severity, y=num, x=Application) ) +
                      geom_bar(position = "dodge", stat="identity") +
                      ylab("Nº Vulns")

# Agrupación de vulnerabilidades por Aplicación
df_app <- app_report %>% group_by(Application) %>% summarise(num = n())


x <- df_app_severity %>% filter(Severity==5)
x[,"Percent"] <- x[,"num"]/df_app[,"num"]*100

x %>%
  mutate(name = fct_reorder(Application, Percent)) %>%
  ggplot( aes(x=Application, y=Percent)) +
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Application") +
  theme_bw()