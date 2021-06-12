library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(forcats)

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
app_report$Found.Date <- as.Date(app_report$Found.Date)
app_report$Removed.Date <- as.Date(app_report$Removed.Date)

# Convierte datos necesarios a datos de tipo Lógico
app_report$Is.Reviewed <- as.logical(app_report$Is.Reviewed)
app_report$Has.Correlated.Issues <- as.logical(app_report$Has.Correlated.Issues)
app_report$Has.Comments <- as.logical(app_report$Has.Comments)
app_report$Is.Removed <- as.logical(app_report$Is.Removed)
app_report$Is.Suppressed <- as.logical(app_report$Is.Suppressed)
app_report$Is.Hidden <- as.logical(app_report$Is.Hidden)

#Transformar CWEs a vector (Recordar que hay blancos)
#x<-list(app_report$CWE)

# Variables Generales
num_vulns_actually = nrow(app_report)

# Agrupación por Criticidad
df <- app_report %>% group_by(Criticality) %>% summarise(num = n())


df[,"Percent"] <- df[,"num"] / num_vulns_actually*100


df %>%
  mutate(name = fct_reorder(Criticality, Percent)) %>%
  ggplot( aes(x=Criticality, y=Percent)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Criticality") +
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


# Agrupación por Aplicación y Criticidad
df_app_criticality <- app_report %>% group_by(Application,Criticality) %>% summarise(num = n())

# Agrupación por Aplicación y Vulnerabilidad
df_app_vuln <- app_report %>% group_by(Application,CWE) %>% summarise(num = n())