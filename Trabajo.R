library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# App1
app1_report <- read.csv("data-csv/App1.csv",header = TRUE, sep = ";",)
# App2
app2_report <- read.csv("data-csv/App2.csv",header = TRUE, sep = ";")

# Union de reporte
app_report <- rbind(app1_report, slice(app2_report, 2:nrow(app2_report)) )

# Convierte datos necesarios a datos categóricos
app_report$Application <- as.factor(app_report$Application)
app_report$Language <- as.factor(app_report$Language)
app_report$Category <- as.factor(app_report$Category)
app_report$Criticality <- as.factor(app_report$Criticality)
app_report$Scan.Status <- as.factor(app_report$Scan.Status)
app_report$Impact <- as.factor(app_report$Impact)
app_report$Severity <- as.factor(app_report$Severity)
app_report$Kingdom <- as.factor(app_report$Kingdom)

#Transformar CWEs a vector (Recordar que hay blancos)
#x<-list(app_report$CWE)

# Variables Generales
num_total_vulnerabilities = nrow(app_report)

# Agrupación por Criticidad
df <- app_report %>% group_by(Criticality) %>% summarise(num = n())


df[,"Percent"] <- df[,"num"] / num_total_vulnerabilities*100


df %>%
  mutate(name = fct_reorder(Criticality, Percent)) %>%
  ggplot( aes(x=Criticality, y=Percent)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Criticality") +
  theme_bw()

# Agrupación por Lenguaje
df_lang <- app_report %>% group_by(Language) %>% summarise(num = n())
df_lang[,"Percent"] <- df_lang[,"num"] / num_total_vulnerabilities*100

df_lang %>%
  mutate(name = fct_reorder(Language, Percent)) %>%
  ggplot( aes(x=Language, y=Percent)) +
  geom_bar(stat="identity", fill="black", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Language") +
  theme_bw()
