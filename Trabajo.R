library(readr)

# Vector de cabeceras necesarias
#cabeceras_reporte <- c("Application","Category")

# App1
app1_report <- read.csv("data-csv/App1.csv",header = TRUE, sep = ";")

# Convierte datos necesarios a datos categóricos
app1_report$Category <- as.factor(app1_report$Category)
app1_report$Criticality <- as.factor(app1_report$Criticality)
app1_report$Scan.Status <- as.factor(app1_report$Scan.Status)
app1_report$Impact <- as.factor(app1_report$Impact)
app1_report$Severity <- as.factor(app1_report$Severity)
app1_report$Kingdom <- as.factor(app1_report$Kingdom)

#Transformar CWEs a vector (Recordar que hay blancos)
#x<-list(app1_report$CWE)

#app1_report$CWE <- as.factor(app1_report$Kingdom)

#summary(app1_report)



# App2
#app2_report <- read.csv("data-csv/App2.csv",header = TRUE, sep = ";")