# Charger les packages nécessaires
library(tidyr)
library(dplyr)
library(tidyverse)
 
# Charger les données
data_actual <-  read.csv("data/dataActuel.csv", header = TRUE, sep = ";")

head(data_actual)
 
# Convertir les colonnes de dates en format Date
data_actual$Date <- as.Date(data_actual$Date, format = "%d/%m/%Y")
data_actual$price <- as.numeric(gsub(",", ".", data_actual$price))
head(data_actual)
 
write_csv(data_actual, "data/dataActuel.csv")
 