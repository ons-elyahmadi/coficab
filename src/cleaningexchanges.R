# Charger les packages nécessaires
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape2) # for melt function
library(ggthemes)
library(mice)
library(infotheo) 
 
data_EUR_CNY <-  read.csv("dataexchanges/EUR_CNY.csv", header = TRUE, sep = ",")
head(data_EUR_CNY)
data_EUR_HNL <-  read.csv("dataexchanges/EUR_HNL.csv", header = TRUE, sep = ",")
head(data_EUR_HNL)
data_EUR_MAD <-  read.csv("dataexchanges/EUR_MAD.csv", header = TRUE, sep = ",")
head(data_EUR_MAD)
data_EUR_MKD <-  read.csv("dataexchanges/EUR_MKD.csv", header = TRUE, sep = ",")
head(data_EUR_MKD)
data_EUR_MXN <-  read.csv("dataexchanges/EUR_MXN.csv", header = TRUE, sep = ",")
head(data_EUR_MXN)
data_EUR_RON <-  read.csv("dataexchanges/EUR_RON.csv", header = TRUE, sep = ",")
head(data_EUR_RON)
data_EUR_RSD <-  read.csv("dataexchanges/EUR_RSD.csv", header = TRUE, sep = ",")
head(data_EUR_RSD)
data_EUR_TND <-  read.csv("dataexchanges/EUR_TND.csv", header = TRUE, sep = ",")
head(data_EUR_TND)
data_EUR_USD <-  read.csv("dataexchanges/EUR_USD.csv", header = TRUE, sep = ",")
head(data_EUR_USD)
data_USD_HNL <-  read.csv("dataexchanges/USD_HNL.csv", header = TRUE, sep = ",")
head(data_USD_HNL)
data_USD_MXN <-  read.csv("dataexchanges/USD_MXN.csv", header = TRUE, sep = ",")
head(data_USD_MXN)
data_USD_RON <-  read.csv("dataexchanges/USD_RON.csv", header = TRUE, sep = ",")
head(data_USD_RON)
 
head(data_EUR_CNY)
data_EUR_HNL$Date<- as.Date(data_EUR_HNL$Date , format =  "%m/%d/%Y")
data_EUR_MAD$Date<- as.Date(data_EUR_MAD$Date , format =  "%m/%d/%Y")
data_EUR_MKD$Date<- as.Date(data_EUR_MKD$Date , format =  "%m/%d/%Y") 
data_EUR_MXN$Date<- as.Date(data_EUR_MXN$Date , format =  "%m/%d/%Y") 
data_EUR_RON$Date<- as.Date(data_EUR_RON$Date , format =  "%m/%d/%Y") 
data_EUR_RSD$Date<- as.Date(data_EUR_RSD$Date , format =  "%m/%d/%Y")
data_EUR_TND$Date<- as.Date(data_EUR_TND$Date , format =  "%m/%d/%Y")
 
data_USD_RON$Date<- as.Date(data_USD_RON$Date , format =  "%m/%d/%Y")
data_USD_HNL$Date<- as.Date(data_USD_HNL$Date , format =  "%m/%d/%Y")
data_USD_MXN$Date<- as.Date(data_USD_MXN$Date , format =  "%m/%d/%Y")


# Convertir les colonnes de dates en format Date
data_EUR_CNY$Price<- as.numeric(data_EUR_CNY$Price)
data_EUR_HNL$Price<- as.numeric(data_EUR_HNL$Price )
data_EUR_MAD$Price<- as.numeric(data_EUR_MAD$Price )
data_EUR_MKD$Price<- as.numeric(data_EUR_MKD$Price) 
data_EUR_MXN$Price<- as.numeric(data_EUR_MXN$Price) 
data_EUR_RON$Price<- as.numeric(data_EUR_RON$Price) 
data_EUR_RSD$Price<- as.numeric(data_EUR_RSD$Price )
data_EUR_TND$Price<- as.numeric(data_EUR_TND$Price )
data_EUR_USD$Price<- as.numeric(data_EUR_USD$Price )
data_USD_HNL$Price<- as.numeric(data_USD_HNL$Price )
data_USD_MXN$Price<- as.numeric(data_USD_MXN$Price )
data_USD_RON$Price<- as.numeric(data_USD_RON$Price )

head(data_USD_RON)
head(data_USD_MXN)
head(data_USD_HNL)
head(data_EUR_USD)
head(data_EUR_TND)
head(data_EUR_RSD)
head(data_EUR_RON)
head(data_EUR_MXN)
head(data_EUR_MKD)
head(data_EUR_MAD)
head(data_EUR_CNY)
head(data_EUR_HNL)
write_csv(data_USD_RON, "dataexchanges/data_USD_RON.csv")
write_csv(data_USD_MXN, "dataexchanges/data_USD_MXN.csv")
write_csv(data_USD_HNL, "dataexchanges/data_USD_HNL.csv")
write_csv(data_EUR_USD, "dataexchanges/data_EUR_USD.csv")
write_csv(data_EUR_TND, "dataexchanges/data_EUR_TND.csv")
write_csv(data_EUR_RSD, "dataexchanges/data_EUR_RSD.csv")
 
write_csv(data_EUR_MXN, "dataexchanges/data_EUR_MXN.csv")
write_csv(data_EUR_MKD, "dataexchanges/data_EUR_MKD.csv")
write_csv(data_EUR_MAD, "dataexchanges/data_EUR_MAD.csv")
write_csv(data_EUR_CNY, "dataexchanges/data_EUR_CNY.csv")
write_csv(data_EUR_HNL, "dataexchanges/data_EUR_HNL.csv")
# Afficher les données complètes
print(data_USD_RON)
View(data_USD_RON)
dim(data_USD_RON)
str(data_USD_RON)
tail(data_USD_RON)
glimpse(data_USD_RON)
unique(data_USD_RON$Price)
 
# Compter les données manquantes dans chaque colonne
missing_valuedata_USD_RON <- sapply(data_USD_RON$Price, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valuedata_USD_RON)
 
total_missingdata_USD_RON <- sum(is.na(data_USD_RON$Price))

data_USD_RON <-  read.csv("dataexchanges/data_USD_RON.csv", header = TRUE, sep = ",")
 
# Afficher le nombre total de données manquantes
print(data_USD_RON)

data_EUR_RON <-  read.csv("dataexchanges/data_EUR_RON.csv", header = TRUE, sep = ",")

# Afficher le nombre total de données manquantes
print(data_EUR_RON)
 
attach(data_USD_RON)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(data_USD_RON$Price)
summary(data_USD_RON$Price)
barplot(data_USD_RON$Price)
hist(data_USD_RON$Price)
par(mfrow = c(1, 2))
hist(data_USD_RON$Price, main = "USD_RON", xlab = "USD_RON ", col = "lightblue")
which(data_USD_RON$Price%in% boxplot(data_USD_RON$Price)$out)
# Pour data_EUR_USD
print(data_EUR_USD)
missing_values_EUR_USD <- sapply(data_EUR_USD$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_USD)
total_missing_EUR_USD <- sum(is.na(data_EUR_USD$Price))
print(total_missing_EUR_USD)

boxplot(data_EUR_USD$Price)
summary(data_EUR_USD$Price)
barplot(data_EUR_USD$Price)
hist(data_EUR_USD$Price)
par(mfrow = c(1, 2))
hist(data_EUR_USD$Price, main = "EUR_USD", xlab = "EUR_USD", col = "lightblue")

# Pour data_EUR_TND
print(data_EUR_TND)
missing_values_EUR_TND <- sapply(data_EUR_TND$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_TND)
total_missing_EUR_TND <- sum(is.na(data_EUR_TND$Price))
print(total_missing_EUR_TND)

boxplot(data_EUR_TND$Price)
summary(data_EUR_TND$Price)
barplot(data_EUR_TND$Price)
hist(data_EUR_TND$Price)
par(mfrow = c(1, 2))
hist(data_EUR_TND$Price, main = "EUR_TND", xlab = "EUR_TND", col = "lightblue")
 

# Pour data_EUR_RSD
print(data_EUR_RSD)
missing_values_EUR_RSD <- sapply(data_EUR_RSD$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_RSD)
total_missing_EUR_RSD <- sum(is.na(data_EUR_RSD$Price))
print(total_missing_EUR_RSD)

boxplot(data_EUR_RSD$Price)
summary(data_EUR_RSD$Price)
barplot(data_EUR_RSD$Price)
hist(data_EUR_RSD$Price)
par(mfrow = c(1, 2))
hist(data_EUR_RSD$Price, main = "EUR_RSD", xlab = "EUR_RSD", col = "lightblue")
 
# Pour data_EUR_RON
print(data_EUR_RON)
missing_values_EUR_RON <- sapply(data_EUR_RON$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_RON)
total_missing_EUR_RON <- sum(is.na(data_EUR_RON$Price))
print(total_missing_EUR_RON)

boxplot(data_EUR_RON$Price)
summary(data_EUR_RON$Price)
barplot(data_EUR_RON$Price)
hist(data_EUR_RON$Price)
par(mfrow = c(1, 2))
hist(data_EUR_RON$Price, main = "EUR_RON", xlab = "EUR_RON", col = "lightblue")

# Pour data_EUR_MXN
print(data_EUR_MXN)
missing_values_EUR_MXN <- sapply(data_EUR_MXN$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_MXN)
total_missing_EUR_MXN <- sum(is.na(data_EUR_MXN$Price))
print(total_missing_EUR_MXN)

boxplot(data_EUR_MXN$Price)
summary(data_EUR_MXN$Price)
barplot(data_EUR_MXN$Price)
hist(data_EUR_MXN$Price)
par(mfrow = c(1, 2))
hist(data_EUR_MXN$Price, main = "EUR_MXN", xlab = "EUR_MXN", col = "lightblue")
 
# Pour data_EUR_MKD
print(data_EUR_MKD)
missing_values_EUR_MKD <- sapply(data_EUR_MKD$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_MKD)
total_missing_EUR_MKD <- sum(is.na(data_EUR_MKD$Price))
print(total_missing_EUR_MKD)

boxplot(data_EUR_MKD$Price)
summary(data_EUR_MKD$Price)
barplot(data_EUR_MKD$Price)
hist(data_EUR_MKD$Price)
par(mfrow = c(1, 2))
hist(data_EUR_MKD$Price, main = "EUR_MKD", xlab = "EUR_MKD", col = "lightblue")
# Pour data_EUR_MAD
print(data_EUR_MAD)
missing_values_EUR_MAD <- sapply(data_EUR_MAD$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_MAD)
total_missing_EUR_MAD <- sum(is.na(data_EUR_MAD$Price))
print(total_missing_EUR_MAD)

boxplot(data_EUR_MAD$Price)
summary(data_EUR_MAD$Price)
barplot(data_EUR_MAD$Price)
hist(data_EUR_MAD$Price)
par(mfrow = c(1, 2))
hist(data_EUR_MAD$Price, main = "EUR_MAD", xlab = "EUR_MAD", col = "lightblue")

# Pour data_EUR_CNY
print(data_EUR_CNY)
missing_values_EUR_CNY <- sapply(data_EUR_CNY$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_CNY)
total_missing_EUR_CNY <- sum(is.na(data_EUR_CNY$Price))
print(total_missing_EUR_CNY)

boxplot(data_EUR_CNY$Price)
summary(data_EUR_CNY$Price)
barplot(data_EUR_CNY$Price)
hist(data_EUR_CNY$Price)
par(mfrow = c(1, 2))
hist(data_EUR_CNY$Price, main = "EUR_CNY", xlab = "EUR_CNY", col = "lightblue")
 

# Pour data_EUR_HNL
print(data_EUR_HNL)
missing_values_EUR_HNL <- sapply(data_EUR_HNL$Price, function(x) sum(is.na(x)))
print(missing_values_EUR_HNL)
total_missing_EUR_HNL <- sum(is.na(data_EUR_HNL$Price))
print(total_missing_EUR_HNL)

boxplot(data_EUR_HNL$Price)
summary(data_EUR_HNL$Price)
barplot(data_EUR_HNL$Price)
hist(data_EUR_HNL$Price)
par(mfrow = c(1, 2))
hist(data_EUR_HNL$Price, main = "EUR_HNL", xlab = "EUR_HNL", col = "lightblue")
 

# Pour data_USD_MXN
print(data_USD_MXN)
missing_values_USD_MXN <- sapply(data_USD_MXN$Price, function(x) sum(is.na(x)))
print(missing_values_USD_MXN)
total_missing_USD_MXN <- sum(is.na(data_USD_MXN$Price))
print(total_missing_USD_MXN)

boxplot(data_USD_MXN$Price)
summary(data_USD_MXN$Price)
barplot(data_USD_MXN$Price)
hist(data_USD_MXN$Price)
par(mfrow = c(1, 2))
hist(data_USD_MXN$Price, main = "USD_MXN", xlab = "USD_MXN", col = "lightblue")

# Pour data_USD_HNL
print(data_USD_HNL)
missing_values_USD_HNL <- sapply(data_USD_HNL$Price, function(x) sum(is.na(x)))
print(missing_values_USD_HNL)
total_missing_USD_HNL <- sum(is.na(data_USD_HNL$Price))
print(total_missing_USD_HNL)

boxplot(data_USD_HNL$Price)
summary(data_USD_HNL$Price)
barplot(data_USD_HNL$Price)
hist(data_USD_HNL$Price)
par(mfrow = c(1, 2))
hist(data_USD_HNL$Price, main = "USD_HNL", xlab = "USD_HNL", col = "lightblue")
# Imputer les données manquantes pour data_USD_MXN
imputed_data_USD_MXN <- complete(mice(data_USD_MXN))

# Étape 2: Identifier les valeurs aberrantes en utilisant un boxplot ou toute autre méthode pour chaque variable d'intérêt
variables_of_interest <- c("Price") # Ajoutez plus de variables au besoin
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_USD_MXN[[variable]] %in% boxplot(imputed_data_USD_MXN[[variable]])$out)
}

# Étape 3: Remplacer les valeurs aberrantes par NA pour chaque variable
for (variable in variables_of_interest) {
      imputed_data_USD_MXN[[variable]][outlier_indices[[variable]]] <- NA
}

# Étape 4: Réexécuter le processus d'imputation
imputed_data_USD_MXN <- complete(mice(imputed_data_USD_MXN))

# Étape 5: Évaluer les résultats
summary(imputed_data_USD_MXN)
which(imputed_data_USD_MXN$Price %in% boxplot(imputed_data_USD_MXN$Price)$out)
 

# Imputer les données manquantes pour data_USD_HNL
imputed_data_USD_HNL <- complete(mice(data_USD_HNL))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_USD_HNL[[variable]] %in% boxplot(imputed_data_USD_HNL[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_USD_HNL[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_USD_HNL <- complete(mice(imputed_data_USD_HNL))
summary(imputed_data_USD_HNL)
which(imputed_data_USD_HNL$Price %in% boxplot(imputed_data_USD_HNL$Price)$out)
# Imputer les données manquantes pour data_EUR_USD
imputed_data_EUR_USD <- complete(mice(data_EUR_USD))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_USD[[variable]] %in% boxplot(imputed_data_EUR_USD[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_USD[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_USD <- complete(mice(imputed_data_EUR_USD))
summary(imputed_data_EUR_USD)
which(imputed_data_EUR_USD$Price %in% boxplot(imputed_data_EUR_USD$Price)$out)
# Imputer les données manquantes pour data_EUR_TND
imputed_data_EUR_TND <- complete(mice(data_EUR_TND))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_TND[[variable]] %in% boxplot(imputed_data_EUR_TND[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_TND[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_TND <- complete(mice(imputed_data_EUR_TND))
summary(imputed_data_EUR_TND)
which(imputed_data_EUR_TND$Price %in% boxplot(imputed_data_EUR_TND$Price)$out)
 
# Imputer les données manquantes pour data_EUR_RSD
imputed_data_EUR_RSD <- complete(mice(data_EUR_RSD))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_RSD[[variable]] %in% boxplot(imputed_data_EUR_RSD[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_RSD[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_RSD <- complete(mice(imputed_data_EUR_RSD))
summary(imputed_data_EUR_RSD)
which(imputed_data_EUR_RSD$Price %in% boxplot(imputed_data_EUR_RSD$Price)$out) 
# Imputer les données manquantes pour data_EUR_RON
imputed_data_EUR_RON <- complete(mice(data_EUR_RON))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_RON[[variable]] %in% boxplot(imputed_data_EUR_RON[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_RON[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_RON <- complete(mice(imputed_data_EUR_RON))
summary(imputed_data_EUR_RON)
which(imputed_data_EUR_RON$Price %in% boxplot(imputed_data_EUR_RON$Price)$out) 
# Imputer les données manquantes pour data_EUR_MXN
imputed_data_EUR_MXN <- complete(mice(data_EUR_MXN))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_MXN[[variable]] %in% boxplot(imputed_data_EUR_MXN[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_MXN[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_MXN <- complete(mice(imputed_data_EUR_MXN))
summary(imputed_data_EUR_MXN)
which(imputed_data_EUR_MXN$Price %in% boxplot(imputed_data_EUR_MXN$Price)$out) 
# Imputer les données manquantes pour data_EUR_MKD
imputed_data_EUR_MKD <- complete(mice(data_EUR_MKD))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_MKD[[variable]] %in% boxplot(imputed_data_EUR_MKD[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_MKD[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_MKD <- complete(mice(imputed_data_EUR_MKD))
summary(imputed_data_EUR_MKD)
which( imputed_data_EUR_MKD$Price %in% boxplot( imputed_data_EUR_MKD$Price)$out)  

# Imputer les données manquantes pour data_EUR_MAD
imputed_data_EUR_MAD <- complete(mice(data_EUR_MAD))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_MAD[[variable]] %in% boxplot(imputed_data_EUR_MAD[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_MAD[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_MAD <- complete(mice(imputed_data_EUR_MAD))
summary(imputed_data_EUR_MAD)
which(imputed_data_EUR_MAD$Price %in% boxplot(imputed_data_EUR_MAD$Price)$out) 
# Imputer les données manquantes pour data_EUR_CNY
imputed_data_EUR_CNY <- complete(mice(data_EUR_CNY))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_CNY[[variable]] %in% boxplot(imputed_data_EUR_CNY[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_CNY[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_CNY <- complete(mice(imputed_data_EUR_CNY))
summary(imputed_data_EUR_CNY)
which(imputed_data_EUR_CNY$Price %in% boxplot(imputed_data_EUR_CNY$Price)$out)  

# Imputer les données manquantes pour data_EUR_HNL
imputed_data_EUR_HNL <- complete(mice(data_EUR_HNL))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data_EUR_HNL[[variable]] %in% boxplot(imputed_data_EUR_HNL[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_EUR_HNL[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_EUR_HNL <- complete(mice(imputed_data_EUR_HNL))
summary(imputed_data_EUR_HNL)
which( imputed_data_EUR_HNL$Price %in% boxplot( imputed_data_EUR_HNL$Price)$out)


# Imputer les données manquantes pour data_EUR_HNL
imputed_data_USD_RON <- complete(mice(data_USD_RON))
variables_of_interest <- c("Price") 
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which( imputed_data_USD_RON[[variable]] %in% boxplot( imputed_data_USD_RON[[variable]])$out)
}
for (variable in variables_of_interest) {
      imputed_data_USD_RON[[variable]][outlier_indices[[variable]]] <- NA
}
imputed_data_USD_RON <- complete(mice( imputed_data_USD_RON))
summary(imputed_data_USD_RON)
which(imputed_data_USD_RON$Price %in% boxplot( imputed_data_USD_RON$Price)$out)
write_csv(imputed_data_USD_RON, "dataexchanges/data_USD_RON.csv")
write_csv(imputed_data_USD_MXN, "dataexchanges/data_USD_MXN.csv")
write_csv(imputed_data_USD_HNL, "dataexchanges/data_USD_HNL.csv")
write_csv(imputed_data_EUR_USD, "dataexchanges/data_EUR_USD.csv")
write_csv(imputed_data_EUR_TND, "dataexchanges/data_EUR_TND.csv")
write_csv(imputed_data_EUR_RSD, "dataexchanges/data_EUR_RSD.csv")
write_csv(imputed_data_EUR_RON, "dataexchanges/data_EUR_RON.csv")
write_csv(imputed_data_EUR_MXN, "dataexchanges/data_EUR_MXN.csv")
write_csv(imputed_data_EUR_MKD, "dataexchanges/data_EUR_MKD.csv")
write_csv(imputed_data_EUR_MAD, "dataexchanges/data_EUR_MAD.csv")
write_csv(imputed_data_EUR_CNY, "dataexchanges/data_EUR_CNY.csv")
write_csv(imputed_data_EUR_HNL, "dataexchanges/data_EUR_HNL.csv")
 


