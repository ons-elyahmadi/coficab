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
 
data_EUR_CNY <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_CNY.csv", header = TRUE, sep = ",")
head(data_EUR_CNY)
data_EUR_HNL <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_HNL.csv", header = TRUE, sep = ",")
head(data_EUR_HNL)
data_EUR_MAD <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MAD.csv", header = TRUE, sep = ",")
head(data_EUR_MAD)
data_EUR_MKD <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MKD.csv", header = TRUE, sep = ",")
head(data_EUR_MKD)
data_EUR_MXN <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MXN.csv", header = TRUE, sep = ",")
head(data_EUR_MXN)
data_EUR_RON <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RON.csv", header = TRUE, sep = ",")
head(data_EUR_RON)
data_EUR_RSD <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RSD.csv", header = TRUE, sep = ",")
head(data_EUR_RSD)
data_EUR_TND <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_TND.csv", header = TRUE, sep = ",")
head(data_EUR_TND)
data_EUR_USD <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_USD.csv", header = TRUE, sep = ",")
head(data_EUR_USD)
data_USD_HNL <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_HNL.csv", header = TRUE, sep = ",")
head(data_USD_HNL)
data_USD_MXN <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_MXN.csv", header = TRUE, sep = ",")
head(data_USD_MXN)
data_USD_RON <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_RON.csv", header = TRUE, sep = ",")
head(data_USD_RON)
data_USD_MKD <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_MKD.csv", header = TRUE, sep = ",")
head(data_USD_MKD)
 
head(data_EUR_CNY)
 
 



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
data_USD_MKD$Price<- as.numeric(data_USD_MKD$Price )

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
head(data_USD_MKD)
# Ordering each data frame by the Date column
# Convert the Date column to Date format and order each data frame
data_USD_RON$Date <- as.Date(data_USD_RON$Date)
data_USD_RON <- data_USD_RON[order(data_USD_RON$Date), ]
head(data_USD_RON)

data_USD_MXN$Date <- as.Date(data_USD_MXN$Date)
data_USD_MXN <- data_USD_MXN[order(data_USD_MXN$Date), ]
head(data_USD_MXN)

data_USD_HNL$Date <- as.Date(data_USD_HNL$Date)
data_USD_HNL <- data_USD_HNL[order(data_USD_HNL$Date), ]
head(data_USD_HNL)

data_EUR_USD$Date <- as.Date(data_EUR_USD$Date)
data_EUR_USD <- data_EUR_USD[order(data_EUR_USD$Date), ]
head(data_EUR_USD)

data_EUR_TND$Date <- as.Date(data_EUR_TND$Date)
data_EUR_TND <- data_EUR_TND[order(data_EUR_TND$Date), ]
head(data_EUR_TND)

data_EUR_RSD$Date <- as.Date(data_EUR_RSD$Date)
data_EUR_RSD <- data_EUR_RSD[order(data_EUR_RSD$Date), ]
head(data_EUR_RSD)

data_EUR_RON$Date <- as.Date(data_EUR_RON$Date)
data_EUR_RON <- data_EUR_RON[order(data_EUR_RON$Date), ]
head(data_EUR_RON)

data_EUR_MXN$Date <- as.Date(data_EUR_MXN$Date)
data_EUR_MXN <- data_EUR_MXN[order(data_EUR_MXN$Date), ]
head(data_EUR_MXN)

data_EUR_MKD$Date <- as.Date(data_EUR_MKD$Date)
data_EUR_MKD <- data_EUR_MKD[order(data_EUR_MKD$Date), ]
head(data_EUR_MKD)

data_EUR_MAD$Date <- as.Date(data_EUR_MAD$Date)
data_EUR_MAD <- data_EUR_MAD[order(data_EUR_MAD$Date), ]
head(data_EUR_MAD)

data_EUR_CNY$Date <- as.Date(data_EUR_CNY$Date)
data_EUR_CNY <- data_EUR_CNY[order(data_EUR_CNY$Date), ]
head(data_EUR_CNY)

data_EUR_HNL$Date <- as.Date(data_EUR_HNL$Date)
data_EUR_HNL <- data_EUR_HNL[order(data_EUR_HNL$Date), ]
head(data_EUR_HNL)

data_USD_MKD$Date <- as.Date(data_USD_MKD$Date)
data_USD_MKD <- data_USD_MKD[order(data_USD_MKD$Date), ]
head(data_USD_MKD)

tail(data_EUR_HNL)
 
write_csv(data_USD_RON, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_RON.csv")
write_csv(data_USD_MXN, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_MXN.csv")
write_csv(data_USD_HNL, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_HNL.csv")
write_csv(data_EUR_USD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_USD.csv")
write_csv(data_EUR_TND, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_TND.csv")
write_csv(data_EUR_RON, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RON.csv")
write_csv(data_EUR_RSD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RSD.csv")
write_csv(data_USD_MKD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_MKD.csv") 
write_csv(data_EUR_MXN, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MXN.csv")
write_csv(data_EUR_MKD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MKD.csv")
write_csv(data_EUR_MAD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MAD.csv")
write_csv(data_EUR_CNY, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_CNY.csv")
write_csv(data_EUR_HNL, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_HNL.csv")

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

data_USD_RON <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_RON.csv", header = TRUE, sep = ",")
 
# Afficher le nombre total de données manquantes
print(data_USD_RON)

data_EUR_RON <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RON.csv", header = TRUE, sep = ",")

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
print(tail(data_EUR_HNL) )

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
# Pour data_USD_MKD
print(data_USD_MKD)
missing_values_USD_MKD <- sapply(data_USD_MKD$Price, function(x) sum(is.na(x)))
print(missing_values_USD_MKD)
total_missing_USD_MKD <- sum(is.na(data_USD_MKD$Price))
print(total_missing_USD_MKD)

boxplot(data_USD_MKD$Price)
summary(data_USD_MKD$Price)
barplot(data_USD_MKD$Price)
hist(data_USD_MKD$Price)
par(mfrow = c(1, 2))
hist(data_USD_MKD$Price, main = "USD_MKD", xlab = "USD_MKD", col = "lightblue")


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
 
library(mice)
library(dplyr)
library(readr)  # For writing CSV files

# Function to identify and replace outliers with NA and impute missing values
remove_outliers_and_impute <- function(data, variables) {
      outlier_indices <- list()
      for (variable in variables) {
            # Identify indices of outliers
            outliers <- boxplot(data[[variable]], plot = FALSE)$out
            outlier_indices[[variable]] <- which(data[[variable]] %in% outliers)
            # Replace outliers with NA
            data[[variable]][outlier_indices[[variable]]] <- NA
      }
      # Perform imputation
      imputed_data <- complete(mice(data))
      return(imputed_data)
}

# List of datasets
datasets <- list(
      USD_MXN = data_USD_MXN,
      USD_MKD = data_USD_MKD,
      USD_HNL = data_USD_HNL,
      EUR_USD = data_EUR_USD,
      EUR_TND = data_EUR_TND,
      EUR_RSD = data_EUR_RSD,
      EUR_RON = data_EUR_RON,
      EUR_MXN = data_EUR_MXN,
      EUR_MKD = data_EUR_MKD,
      EUR_MAD = data_EUR_MAD,
      EUR_CNY = data_EUR_CNY,
      EUR_HNL = data_EUR_HNL,
      USD_RON = data_USD_RON
)

# Variable of interest
variables_of_interest <- c("Price")

# Process datasets
imputed_datasets <- list()
for (dataset_name in names(datasets)) {
      cat("Processing:", dataset_name, "\n")
      data <- datasets[[dataset_name]]
      
      # Execute the process twice
      for (i in 1:2) {
            data <- remove_outliers_and_impute(data, variables_of_interest)
      }
      
      imputed_datasets[[dataset_name]] <- data
      
      # Evaluate results
      summary(data)
      remaining_outliers <- which(data$Price %in% boxplot(data$Price, plot = FALSE)$out)
      cat("Number of remaining outliers in", dataset_name, ":", length(remaining_outliers), "\n\n")
}

# Save each imputed dataset to a CSV file
write_csv(imputed_datasets$USD_RON, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_RON.csv")
write_csv(imputed_datasets$USD_MXN, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_MXN.csv")
write_csv(imputed_datasets$USD_MKD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_MKD.csv")
write_csv(imputed_datasets$USD_HNL, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_USD_HNL.csv")
write_csv(imputed_datasets$EUR_USD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_USD.csv")
write_csv(imputed_datasets$EUR_TND, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_TND.csv")
write_csv(imputed_datasets$EUR_RSD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RSD.csv")
write_csv(imputed_datasets$EUR_RON, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RON.csv")
write_csv(imputed_datasets$EUR_MXN, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MXN.csv")
write_csv(imputed_datasets$EUR_MKD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MKD.csv")
write_csv(imputed_datasets$EUR_MAD, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_MAD.csv")
write_csv(imputed_datasets$EUR_CNY, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_CNY.csv")
write_csv(imputed_datasets$EUR_HNL, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_HNL.csv")
print(tail(imputed_datasets$EUR_HNL))
library(readr)
library(dplyr)
library(lubridate)

# Lire les fichiers CSV
eur_ron <- read_csv('C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RON.csv')
eur_rsd <- read_csv('C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_RSD.csv')

# Convertir la colonne 'Date' en format datetime pour les deux DataFrames
eur_ron$Date <- as.Date(eur_ron$Date, format = "%m/%d/%Y")
eur_rsd$Date <- as.Date(eur_rsd$Date, format = "%m/%d/%Y")
# Afficher les premières lignes des DataFrames pour vérifier les colonnes
print(head(eur_ron))
print(head(eur_rsd))

# Convertir la colonne 'Date' en format datetime pour les deux DataFrames
eur_ron$Date <- ymd(eur_ron$Date)
eur_rsd$Date <- ymd(eur_rsd$Date)

# Vérifier les dates min et max pour s'assurer qu'il y a des dates correspondantes
print(range(eur_ron$Date))
print(range(eur_rsd$Date))

# Vérifier les valeurs manquantes dans les colonnes 'Date'
print(sum(is.na(eur_ron$Date)))
print(sum(is.na(eur_rsd$Date)))


# Fusionner les deux DataFrames sur la colonne 'Date'
merged_df <- merge(eur_ron, eur_rsd, by = 'Date', suffixes = c('_RON', '_RSD'))

# Supprimer les lignes où 'Price_RON' ou 'Price_RSD' sont NA
merged_df <- merged_df %>% filter(!is.na(Price_RON) & !is.na(Price_RSD))


# Calculer le taux de change RON/RSD
merged_df$Price <- merged_df$Price_RON / merged_df$Price_RSD
print(merged_df)
# Calculer le taux de change RON/RSD
merged_df$Price <- merged_df$Price_RON / merged_df$Price_RSD
# Créer un nouveau DataFrame pour le fichier RON/RSD
ron_rsd_df <- merged_df %>% select(Date, Price )

# Sauvegarder le nouveau DataFrame dans un fichier CSV
write_csv(ron_rsd_df, 'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_RON_RSD.csv')
print(ron_rsd_df)



