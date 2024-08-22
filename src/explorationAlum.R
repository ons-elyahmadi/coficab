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
 
data_aluminum <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/aluminium_pricesLME.csv", header = TRUE, sep = ",")
head(data_aluminum)
 
 
data_aluminum$date <- as.Date(data_aluminum$date)
 
data_aluminum <- data_aluminum[order(data_aluminum$date), ]
 
head(data_aluminum)
 
 
# Afficher les données complètes
print(data_aluminum)
View(data_aluminum)
dim(data_aluminum)
str(data_aluminum)
tail(data_aluminum)
glimpse(data_aluminum)
 
unique(data_aluminum$price_aluminium_LME)
 
# Compter les données manquantes dans chaque colonne
missing_valueLMEs <- sapply(data_aluminum$price_aluminium_LME, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
 
total_missingLME <- sum(is.na(data_aluminum$price_aluminium_LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
attach(data_aluminum)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(data_aluminum$price_aluminium_LME)
summary(data_aluminum$price_aluminium_LME)
 
barplot(data_aluminum$price_aluminium_LME)
 
hist(data_aluminum$price_aluminium_LME)
 
# Calcul de la variation pour chaque variable
variation <- sapply(data_aluminum, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Affichage de la variation pour chaque variable
print(variation)
# Calcul de l'écart-type pour chaque variable
ecart_type <- sapply(data_aluminum, function(x) sd(x, na.rm = TRUE))

# Affichage de l'écart-type pour chaque variable
print(ecart_type)
 

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_Al <- cut(data_aluminum$price_aluminium_LME, breaks = num_bins, labels = FALSE)
 


 
view(data_aluminum) 
 
which(data_aluminum$price_aluminium_LME %in% boxplot(data_aluminum$price_aluminium_LME)$out)
 
# Assuming data_aluminum is your dataset

library(mice)
replace_outliers <- function(data, variables) {
      for (variable in variables) {
            outlier_indices <- which(data[[variable]] %in% boxplot(data[[variable]], plot = FALSE)$out)
            data[[variable]][outlier_indices] <- NA
      }
      return(data)
}
# Assuming data_aluminum is your dataset
# Step 1: Impute missing values using mice
imputed_data <- complete(mice(data_aluminum))

# Step 2: Identify aberrant values using boxplot or any other method for each variable of interest
variables_of_interest <- c("price_aluminium_LME") # Add more variables as needed
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data[[variable]] %in% boxplot(imputed_data[[variable]])$out)
}

# Step 3: Replace aberrant values with NA for each variable
for (variable in variables_of_interest) {
      imputed_data[[variable]][outlier_indices[[variable]]] <- NA
}
for (i in 1:3) {
      imputed_data <- replace_outliers(imputed_data, variables_of_interest)
      imputed_data <- complete(mice(imputed_data))
}
 
total_missingLME <- sum(is.na(imputed_data$price_aluminium_LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
# Step 5: Evaluate the results
summary(imputed_data)



which(imputed_data$price_aluminium_LME %in% boxplot(imputed_data$price_aluminium_LME)$out)
 
view(imputed_data)
write_csv(imputed_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/cleanindataAlum.csv")
