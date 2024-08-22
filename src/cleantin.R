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
# Charger les données 
data_tin <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/tin_pricesLME.csv", header = TRUE, sep = ",")
head(data_tin)
 
# Convertir les colonnes de dates en format Da
data_tin$date <- as.Date( data_tin$date  )
data_tin$price_tin_LME<- as.numeric(gsub(",", ".",data_tin$price_tin_LME))
head(data_tin)
 
  
write_csv(data_tin , "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/data_tin.csv")

# Afficher les données complètes
print(data_tin)
View(data_tin)
dim(data_tin)
str(data_tin)
tail(data_tin)
glimpse(data_tin)
unique(data_tin$price_tin_LME)
# Assuming merged_data is your dataset
library(mice) 
# Compter les données manquantes dans chaque colonne
missing_valueoils <- sapply(data_tin$price_tin_LME, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueoils)
# Compter les données manquantes dans chaque colonne
missing_valueLMEs <- sapply(data_tin$price_tin_LME , function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
total_missingoil <- sum(is.na(data_tin$price_tin_LME))

# Afficher le nombre total de données manquantes
print(total_missingoil)
 
attach(data_tin)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(data_tin$price_tin_LME)
summary(data_tin$price_tin_LME)
barplot(data_tin$price_tin_LME)
hist(data_tin$price_tin_LME)
 
# Calcul de la variation pour chaque variable
variation <- sapply(data_tin, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Affichage de la variation pour chaque variable
print(variation)
# Calcul de l'écart-type pour chaque variable
ecart_type <- sapply(data_tin, function(x) sd(x, na.rm = TRUE))

# Affichage de l'écart-type pour chaque variable
print(ecart_type)
 

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_tin <- cut(data_tin$price_tin_LME, breaks = num_bins, labels = FALSE)
 
 
# Histogrammes
par(mfrow = c(1, 2))
hist(  data_tin$price_tin_LME, main = "Histogramme TIN", xlab = "Official Ask LME USD  TIN", col = "lightblue")
 
 
which(data_tin$price_tin_LME %in% boxplot(data_tin$price_tin_LME)$out)
 
# Assuming merged_data is your dataset
# Outlier detection
outlier_indices <- which(data_tin$price_tin_LME %in% boxplot(data_tin$price_tin_LME)$out)

# Outlier handling and imputation
replace_outliers <- function(data, variable) {
      if (variable %in% colnames(data)) {
            boxplot_stats <- boxplot(data[[variable]], plot = FALSE)
            outlier_indices <- which(data[[variable]] %in% boxplot_stats$out)
            data[[variable]][outlier_indices] <- NA
      } else {
            warning(paste("Variable", variable, "is not in the dataset."))
      }
      return(data)
}

for (i in 1:3) {
      data_tin <- replace_outliers(data_tin, "price_tin_LME")
      imputed_data <- complete(mice(data_tin))
}





which(imputed_data$price_tin_LME %in% boxplot(imputed_data$price_tin_LME)$out)
view(imputed_data$price_tin_LME)
write_csv(imputed_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/cleanindataTin.csv")
