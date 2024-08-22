# Charger les packages nécessaires
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(reshape2) # for melt function
library(ggthemes)
library(infotheo) 

# Charger les données
data_oil <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/Cushing_OK_WTI_Spot_Price.csv", header = TRUE, sep = ",")
data_lme <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/copper_pricesLME.csv", header = TRUE, sep = ",")
head(data_oil)
head(data_lme)

# Convertir les colonnes de dates en format Date
data_oil$Date <- as.Date(data_oil$Date )
data_lme$date <- as.Date(data_lme$date)
data_lme$price_copper_LME <- as.numeric(gsub(",", ".", data_lme$price_copper_LME))
data_oil$Price <- as.numeric(gsub(",", ".", data_oil$Price))
head(data_oil)
head(data_lme)

# Fusionner les deux ensembles de données en utilisant left_join
merged_data <- left_join( data_lme , data_oil , by = c("date" = "Date"))
head(merged_data)
merged_data <- merged_data[order(merged_data$date), ]
head(merged_data)
# Compléter les dates manquantes
#merged_data_complete <- merged_data %>%
#      complete(Date = seq(min(Date), max(Date), by = "day"))
write_csv(merged_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/dataCOPPERlme.csv")

# Afficher les données complètes
print(merged_data)
View(merged_data)
dim(merged_data)
str(merged_data)
tail(merged_data)
glimpse(merged_data)
unique(merged_data$Price)
unique(merged_data$price_copper_LME)
# Compter les données manquantes dans chaque colonne
missing_valueoils <- sapply(merged_data$Price, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueoils)
# Compter les données manquantes dans chaque colonne
missing_valueLMEs <- sapply(merged_data$price_copper_LME, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
total_missingoil <- sum(is.na(merged_data$Price))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingLME <- sum(is.na(merged_data$price_copper_LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
attach(merged_data)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(price_copper_LME)
summary(merged_data$price_copper_LME)
boxplot(merged_data$Price)
summary(merged_data$Price)
barplot(merged_data$price_copper_LME)
barplot(merged_data$Price)
hist(price_copper_LME)
hist(Price)
# Calcul de la variation pour chaque variable
variation <- sapply(merged_data, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Affichage de la variation pour chaque variable
print(variation)
# Calcul de l'écart-type pour chaque variable
ecart_type <- sapply(merged_data, function(x) sd(x, na.rm = TRUE))

# Affichage de l'écart-type pour chaque variable
print(ecart_type)
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Price, merged_data$price_copper_LME, use = "complete.obs")

# Affichage de la corrélation
print(correlation)

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_lme <- cut(merged_data$price_copper_LME, breaks = num_bins, labels = FALSE)
discretized_wti <- cut(merged_data$Price , breaks = num_bins, labels = FALSE)

# Calculate mutual information
mutual_info <- mutinformation(discretized_lme, discretized_wti)

# Print the result
print(mutual_info)
# Nuage de points avec ligne de régression
plot(merged_data$price_copper_LME, merged_data$Price, 
     xlab = "Official Ask LME USD TONNE", ylab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)")
abline(lm(merged_data$Price ~ merged_data$price_copper_LME), col = "red")

# Histogrammes
par(mfrow = c(1, 2))
hist(merged_data$price_copper_LME, main = "Histogramme LME", xlab = "Official Ask LME USD TONNE", col = "lightblue")
hist(merged_data $Price, main = "Histogramme Cushing", xlab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)", col = "lightgreen")



# Diagramme en barres de l'information mutuelle
library(ggplot2)
data <- data.frame(discretized_lme = discretized_lme, discretized_wti = discretized_wti)
ggplot(data, aes(x = discretized_lme, fill = discretized_wti)) +
      geom_bar() +
      labs(x = "Official Ask LME USD TONNE (Discretized)", y = "Frequency", fill = "Cushing, OK WTI Spot Price FOB (Discretized)") +
      theme_minimal()

# Diagramme de dispersion de l'information mutuelle
plot(discretized_lme, discretized_wti, col = heat.colors(length(discretized_lme)), pch = 19, cex = 2)
library(zoo)
# Assuming 'df' is your dataframe with the time series data

# Find indices of NA values
na_indices <- which(is.na(merged_data$Price))
na_indices_lme <- which(is.na(merged_data$price_copper_LME))

 

total_missingoil <- sum(is.na(merged_data$Price))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingLME <- sum(is.na(merged_data$price_copper_LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
view(merged_data) 
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Price, merged_data$price_copper_LME, use = "complete.obs")

# Affichage de la corrélation
print(correlation)
which(merged_data$price_copper_LME %in% boxplot(price_copper_LME)$out)
which(merged_data$Price %in% boxplot(merged_data$Price)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
library(mice)

# Assuming merged_data is your dataset
# Initial imputation
imputed_data <- complete(mice(merged_data))

# Define the variables of interest
variables_of_interest <- c("price_copper_LME", "Price") # Add more variables as needed

# Function to identify and replace outliers
replace_outliers <- function(data, variables) {
      for (variable in variables) {
            # Ensure the variable exists in the data
            if (variable %in% colnames(data)) {
                  # Identify outliers using boxplot
                  boxplot_stats <- boxplot(data[[variable]], plot = FALSE)
                  outlier_indices <- which(data[[variable]] %in% boxplot_stats$out)
                  
                  # Replace outliers with NA
                  data[[variable]][outlier_indices] <- NA
            } else {
                  warning(paste("Variable", variable, "is not in the dataset."))
            }
      }
      return(data)
}

# Loop to replace outliers and re-run imputation 3 times
for (i in 1:3) {
      imputed_data <- replace_outliers(imputed_data, variables_of_interest)
      imputed_data <- complete(mice(imputed_data))
}

# Evaluate the results
summary(imputed_data)



which(imputed_data$price_copper_LME %in% boxplot(imputed_data$price_copper_LME)$out)
which(imputed_data$Price %in% boxplot(imputed_data$Price)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
view(imputed_data)

write_csv(imputed_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/cleaningdata.csv")

