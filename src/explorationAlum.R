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
data_oil <-  read.csv("data/oildata.csv", header = TRUE, sep = ";")
data_aluminum <-  read.csv("data/aluminum.csv", header = TRUE, sep = ";")
head(data_aluminum)
head(data_lme)
# Convertir les colonnes de dates en format Date
data_oil$Dateoil <- as.Date(data_oil$Dateoil, format = "%d/%m/%Y")
data_aluminum$Date <- as.Date(data_aluminum$Date, format = "%d/%m/%Y")
data_aluminum$Alumnim...ton<- as.numeric(gsub(",", ".", data_aluminum$Alumnim...ton))
data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. <- as.numeric(gsub(",", ".", data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
head(data_oil)
head(data_aluminum)
# Fusionner les deux ensembles de données
merged_data <- merge(data_oil, data_aluminum, by.x = "Dateoil", by.y = "Date", all = TRUE)

# Compléter les dates manquantes
#merged_data_complete <- merged_data %>%
#      complete(DateOIL = seq(min(DateOIL), max(DateOIL), by = "day"))
write_csv(merged_data, "data/dataAl.csv")

# Afficher les données complètes
print(merged_data)
View(merged_data)
dim(merged_data)
str(merged_data)
tail(merged_data)
glimpse(merged_data)
unique(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
unique(merged_data$Alumnim...ton)
# Compter les données manquantes dans chaque colonne
missing_valueoils <- sapply(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueoils)
# Compter les données manquantes dans chaque colonne
missing_valueLMEs <- sapply(merged_data$Alumnim...ton, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingLME <- sum(is.na(merged_data$Alumnim...ton))

# Afficher le nombre total de données manquantes
print(total_missingLME)
attach(merged_data)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(merged_data$Alumnim...ton)
summary(merged_data$Alumnim...ton)
boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
summary(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
barplot(merged_data$Alumnim...ton)
barplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
hist(merged_data$Alumnim...ton)
hist(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
# Calcul de la variation pour chaque variable
variation <- sapply(merged_data, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

# Affichage de la variation pour chaque variable
print(variation)
# Calcul de l'écart-type pour chaque variable
ecart_type <- sapply(merged_data, function(x) sd(x, na.rm = TRUE))

# Affichage de l'écart-type pour chaque variable
print(ecart_type)
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.,  merged_data$Alumnim...ton, use = "complete.obs")

# Affichage de la corrélation
print(correlation)

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_Al <- cut(merged_data$Alumnim...ton, breaks = num_bins, labels = FALSE)
discretized_wti <- cut(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. , breaks = num_bins, labels = FALSE)

# Calculate mutual information
mutual_info <- mutinformation(discretized_Al, discretized_wti)

# Print the result
print(mutual_info)
# Nuage de points avec ligne de régression
plot(merged_data$Alumnim...ton, merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., 
     xlab = "Official Ask LME USD TONNE Alum", ylab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)")
abline(lm(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. ~  merged_data$Alumnim...ton), col = "red")

# Histogrammes
par(mfrow = c(1, 2))
hist( merged_data$Alumnim...ton, main = "Histogramme LME Alum", xlab = "Official Ask LME USD TONNE Alum", col = "lightblue")
hist(merged_data $Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., main = "Histogramme Cushing", xlab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)", col = "lightgreen")



# Diagramme en barres de l'information mutuelle
library(ggplot2)
data <- data.frame(discretized_Al = discretized_Al, discretized_wti = discretized_wti)
ggplot(data, aes(x = discretized_Al, fill = discretized_wti)) +
      geom_bar() +
      labs(x = "Official Ask LME USD TONNE (Discretized) Alum ", y = "Frequency", fill = "Cushing, OK WTI Spot Price FOB (Discretized)") +
      theme_minimal()

# Diagramme de dispersion de l'information mutuelle
plot(discretized_Al, discretized_wti, col = heat.colors(length(discretized_Al)), pch = 19, cex = 2)
library(zoo)
# Assuming 'df' is your dataframe with the time series data





view(merged_data) 
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.,  merged_data$Alumnim...ton, use = "complete.obs")

# Affichage de la corrélation
print(correlation)
which(merged_data$Alumnim...ton %in% boxplot(merged_data$Alumnim...ton)$out)
which(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
# Assuming merged_data is your dataset

library(mice)

# Assuming merged_data is your dataset
# Step 1: Impute missing values using mice
imputed_data <- complete(mice(merged_data))

# Step 2: Identify aberrant values using boxplot or any other method for each variable of interest
variables_of_interest <- c("Alumnim...ton", "Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.") # Add more variables as needed
outlier_indices <- list()
for (variable in variables_of_interest) {
      outlier_indices[[variable]] <- which(imputed_data[[variable]] %in% boxplot(imputed_data[[variable]])$out)
}

# Step 3: Replace aberrant values with NA for each variable
for (variable in variables_of_interest) {
      imputed_data[[variable]][outlier_indices[[variable]]] <- NA
}

# Step 4: Re-run the imputation process
imputed_data <- complete(mice(imputed_data))
total_missingoil <- sum(is.na(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingLME <- sum(is.na(imputed_data$Alumnim...ton))

# Afficher le nombre total de données manquantes
print(total_missingLME)
# Step 5: Evaluate the results
summary(imputed_data)



which(imputed_data$Alumnim...ton %in% boxplot(imputed_data$Alumnim...ton)$out)
which(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
view(imputed_data)
write_csv(imputed_data, "data/cleanindataAlum.csv")
