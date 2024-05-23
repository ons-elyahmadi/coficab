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
data_oil <-  read.csv("data/oil.csv", header = TRUE, sep = ";")
data_lme <-  read.csv("data/LME.csv", header = TRUE, sep = ";")
head(data_oil)
head(data_lme)
# Convertir les colonnes de dates en format Date
data_oil$Dateoil <- as.Date(data_oil$Dateoil, format = "%d/%m/%Y")
data_lme$DATElme <- as.Date(data_lme$DATElme, format = "%d/%m/%Y")
data_lme$Official.Ask.LME <- as.numeric(gsub(",", ".", data_lme$Official.Ask.LME))
data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. <- as.numeric(gsub(",", ".", data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
head(data_oil)
head(data_lme)
# Fusionner les deux ensembles de données
merged_data <- merge(data_oil, data_lme, by.x = "Dateoil", by.y = "DATElme", all = TRUE)

# Compléter les dates manquantes
#merged_data_complete <- merged_data %>%
#      complete(DateOIL = seq(min(DateOIL), max(DateOIL), by = "day"))
write_csv(merged_data, "data/datacl.csv")

# Afficher les données complètes
print(merged_data)
View(merged_data)
dim(merged_data)
str(merged_data)
tail(merged_data)
glimpse(merged_data)
unique(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
unique(merged_data$Official.Ask.LME)
# Compter les données manquantes dans chaque colonne
missing_valueoils <- sapply(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueoils)
# Compter les données manquantes dans chaque colonne
missing_valueLMEs <- sapply(merged_data$Official.Ask.LME, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingLME <- sum(is.na(merged_data$Official.Ask.LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
attach(merged_data)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(Official.Ask.LME)
summary(merged_data$Official.Ask.LME)
boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
summary(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
barplot(merged_data$Official.Ask.LME)
barplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
hist(Official.Ask.LME)
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
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$Official.Ask.LME, use = "complete.obs")

# Affichage de la corrélation
print(correlation)

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_lme <- cut(merged_data$Official.Ask.LME, breaks = num_bins, labels = FALSE)
discretized_wti <- cut(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. , breaks = num_bins, labels = FALSE)

# Calculate mutual information
mutual_info <- mutinformation(discretized_lme, discretized_wti)

# Print the result
print(mutual_info)
# Nuage de points avec ligne de régression
plot(merged_data$Official.Ask.LME, merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., 
     xlab = "Official Ask LME USD TONNE", ylab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)")
abline(lm(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. ~ merged_data$Official.Ask.LME), col = "red")

# Histogrammes
par(mfrow = c(1, 2))
hist(merged_data$Official.Ask.LME, main = "Histogramme LME", xlab = "Official Ask LME USD TONNE", col = "lightblue")
hist(merged_data $Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., main = "Histogramme Cushing", xlab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)", col = "lightgreen")



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

 


 
view(merged_data) 
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$Official.Ask.LME, use = "complete.obs")

# Affichage de la corrélation
print(correlation)
which(merged_data$Official.Ask.LME %in% boxplot(Official.Ask.LME)$out)
which(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
# Assuming merged_data is your dataset

library(mice)

# Assuming merged_data is your dataset
# Step 1: Impute missing values using mice
imputed_data <- complete(mice(merged_data))

# Step 2: Identify aberrant values using boxplot or any other method for each variable of interest
variables_of_interest <- c("Official.Ask.LME", "Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.") # Add more variables as needed
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
total_missingLME <- sum(is.na(imputed_data$Official.Ask.LME))

# Afficher le nombre total de données manquantes
print(total_missingLME)
# Step 5: Evaluate the results
summary(imputed_data)



which(imputed_data$Official.Ask.LME %in% boxplot(imputed_data$Official.Ask.LME)$out)
which(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
view(imputed_data)
write_csv(imputed_data, "data/cleanindata.csv")
