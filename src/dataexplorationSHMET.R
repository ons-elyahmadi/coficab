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
data_oil <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/oil.csv", header = TRUE, sep = ";")
data_shmet <-  read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/SHMET.csv", header = TRUE, sep = ";")
head(data_oil)
head(data_shmet)
# Convertir les colonnes de dates en format Date
data_oil$Dateoil <- as.Date(data_oil$Dateoil, format = "%d/%m/%Y")
data_shmet$Date <- as.Date(data_shmet$Date , format = "%d/%m/%Y")
data_shmet$CopperSHMET <- as.numeric(gsub(",", ".",  data_shmet$CopperSHMET ))
data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. <- as.numeric(gsub(",", ".", data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
head(data_oil)
head(data_shmet)
# Fusionner les deux ensembles de données
merged_data <- merge(data_oil, data_shmet , by.x = "Dateoil", by.y = "Date", all = TRUE)
head(merged_data)
merged_data <- merged_data[order(merged_data$Dateoil), ]
head(merged_data)
# Compléter les dates manquantes
#merged_data_complete <- merged_data %>%
#      complete(DateOIL = seq(min(DateOIL), max(DateOIL), by = "day"))
write_csv(merged_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/dataclshmet.csv")

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
missing_valueLMEs <- sapply(merged_data$CopperSHMET, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueLMEs)
total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingSHMET <- sum(is.na(merged_data$CopperSHMET))

# Afficher le nombre total de données manquantes
print(total_missingSHMET)
attach(merged_data)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(CopperSHMET)
summary(merged_data$CopperSHMET)
boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
summary(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
barplot(merged_data$CopperSHMET)
barplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
hist(CopperSHMET)
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
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$CopperSHMET, use = "complete.obs")

# Affichage de la corrélation
print(correlation)

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_CopperSHMET <- cut(merged_data$CopperSHMET, breaks = num_bins, labels = FALSE)
discretized_wti <- cut(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. , breaks = num_bins, labels = FALSE)

# Calculate mutual information
mutual_info <- mutinformation(discretized_CopperSHMET, discretized_wti)

# Print the result
print(mutual_info)
# Nuage de points avec ligne de régression
plot(merged_data$CopperSHMET, merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., 
     xlab = "Official CopperSHMET USD TONNE", ylab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)")
abline(lm(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. ~ merged_data$CopperSHMET), col = "red")

# Histogrammes
par(mfrow = c(1, 2))
hist(merged_data$CopperSHMET, main = "Histogramme CopperSHMET", xlab = "Official CopperSHMET USD TONNE", col = "lightblue")
hist(merged_data $Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., main = "Histogramme Cushing", xlab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)", col = "lightgreen")



# Diagramme en barres de l'information mutuelle
library(ggplot2)
data <- data.frame(discretized_CopperSHMET = discretized_CopperSHMET, discretized_wti = discretized_wti)
ggplot(data, aes(x = discretized_CopperSHMET, fill = discretized_wti)) +
      geom_bar() +
      labs(x = "Official  CopperSHMETUSD TONNE (Discretized)", y = "Frequency", fill = "Cushing, OK WTI Spot Price FOB (Discretized)") +
      theme_minimal()

# Diagramme de dispersion de l'information mutuelle
plot(discretized_CopperSHMET, discretized_wti, col = heat.colors(length(discretized_CopperSHMET)), pch = 19, cex = 2)
library(zoo)
# Assuming 'df' is your dataframe with the time series data

# Find indices of NA values
na_indices <- which(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
na_indices_CopperSHMET <- which(is.na(merged_data$CopperSHMET))

# Iteration sur les indices NA pour la première colonne
for (i in na_indices) {
      # Vérifier si la valeur précédente est disponible
      if (!is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i - 1]) &  !is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i + 1])) {
            # Remplacer NA par la moyenne de la valeur précédente et de la valeur suivante
            merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i] <- (merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i - 1] + merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i + 1]) / 2
      } else if (!is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i + 1] ) &  is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i] <- merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i + 1]
      }
      else if (is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i + 1] ) &  !is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i] <- merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.[i - 1]
      }
}

# Iteration sur les indices NA pour la deuxième colonne
for (i in na_indices_CopperSHMET) {
      # Vérifier si la valeur précédente est disponible
      if (!is.na(merged_data$CopperSHMET[i - 1]) & !is.na(merged_data$CopperSHMET[i + 1])) {
            # Remplacer NA par la moyenne de la valeur précédente et de la valeur suivante
            merged_data$CopperSHMET[i] <- (merged_data$CopperSHMET[i - 1] + merged_data$CopperSHMET[i + 1]) / 2
      } else if (!is.na(merged_data$CopperSHMET[i + 1]) & is.na(merged_data$CopperSHMET[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$CopperSHMET[i] <- merged_data$CopperSHMET[i + 1]
      }
      else if (is.na(merged_data$CopperSHMET[i + 1]) & !is.na(merged_data$CopperSHMET[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$CopperSHMET[i] <- merged_data$CopperSHMET[i - 1]
      }
}


total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingCopperSHMET <- sum(is.na(merged_data$CopperSHMET))

# Afficher le nombre total de données manquantes
print(total_missingCopperSHMET)
view(merged_data) 
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$CopperSHMET, use = "complete.obs")

# Affichage de la corrélation
print(correlation)
which(merged_data$CopperSHMET %in% boxplot(CopperSHMET)$out)
which(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
# Assuming merged_data is your dataset
library(mice)

# Assuming merged_data is your dataset
# Step 1: Impute missing values using mice
imputed_data <- complete(mice(merged_data))

# Define the variables of interest
variables_of_interest <- c("CopperSHMET", "Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.") # Add more variables as needed

# Function to identify and replace outliers
replace_outliers <- function(data, variables) {
      for (variable in variables) {
            # Check if the variable exists in the data
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



which(imputed_data$CopperSHMET %in% boxplot(imputed_data$CopperSHMET)$out)
which(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
view(imputed_data)
write_csv(imputed_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/cleaningdataCopperSHMET.csv")

