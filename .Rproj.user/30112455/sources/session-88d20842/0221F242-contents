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
data_oil <-  read.csv("data/oil.csv", header = TRUE, sep = ";")
data_COMEX <-  read.csv("data/COMEX.csv", header = TRUE, sep = ";")
head(data_oil)
head(data_COMEX)
# Convertir les colonnes de dates en format Date
data_oil$Dateoil <- as.Date(data_oil$Dateoil, format = "%d/%m/%Y")
data_COMEX$DATE <- as.Date(data_COMEX$DATE , format = "%d/%m/%Y")
data_COMEX$COMEX <- as.numeric(gsub(",", ".",  data_COMEX$COMEX ))
data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. <- as.numeric(gsub(",", ".", data_oil$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
head(data_oil)
head(data_COMEX)
# Fusionner les deux ensembles de données
merged_data <- merge(data_oil, data_COMEX , by.x = "Dateoil", by.y = "DATE", all = TRUE)

# Compléter les dates manquantes
#merged_data_complete <- merged_data %>%
#      complete(DateOIL = seq(min(DateOIL), max(DateOIL), by = "day"))
write_csv(merged_data, "data/dataclCOMEX.csv")

# Afficher les données complètes
print(merged_data)
View(merged_data)
dim(merged_data)
str(merged_data)
tail(merged_data)
glimpse(merged_data)
unique(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
unique(merged_data$COMEX)
# Compter les données manquantes dans chaque colonne
missing_valueoils <- sapply(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueoils)
# Compter les données manquantes dans chaque colonne
missing_valueCOMEX <- sapply(merged_data$COMEX, function(x) sum(is.na(x)))

# Afficher le nombre de données manquantes dans chaque colonne
print(missing_valueCOMEX)
total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingCOMEX <- sum(is.na(merged_data$COMEX))

# Afficher le nombre total de données manquantes
print(total_missingCOMEX)
attach(merged_data)
# Tracer des boîtes à moustaches pour chaque colonne
boxplot(COMEX)
summary(merged_data$COMEX)
boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
summary(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
barplot(merged_data$COMEX)
barplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
hist(COMEX)
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
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$COMEX, use = "complete.obs")

# Affichage de la corrélation
print(correlation)

# Define the number of bins for discretization
num_bins <- 10

# Discretize the variables
discretized_COMEX <- cut(merged_data$COMEX, breaks = num_bins, labels = FALSE)
discretized_wti <- cut(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. , breaks = num_bins, labels = FALSE)

# Calculate mutual information
mutual_info <- mutinformation(discretized_COMEX, discretized_wti)

# Print the result
print(mutual_info)
# Nuage de points avec ligne de régression
plot(merged_data$COMEX, merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., 
     xlab = "Official COMEX USD TONNE", ylab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)")
abline(lm(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. ~ merged_data$COMEX), col = "red")

# Histogrammes
par(mfrow = c(1, 2))
hist(merged_data$COMEX, main = "Histogramme  COMEX", xlab = "Official COMEX USD TONNE", col = "lightblue")
hist(merged_data $Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., main = "Histogramme Cushing", xlab = "Cushing, OK WTI Spot Price FOB (Dollars per Barrel)", col = "lightgreen")



# Diagramme en barres de l'information mutuelle
library(ggplot2)
data <- data.frame(discretized_COMEX = discretized_CopperSHMET, discretized_wti = discretized_wti)
ggplot(data, aes(x = discretized_COMEX, fill = discretized_wti)) +
      geom_bar() +
      labs(x = "Official  CopperSHMETUSD TONNE (Discretized)", y = "Frequency", fill = "Cushing, OK WTI Spot Price FOB (Discretized)") +
      theme_minimal()

# Diagramme de dispersion de l'information mutuelle
plot(discretized_COMEX, discretized_wti, col = heat.colors(length(discretized_COMEX)), pch = 19, cex = 2)
library(zoo)
# Assuming 'df' is your dataframe with the time series data

# Find indices of NA values
na_indices <- which(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))
na_indices_COMEX <- which(is.na(merged_data$COMEX))

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
for (i in na_indices_COMEX) {
      # Vérifier si la valeur précédente est disponible
      if (!is.na(merged_data$COMEX[i - 1]) & !is.na(merged_data$COMEX[i + 1])) {
            # Remplacer NA par la moyenne de la valeur précédente et de la valeur suivante
            merged_data$COMEX[i] <- (merged_data$COMEX[i - 1] + merged_data$COMEX[i + 1]) / 2
      } else if (!is.na(merged_data$COMEX[i + 1]) & is.na(merged_data$COMEX[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$CopperSHMET[i] <- merged_data$CopperSHMET[i + 1]
      }
      else if (is.na(merged_data$COMEX[i + 1]) & !is.na(merged_data$COMEX[i - 1])) {
            # Si la valeur précédente est manquante mais la valeur suivante est disponible, remplacer par la valeur suivante
            merged_data$COMEX[i] <- merged_data$COMEX[i - 1]
      }
}


total_missingoil <- sum(is.na(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.))

# Afficher le nombre total de données manquantes
print(total_missingoil)
total_missingCOMEX <- sum(is.na(merged_data$COMEX))

# Afficher le nombre total de données manquantes
print(total_missingCOMEX)
view(merged_data) 
# Calcul de la corrélation entre les deux variables
correlation <- cor(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., merged_data$COMEX, use = "complete.obs")

# Affichage de la corrélation
print(correlation)
which(merged_data$COMEX %in% boxplot(COMEX)$out)
which(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(merged_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
# Assuming merged_data is your dataset

library(mice)

# Assuming merged_data is your dataset
# Step 1: Impute missing values using mice
imputed_data <- complete(mice(merged_data))

# Step 2: Identify aberrant values using boxplot or any other method for each variable of interest
variables_of_interest <- c("COMEX", "Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.") # Add more variables as needed
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

# Step 5: Evaluate the results
summary(imputed_data)



which(imputed_data$COMEX %in% boxplot(imputed_data$COMEX)$out)
which(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. %in% boxplot(imputed_data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)$out)
# Interpolation linéaire pour remplacer les valeurs aberrantes
view(imputed_data)
write_csv(imputed_data, "data/cleaningdataCopperCOMEX.csv")
