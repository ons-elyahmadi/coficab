# Charger les bibliothèques nécessaires
library(forecast)
library(tseries)
library(ggplot2)
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(Metrics)
 

# Lire les données
data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/cleanindataTin.csv", header = TRUE)
head(data)

data <- data[order(data$date), ]
head(data)
##data <- head(data, n = nrow(data) - 3)
# Ajuster le modèle ARIMA avec le meilleur ordre
fit <- Arima(data[,2], order = c(2, 1, 0))

# Ajuster les marges de la figure
par(mar = c(1, 4, 4, 2) + 0.1)

# Diagramme de diagnostic
tsdiag(fit)

# Prévision pour les 22 prochaines périodes
forecast <- forecast(fit, h = 3)

# Imprimer les 18 dernières valeurs des données réelles
print(tail(data[, 2], 18))

# Imprimer les prévisions
print(forecast)

# Liste des jours fériés de la LME pour 2024
public_holidays <- as.Date(c(
      "2024-03-29", "2024-03-28", # Good Friday
      "2024-04-01",              # Easter Monday
      "2024-05-06", "2024-05-07", # Early May Bank Holiday
      "2024-05-27", "2024-05-28", # Spring Bank Holiday
      "2024-08-26",              # Summer Bank Holiday
      "2024-12-25", "2024-12-27", # Christmas Day
      "2024-12-26", "2024-12-27", # Boxing Day
      "2025-01-01", "2025-01-02"  # New Year's Day
))

# Générer une séquence de dates pour les périodes prévisionnelles excluant les week-ends et les jours fériés
start_date <- as.Date(tail(data[, 1], 1)) + 1 # Incrémenter la date de début d'un jour
forecast_dates <- seq.Date(start_date, by = "day", length.out = 100) # Générer plus de dates initialement

# Filtrer les week-ends (samedi et dimanche) et les jours fériés
forecast_dates <- forecast_dates[!(weekdays(forecast_dates) %in% c("Saturday", "Sunday"))]
forecast_dates <- forecast_dates[!forecast_dates %in% public_holidays]

# Prendre uniquement le nombre nécessaire de dates pour les périodes prévisionnelles
forecast_dates <- forecast_dates[1:length(forecast$mean)]

# Combiner les valeurs prévisionnelles avec les dates
forecasted_data <- data.frame(date = as.Date(forecast_dates), Forecast = as.numeric(forecast$mean))
print(forecasted_data)
forecasted_data<-  forecasted_data[order(forecasted_data$date), ]
# Sauvegarde des données prévisionnelles dans un fichier CSV
write.csv(forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_22data_TIN.csv", row.names = FALSE)

# Vérifier si le répertoire 'data' existe, sinon le créer
if (!dir.exists("data")) {
      dir.create("data")
}

# Charger les anciennes données prévisionnelles
if (file.exists("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesTIN.csv")) {
      old_forecasted_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesTIN.csv", header = TRUE)
      
      # Convertir la colonne Date de old_forecasted_data en type Date
      old_forecasted_data$date <- as.Date(old_forecasted_data$date)
      
      # Combiner les anciennes données avec les nouvelles
      combined_forecasted_data <- bind_rows(old_forecasted_data, forecasted_data)
      
      # Éliminer les doublons basés sur la colonne "Date"
      combined_forecasted_data <- combined_forecasted_data %>% distinct(date, .keep_all = TRUE)
      combined_forecasted_data<-   combined_forecasted_data[order(combined_forecasted_data$date), ]
      # Enregistrer les données combinées dans un fichier CSV
      write.csv(combined_forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesTIN.csv", row.names = FALSE)
} else {
      # Si le fichier n'existe pas, enregistrer les nouvelles données
      write.csv(forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesTIN.csv", row.names = FALSE)
}

# Lire les données prévisionnelles combinées
forecasted_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesTIN.csv", header = TRUE)
print(forecasted_data)

data$date <- as.Date(data$date)
forecasted_data$date <- as.Date(forecasted_data$date)

# Extraire les 22 derniers jours de données réelles
last_22_real <- tail(data[1:2], 9)
print(last_22_real)
last_22_real <-  last_22_real %>% filter(date %in% forecasted_data$date)
print(last_22_real)

# Filtrer les prévisions pour correspondre aux dates des 22 derniers jours
last_22_forecast <- forecasted_data %>% filter(date %in% last_22_real$date)
print(last_22_real)
print(last_22_forecast)

# Combinaison des 22 dernières données réelles et des 22 dernières prévisions
combined_data <- cbind(last_22_real, last_22_forecast$Forecast)

# Renommer la colonne de prévisions
colnames(combined_data)[3] <- "Forecast"

# Sauvegarde dans un fichier CSV spécifique
write.csv(combined_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/last_real_and_forecasted_values_TIN.csv", row.names = FALSE)
print(combined_data)

# Calculer les métriques
mae_value <- mae(last_22_real[, 2], last_22_forecast$Forecast)
rmse_value <- rmse(last_22_real[, 2], last_22_forecast$Forecast)
r2_value <- cor(last_22_real[, 2], last_22_forecast$Forecast)^2
pmae_value <- sum(abs(last_22_real[, 2] - last_22_forecast$Forecast)) / sum(abs(last_22_real[, 2]))
mse_value <- mse(last_22_real[, 2], last_22_forecast$Forecast)
mape_value <- mape(last_22_real[, 2], last_22_forecast$Forecast)

# Extraire la dernière date des prévisions
last_forecast_date <- tail(last_22_forecast$date, 1)

# Afficher les résultats
cat("MAE:", mae_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("R²:", r2_value, "\n")
cat("PMAE:", pmae_value, "\n")
cat("MSE:", mse_value, "\n")
cat("MAPE:", mape_value, "\n")
cat("Last Forecast Date:", last_forecast_date, "\n")

# Création d'un data frame pour les métriques avec la dernière date des prévisions
metrics_data <- data.frame(Date = last_forecast_date,
                           MAE = mae_value,
                           RMSE = rmse_value,
                           MSE = mse_value,
                           MAPE = mape_value)

# Sauvegarde des métriques dans un fichier spécifique
write.csv(metrics_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/metrics_TIN.csv", row.names = FALSE)

