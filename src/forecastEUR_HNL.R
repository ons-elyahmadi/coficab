# Charger les bibliothèques nécessaires
library(forecast)
library(tseries)
library(ggplot2)
library(rvest)
library(dplyr)
library(lubridate)
library(stringr)

# URL de la page à scraper
url <- 'https://www.ecb.europa.eu/ecb/contacts/working-hours/html/index.fr.html'

# Lire le contenu HTML de l'URL
webpage <- read_html(url)

# Extraire les dates des éléments <td>
dates <- webpage %>%
      html_nodes('table tbody tr td span') %>%
      html_text() %>%
      str_trim()  # Supprimer les espaces supplémentaires

# Convertir les dates en objets Date
dates_converted <- dmy(dates)

# Afficher les dates converties
print(dates_converted)

# Lire les données
data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/data_EUR_HNL.csv", header = TRUE)
head(data)
data$Date <- as.Date(data$Date)
data <- data[order(data$Date),  ]
##data <- head(data, n = nrow(data) - 22)
head(data)
print(tail(data)) 
# Ajuster le modèle ARIMA avec le meilleur ordre
fit <-  Arima(data[,2]  , order = c(3, 1, 3))

# Ajuster les marges de la figure
par(mar = c(1, 4, 4, 2) + 0.1)

# Diagramme de diagnostic
tsdiag(fit)

# Prévision pour les 22 prochaines périodes
forecast <- forecast(fit, h = 22)

# Imprimer les 18 dernières valeurs des données réelles
print(tail(data[, 2], 18))

# Imprimer les prévisions
print(forecast)

# Liste des jours fériés européens pour 2024
public_holidays <- dates_converted

# Générer une séquence de dates pour les périodes prévisionnelles excluant les week-ends et les jours fériés
start_date <- as.Date(tail(data[, 1], 1)) + 1 # Incrémenter la date de début d'un jour
forecast_dates <- seq.Date(start_date, by = "day", length.out = 100) # Générer plus de dates initialement

# Filtrer les week-ends (samedi et dimanche) et les jours fériés
forecast_dates <- forecast_dates[!(weekdays(forecast_dates) %in% c("samedi", "dimanche"))]
 

# Prendre uniquement le nombre nécessaire de dates pour les périodes prévisionnelles
forecast_dates <- forecast_dates[1:length(forecast$mean)]

# Combiner les valeurs prévisionnelles avec les dates
forecasted_data <- data.frame(Date = as.Date(forecast_dates), Forecast = as.numeric(forecast$mean))
forecasted_data<-  forecasted_data[order(forecasted_data$Date), ]
print(forecasted_data)
write.csv(forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_22data_EUR_HNL.csv", row.names = FALSE)
# Vérifier si le répertoire 'data' existe, sinon le créer
if (!dir.exists("data")) {
      dir.create("data")
}

# Charger les anciennes données prévisionnelles
if (file.exists("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv")) {
      old_forecasted_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv", header = TRUE)
      
      # Convertir la colonne Date de old_forecasted_data en type Date
      old_forecasted_data$Date <- as.Date(old_forecasted_data$Date)
      
      # Combiner les anciennes données avec les nouvelles
      combined_forecasted_data <- bind_rows(old_forecasted_data, forecasted_data)
      
      # Éliminer les doublons basés sur la colonne "Date"
      combined_forecasted_data <- combined_forecasted_data %>% distinct(Date, .keep_all = TRUE)
      combined_forecasted_data<-   combined_forecasted_data[order(combined_forecasted_data$Date), ]
      # Enregistrer les données combinées dans un fichier CSV
      write.csv(combined_forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv", row.names = FALSE)
} else {
      # Si le fichier n'existe pas, enregistrer les nouvelles données
      write.csv(forecasted_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv", row.names = FALSE)
}

# Lire les données prévisionnelles combinées
daforcasted <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv", header = TRUE)
print(daforcasted)

# Lire les données prévisionnelles combinées
forecasted_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/forecasted_valuesEUR_HNL.csv", header = TRUE)
print(forecasted_data)
data$Date <- as.Date(data$Date)
forecasted_data$Date <- as.Date(forecasted_data$Date)
library(forecast)
library(tseries)
library(ggplot2)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(Metrics)

# Extraire les 22 derniers jours de données réelles
last_22_real <- tail(data[1:2], 22)

# Filtrer les prévisions pour correspondre aux dates des 22 derniers jours
 
last_22_real <-  last_22_real %>% filter(Date %in% forecasted_data$Date)
last_22_forecast <- forecasted_data %>% filter(Date %in% last_22_real$Date)
print(last_22_real)
print(last_22_forecast)

# Combinaison des 22 dernières données réelles et des 22 dernières prévisions
combined_data <- cbind(last_22_real, last_22_forecast$Forecast)

# Renommer la colonne de prévisions
colnames(combined_data)[3] <- "Forecast"

# Sauvegarde dans un fichier CSV spécifique
write.csv(combined_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/last_real_and_forecasted_values_EUR_HNL.csv", row.names = FALSE)
print(combined_data)
# Calculer les métriques
mae_value <- mae(last_22_real[,2], last_22_forecast$Forecast)
rmse_value <- rmse(last_22_real[,2], last_22_forecast$Forecast)
r2_value <- cor(last_22_real[,2], last_22_forecast$Forecast)^2
pmae_value <- sum(abs(last_22_real[,2] - last_22_forecast$Forecast)) / sum(abs(last_22_real[,2]))
mse_value <- mse(last_22_real[,2], last_22_forecast$Forecast)
mape_value <- mape(last_22_real[,2], last_22_forecast$Forecast)

# Extraire la dernière date des prévisions
last_forecast_date <- tail(last_22_forecast$Date, 1)

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
write.csv(metrics_data, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/metrics_EUR_HNL.csv", row.names = FALSE)

