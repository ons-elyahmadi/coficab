# Charger les bibliothèques nécessaires
library(forecast)
library(tseries)
library(ggplot2)

# Lire les données
data <- read.csv("data/cleanindata.csv", header = TRUE)

# Filtrer les données pour inclure la période de janvier 2017 à janvier 2024
filtered_data <- subset(data, as.Date(Dateoil) >= as.Date("2017-01-01") & as.Date(Dateoil) <= as.Date("2024-01-31"))

# Convertir la colonne de date en format de date
filtered_data$Dateoil <- as.Date(filtered_data$Dateoil)

# Ajuster le modèle ARIMA avec la meilleure combinaison
fit <- Arima(filtered_data[,3], xreg = filtered_data[,2], order = c(0, 1, 1))

# Prévoir les 20 prochaines périodes
forecast <- forecast(fit, xreg = tail(filtered_data[, 2], 20), h = 20)

# Fonction pour obtenir la prochaine date ouvrable
next_business_day <- function(date) {
      date <- as.Date(date)
      while (format(date, "%u") %in% c("6", "7")) { # Samedi ou dimanche
            date <- date + 1
      }
      return(date)
}

# Générer les 20 prochaines dates ouvrables à partir du 1er février 2024
start_date <- as.Date("2024-02-01")
forecast_dates <- data.frame(Date = sapply(1:20, function(i) next_business_day(start_date + i - 1)), Forecast = forecast$mean)

# Convertir les dates au format de date
forecast_dates$Date <- as.Date(forecast_dates$Date)

# Afficher les prévisions avec les dates
print(forecast_dates)
