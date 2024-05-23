library(forecast)

# Lire les données
data <- read.csv("data/cleaningdata.csv", header = TRUE)
head(data)
# Convertir la colonne de date au format de date approprié si elle n'est pas déjà au bon format
data$date <- as.Date(data$Dateoil)

# Définir la colonne de date comme noms de ligne si elle n'est pas déjà définie
rownames(data) <- data$date
data <- data[, -1]  # Suppression de la colonne de date car elle est désormais utilisée comme noms de ligne

# Convertir les données en série temporelle avec une fréquence quotidienne
data_ts <- ts(data[, 3], frequency = 5)  # Supposant une année avec 365 jours

# Use auto.arima to fit the model
fit <- auto.arima(data_ts, xreg = data[, 2], seasonal = FALSE)

# Forecast the next 5 days
forecast <- forecast(fit, xreg = tail(data[, 2], 30), h = 30)

# Print the forecast
print(forecast)
# Calculer les résidus du modèle
residuals <- resid(fit)

# Calculer les prévisions pour les données d'entraînement
training_forecast <- fitted(fit)

# Calculer les mesures d'évaluation
rmse <- sqrt(mean(residuals^2))
mae <- mean(abs(residuals))
correlation <- cor(data_ts, training_forecast)

# Afficher les résultats
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("Corrélation entre les observations et les prévisions:", correlation, "\n")

 