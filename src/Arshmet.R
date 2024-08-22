# Load necessary libraries
library(forecast)
library(tseries)
library(ggplot2)
# Read the data
data <- read.csv("data/cleaningdataCopperSHMET.csv", header = TRUE)
head(data)


# Attach the data
attach(data)

# Check the data


head(data)
# Convertir la colonne Dateoil en format de date
data$Dateoil <- as.Date(data$Dateoil)

# Tracer le graphique
ggplot(data, aes(x = Dateoil, y = CopperSHMET)) +
      geom_line() +
      labs(x = "Dateoil", y = "Prix du Cuivre (SHMET)", title = "Prix du Cuivre (SHMET) en fonction de la date du pétrole")


# Plot the variables
plot(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
plot(Official.Ask.LME)
 

# Plot Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. against Dateoil
plot(data$Dateoil, data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., type="l", col="blue",
     xlab="Date", ylab="WTI Spot Price (Dollars per Barrel)",
     main="WTI Spot Price Over Time")

# Plot Official.Ask.LME against Dateoil
plot(data$Dateoil, data$CopperSHMET, type="l", col="red",
     xlab="Date", ylab="Official CopperSHMET",
     main="Official Ask SHMET Over Time")
# Supposons que data est déjà chargé et Dateoil est converti en classe Date

# Ajouter une colonne pour l'année
data$Year <- format(data$Dateoil, "%Y")

# Calculer la moyenne des prix pour chaque année
mean_wti_per_year <- aggregate(data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., 
                               by = list(data$Year), 
                               FUN = mean, na.rm = TRUE)
colnames(mean_wti_per_year) <- c("Year", "Mean_WTI_Spot_Price")

mean_shmet_per_year <- aggregate(data$CopperSHMET, 
                               by = list(data$Year), 
                               FUN = mean, na.rm = TRUE)
colnames(mean_shmet_per_year) <- c("Year", "Mean_Official_SHMET")

# Afficher les moyennes par année
print(mean_wti_per_year)
print(mean_shmet_per_year)

# Calculer la moyenne générale du prix au comptant WTI
mean_wti_spot_price <- mean(data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., na.rm = TRUE)

# Calculer la moyenne générale du prix officiel LME
mean_official_ask_shmet <- mean(data$CopperSHMET, na.rm = TRUE)

# Afficher les moyennes générales
print(paste("Moyenne générale du prix au comptant WTI: ", mean_wti_spot_price))
print(paste("Moyenne générale du prix officiel SHMET: ", mean_official_ask_shmet))
# Installer et charger le package urca pour le test KPSS

library(urca)

# Exécuter le test KPSS sur les séries temporelles
kpss_test_wti <- ur.kpss(data$Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
summary(kpss_test_wti)

kpss_test_shmet <- ur.kpss(data$CopperSHMET)
summary(kpss_test_shmet)
# Plot the variables
plot(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
plot(CopperSHMET)

# Check stationarity
adf.test(CopperSHMET, alternative = "stationary")
adf.test(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., alternative = "stationary")
# Compute ACF and PACF
acf(CopperSHMET)
pacf(CopperSHMET)
# Take differences for stationarity
d.CopperSHMET <- diff(CopperSHMET)
d.Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel. <- diff(Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)

# Plot differenced variables
plot(d.Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel.)
plot(d.CopperSHMET)

# Check stationarity of differenced series
adf.test(d.CopperSHMET, alternative = "stationary")
adf.test(d.Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., alternative = "stationary")

# Compute ACF and PACF
acf(d.CopperSHMET)
pacf(d.CopperSHMET)

Arima(data[,3] ,  xreg = data[, 2] , order = c(1, 1, 0))
Arima(data[,3] ,  xreg = data[, 2], order = c(2, 1, 0))
Arima(data[,3] ,  xreg = data[, 2], order = c(1, 1, 1))
Arima(data[,3] ,  xreg = data[, 2], order = c(0, 1, 1))
Arima(data[,3],  xreg =  data[, 2], order = c(0, 1, 2))
Arima(data[,3],  xreg =data[, 2] , order = c(2, 1, 1))
Arima(data[,3] , xreg = data[, 2] , order = c(1, 1, 2))
Arima(data[,3],  xreg = data[, 2] , order = c(2, 1, 2))
# Fit ARIMA models
models <- list(
      Arima(data[,3],  xreg = data[,2], order = c(1, 1, 6)),
      Arima(data[,3],  xreg = data[,2], order = c(0, 1, 6)),
      Arima(data[,3],  xreg = data[,2], order = c(0, 1, 9)),
      Arima(data[,3],  xreg = data[,2], order = c(0, 1, 8)),
      Arima(data[,3], xreg = data[,2], order = c(0, 1, 7)),
      Arima(data[,3], xreg = data[,2], order = c(1, 1, 7)),
      Arima(data[,3],  xreg = data[,2], order = c(1, 1, 9)),
      Arima(data[,3],  xreg = data[,2], order = c(1, 1, 8))
)

# Calculate AIC for each model
aic <- sapply(models, AIC)

# Create a data frame for AIC
aic_df <- data.frame(Model = paste("Model", 1:8), AIC = aic)

# Plot AIC
ggplot(aic_df, aes(x = Model, y = AIC)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(AIC, 2)), vjust = -0.4) +
      labs(title = "AIC for ARIMA Models",
           x = "ARIMA Model",
           y = "AIC") +
      theme_minimal()
# Compute BIC for each model
bic <- sapply(models, BIC)

# Create a data frame for BIC
bic_df <- data.frame(Model = paste("Model", 1:8), BIC = bic)

# Plot BIC
ggplot(bic_df, aes(x = Model, y = BIC)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(BIC, 2)), vjust = 0.5) +
      labs(title = "Bayesian Information Criterion (BIC) for ARIMA Models",
           x = "ARIMA Model",
           y = "BIC") +
      theme_minimal()
# Compute RMSE for each model
rmse <- sapply(models, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      SSE <- sum(residuals^2)
      MSE <- SSE / n
      RMSE <- sqrt(MSE)
      return(RMSE)
})

# Create a data frame for RMSE
rmse_df <- data.frame(Model = paste("Model", 1:8), RMSE = rmse)

# Plot RMSE
ggplot(rmse_df, aes(x = Model, y = RMSE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(RMSE, 2)), vjust = 0.5) +
      labs(title = "RMSE for ARIMA Models",
           x = "ARIMA Model",
           y = "RMSE") +
      theme_minimal()




# Compute log-likelihood for each model
likelihood <- sapply(models, function(model) {
      logLik(model)
})

# Create a data frame for log-likelihood
likelihood_df <- data.frame(Model = paste("Model", 1:8), Likelihood = likelihood)

# Plot log-likelihood
ggplot(likelihood_df, aes(x = Model, y = Likelihood)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(Likelihood, 2)), vjust = 0.5) +
      labs(title = "Log-Likelihood for ARIMA Models",
           x = "ARIMA Model",
           y = "Log-Likelihood") +
      theme_minimal()
# Compute R-squared for each model
r_squared <- sapply(models, function(model) {
      residuals <- residuals(model)
      SSE <- sum(residuals^2)
      SST <- sum((data[,3] - mean(data[,3]))^2)
      R_squared <- 1 - SSE / SST
      return(R_squared)
})
# Create a data frame for R-squared
r_squared_df <- data.frame(Model = paste("Model", 1:8), R_squared = r_squared)

# Plot R-squared
ggplot(r_squared_df, aes(x = Model, y = R_squared)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(R_squared, 2)), vjust = 0.5) +
      labs(title = "R-squared for ARIMA Models",
           x = "ARIMA Model",
           y = "R-squared") +
      theme_minimal()
# Compute Schwarz criterion for each model
schwarz_criterion <- sapply(models, function(model) {
      AIC(model, k = 2)
})

# Create a data frame for Schwarz criterion
schwarz_criterion_df <- data.frame(Model = paste("Model", 1:8), Schwarz_Criterion = schwarz_criterion)

# Plot Schwarz criterion
ggplot(schwarz_criterion_df, aes(x = Model, y = Schwarz_Criterion)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(Schwarz_Criterion, 2)), vjust = 0.5) +
      labs(title = "Schwarz Criterion for ARIMA Models",
           x = "ARIMA Model",
           y = "Schwarz Criterion") +
      theme_minimal()
# Compute standard error of regression for each model
se_regression <- sapply(models, function(model) {
      sqrt(sum(residuals(model)^2) / (length(model$residuals) - length(model$coef)))
})

# Create a data frame for standard error of regression
se_regression_df <- data.frame(Model = paste("Model", 1:8), SE_Regression = se_regression)

# Plot standard error of regression
ggplot(se_regression_df, aes(x = Model, y = SE_Regression)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(SE_Regression, 2)), vjust = 0.5) +
      labs(title = "Standard Error of Regression for ARIMA Models",
           x = "ARIMA Model",
           y = "Standard Error of Regression") +
      theme_minimal()
# Compute MAE for each model
mae <- sapply(models, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      MAE <- sum(abs(residuals)) / n
      return(MAE)
})

# Create a data frame for MAE
mae_df <- data.frame(Model = paste("Model", 1:8), MAE = mae)

# Plot MAE
ggplot(mae_df, aes(x = Model, y = MAE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(MAE, 2)), vjust = 0.5) +
      labs(title = "Mean Absolute Error (MAE) for ARIMA Models",
           x = "ARIMA Model",
           y = "MAE") +
      theme_minimal()
# Compute MSE for each model
mse <- sapply(models, function(model) {
      residuals <- residuals(model)
      n <- length(residuals)
      SSE <- sum(residuals^2)
      MSE <- SSE / n
      return(MSE)
})

# Create a data frame for MSE
mse_df <- data.frame(Model = paste("Model", 1:8), MSE = mse)

# Plot MSE
ggplot(mse_df, aes(x = Model, y = MSE)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(MSE, 2)), vjust = 0.5) +
      labs(title = "Mean Squared Error (MSE) for ARIMA Models",
           x = "ARIMA Model",
           y = "MSE") +
      theme_minimal()


# Fit ARIMA with best  model with exogenous variable
fit <-    Arima(data[,3],  xreg = data[,2], order = c(1, 1, 9))
# Adjust figure margins
par(mar = c(1, 4, 4, 2) + 0.1)

# Plot diagnostics
tsdiag(fit)
# Forecast the next 20 periods
forecast <- forecast(fit  , xreg = tail(data[, 2], 19) , h = 19)
print(tail(data[, 2], 18))

# Print the forecast
print(forecast)

# Save forecasted values into a CSV file
write.csv(forecast$mean, file = "data/forecasted_valuescoppershmet.csv")
daforcasted <- read.csv("data/forecasted_valuescoppershmet.csv" , header = TRUE  )
print(daforcasted[,2])


# Read the data
daActuel <- read.csv("data/actuelshmet.csv", header = TRUE , sep = ";")
print(daActuel)


daActuel_values <- as.numeric(gsub(",", ".",daActuel$Shmet))
print(daActuel_values)

# Plot actual values
plot(daActuel_values, type = "l", col = "blue", xlab = "Time", ylab = "Price", main = "Actual Copper Prices VS forcasted value", ylim = c(55000, 85000))

# Add the forecasted values to the plot
lines(daforcasted[,2], col = "red")

# Add legend
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


library(ggplot2)

# Read the actual data


# Calculate the Mean Absolute Error (MAE) and Mean Squared Error (MSE)
mae <- mean(abs(forecast$mean -  daActuel_values))
mse <- mean((forecast$mean -  daActuel_values)^2)

# Print MAE and MSE
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((forecast$mean -  daActuel_values)^2))
# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((forecast$mean -  daActuel_values) / daActuel_values)) * 100
# Print RMSE, MAPE, and Theil's U Statistic
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
plot(forecast, main ="ARIMA Forecast for Copper") 
###33
# Fit ARIMA with best  model with exogenous variable
fit <-  Arima(data[,3] , xreg = data[, 2] , order = c(0, 1, 2))
# Adjust figure margins
par(mar = c(1, 4, 4, 2) + 0.1)

# Plot diagnostics
tsdiag(fit)
# Forecast the next 20 periods
forecast <- forecast(fit  , xreg = tail(data[, 2], 3) , h = 3)
print(tail(data[, 2], 3))

# Print the forecast
print(forecast)

# Save forecasted values into a CSV file
write.csv(forecast$mean, file = "data/forecasted_valuescoppershmet3.csv")
daforcasted <- read.csv("data/forecasted_valuescoppershmet3.csv" , header = TRUE  )
print(daforcasted[,2])


# Read the data
daActuel <- read.csv("data/shmet3days.csv", header = TRUE , sep = ";")
print(daActuel)


daActuel_values <- as.numeric(gsub(",", ".",daActuel$Shmet))
print(daActuel_values)

# Plot actual values
plot(daActuel_values, type = "l", col = "blue", xlab = "Time", ylab = "Price", main = "Actual Copper Prices VS forcasted value", ylim = c(65000, 75000))

# Add the forecasted values to the plot
lines(daforcasted[,2], col = "red")

# Add legend
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


library(ggplot2)

# Read the actual data


# Calculate the Mean Absolute Error (MAE) and Mean Squared Error (MSE)
mae <- mean(abs(forecast$mean -  daActuel_values))
mse <- mean((forecast$mean -  daActuel_values)^2)

# Print MAE and MSE
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((forecast$mean -  daActuel_values)^2))
# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((forecast$mean -  daActuel_values) / daActuel_values)) * 100
# Print RMSE, MAPE, and Theil's U Statistic
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
plot(forecast, main ="ARIMA Forecast for Copper") 


