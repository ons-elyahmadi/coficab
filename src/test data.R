 

# Charger les packages nécessaires
library(readr)
library(stats)

# Charger les données
data <- read_csv("data/Cleaning_Data.csv")

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)  # Pour la reproductibilité
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Définir les modèles de régression linéaire
lm_models <- list(
      lm(Price_LME ~ Price_PMI + Price_Oil, data = train_data),
      lm(Price_LME ~ Price_PMI + Price_Gold, data = train_data),
      lm(Price_LME ~ Price_Oil + Price_Gold, data = train_data),
      lm(Price_LME ~ Price_Oil, data = train_data),
      lm(Price_LME ~ Price_PMI, data = train_data),
      lm(Price_LME ~ Price_Gold, data = train_data),
      lm(Price_SHMET ~ Price_PMI + Price_Oil, data = train_data),
      lm(Price_SHMET ~ Price_PMI + Price_Gold, data = train_data),
      lm(Price_SHMET ~ Price_Oil + Price_Gold, data = train_data),
      lm(Price_SHMET ~ Price_Oil, data = train_data),
      lm(Price_SHMET ~ Price_PMI, data = train_data),
      lm(Price_SHMET ~ Price_Gold, data = train_data),
      lm(Price_COMEX ~ Price_PMI + Price_Oil, data = train_data),
      lm(Price_COMEX ~ Price_PMI + Price_Gold, data = train_data),
      lm(Price_COMEX ~ Price_Oil + Price_Gold, data = train_data),
      lm(Price_COMEX ~ Price_Oil, data = train_data),
      lm(Price_COMEX ~ Price_PMI, data = train_data),
      lm(Price_COMEX ~ Price_Gold, data = train_data)
)

# Définir les modèles de régression non linéaire
nls_models <- list(
      nls(Price_LME ~ a * Price_PMI + b * Price_Oil + c * Price_PMI^2 + d * Price_Oil^2, 
          data = train_data, 
          start = list(a = 1, b = 1, c = 1, d = 1)),
      nls(Price_SHMET ~ a * Price_PMI + b * Price_Oil + c * Price_PMI^2 + d * Price_Oil^2, 
          data = train_data, 
          start = list(a = 1, b = 1, c = 1, d = 1)),
      nls(Price_COMEX ~ a * Price_PMI + b * Price_Oil + c * Price_PMI^2 + d * Price_Oil^2, 
          data = train_data, 
          start = list(a = 1, b = 1, c = 1, d = 1))
)

# Fonction pour évaluer le modèle
evaluate_model <- function(model, test_data) {
      predicted <- predict(model, newdata = test_data)
      actual <- test_data$Price_LME
      rmse <- sqrt(mean((predicted - actual)^2))
      return(rmse)
}

# Fonction pour afficher les mesures de performance du modèle
print_model_performance <- function(model, test_data) {
      summary_data <- summary(model)
      r_squared <- summary_data$r.squared
      se_regression <- summary_data$sigma
      log_likelihood <- logLik(model)
      
      
      aic <- AIC(model)
      bic <- BIC(model)
      rmse <- evaluate_model(model, test_data)
      
      performance <- c(
            R_squared = r_squared,
            SE_regression = se_regression,
            Log_likelihood = log_likelihood,
            
            AIC = aic,
            BIC = bic,
            RMSE = rmse
      )
      return(performance)
}

# Afficher les performances pour chaque modèle linéaire
for (i in seq_along(lm_models)) {
      model <- lm_models[[i]]
      model_name <- paste("Modèle linéaire", i)
      cat(model_name, ":", "\n")
      print(summary(model))
      cat("Performances sur l'ensemble de test:", "\n")
      print(print_model_performance(model, test_data))
      cat("\n")
}

# Afficher les performances pour chaque modèle non linéaire
for (i in seq_along(nls_models)) {
      model <- nls_models[[i]]
      model_name <- paste("Modèle non linéaire", i)
      cat(model_name, ":", "\n")
      print(summary(model))
      cat("Performances sur l'ensemble de test:", "\n")
      print(print_model_performance(model, test_data))
      cat("\n")
}

