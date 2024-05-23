# Chargement des bibliothèques
library(tidyverse)
library(forecast)
library(randomForest)
library(e1071) # Pour SVM
library(class)
 # Pour kNN
library(keras) # Pour LSTM


# Chargement des données
data <- read_csv("data/cleaned_data3.csv")
data[c(2, 3, 4)] <- lapply(data[c(2, 3, 4)], as.numeric) 

# Définition des ensembles d'entraînement et de test
train <- data[1302:4339,]
test <- data[1:1302,]

################
# 1. Régression linéaire simple, sans variables de pourcentage de changement
################

simple_LR_copper <- lm(copper_price ~ gold_price + oil_price, data = train)
summary(simple_LR_copper)

##################
# 2. Prévision de séries temporelles - ARIMA
##################

# Ajustement du modèle ARIMA pour le cuivre
fit_copper <- auto.arima(ts(data[1:261, 2], start = c(2023, 3 ,3), end = c(2024, 3 ,3), frequency = 365), D = 1)

forecast_copper <- forecast(fit_copper, h = 1302)

# Affichage des prédictions ARIMA
plot(forecast_copper, main = "Prévision ARIMA pour le cuivre")

# Prédictions numériques - la moyenne des prédictions ARIMA
ARIMA_copper_pred <- as.numeric(forecast_copper$mean)
 
# 3. Prévision de séries temporelles - Réseau de neurones
#################

# Création de la série temporelle pour le cuivre
copper_ts <- ts(data[1:261, 2], start = c(2023, 3 ,3), end = c(2024, 3 ,3), frequency = 356) 

# Prévision avec le réseau de neurones
nn_forecast_copper <- forecast(nnetar(copper_ts), h = 1302)

# Affichage des prédictions du réseau de neurones
plot(nn_forecast_copper, main = "Prévision NN pour le cuivre")

# Prédictions numériques - modèle de prévision NN
NN_copper_pred <- data.frame(nn_forecast_copper)

#################
# 4. Arbres de décision - Forêt aléatoire - 600 arbres
#################

set.seed(1234567) # pour la reproductibilité

# Modèle de forêt aléatoire pour le prix du cuivre
RF_copper <- randomForest(copper_price ~ gold_price + oil_price,
                          data = data, ntree = 600, subset =  1302:4339, mtry = 2, na.action = na.omit)

# Affichage du modèle
print(RF_copper)
importance(RF_copper)
 
# Sauvegarder les modèles
save.image(file = "bin/model.RData")

