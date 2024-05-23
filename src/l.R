 
require(mice)
require(randomForest)
library(tidyverse)
 

 
# Charger le package VIM
library(VIM)

# Imputation par KNN
imputed_data_knn <- kNN(data, k = 5)  # Utiliser k = 5 ou un autre nombre approprié de voisins

   # Utiliser k = 5 ou un autre nombre approprié de voisins

imputed_data_multiple <- mice(data, m = 5, maxit = 50, method = "pmm")
completed_data <- complete(imputed_data_multiple)

# Entraîner le modèle de forêt aléatoire sur les données imputées par KNN
model_knn <- randomForest(Official.Ask.LME.USD.TONNE ~ Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., data = imputed_data_knn)

# Prédictions sur les données de test
predictions_knn <- predict(model_knn, newdata = test_data)

# Évaluation du modèle
RMSE_knn <- sqrt(mean((test_data$Official.Ask.LME.USD.TONNE - predictions_knn)^2))
R_squared_knn <- cor(test_data$Official.Ask.LME.USD.TONNE, predictions_knn)^2

# Afficher les performances pour KNN
print("Performance du modèle avec imputation par KNN :")
print(paste("RMSE:", RMSE_knn))
print(paste("R_squared:", R_squared_knn))

# Entraîner le modèle de forêt aléatoire sur les données imputées par imputation multiple
model_multiple <- randomForest(Official.Ask.LME.USD.TONNE ~ Cushing..OK.WTI.Spot.Price.FOB..Dollars.per.Barrel., data = imputed_data_combined)

# Prédictions sur les données de test
predictions_multiple <- predict(model_multiple, newdata = test_data)

# Évaluation du modèle
RMSE_multiple <- sqrt(mean((test_data$Official.Ask.LME.USD.TONNE - predictions_multiple)^2))
R_squared_multiple <- cor(test_data$Official.Ask.LME.USD.TONNE, predictions_multiple)^2

# Afficher les performances pour l'imputation multiple
print("Performance du modèle avec imputation multiple :")
print(paste("RMSE:", RMSE_multiple))
print(paste("R_squared:", R_squared_multiple))

