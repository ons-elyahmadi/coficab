# Assurez-vous que le package 'readr' est installé
# Si ce n'est pas le cas, installez-le en utilisant install.packages("readr")

# Chargez le package 'readr'
library(readr)

# Charger les données
data <- read_csv("data/Cleaning_Data.csv")

# Afficher les premières lignes des données
head(data)

# Calculer R-squared
lm_model <- lm(Price_LME ~ Price_PMI + Price_Oil , data = data)
summary(lm_model)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model)

# Calculer AIC
AIC(lm_model)

# Calculer BIC
BIC(lm_model)
lm_model2 <- lm(Price_LME ~ Price_PMI + Price_Gold , data = data)
summary(lm_model2)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model2)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model2)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model2)

# Calculer AIC
AIC(lm_model2)

# Calculer BIC
BIC(lm_model2)
lm_model6 <- lm(Price_LME ~ Price_Oil + Price_Gold , data = data)
summary(lm_model6)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model6)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model6)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model6)

# Calculer AIC
AIC(lm_model6)

# Calculer BIC
BIC(lm_model6)
# Calculer R-squared
lm_model3 <- lm(Price_LME ~ Price_Oil , data = data)
summary(lm_model3)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model3)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model3)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model3)

# Calculer AIC
AIC(lm_model3)

# Calculer BIC
BIC(lm_model3)
# Calculer R-squared
lm_model4 <- lm(Price_LME ~ Price_PMI  , data = data)
summary(lm_model4)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model4)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model4)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model4)

# Calculer AIC
AIC(lm_model4)

# Calculer BIC
BIC(lm_model4)
# Calculer R-squared
lm_model5 <- lm(Price_LME ~ Price_Gold  , data = data)
summary(lm_model5)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model5)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model5)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model5)

# Calculer AIC
AIC(lm_model5)

# Calculer BIC
BIC(lm_model5)
####SHMET
# Calculer R-squared
lm_model7 <- lm(Price_SHMET ~ Price_PMI + Price_Oil , data = data)
summary(lm_model7)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model7)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model7)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model7)

# Calculer AIC
AIC(lm_model7)

# Calculer BIC
BIC(lm_model7)
lm_model8 <- lm(Price_SHMET ~ Price_PMI + Price_Gold , data = data)
summary(lm_model8)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model8)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model8)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model8)

# Calculer AIC
AIC(lm_model8)

# Calculer BIC
BIC(lm_model8)
lm_model9 <- lm(Price_SHMET ~ Price_Oil + Price_Gold , data = data)
summary(lm_model9)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model9)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model9)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model9)

# Calculer AIC
AIC(lm_model9)

# Calculer BIC
BIC(lm_model9)
# Calculer R-squared
lm_model10 <- lm(Price_SHMET ~ Price_Oil , data = data)
summary(lm_model10)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model10)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model10)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model10)

# Calculer AIC
AIC(lm_model10)

# Calculer BIC
BIC(lm_model10)
# Calculer R-squared
lm_model11 <- lm(Price_SHMET ~ Price_PMI  , data = data)
summary(lm_model11)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model11)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model11)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model11)

# Calculer AIC
AIC(lm_model11)

# Calculer BIC
BIC(lm_model11)
# Calculer R-squared
lm_model12 <- lm(Price_SHMET ~ Price_Gold  , data = data)
summary(lm_model12)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model12)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model12)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model12)

# Calculer AIC
AIC(lm_model12)

# Calculer BIC
BIC(lm_model12)
####SHMET
# Calculer R-squared
lm_model7 <- lm(Price_SHMET ~ Price_PMI + Price_Oil , data = data)
summary(lm_model7)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model7)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model7)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model7)

# Calculer AIC
AIC(lm_model7)

# Calculer BIC
BIC(lm_model7)
lm_model8 <- lm(Price_SHMET ~ Price_PMI + Price_Gold , data = data)
summary(lm_model8)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model8)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model8)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model8)

# Calculer AIC
AIC(lm_model8)

# Calculer BIC
BIC(lm_model8)
lm_model9 <- lm(Price_SHMET ~ Price_Oil + Price_Gold , data = data)
summary(lm_model9)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model9)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model9)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model9)

# Calculer AIC
AIC(lm_model9)

# Calculer BIC
BIC(lm_model9)
# Calculer R-squared
lm_model10 <- lm(Price_SHMET ~ Price_Oil , data = data)
summary(lm_model10)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model10)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model10)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model10)

# Calculer AIC
AIC(lm_model10)

# Calculer BIC
BIC(lm_model10)
# Calculer R-squared
lm_model11 <- lm(Price_SHMET ~ Price_PMI  , data = data)
summary(lm_model11)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model11)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model11)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model11)

# Calculer AIC
AIC(lm_model11)

# Calculer BIC
BIC(lm_model11)
# Calculer R-squared
lm_model12 <- lm(Price_SHMET ~ Price_Gold  , data = data)
summary(lm_model12)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model12)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model12)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model12)

# Calculer AIC
AIC(lm_model12)

# Calculer BIC
BIC(lm_model12)

####CMOEX
# Calculer R-squared
lm_model13 <- lm(Price_COMEX  ~ Price_PMI + Price_Oil , data = data)
summary(lm_model13)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model13)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model13)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model13)

# Calculer AIC
AIC(lm_model13)

# Calculer BIC
BIC(lm_model13)
lm_model14 <- lm( Price_COMEX ~ Price_PMI + Price_Gold , data = data)
summary(lm_model14)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model14)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model14)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model14)

# Calculer AIC
AIC(lm_model14)

# Calculer BIC
BIC(lm_model14)
lm_model15 <- lm(Price_COMEX ~ Price_Oil + Price_Gold , data = data)
summary(lm_model15)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model15)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model15)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model15)

# Calculer AIC
AIC(lm_model15)

# Calculer BIC
BIC(lm_model15)
# Calculer R-squared
lm_model16 <- lm(Price_COMEX ~ Price_Oil , data = data)
summary(lm_model16)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model16)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model16)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model16)

# Calculer AIC
AIC(lm_model16)

# Calculer BIC
BIC(lm_model16)
# Calculer R-squared
lm_model17 <- lm(Price_COMEX ~ Price_PMI  , data = data)
summary(lm_model17)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model17)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model17)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model17)

# Calculer AIC
AIC(lm_model17)

# Calculer BIC
BIC(lm_model17)
# Calculer R-squared
lm_model18 <- lm(Price_COMEX ~ Price_Gold  , data = data)
summary(lm_model18)$r.squared

# Calculer l'erreur standard de régression (SE)
summary(lm_model18)$sigma

# Calculer le logarithme de la vraisemblance
logLik(lm_model18)

# Calculer F-statistic et Prob(F-statistic)
anova(lm_model18)

# Calculer AIC
AIC(lm_model18)

# Calculer BIC
BIC(lm_model18)


# Assurez-vous que le package 'readr' est installé
# Si ce n'est pas le cas, installez-le en utilisant install.packages("readr")

# Chargez le package 'readr'
library(readr)

# Charger les données
data <- read_csv("data/Cleaning_Data.csv")

# Afficher les premières lignes des données
head(data)

# Diviser les données en ensembles d'entraînement et de test
set.seed(123)  # Pour la reproductibilité
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Fonction pour évaluer le modèle
evaluate_model <- function(model, test_data) {
      predicted <- predict(model, newdata = test_data)
      actual <- test_data$Price_LME
      rmse <- sqrt(mean((predicted - actual)^2))
      return(rmse)
}

# Modèle 1 : Price_LME ~ Price_PMI + Price_Oil
lm_model1 <- lm(Price_LME ~ Price_PMI + Price_Oil, data = train_data)
print("Modèle 1 : Price_LME ~ Price_PMI + Price_Oil")
print(summary(lm_model1))
print(paste("RMSE:", evaluate_model(lm_model1, test_data)))

# Modèle 2 : Price_LME ~ Price_PMI + Price_Gold
lm_model2 <- lm(Price_LME ~ Price_PMI + Price_Gold, data = train_data)
print("Modèle 2 : Price_LME ~ Price_PMI + Price_Gold")
print(summary(lm_model2))
print(paste("RMSE:", evaluate_model(lm_model2, test_data)))

# Modèle 3 : Price_LME ~ Price_Oil + Price_Gold
lm_model3 <- lm(Price_LME ~ Price_Oil + Price_Gold, data = train_data)
print("Modèle 3 : Price_LME ~ Price_Oil + Price_Gold")
print(summary(lm_model3))
print(paste("RMSE:", evaluate_model(lm_model3, test_data)))

# Modèle 4 : Price_LME ~ Price_Oil
lm_model4 <- lm(Price_LME ~ Price_Oil, data = train_data)
print("Modèle 4 : Price_LME ~ Price_Oil")
print(summary(lm_model4))
print(paste("RMSE:", evaluate_model(lm_model4, test_data)))

# Modèle 5 : Price_LME ~ Price_PMI
lm_model5 <- lm(Price_LME ~ Price_PMI, data = train_data)
print("Modèle 5 : Price_LME ~ Price_PMI")
print(summary(lm_model5))
print(paste("RMSE:", evaluate_model(lm_model5, test_data)))

# Modèle 6 : Price_LME ~ Price_Gold
lm_model6 <- lm(Price_LME ~ Price_Gold, data = train_data)
print("Modèle 6 : Price_LME ~ Price_Gold")
print(summary(lm_model6))
print(paste("RMSE:", evaluate_model(lm_model6, test_data)))

# Modèle 7 : Price_SHMET ~ Price_PMI + Price_Oil
lm_model7 <- lm(Price_SHMET ~ Price_PMI + Price_Oil, data = train_data)
print("Modèle 7 : Price_SHMET ~ Price_PMI + Price_Oil")
print(summary(lm_model7))
print(paste("RMSE:", evaluate_model(lm_model7, test_data)))

# Modèle 8 : Price_SHMET ~ Price_PMI + Price_Gold
lm_model8 <- lm(Price_SHMET ~ Price_PMI + Price_Gold, data = train_data)
print("Modèle 8 : Price_SHMET ~ Price_PMI + Price_Gold")
print(summary(lm_model8))
print(paste("RMSE:", evaluate_model(lm_model8, test_data)))

# Modèle 9 : Price_SHMET ~ Price_Oil + Price_Gold
lm_model9 <- lm(Price_SHMET ~ Price_Oil + Price_Gold, data = train_data)
print("Modèle 9 : Price_SHMET ~ Price_Oil + Price_Gold")
print(summary(lm_model9))
print(paste("RMSE:", evaluate_model(lm_model9, test_data)))

# Modèle 10 : Price_SHMET ~ Price_Oil
lm_model10 <- lm(Price_SHMET ~ Price_Oil, data = train_data)
print("Modèle 10 : Price_SHMET ~ Price_Oil")
print(summary(lm_model10))
print(paste("RMSE:", evaluate_model(lm_model10, test_data)))

# Modèle 11 : Price_SHMET ~ Price_PMI
lm_model11 <- lm(Price_SHMET ~ Price_PMI, data = train_data)
print("Modèle 11 : Price_SHMET ~ Price_PMI")
print(summary(lm_model11))
print(paste("RMSE:", evaluate_model(lm_model11, test_data)))

# Modèle 12 : Price_SHMET ~ Price_Gold
lm_model12 <- lm(Price_SHMET ~ Price_Gold, data = train_data)
print("Modèle 12 : Price_SHMET ~ Price_Gold")
print(summary(lm_model12))
print(paste("RMSE:", evaluate_model(lm_model12, test_data)))

# Modèle 13 : Price_COMEX ~ Price_PMI + Price_Oil
lm_model13 <- lm(Price_COMEX ~ Price_PMI + Price_Oil, data = train_data)
print("Modèle 13 : Price_COMEX ~ Price_PMI + Price_Oil")
print(summary(lm_model13))
print(paste("RMSE:", evaluate_model(lm_model13, test_data)))

# Modèle 14 : Price_COMEX ~ Price_PMI + Price_Gold
lm_model14 <- lm(Price_COMEX ~ Price_PMI + Price_Gold, data = train_data)
print("Modèle 14 : Price_COMEX ~ Price_PMI + Price_Gold")
print(summary(lm_model14))
print(paste("RMSE:", evaluate_model(lm_model14, test_data)))

# Modèle 15 : Price_COMEX ~ Price_Oil + Price_Gold
lm_model15 <- lm(Price_COMEX ~ Price_Oil + Price_Gold, data = train_data)
print("Modèle 15 : Price_COMEX ~ Price_Oil + Price_Gold")
print(summary(lm_model15))
print(paste("RMSE:", evaluate_model(lm_model15, test_data)))

# Modèle 16 : Price_COMEX ~ Price_Oil
lm_model16 <- lm(Price_COMEX ~ Price_Oil, data = train_data)
print("Modèle 16 : Price_COMEX ~ Price_Oil")
print(summary(lm_model16))
print(paste("RMSE:", evaluate_model(lm_model16, test_data)))

# Modèle 17 : Price_COMEX ~ Price_PMI
lm_model17 <- lm(Price_COMEX ~ Price_PMI, data = train_data)
print("Modèle 17 : Price_COMEX ~ Price_PMI")
print(summary(lm_model17))
print(paste("RMSE:", evaluate_model(lm_model17, test_data)))
      
# Modèle 18 : Price_COMEX ~ Price_Gold
lm_model18 <- lm(Price_COMEX ~ Price_Gold, data = train_data)
print("Modèle 18 : Price_COMEX ~ Price_Gold")
print(summary(lm_model18))
print(paste("RMSE:", evaluate_model(lm_model18, test_data)))
      
