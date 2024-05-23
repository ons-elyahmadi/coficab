library(TSA)
library(tseries)
library(fGarch)
library(forecast)
library(tseries)
library(ggplot2)
# Read the data
data <- read.csv("data/cleanindata.csv", header = TRUE)
head(data)


# Attach the data
attach(data)
# Fit ARIMA with best  model with exogenous variable
fitArima <- Arima(data[,3] , xreg = data[, 2] ,order = c(0, 1, 1) )
res.fitArima<-fitArima$residuals
print(res.fitArima)
garch01=garch(x=res.fitArima ,order=c(0,1))
AIC(garch01)
garch02=garch(x=res.fitArima ,order=c(0,2))
AIC(garch02)
garch03=garch(x=res.fitArima ,order=c(0,3))
AIC(garch03)
garch04=garch(x=res.fitArima ,order=c(0,4))
AIC(garch04)
garch05=garch(x=res.fitArima ,order=c(0,5))
AIC(garch05)
garch06=garch(x=res.fitArima ,order=c(0,6))
AIC(garch06)
garch07=garch(x=res.fitArima ,order=c(0,7))
AIC(garch07)
garch08=garch(x=res.fitArima ,order=c(0,8))
AIC(garch08)
garch09=garch(x=res.fitArima ,order=c(0,9))
AIC(garch09)
garch10=garch(x=res.fitArima ,order=c(0,10))
AIC(garch10)
garch11=garch(x=res.fitArima ,order=c(1,1))
AIC(garch11)
garch12=garch(x=res.fitArima ,order=c(1,2))
AIC(garch12)
garch13=garch(x=res.fitArima ,order=c(1,3))
AIC(garch13)
garch14=garch(x=res.fitArima ,order=c(1,4))
AIC(garch04)
garch15=garch(x=res.fitArima ,order=c(1,5))
AIC(garch15)
garch16=garch(x=res.fitArima ,order=c(1,6))
AIC(garch06)
garch17=garch(x=res.fitArima ,order=c(1,7))
AIC(garch17)
garch18=garch(x=res.fitArima ,order=c(1,8))
AIC(garch18)
garch19=garch(x=res.fitArima ,order=c(1,9))
AIC(garch19)
garch11=garch(x=res.fitArima ,order=c(1,10))
AIC(garch11)
models <- list(
      garch01=garch(x=res.fitArima ,order=c(0,1)),
      garch02=garch(x=res.fitArima ,order=c(0,2)),
      garch03=garch(x=res.fitArima ,order=c(0,3)),
      garch04=garch(x=res.fitArima ,order=c(0,4)),
      garch05=garch(x=res.fitArima ,order=c(0,5)),
      garch06=garch(x=res.fitArima ,order=c(0,6)),
      garch07=garch(x=res.fitArima ,order=c(0,7)),
      garch08=garch(x=res.fitArima ,order=c(0,8)),
      garch09=garch(x=res.fitArima ,order=c(0,9)),
      garch10=garch(x=res.fitArima ,order=c(0,10)),     
      garch11=garch(x=res.fitArima ,order=c(1,1)),
      garch12=garch(x=res.fitArima ,order=c(1,2)),
      garch13=garch(x=res.fitArima ,order=c(1,3)),
      garch14=garch(x=res.fitArima ,order=c(1,4)),
      garch15=garch(x=res.fitArima ,order=c(1,5)),
      garch16=garch(x=res.fitArima ,order=c(1,6)),
      garch17=garch(x=res.fitArima ,order=c(1,7)),
      garch18=garch(x=res.fitArima ,order=c(1,8)),
      garch19=garch(x=res.fitArima ,order=c(1,9)),
      garch11=garch(x=res.fitArima ,order=c(1,10))
       
)
# Calculate AIC for each model
aic <- sapply(models, AIC)

# Create a data frame for AIC
aic_df <- data.frame(Model = paste("Model", 1:20), AIC = aic)

# Plot AIC
ggplot(aic_df, aes(x = Model, y = AIC)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = round(AIC, 2)), vjust = -0.4) +
      labs(title = "AIC for ARIMA Models",
           x = "ARIMA Model",
           y = "AIC") +
      theme_minimal()
arch_fit <- garch(x=res.fitArima ,order=c(0,5))
# Fit the GARCH model
arch_fit <- garchFit(formula = ~ garch(1, 5), data = res.fitArima)
fitgar = garchFit(formula = ~ garch(1, 5), data = res.fitArima, trace = FALSE)
predgar<-predict(fitgar, n.ahead=20, plot=T)
print(predgar)
# Forecast volatility for the next 20 days
volatility_forecast <- predict(arch_fit, n.ahead = 20)

# Print the forecast
print(volatility_forecast)
# Fit the GARCH model
arch_fit <- ugarchfit(data = res.fitArima, spec = ugarchspec(variance.model = list(model = "sGARCH")))

# Forecast volatility for the next 20 days
volatility_forecast <- ugarchforecast(arch_fit, n.ahead = 20)

# Print the forecast
print(volatility_forecast)
