# Charger les bibliothèques nécessaires
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2) # for melt function
library(ggthemes)

# Charger les informations
data <- read_csv("data/Cleaning_merge.csv")
head(data)


# La commande read_csv est plus rapide, et elle analyse automatiquement la plupart des données
colnames(data) <- c("date", "Price_PMI","Price_LME","Price_COMEX", "Price_Gold" ,"Price_Oil","Price_SHMET")
 
data[c(2, 3 , 4 , 5 , 6 , 7)] <- lapply(data[c(2, 3 , 4 , 5 , 6 , 7)], as.numeric) # ces colonnes doivent être converties en numérique
head(data)
write.csv(data,"data/Cleaning_Data.csv", row.names = FALSE)# Voir un résumé des données - min, quartiles, moyenne, médiane et max
summary(data)

# Examiner d'abord le graphique de séries chronologiques pour les deux variables d'intérêt : prix du cuivre et prix de l'or
output_vars_melted <- melt(data[1:7], id = "date")
ggplot(output_vars_melted, aes(x=date, y=value, col=variable)) + 
      geom_line() +
      ggtitle("Copper, Gold, and Oil Prices , pmi Over Time") +
      theme_stata()


# Examiner les différences de prix entre LME
price_dif <- data.frame(date = data$date, price_difference =   data$Price_LME - data$Price_PMI )

ggplot(data = price_dif, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper LME and PMI") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price LME - PMI",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))
price_difG <- data.frame(date = data$date, price_difference =   data$Price_LME - data$Price_Gold )

ggplot(data = price_difG, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper LME and Price_Gold") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price LME - Price_Gold",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))
price_difO <- data.frame(date = data$date, price_difference =   data$Price_LME - data$Price_Oil )

ggplot(data = price_difO, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper LME and Price_Oil") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price LME - Price_Oil",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))
# Examiner les différences de prix SHmet
price_difS <- data.frame(date = data$date, price_difference =   data$Price_SHMET - data$Price_PMI )

ggplot(data = price_difS, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper SHMET and PMI") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price SHmet - PMI",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))
price_difGS <- data.frame(date = data$date, price_difference =   data$Price_SHMET - data$Price_Gold )

ggplot(data = price_difGS, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper SHMET and Price_Gold") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price SHMET - Price_Gold",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))
price_difOS <- data.frame(date = data$date, price_difference =   data$Price_SHMET - data$Price_Oil )

ggplot(data = price_difOS, aes(x = date, y = price_difference)) +
      geom_area() +
      ggtitle("Price Differences between Copper  SHMET and Price_Oil") +
      scale_x_date(date_breaks = "years" , date_labels = "%Y") +
      labs(title = "Price Differences",
           subtitle = "Copper Price SHMET - Price_Oil",
           x = "Year", y = "Price Difference") +
      theme_stata() +
      theme(axis.text.x = element_text(angle=90))

 
# Générer une série temporelle de toutes les variables
all_vars_melted <- melt(data, id = "date")
library(ggplot2)

ggplot(all_vars_melted, aes(x = date, y = value, group = variable, color = variable)) +
      geom_line() +
      facet_grid(variable ~ ., scale = "free_y") +
      theme_bw() +
      ggtitle("Time Series of All Variables")
 

plot_correlation(data[1:1892, -1], type = 'all')

