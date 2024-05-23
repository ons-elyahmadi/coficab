library(dplyr)

library(tidyr)
library(zoo)
# Charger les données
data <- read.csv("data/COPPER.csv") # Assurez-vous de remplacer "votre_fichier.csv" par le chemin réel de votre fichier
head(data)
colnames(data) <- c("Date.LME", "Copper.Ask.LME", "Date.Actual.PMI", "Actual.PMI","Date.Gold." ,"Price.Gold" ,"Date.Shmet" ,"COPPER.Price..SHMET" ,"Date.oil","Price.Oil","DATECompex","COMEX")
# Convertir les colonnes de date en type Date

data$Date.Actual.PMI <- as.Date(data$Date.Actual.PMI , format = "%m/%d/%Y"  )
data$Date.LME <- as.Date(data$Date.LME  , format = "%m/%d/%Y" )
 
data$Date.Shmet <- as.Date(data$Date.Shmet  , format = "%m/%d/%Y")
data$Date.oil <- as.Date(data$Date.oil  , format = "%m/%d/%Y")
data$DATECompex <- as.Date(data$DATECompex  , format = "%m/%d/%Y")
head(data)
write.csv(data, "data/c.csv", row.names = FALSE)
 
 

#######  
all_dates <- seq(as.Date("1999-03-08"), as.Date("2024-03-04"), by = "day")
 


# Remplir les valeurs manquantes par les valeurs précédentes
data_filled <- data %>%
      tidyr::fill(everything(), .direction = "down") %>%
      filter(Date.LME %in% all_dates) # Filtrer uniquement les dates qui nous intéressent

head(data_filled)
 
data <- data %>%
      mutate(
            Copper.Ask.LME = na.locf(Copper.Ask.LME),
            Actual.PMI = na.locf(Actual.PMI),
            Price.Gold = na.locf(Price.Gold),
            COPPER.Price..SHMET = na.locf(COPPER.Price..SHMET),
            Price.Oil = na.locf(Price.Oil),
            COMEX = na.locf(COMEX)
      )

head(data)
write.csv(data, "data/donnesComplet.csv", row.names = FALSE)
