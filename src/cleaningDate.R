# Chargement des packages nécessaires
library(tidyverse)
library(lubridate)

# Chemin du fichier CSV
file_path <- "data/c.csv"

# Charger les données depuis le fichier CSV
data <- read.csv(file_path)

# Liste des couples de colonnes de dates et de prix
date_price_pairs <- list(
      c("Date.LME", "Copper.Ask.LME"),
      c("Date.Actual.PMI", "Actual.PMI"),
      c("Date.Gold.", "Price.Gold"),
      c("Date.Shmet", "COPPER.Price..SHMET"),
      c("Date.oil", "Price.Oil"),
      c("DATECompex", "COMEX")
)

# Effectuer le traitement pour chaque couple de colonnes
for(pair in date_price_pairs) {
      date_col <- pair[[1]]
      price_col <- pair[[2]]
      
      # Transformer les données mensuelles en données journalières avec répétition des valeurs
      daily_data <- data.frame(Date=character(), Price=numeric())
      
      for(i in 1:nrow(data)) {
            if (!is.na(data[[date_col]][i]) && !is.na(data[[price_col]][i])) {
                  # Convertir les dates en objets Date
                  start_date <- as.Date(data[[date_col]][i], ifelse(grepl("/", data[[date_col]][i]), "%m/%d/%Y", "%Y-%m-%d"))
                  end_date <- start_date + days(31)  # Supposons 31 jours pour simplifier
                  dates <- seq(start_date, end_date, by="day")
                  daily_data <- rbind(daily_data, data.frame(Date=dates, Price=data[[price_col]][i]))
            }
      }
      
      # Supprimer les lignes avec des dates répétées en prenant la valeur de la date précédente
      daily_data <- daily_data %>% distinct(Date, .keep_all = TRUE)
      
      # Ordonner les dates de manière croissante
      daily_data <- daily_data %>% arrange(Date)
      
      # Écrire les données transformées dans un nouveau fichier CSV
      new_file_path <- paste0("data/Cleaning_", price_col, ".csv")
      write.csv(daily_data, file=new_file_path, row.names=FALSE)
}
# Chemins d'accès aux fichiers CSV sur Google Drive
chemin_fichiers <- c(
      "data/Cleaning_ActualPMI.csv",
      "data/Cleaning_Copper.Ask.LME.csv",
      "data/Cleaning_COMEX.csv",
      "data/Cleaning_Price.Gold.csv",
      "data/Cleaning_Price.Oil.csv",
      "data/Cleaning_COPPER.Price..SHMET.csv"
)

# Parcourir chaque fichier et appliquer le traitement
for (fichier in chemin_fichiers) {
      df <- read.csv(fichier)
      
      # Convertir les colonnes de date en format datetime
      date_columns <- grep("Date", names(df))
      for (col in date_columns) {
            df[[col]] <- as.Date(df[[col]])
      }
      
      # Définir les bornes de la marge
      start_date <- as.Date("2018-12-30")
      end_date <- as.Date("2024-03-04")
      
      # Filtrer les lignes en fonction de la marge spécifiée
      date_filtered_df <- subset(df, df[[date_columns[1]]] >= start_date & df[[date_columns[1]]] <= end_date)
      
      # Sélectionner uniquement les colonnes de prix
      price_columns <- grep("Price", names(df))
      
      # Fusionner les données filtrées sur les dates avec les colonnes de prix
      df_filtered <- date_filtered_df[c(date_columns, price_columns)]
      
      
      new_file_path <- paste0("data/Cleaningfiltred_", price_col, ".csv")
      write.csv(df_filtered, file=new_file_path, row.names=FALSE)
      
}

# Charger le package readr pour la lecture des fichiers CSV
library(readr)

# Chemins d'accès aux fichiers CSV
chemin_fichier1 <- 'data/Cleaningfiltred_ActualPMI.csv'
chemin_fichier2 <- 'data/Cleaningfiltred_Copper.Ask.LME.csv'
chemin_fichier3 <- 'data/Cleaningfiltred_COMEX.csv'
chemin_fichier4 <- 'data/Cleaningfiltred_Price.Gold.csv'
chemin_fichier5 <- 'data/Cleaningfiltred_Price.Oil.csv'
chemin_fichier6 <- 'data/Cleaningfiltred_COPPER.Price..SHMET.csv'

# Fonction pour renommer les colonnes
rename_columns <- function(df, suffix) {
      names(df) <- paste0(names(df), "_", suffix)
      return(df)
}

# Lire les fichiers CSV
df1 <- read_csv(chemin_fichier1)
df2 <- read_csv(chemin_fichier2)
df3 <- read_csv(chemin_fichier3)
df4 <- read_csv(chemin_fichier4)
df5 <- read_csv(chemin_fichier5)
df6 <- read_csv(chemin_fichier6)

 
rename_columns <- function(df, suffix) {
      names(df) <- paste(names(df), suffix, sep = "_")
      return(df)
}

# Renommer les colonnes pour chaque DataFrame
df1 <- rename_columns(df1, "PMI")
df2 <- rename_columns(df2, "LME")
df3 <- rename_columns(df3, "COMEX")
df4 <- rename_columns(df4, "Gold")
df5 <- rename_columns(df5, "Oil")
df6 <- rename_columns(df6, "SHMET")

# Fusionner les fichiers
df_fusionne <- cbind(df1, df2, df3, df4, df5, df6)
# Enregistrer le DataFrame fusionné dans un fichier CSV
new_file_path <- 'data/Cleaning_mergeddd.csv'
write_csv(df_fusionne, new_file_path)

print("Le fichier fusionné a été sauvegardé avec succès.")
