# Charger les packages nécessaires
library(rvest)
library(dplyr)
library(lubridate)

# URL de base pour l'étain (tin)
base_url <- 'https://www.westmetall.com/en/markdaten.php?action=table&field=LME_Sn_cash'

# Liste des identifiants d'années à scraper
years <- 2024:2008
year_ids <- paste0("#y", years)

# Nom du fichier CSV
file_name <- "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/tin_pricesLME.csv"

# Fonction pour scraper les données d'une année spécifique
scrape_year <- function(year_id) {
      # Lire le contenu de la page web
      webpage <- read_html(base_url)
      
      # Extraire les données du tableau pour l'année spécifiée
      table <- webpage %>%
            html_node(paste0("div#content ", year_id, " ~ table")) %>%
            html_table(fill = TRUE)
      
      # Convertir les dates au format "%d/%m/%Y" et filtrer les lignes avec des dates valides
      table <- table %>%
            mutate(date = dmy(date),
                   price_tin_LME = as.numeric(gsub(",", "", `LME Tin Cash-Settlement`))) %>%
            filter(!is.na(date)) %>%
            select(date, price_tin_LME)
      
      return(table)
}

# Initialiser le fichier CSV
if (file.exists(file_name)) {
      file.remove(file_name)
}

# Scraper les données pour chaque année et les sauvegarder dans le fichier CSV
for (year_id in year_ids) {
      data <- scrape_year(year_id)
      
      if (!file.exists(file_name)) {
            write.csv(data, file_name, row.names = FALSE)
      } else {
            write.table(data, file_name, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      }
}

# Afficher les premières lignes du fichier CSV final
final_data <- read.csv(file_name)


head(final_data)
