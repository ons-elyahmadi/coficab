library(rvest)
library(dplyr)
library(rvest)
library(dplyr)
library(lubridate)

# URL of the page to scrape
url <- "https://www.investing.com/currencies/usd-mxn-historical-data"

# Read the HTML content of the page
page <- read_html(url)

# Check if the specific class exists
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scrape the table data
if (class_exists) {
      # Select the table with the specific class
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Select the default table
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Load existing data from the CSV file
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_MXN.csv", header = TRUE, sep = ",")
print(existing_data)

# Rename the columns of the scraped table to match those of the existing CSV file
names(table) <- names(existing_data)

# Convert the date columns to Date format for better manipulation
existing_data$Date <- as.Date(existing_data$Date)
print(table)

# Remove any leading or trailing whitespace from the Date column
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

print(existing_data)
print(table)

# Add the new data just after the header of the existing data
combined_data <- bind_rows(table, existing_data)

# Remove duplicates based on the Date column
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Save the combined data in the same CSV file
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_MXN.csv", row.names = FALSE)

# Display the first few rows to verify
head(combined_data)

########USD_RON######
# URL de la page à scraper
url <- "https://www.investing.com/currencies/usd-ron-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_RON.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_RON.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
##########USD_HNL####
# URL de la page à scraper
url <- "https://www.investing.com/currencies/usd-hnl-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv( "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_HNL.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_HNL.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
###EUR_TND####
url <- "https://www.investing.com/currencies/eur-tnd-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_TND.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_TND.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
###EUR_RSD###
url <- "https://www.investing.com/currencies/eur-rsd-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RSD.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RSD.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
####EUR_RON####
url <- "https://www.investing.com/currencies/eur-ron-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RON.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)
print(existing_data)
# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)
# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_RON.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
#####EUR_MXN####
url <- "https://www.investing.com/currencies/eur-mxn-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MXN.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table) 

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MXN.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
#####EUR_MKD####
url <- "https://www.investing.com/currencies/eur-mkd-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MKD.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MKD.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
#####EUR_MAD####
url <- "https://www.investing.com/currencies/eur-mad-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MAD.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_MAD.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
#####EUR_HNL####
url <- "https://www.investing.com/currencies/eur-hnl-historical-data"

# Lire le contenu HTML de la page
page <- read_html(url)

# Vérifier si la classe existe
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scraper les données du tableau
if (class_exists) {
      # Sélectionner le tableau avec la classe spécifique
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Sélectionner le tableau par défaut
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Charger les données existantes du fichier CSV
existing_data <- read.csv("C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_HNL.csv", header = TRUE, sep = ",")

# Renommer les colonnes du tableau scraped pour correspondre à celles du fichier CSV existant
names(table) <- names(existing_data)

# Convertir les colonnes de date en format Date pour une meilleure manipulation
existing_data$Date <-   as.Date(existing_data$Date)
table <- table %>%
      mutate(Date = trimws(Date))
print(table)
table$Date <- mdy(table$Date)

# Print the table to check the result
print(table)

# Ajouter les nouvelles données juste après l'en-tête des données existantes
combined_data <- bind_rows(table, existing_data)

# Enlever les doublons basés sur la colonne Date
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Sauvegarder les données combinées dans le même fichier CSV
write.csv(combined_data, "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_HNL.csv", row.names = FALSE)

# Afficher les premières lignes pour vérifier
head(combined_data)
##USD_MKD##
library(rvest)
library(dplyr)
library(lubridate)

# Define the URL for USD/MKD historical data
url <- "https://www.investing.com/currencies/usd-mkd-historical-data"

# Read the HTML content of the page
page <- read_html(url)

# Check if the specific class exists
class_exists <- length(html_nodes(page, ".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4")) > 0

# Scrape the table data
if (class_exists) {
      # Select the table with the specific class
      table <- page %>%
            html_node(".freeze-column-w-1.w-full.overflow-x-auto.text-xs.leading-4") %>%
            html_table()
} else {
      # Select the default table
      table <- page %>%
            html_node("table") %>%
            html_table()
}

# Load the existing data from the CSV file
csv_path <- "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/USD_MKD.csv"
existing_data <- read.csv(csv_path, header = TRUE, sep = ",")

# Rename the columns of the scraped table to match those of the existing CSV file
names(table) <- names(existing_data)

# Convert the date columns to Date format for better manipulation
existing_data$Date <-  as.Date(existing_data$Date  )
table <- table %>%
      mutate(Date = trimws(Date))

# Print the table to check the result
print(table)
table$Date <- lubridate::mdy(table$Date)

# Add the new data just after the header of the existing data
combined_data <- bind_rows(table, existing_data)

# Remove duplicates based on the Date column
combined_data <- combined_data %>% distinct(Date, .keep_all = TRUE)

# Save the combined data in the same CSV file
write.csv(combined_data, csv_path, row.names = FALSE)

###EUR_CNY/EUR_USD####
library(xml2)
library(rvest)
library(tidyverse)

# Fonction pour télécharger et extraire les données XML
download_and_extract_xml <- function(url, xml_filename, csv_filename) {
      # Lire le contenu de la page HTML
      page <- read_html(url)
      
      # Trouver le lien vers le fichier XML
      link <- page %>%
            html_node(paste0("a[href='/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/", xml_filename, "']")) %>%
            html_attr("href")
      
      # Vérifier si le lien a été trouvé
      if (!is.na(link)) {
            # Construire l'URL complète du fichier XML
            xml_url <- paste0("https://www.ecb.europa.eu", link)
            
            # Télécharger le fichier XML
            download.file(xml_url, destfile = xml_filename, mode = "wb")
            
            # Vérifier si le téléchargement a réussi
            if (file.exists(xml_filename)) {
                  cat("Téléchargement réussi. Fichier enregistré à :", xml_filename, "\n")
                  
                  # Lire le fichier XML
                  tree <- read_xml(xml_filename)
                  
                  # Extraire les espaces de noms définis dans l'élément racine
                  namespace <- xml_ns(tree)
                  print(namespace)
                  
                  # Définir l'espace de noms par défaut
                  ns <- c(d1 = "http://www.ecb.europa.eu/vocabulary/stats/exr/1")
                  
                  # Extraire les données des observations
                  data <- tree %>%
                        xml_find_all(".//d1:Series", ns = ns) %>%
                        map(function(series) {
                              series %>%
                                    xml_find_all(".//d1:Obs", ns = ns) %>%
                                    map_df(function(obs) {
                                          tibble(
                                                Date = as.Date(xml_attr(obs, "TIME_PERIOD")),
                                                Price = as.numeric(xml_attr(obs, 'OBS_VALUE'))
                                          )
                                    })
                        }) %>%
                        bind_rows()
                  
                  # Vérifier si des données ont été extraites
                  if (nrow(data) == 0) {
                        cat("Aucune donnée n'a été trouvée. Vérifiez les espaces de noms et les sélecteurs XPath.\n")
                  } else {
                        # Enregistrer en CSV avec types de données corrects
                        write.csv(data, csv_filename, row.names = FALSE)
                        
                        # Afficher le contenu du CSV
                        print(data)
                  }
            } else {
                  cat("Échec du téléchargement du fichier XML.\n")
            }
      } else {
            cat("Le lien vers le fichier XML n'a pas été trouvé sur la page.\n")
      }
}

# Appeler la fonction pour EUR/CNY
download_and_extract_xml(
      url = "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-cny.fr.html",
      xml_filename = "cny.xml",
      csv_filename = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_CNY.csv"
)

# Appeler la fonction pour USD/EUR
download_and_extract_xml(
      url = "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-usd.fr.html",
      xml_filename = "usd.xml",
      csv_filename = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/dataexchanges/EUR_USD.csv"
)

