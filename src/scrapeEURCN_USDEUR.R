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
      csv_filename = "dataexchanges/EUR_CNY.csv"
)

# Appeler la fonction pour USD/EUR
download_and_extract_xml(
      url = "https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-usd.fr.html",
      xml_filename = "usd.xml",
      csv_filename = "dataexchanges/EUR_USD.csv"
)

