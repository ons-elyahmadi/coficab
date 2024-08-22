# Load necessary packages
library(rvest)
library(dplyr)
library(lubridate)

# Base URL
base_url <- 'https://www.westmetall.com/en/markdaten.php?action=table&field=LME_Cu_cash'

# List of year identifiers to scrape
years <- 2024:2008
year_ids <- paste0("#y", years)

# CSV file name
file_name <- "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/copper_pricesLME.csv"

# Function to scrape data for a specific year
scrape_year <- function(year_id) {
      # Read the content of the web page
      webpage <- read_html(base_url)
      
      # Extract table data for the specified year
      table <- webpage %>%
            html_node(paste0("div#content ", year_id, " ~ table")) %>%
            html_table(fill = TRUE)
      
      # Convert dates to format "%d/%m/%Y" and filter rows with valid dates
      table <- table %>%
            mutate(date = dmy(date),
                   price_copper_LME = as.numeric(gsub(",", "", `LME Copper Cash-Settlement`))) %>%
            filter(!is.na(date)) %>%
            select(date, price_copper_LME)
      
      return(table)
}

# Initialize the CSV file
if (file.exists(file_name)) {
      file.remove(file_name)
}

# Scrape data for each year and save it to the CSV file
for (year_id in year_ids) {
      data <- scrape_year(year_id)
      
      if (!file.exists(file_name)) {
            write.csv(data, file_name, row.names = FALSE)
      } else {
            write.table(data, file_name, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      }
}

# Display the first few rows of the final CSV file
final_data <- read.csv(file_name)
head(final_data)
