# Load necessary libraries
library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)

# URL of the website
url <- "https://www.eia.gov/dnav/pet/hist/RWTCD.htm"

# Read the content of the page
page <- read_html(url)

# Extract the specific table with summary="Cushing, OK WTI Spot Price FOB  (Dollars per Barrel)"
table <- page %>%
      html_node('table[summary="Cushing, OK WTI Spot Price FOB  (Dollars per Barrel)"]') %>%
      html_table(fill = TRUE)

# Display the first few rows of the table for inspection
print(head(table))

# Check the structure of the table's columns
str(table)

# Function to extract the start date from the week string
extract_start_date <- function(week_str) {
      cat("Processing:", week_str, "\n") # Debug: Display each week string
      
      # Extract the year
      year <- as.integer(gsub(".*(\\d{4}).*", "\\1", week_str))
      cat("Year extracted:", year, "\n") # Debug: Display the extracted year
      
      # Extract the start month and day
      month_day_str <- gsub(".* (\\w{3})- *(\\d{1,2}) to.*", "\\1-\\2", week_str)
      cat("Month and day string:", month_day_str, "\n") # Debug: Display the month-day string
      
      month_day <- strsplit(month_day_str, "-")[[1]]
      cat("Month and day extracted:", month_day, "\n") # Debug: Display the extracted month and day
      
      if (length(month_day) == 2) {
            month <- match(month_day[1], month.abb)
            day <- as.integer(month_day[2])
            cat("Month:", month, "Day:", day, "\n") # Debug: Display the converted month and day
            if (!is.na(year) && !is.na(month) && !is.na(day)) {
                  return(as.Date(paste(year, month, day, sep = "-")))
            }
      }
      return(NA)
}

# Display the original table
print(table)

# Rename the 'Week Of' column and transform the data
cleaned_table <- table %>%
      rename(Week = `Week Of`) %>%
      pivot_longer(cols = c(Mon, Tue, Wed, Thu, Fri), names_to = "Day", values_to = "Price") %>%
      mutate(Date = mapply(function(week, day) {
            start_date <- extract_start_date(week)
            cat("Week:", week, "Day:", day, "Start Date:", start_date, "\n")  # Debugging
            if (!is.na(start_date)) {
                  start_date + match(day, c("Mon", "Tue", "Wed", "Thu", "Fri")) - 1
            } else {
                  NA
            }
      }, Week, Day)) %>%
      mutate(Date = as.Date(Date)) %>%
      mutate(Price = as.numeric(Price)) %>%
      filter(!is.na(Price)) %>%
      select(Date, Price)

# Save the cleaned table to a CSV file
write.csv(cleaned_table, file = "C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/Cushing_OK_WTI_Spot_Price.csv", row.names = FALSE)

# Display a confirmation message
cat("Data has been saved to 'C:/Users/HP/Downloads/Predicting_Metal_Cost_with_R-master-20240318T152350Z-001/Predicting_Metal_Cost_with_R-master/data/Cushing_OK_WTI_Spot_Price.csv'.\n")

# Test the date extraction
test_dates <- c("Jun- 3 to Jun- 7 2024", "Jun-10 to Jun-14 2024")
sapply(test_dates, extract_start_date)

