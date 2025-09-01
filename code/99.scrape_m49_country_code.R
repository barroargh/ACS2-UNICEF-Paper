# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Scrape UNSD M49 country code 
# Author: Daniele Barro
# Date: 25th July 2025
# Editied by: DB
# Last edit: 

# TOC:
#   1. Scrape data from "https://unstats.un.org/unsd/methodology/m49/"
#   2. Clean data
#   3. Export data


# Input: UNSTASTS website: "https://unstats.un.org/unsd/methodology/m49/"

# Output: excel file with all M49 country code for the entire WORLD - UNSD_M49_Country_Codes.xlsx

# 1. Scrape data ----------------------------------------------------------

# Scrape m49 country codes ------------------------------------------------
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# URL of the UNSD M49 page
url <- "https://unstats.un.org/unsd/methodology/m49/"

# Read the HTML content with UTF-8 encoding
page <- read_html(url, encoding = "UTF-8")

# Extract the first table
country_data <- page %>%
  html_table(fill = TRUE) %>%
  .[[1]]

# View the first few rows
head(country_data)

# 2. Clean data -----------------------------------------------------------

# rename column names
country_data_clean = country_data %>%
  dplyr::rename(
    country_name = `Country or Area`,
    m4_code = `M49 code`,
    iso_apha_code = `ISO-alpha3 code`
  )

# Clean country names to remove special characters
country_data_clean$country_name = iconv(country_data_clean$country_name, from = "UTF-8", to = "ASCII//TRANSLIT")

# update country name to match with the ccri data and the other dataset
country_data_clean_new = country_data_clean %>%
  dplyr::mutate(country_name = dplyr::case_when(
    country_name == "Cabo Verde" ~ "Cape Verde",
    country_name == "Republic of Moldova" ~ "Moldova",  # or "Moldovia" depending on context
    country_name == "Netherlands (Kingdom of the)" ~ "Netherlands",
    country_name == "Turkiye" ~ "Republic of Turkiye",
    country_name == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom of Great Britain & Northern Ireland",
    country_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country_name == "Swaziland" ~ "Eswatini",
    TRUE ~ country_name
  ))

# join with african regions
df_country_code_regions = left_join(country_data_clean_new,african_countries, by = "country_name")


# 3. Export data ----------------------------------------------------------
# Create worksheet
wb_new <- createWorkbook()

# Add worksheet and write data
addWorksheet(wb_new, sheetName = "unsd_m49_codes")

writeData(wb_new, sheet = "unsd_m49_codes", df_country_code_regions)

# Save the Excel file
saveWorkbook(wb_new, file.path(country_code, "/UNSD_M49_Country_Codes.xlsx"), overwrite = TRUE)
