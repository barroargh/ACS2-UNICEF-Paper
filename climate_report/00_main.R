
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: master script               
# Author: Daniele Barro
# Date: 10th February 2025
# Editied by: DB
# Last edit: 
#  
#      
# Content
#     1. UNICEF official colors
#     2. GENERAL SET UP (load packages, and common functions)
#     3. SET-UP FOLDER STRUCTURE
#     4. MASTER SET-UP
#     5. RUN DO FILES

# 1. UNICEF official colors  --------------------------------------------------

# UNICEF BLUE
# WHITE  = FFFFFF
# light blue = #B3E7FA
# LIGHT blue - 1 = #6DCFF6
# LIGHT blue - 2 = #40C2F3
# LIGHT blue - 3 = #00AEEF

unicef_white ="#FFFFFF"
light_blue1 = "#B3E7FA"
light_blue2 = "#6DCFF6"
light_blue3 = "#40C2F3"
light_blue4 = "#00AEEF"

unicef_blue = "#00AEEF"
# GRAY
# light gray = #D7D2CB
# light gray = #BEBAB6
# light gray = #818180
# light gray = #6E6968
# light gray = #000000
gray = "#D7D2CB"
gray1 = "#BEBAB6"
gray2 = "#818180"
gray3 = "#6E6968"
black1 = "#000000"

unicef_gray = gray1
# LIGHT BLUE - 70/50/30/15 % 
#9ADBE8 - 100%
lightblue = "#9ADBE8"

# DARK BLUE - 70/50/30/15 % 
#0047BB - 100%
dark_blue = "#0047BB"

# GREEN - 70/50/30/15 % 
#004C45 - 100%
dark_green = "#004C45"

# LIGHT YELLOW-ORANGE - 70/50/30/15 % 
#FFB500 - 100%
light_yellow = "#FFB500"
# ORANGE - 70/50/30/15 %
#FF8200 - 100%
orange = "#FF8200" 

# GENERAL COLORO USED
# unicef_blue
# orange
# gray
# dark_blue


# 2.  GENERAL SET UP ---------------------------------------------------------------
options(scipen = 999)
options(max.print = 5000)
options(tibble.width = Inf)
memory.limit(size = 8000)

# Run the install.packages() lines if you haven't installed there packages yet

# REQUIRED PACKAGE -- install.packages("anthroplus")
# List of required packages
required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "tidyselect",
  "data.table",
  "readxl",
  "writexl",
  "openxlsx",
  "ggplot2",
  "rmarkdown",
  "rlang",
  "purrr",
  "gt",
  "webr",
  "ggthemes",
  "ggrepel",
  "Sf",
  "rmapshaper",
  "ggspatial",
  "ggtext",
  "ggnewscale",
  "patchwork"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)

library(here)
library(readr) 
library(dplyr) 
library(tidyr) 
library(tidyselect)
library(data.table)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(officer)  # For PowerPoint export
library(rvg) 
library(scales)  # For formatting as currency
library(rmarkdown)
library(scales)  # For formatting
library(plyr)    # For rounding to nearest multiple
library(lubridate)
library(rlang)
library(purrr)
library(gt)
library(flextable)
library(webr)
library(ggthemes)
library(ggrepel)
library(rlang)
library(stringr)
library(sf)
library(rmapshaper)
library(scales)
library(ggspatial)
library(ggtext)
library(haven)
library(tibble)
library(gt)
library(webshot2)
library(ggnewscale)
library(patchwork)  
library(knitr)
# for combining plots

# FUNCTIONS ---------------------------------------------------------------




# 3. SET-UP FOLDER STRUCTURE ----------------------------------------------

here::here()
# a. IMPUT DATA
# Define relative path using `here()`
input_dta_fld = here::here("input")
# education fld
edu_fld = paste0(input_dta_fld,"/education_dta")
# shape file
shp_file_fld = paste0(input_dta_fld,"/shpfile_africa")

# government spending folder
gov_spending_fld = paste0(input_dta_fld,"/gov_budget")
# UNSD M49 country code
country_code = paste0(input_dta_fld,"/country_code")
# CCRI data
ccri_dta_fld = paste0(input_dta_fld,"/ccri")

  
# shared folder draft paper
shared_fld_draf_paper = "C:/Users/dbarro/UNICEF/ESARO-PPM Collaboration Site - Research/ESARO/8.Advocacy & Communication/2025/Climate Summit - data and evidence rep/Draft paper/Immages"


# b.  CODE
code_data_root_fld = "C:/Users/dbarro/GitHub_UNICEF/unicef_code/ESARO/climate_report"


# c. OUTPUT 
out_fld = here::here("output")

out_fld_maps = paste0(out_fld,"/maps")
out_path_africa_subregions_map = file.path(out_fld_maps,"african_subregions.png")

out_fld_hazard_maps = paste0(out_fld,"/hazard_analysis")

out_fld_edu = paste0(out_fld,"/education")
out_fld_sp = paste0(out_fld,"/social_protection")
out_fld_health = paste0(out_fld,"/health")

# 4. MASTER SET-UP --------------------------------------------------------
    
# data preparation and cleaningcode_section_report_fld = "section_report/"
    
    africa_sub_regions        = 1
    scrape_m49_cc             = 1
    africa_country_code       = 1
    import_shp_file           = 1
    
    # Map of Africa with mean temperature
    africa_map                = 1
    
    # education analysis
    education_hazards         = 1 
    edu_gov_spending          = 1
    
    # health analysis
    health_gov_spending       = 1
    
    # Analysis of all CC hazards
    hazard_analysis           = 1
    
# 5. RUN DO FILES ---------------------------------------------------------

# Define Africa sub regions
if(africa_sub_regions == 1) {
  
  source(paste0(code_data_root_fld,  "98.africa_sub_regions.R"))
  
  print("Script to SCRAPE M49 country code - Done.")
  
}
    
# Scrape M49 Country CODE data  
if(scrape_m49_cc == 1) {
  
  source(paste0(code_data_root_fld,  "99.scrape_m49_country_code.R"))
  
  print("Script to SCRAPE M49 country code - Done.")
  
}

# Define Africa M49 country code
if(africa_country_code == 1) {
      
  source(paste0(code_data_root_fld,  "99.africa_country_code.R"))
      
  print("Script to define M49 country code used in the analysis - Done.")
      
}
    
# Import shape file
if(import_shp_file == 1) {
      
  source(paste0(code_data_root_fld,  "01.import_dta.R"))
      
  print("Script to import Shape File - Done.")
      
}
    
# CLIMATE ANALYSIS --------------------------------------------------------

# AFRICA MAP - WITH AVG. TEMPERATURE  
if(africa_map == 1) {
  
  source(paste0(code_data_root_fld,  "1.afica_map.R"))
  
  print("Script AFRICA MAP - Done.")
  
}

# EDUCATION ANALYSIS ------------------------------------------------------

# Analysis EDUCATION data  
if(education_hazards == 1) {
  
  source(paste0(code_data_root_fld,  "1.education.R"))
  
  print("Script EDUCATION analysis - Done.")
  
}

# Regional analysis of EDUCATION budget 
if(edu_gov_spending == 1) {
  
  source(paste0(code_data_root_fld,  "3.gov_spending_edu.R"))
  
  print("Script EDUCATION analysis of GOV budget - Done.")
  
}

# HEALTH ANALYSIS ---------------------------------------------------------

# Regional analysis of HEALTH budget 
if(health_gov_spending == 1) {
  
  source(paste0(code_data_root_fld,  "3.gov_spending_health.R"))
  
  print("Script HEALTH analysis of GOV budget - Done.")
  
}

# HAZARD ANALYSIS ---------------------------------------------------------
    
# Single and multi hazard analysis  
if(hazard_analysis == 1) {
      
  source(paste0(code_data_root_fld,  "4.hazard_analysis.R"))
      
  print("Script HAZARD analysis - Done.")
      
}
    
