# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Import the Africa shape file and do very basic cleaning of country name misalignment with other dataset in the analysis 
# Author: Daniele Barro
# Date: 7th August 2025
# Edited by: DB
# Last edit: 
# 

# Input: shape files downloaded from: https://open.africa/dataset/africa-shapefiles

# Import shape file for AFRICA ---------------------------------------------
shp_path = file.path(shp_file_fld,"afr_g2014_2013_0.shp")
shape_data <- st_read(shp_path) %>%
  # update shape file country name
  dplyr::mutate(ADM0_NAME = case_when(
    ADM0_NAME == "C\xf4te d'Ivoire" ~ "Cote d'Ivoire",
    ADM0_NAME == "Swaziland" ~ "Eswatini",
    TRUE ~ ADM0_NAME
  )) %>%
  mutate(ADM0_NAME = case_when(
    ADM0_CODE == 66 ~ "Cote d'Ivoire",
    TRUE ~ ADM0_NAME
  ))%>%
  arrange(ADM0_NAME)


# Then, join with african_countries to assign sub-regions
shape_data <- shape_data %>%
  left_join(african_countries, by = c("ADM0_NAME" = "country_name")) %>%
  dplyr::rename(sub_region = region)


