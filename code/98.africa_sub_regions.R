# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Create Africa sub-regions
# Author: Daniele Barro
# Date: 25th July 2025
# Editied by: DB
# Last edit: 
# 

# Input: this script has no input
# Output: this script has no output

# Create a data frame of African countries by sub-region
african_countries <- data.frame(
  country_name = c(
    "Algeria","Egypt","Libya","Morocco","Tunisia","Western Sahara","Hala'ib triangle","Ma'tan al-Sarra",
    "Burundi","Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Malawi","Mayotte","Reunion","Rwanda","Somalia",
    "United Republic of Tanzania","Uganda","Abyei","Ilemi triangle","South Sudan","Sudan",
    "Benin","Burkina Faso","Cape Verde","Cote d'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia",
    "Mali","Mauritania","Niger","Nigeria","Saint Helena","Senegal","Sierra Leone","Sao Tome and Principe","Togo",
    "Botswana","Lesotho","Namibia","South Africa","Eswatini","Zimbabwe","Zambia","Mozambique","Angola",
    "Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo","Equatorial Guinea","Gabon",
    "Madagascar","Mauritius","Seychelles"
  )
)


# Assign sub-regions using case_when logic
african_countries$region <- dplyr::case_when(
  african_countries$country_name %in% c("Algeria","Egypt","Libya","Morocco","Tunisia","Western Sahara","Hala'ib triangle","Ma'tan al-Sarra") ~ "North Africa",
  african_countries$country_name %in% c("Burundi","Comoros","Djibouti","Eritrea","Ethiopia","Kenya","Malawi","Mayotte","Reunion","Rwanda","Somalia",
                                        "United Republic of Tanzania","Uganda","Abyei","Ilemi triangle","South Sudan","Sudan") ~ "East Africa",
  african_countries$country_name %in% c("Benin","Burkina Faso","Cape Verde","Cote d'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia",
                                        "Mali","Mauritania","Niger","Nigeria","Saint Helena","Senegal","Sierra Leone","Sao Tome and Principe","Togo") ~ "West Africa",
  african_countries$country_name %in% c("Botswana","Lesotho","Namibia","South Africa","Eswatini","Zimbabwe","Zambia","Mozambique","Angola") ~ "Southern Africa",
  african_countries$country_name %in% c("Cameroon","Central African Republic","Chad","Congo","Democratic Republic of the Congo","Equatorial Guinea","Gabon") ~ "Central Africa",
  african_countries$country_name %in% c("Madagascar","Mauritius","Seychelles") ~ "Indian Ocean island countries",
  TRUE ~ "Others"
)

