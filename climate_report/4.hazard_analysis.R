
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Hazard analysis  
# Author: Daniele Barro
# Date: 25th July 2025
# Editied by: DB
# Last edit: 
# 
# ToC:
#   1. Import data
#   2. Define variable of interest from the CCRI 2.0 dataset 
#   3. Prep data
#   4. Generate and save continental Hazard Exposure maps and sub-regions Hazard Exposure summary


# Input: 1. Government expenditure in health - UNSTAST: https://unstats.un.org/sdgs/dataportal/database

# Output: 1. Historical expenditure on health (% of gov. budget) for African sub-regions - gov_expenditure_health_benchmark.png
#         2. Historical expenditure on health (% of gov. budget): reference reference economies, Africa, Europe, World - gov_expenditure_refrence.png
#         3. Table with all % expenditures on health per year - africa_summary_table_health.png 

# 1. Import data -------------------------------------------------------------

# Country M49 codes -------------------------------------------------------
df_m49_codes = read_excel(file.path(country_code,"UNSD_M49_Country_Codes.xlsx"))

# Hazard data - CCRI 2.0 --------------------------------------------------
ccri_dta_file <- file.path(ccri_dta_fld,"ccri_20250723.xlsx")

ccri_dta_raw <- read_excel(
  ccri_dta_file,
  sheet = "Hazard Exposure "
)

# 2. Define variable of interest from the CCRI 2.0 dataset  ---------------

# Define all the hazard maps -----------
# Define the vectors
hazards_n <- c(
  "children_exposed_riverine_floods_n",
  "children_exposed_coastal_floods_n",
  "children_exposed_tropical_storms_n",
  "children_exposed_agri_droughts_n",
  "children_exposed_heatwave_freq_n",
  "children_exposed_heatwave_duration_n",
  "children_exposed_fire_freq_n",
  "children_exposed_sand_dust_storm_n",
  "children_exposed_air_pollution_pm25_n",
  "children_exposed_malaria_pv_n",
  "children_exposed_malaria_pf_n",
  "children_exposed_2plus_hazards_n",
  "children_exposed_5plus_hazards_n",
  "children_exposed_8plus_hazards_n"
  
)

hazards_pct <- c(
  "children_exposed_riverine_floods_pct",
  "children_exposed_coastal_floods_pct",
  "children_exposed_tropical_storms_pct",
  "children_exposed_agri_droughts_pct",
  "children_exposed_heatwave_freq_pct",
  "children_exposed_heatwave_duration_pct",
  "children_exposed_fire_freq_pct",
  "children_exposed_sand_dust_storm_pct",
  "children_exposed_air_pollution_pm25_pct",
  "children_exposed_malaria_pv_pct",
  "children_exposed_malaria_pf_pct",
  "children_exposed_2plus_hazards_pct",
  "children_exposed_5plus_hazards_pct",
  "children_exposed_8plus_hazards_pct"
)

hazard_name <- c(
  "riverine_floods",
  "costal_floods",
  "tropical_storms",
  "agri_droughts",
  "heatwave_freq",
  "heatwave_duration",
  "fire_freq",
  "sand_dust_storm",
  "air_pollution_pm25",
  "malaria_pv",
  "malaria_pf",
  "2_plus_hazard",
  "5_plus_hazard",
  "8_plus_hazard"
)

hazard_file_name <- c(
  "Riverine Floods",
  "Costal Floods",
  "Tropical Storms",
  "Agricultural Droughts",
  "Heatwave Frequency",
  "Heatwave Duration",
  "Fire Frequency",
  "Sand Dust Storm",
  "Air Pollution pm25",
  "Malaria PV",
  "Malaria PF",
  "2 Plus Hazards",
  "5 Plus Hazards",
  "8 Plus Hazards"
)

# Combine into a data frame
hazards_matrix <- data.frame(
  hazard_n = hazards_n,
  hazard_pct = hazards_pct,
  hazard_name = hazard_name,
  hazard_file_name = hazard_file_name,
  stringsAsFactors = FALSE
)

# View the result
print(hazards_matrix)


# 3. Prep data -----------------------------------------------------------
# change variable names
df_ccri <- ccri_dta_raw %>%
  dplyr::rename(
    country_name = Country,
    wb_income_group = "World Bank Income Group",
    unicef_regional_office = "UNICEF Regional Office",
    oecd_fragility = "OECD Fragility",
    total_population = "Total Population",
    children_population = "Children (Under 18) Population",
    children_exposed_riverine_floods_n = "Number of Children Exposed to Riverine Floods",
    children_exposed_riverine_floods_pct = "Percentage of Children Exposed to Riverine Floods",
    children_exposed_coastal_floods_n = "Number of Children Exposed to Coastal Floods",
    children_exposed_coastal_floods_pct = "Percentage of Children Exposed to Coastal Floods",
    children_exposed_tropical_storms_n = "Number of Children Exposed to Tropical Storms",
    children_exposed_tropical_storms_pct = "Percentage of Children Exposed to Tropical Storms",
    children_exposed_agri_droughts_n = "Number of Children Exposed to Agricultural Droughts",
    children_exposed_agri_droughts_pct = "Percentage of Children Exposed to Agricultural Droughts",
    children_exposed_meteo_droughts_spei_n = "Number of Children Exposed to Meteorological Droughts (spei)",
    children_exposed_meteo_droughts_spei_pct = "Percentage of Children Exposed to Meteorological Droughts (spei)",
    children_exposed_meteo_droughts_spi_n = "Number of Children Exposed to Meteorological Droughts (spi)",
    children_exposed_meteo_droughts_spi_pct = "Percentage of Children Exposed to Meteorological Droughts (spi)",
    children_exposed_heatwave_freq_n = "Number of Children Exposed to Heatwave Frequency",
    children_exposed_heatwave_freq_pct = "Percentage of Children Exposed to Heatwave Frequency",
    children_exposed_heatwave_duration_n = "Number of Children Exposed to Heatwave Duration",
    children_exposed_heatwave_duration_pct = "Percentage of Children Exposed to Heatwave Duration",
    children_exposed_heatwave_severity_n = "Number of Children Exposed to Heatwave Severity",
    children_exposed_heatwave_severity_pct = "Percentage of Children Exposed to Heatwave Severity",
    children_exposed_extreme_heat_n = "Number of Children Exposed to Extreme Heat",
    children_exposed_extreme_heat_pct = "Percentage of Children Exposed to Extreme Heat",
    children_exposed_fire_freq_n = "Number of Children Exposed to Fire Frequency",
    children_exposed_fire_freq_pct = "Percentage of Children Exposed to Fire Frequency",
    children_exposed_fire_intensity_n = "Number of Children Exposed to Fire Intensity",
    children_exposed_fire_intensity_pct = "Percentage of Children Exposed to Fire Intensity",
    children_exposed_sand_dust_storm_n = "Number of Children Exposed to Sand and Dust Storm",
    children_exposed_sand_dust_storm_pct = "Percentage of Children Exposed to Sand and Dust Storm",
    children_exposed_air_pollution_pm25_n = "Number of Children Exposed to Air Pollution (PM2.5)",
    children_exposed_air_pollution_pm25_pct = "Percentage of Children Exposed to Air Pollution (PM2.5)",
    children_exposed_malaria_pv_n = "Number of Children Exposed to Malaria (Pv)",
    children_exposed_malaria_pv_pct = "Percentage of Children Exposed to Malaria (Pv)",
    children_exposed_malaria_pf_n = "Number of Children Exposed to Malaria (Pf)",
    children_exposed_malaria_pf_pct = "Percentage of Children Exposed to Malaria (Pf)",
    children_exposed_1plus_hazards_n = "Number of Children Exposed to at-least 1 hazard",
    children_exposed_1plus_hazards_pct = "Percentage of Children Exposed to at-least 1 hazard",
    children_exposed_2plus_hazards_n = "Number of Children Exposed to at-least 2 hazards",
    children_exposed_2plus_hazards_pct = "Percentage of Children Exposed to at-least 2 hazards",
    children_exposed_3plus_hazards_n = "Number of Children Exposed to at-least 3 hazards",
    children_exposed_3plus_hazards_pct = "Percentage of Children Exposed to at-least 3 hazards",
    children_exposed_4plus_hazards_n = "Number of Children Exposed to at-least 4 hazards",
    children_exposed_4plus_hazards_pct = "Percentage of Children Exposed to at-least 4 hazards",
    children_exposed_5plus_hazards_n = "Number of Children Exposed to at-least 5 hazards",
    children_exposed_5plus_hazards_pct = "Percentage of Children Exposed to at-least 5 hazards",
    children_exposed_6plus_hazards_n = "Number of Children Exposed to at-least 6 hazards",
    children_exposed_6plus_hazards_pct = "Percentage of Children Exposed to at-least 6 hazards",
    children_exposed_7plus_hazards_n = "Number of Children Exposed to at-least 7 hazards",
    children_exposed_7plus_hazards_pct = "Percentage of Children Exposed to at-least 7 hazards",
    children_exposed_8plus_hazards_n = "Number of Children Exposed to at-least 8 hazards",
    children_exposed_8plus_hazards_pct = "Percentage of Children Exposed to at-least 8 hazards",
    children_exposed_9plus_hazards_n = "Number of Children Exposed to at-least 9 hazards",
    children_exposed_9plus_hazards_pct = "Percentage of Children Exposed to at-least 9 hazards",
    children_exposed_10plus_hazards_n = "Number of Children Exposed to at-least 10 hazards",
    children_exposed_10plus_hazards_pct = "Percentage of Children Exposed to at-least 10 hazards",
    children_exposed_multi_hazard_75p_n = "Number of Children Exposed to Multi-Hazard Intensity (75th Percentile)",
    children_exposed_multi_hazard_75p_pct = "Percentage of Children Exposed to Multi-Hazard Intensity (75th Percentile)",
    children_exposed_multi_hazard_80p_n = "Number of Children Exposed to Multi-Hazard Intensity (80th Percentile)",
    children_exposed_multi_hazard_80p_pct = "Percentage of Children Exposed to Multi-Hazard Intensity (80th Percentile)",
    children_exposed_multi_hazard_85p_n = "Number of Children Exposed to Multi-Hazard Intensity (85th Percentile)",
    children_exposed_multi_hazard_85p_pct = "Percentage of Children Exposed to Multi-Hazard Intensity (85th Percentile)",
    children_exposed_multi_hazard_90p_n = "Number of Children Exposed to Multi-Hazard Intensity (90th Percentile)",
    children_exposed_multi_hazard_90p_pct = "Percentage of Children Exposed to Multi-Hazard Intensity (90th Percentile)",
    children_exposed_multi_hazard_95p_n = "Number of Children Exposed to Multi-Hazard Intensity (95th Percentile)",
    children_exposed_multi_hazard_95p_pct = "Percentage of Children Exposed to Multi-Hazard Intensity (95th Percentile)"
  ) %>%
  arrange(country_name) %>%
  dplyr::mutate(
    country_name = case_when(
      country_name == "CÃ´te d'Ivoire" ~ "Cote d'Ivoire",
      TRUE ~ country_name
    )
  )

# merge ccri data with m49 country code
df_ccri_new = left_join(df_ccri,df_m49_codes, by ="country_name") %>%
  # move m4_code,iso_apha_code in front 
  select(1, c(m4_code,iso_apha_code,region), everything()) %>%
  # select variables relevant only numbers of exposed children
  # select(country_name,m4_code,iso_apha_code,region,total_population,children_population,
    #     ends_with("pct"),ends_with("n")
    #    ) %>%
  # get only African countries
  filter(!is.na(region))


# test - TO BE DELETED
country_highest_hazard <- df_ccri_new %>%
  filter(children_exposed_riverine_floods_pct>20) %>%
  select(country_name,children_exposed_riverine_floods_pct,region) %>%
  arrange(desc(children_exposed_riverine_floods_pct))

print(country_highest_hazard)


#   4. Generate and save continental Hazard Exposure maps and summaries --------
for (i in 1:nrow(hazards_matrix)) {
  hazard_n <- hazards_matrix[i,1]
  hazard_pct <- hazards_matrix[i,2] 
  hazard_name <- hazards_matrix[i,3]
  hazard_fname <- hazards_matrix[i,4]
  
  message("Processing: ", hazard_name)
  
# Hazard dataset (floods)
df_floods = df_ccri_new %>%
  select(country_name,
         m4_code,
         iso_apha_code,
         region,
         total_population,
         children_population,
         !!sym(hazard_n),
         !!sym(hazard_pct))

# Join shapefile with flood data
shape_data_joined <- shape_data %>%
  left_join(df_floods, by = c("ADM0_NAME" = "country_name"))


# simplify the picture - it allows to process the graph faster
sub_regions_simplified <- ms_simplify(shape_data_joined, keep = 0.2, keep_shapes = TRUE)

# Plot choropleth map

hazard_riverine_floods <- ggplot(data = sub_regions_simplified) +
  geom_sf(aes(fill = !!sym(hazard_pct)), color = "white", linewidth = 0.3) +
  
  scale_fill_viridis_c(option = "C", na.value = "grey90", name = "% Children Exposed") +
  
# Title and caption
labs(title = paste0("Children Exposed to ", hazard_fname, " in Africa"),
     caption = "Source: UNICEF ccri2 data") +
  
  # Theme
  theme_minimal() +
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 15),
    panel.grid = element_blank(),
    #panel.background = element_rect(fill = "white", color = NA),
    #plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 14, face = "italic", hjust = 0)
  ) +
  
  # Accurate scale bar and north arrow (only works with projected CRS)
 # annotation_scale(location = "bl", width_hint = 0.5, text_cex = 2, height = unit(0.4, "cm")) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 8),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

ggsave(file.path(
  out_fld_hazard_maps,
  paste0("children_", hazard_name,"_map.png")),
  plot = hazard_riverine_floods, 
  width = 10, 
  height = 8, 
  dpi = 150, 
  limitsize = FALSE
)

message("Saved map: ", hazard_fname)

# SUB-REGIONAL table (region/continent) -------------------
# summary table of children affected by floods
summary_table <- df_floods %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    total_children_population = sum(children_population, na.rm = TRUE),
    total_children_affected = sum( !!sym(hazard_n), na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    pct_children_affected = (total_children_affected / total_children_population) * 100
  )

# Add continent-wide summary
continent_summary <- df_floods %>%
  dplyr::summarise(
    region = "Continent Total",
    total_children_population = sum(children_population, na.rm = TRUE),
    total_children_affected = sum( !!sym(hazard_n), na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    pct_children_affected = (total_children_affected / total_children_population) * 100
  )

# Print table 
# Combine region and continent summaries
final_summary <- bind_rows(summary_table, continent_summary)

# Create a nicely formatted table
gt_table = final_summary %>%
  gt() %>%
  tab_header(
    title = md(paste0("**Children Affected by ",  hazard_fname, "**")),
    subtitle = "Summary by Region/Continent"
  ) %>%
  fmt_number(
    columns = c(total_children_population, total_children_affected),
    sep_mark = ",",
    decimals = 0
  ) %>%
  fmt_number(
    columns = pct_children_affected,
    decimals = 2,
    suffixing = TRUE,
    pattern = "{x}%"
  ) %>%
  cols_label(
    region = "Region",
    total_children_population = "Total Children",
    total_children_affected = "Children Affected",
    pct_children_affected = "% Affected"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = region == "Continent Total")
  )

# Save the table
gtsave(gt_table, file.path(out_fld_hazard_maps,paste0("children_",  hazard_name,"_summary.png")))
message("Saved summary stats: ", hazard_fname)
}






