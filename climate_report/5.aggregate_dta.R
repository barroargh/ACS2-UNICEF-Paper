# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Total number of children impacted in the continent
# Author: Daniele Barro
# Date: 7th August 2025
# Editied by: DB
# Last edit: 
# 
# Content
#   1. Import data
#   2. Clean data
#   3. Prepare for the summary
#   4. Print the results


ccri_dta_raw_children_total = df_ccri_new %>%
  dplyr::summarise(children_exposed_air_pollution_pm25_n = sum(children_exposed_air_pollution_pm25_n,na.rm =TRUE), 
                   children_exposed_heatwave_freq_n = sum(children_exposed_heatwave_freq_n,na.rm =TRUE),
                   children_exposed_tropical_storms_n = sum(children_exposed_tropical_storms_n,na.rm =TRUE),
                   children_exposed_riverine_floods_n = sum(children_exposed_riverine_floods_n,na.rm =TRUE),
                   children_exposed_agri_droughts_n = sum(children_exposed_agri_droughts_n,na.rm =TRUE),
                   children_exposed_malaria_pf_n = sum(children_exposed_malaria_pf_n,na.rm =TRUE),
                   children_exposed_fire_freq_n = sum(children_exposed_fire_freq_n,na.rm = TRUE),
                   children_exposed_sand_dust_storm_n = sum(children_exposed_sand_dust_storm_n, na.rm = TRUE),
                   children_population  = sum(children_population,na.rm = TRUE)
  ) %>%
  dplyr::mutate(# percentages
    children_exposed_air_pollution_pm25_pct = round(children_exposed_air_pollution_pm25_n/children_population *100,2), 
    children_exposed_heatwave_freq_pct = round(children_exposed_heatwave_freq_n/children_population *100,2), 
    children_exposed_tropical_storms_pct = round(children_exposed_tropical_storms_n/children_population *100,2), 
    children_exposed_riverine_floods_pct = round(children_exposed_riverine_floods_n/children_population *100,2),
    children_exposed_agri_droughts_pct = round(children_exposed_agri_droughts_n/children_population *100,2), 
    children_exposed_malaria_pf_pct = round(children_exposed_malaria_pf_n/children_population *100,2), 
    children_exposed_fire_freq_pct = round(children_exposed_fire_freq_n/children_population *100,2), 
    children_exposed_sand_dust_storm_pct = round(children_exposed_sand_dust_storm_n/children_population *100,2)
    )


# CCRI N long
ccri_long_n = pivot_longer(ccri_dta_raw_children_total, cols = c(children_exposed_air_pollution_pm25_n,
                                                                 children_exposed_heatwave_freq_n,
                                                                 children_exposed_tropical_storms_n,
                                                                 children_exposed_riverine_floods_n,
                                                                 children_exposed_agri_droughts_n,
                                                                 children_exposed_malaria_pf_n,
                                                                 children_exposed_fire_freq_n,
                                                                 children_exposed_sand_dust_storm_n,
), names_to = "ccri_type" , values_to = "value")%>%
  select(ccri_type,value) %>%
  dplyr::mutate(ccri_type = case_when(
    ccri_type == "children_exposed_air_pollution_pm25_n" ~ "Air pollution pm25",
    ccri_type == "children_exposed_heatwave_freq_n" ~ "Heatwaves Frequency",
    ccri_type == "children_exposed_tropical_storms_n" ~ "Tropical Storms",
    ccri_type == "children_exposed_riverine_floods_n" ~ "Riverine Floods",
    ccri_type == "children_exposed_agri_droughts_n" ~ "Agricultural Droughts",
    ccri_type == "children_exposed_malaria_pf_n" ~ "Malaria PF",
    ccri_type == "children_exposed_fire_freq_n" ~ "Fire Frequency",
    ccri_type == "children_exposed_sand_dust_storm_n" ~ "Sand and Dust Storms",
    
  ))


ccri_long_pct = pivot_longer(ccri_dta_raw_children_total, cols = c(children_exposed_air_pollution_pm25_pct,
                                                                   children_exposed_heatwave_freq_pct,
                                                                   children_exposed_tropical_storms_pct,
                                                                   children_exposed_riverine_floods_pct,
                                                                   children_exposed_agri_droughts_pct,
                                                                   children_exposed_malaria_pf_pct,
                                                                   children_exposed_fire_freq_pct,
                                                                   children_exposed_sand_dust_storm_pct), names_to = "ccri_type" , values_to = "value") %>%
  select(ccri_type,value) %>%
  dplyr::mutate(ccri_type = case_when(
    ccri_type == "children_exposed_air_pollution_pm25_pct" ~ "Air pollution pm25",
    ccri_type == "children_exposed_heatwave_freq_pct" ~ "Heatwaves Frequency",
    ccri_type == "children_exposed_tropical_storms_pct" ~ "Tropical Storms",
    ccri_type == "children_exposed_riverine_floods_pct" ~ "Riverine Floods",
    ccri_type == "children_exposed_agri_droughts_pct" ~ "Agricultural Droughts",
    ccri_type == "children_exposed_malaria_pf_pct" ~ "Malaria PF",
    ccri_type == "children_exposed_fire_freq_pct" ~ "Fire Frequency",
    ccri_type == "children_exposed_sand_dust_storm_pct" ~ "Sand and Dust Storms",
  )) %>%
  dplyr::rename(pct_value = value )


# merge total nbr. of children with percentage of totals
ccri_summary = merge(ccri_long_n,ccri_long_pct, by = "ccri_type")
# adjust format for value column
ccri_summary$value <- format(ccri_summary$value, big.mark = ",", scientific = FALSE)


# Create a nice table
gttbl <- ccri_summary %>%
  gt() %>%
  tab_header(
    title = "Summary of Continental Children Exposure to Hazards",
    subtitle = "Across All Regions"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  cols_label(
    value = "Nbr. of Children",
    pct_value = "(%) of children in the continent"
  ) %>%
  cols_align(
    align = "center",
    columns = c(value,pct_value)
  )
