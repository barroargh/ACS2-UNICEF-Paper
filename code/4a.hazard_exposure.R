# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Hazard exposure analysis
# Author: Daniele Barro
# Date: 1st September 2025
# Editied by: DB
# Last edit: 

# Content
#     1. Multi-hazards exposure 
#       a. Define hazards to use
#       b. Transform to long format
#       c. Graph
#    2. Hazard exposure 
#       a. Define hazards to use
#       b. Transform to long format
#       c. Graph
#   3. Save graphs

# Output: There are 2 outputs
#       1. Multi-hazards exposure graph - multihazard_exposure.png
#       2. Hazard exposure - hazards_prevalence.png


# 1. Multi-hazards exposure ----------------------------------------------------

# a. Define hazards to use -----------------------------------------------------

hazards_n <- c(
  "children_exposed_2plus_hazards_n",
  "children_exposed_3plus_hazards_n",
  "children_exposed_4plus_hazards_n",
  "children_exposed_5plus_hazards_n"
)

# hazard names
hazard_names <- c(
  "2+ hazards", 
  "3+ hazards", 
  "4+ hazards", 
  "5+ hazards"
)

hazard_map <- setNames(hazard_names, hazards_n)


# b. Transform to long format --------------------------------------------------
df_hazard_long <- df_ccri_new %>%
  select(iso3, country_name, region, children_population, all_of(hazards_n)) %>%
  pivot_longer(cols = all_of(hazards_n), names_to = "hazard", values_to = "n_children") %>%
  mutate(hazard = as.character(hazard_map[hazard]))


# Aggregate by hazard
df_region <- df_hazard_long %>%
  dplyr::group_by(hazard) %>%
  dplyr::summarise(
    exposed_children = sum(n_children, na.rm = TRUE),
    total_children_pop = sum(children_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_hazard_exposure = round(exposed_children / total_children_pop,2)   # % of children in the region exposed
  )

colnames(df_region)

# c. Graph ----------------------------------------------------

multihazard_exposure = ggplot(df_region, aes(y = pct_hazard_exposure, x = fct_reorder(hazard, pct_hazard_exposure))) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  
  geom_text(
    aes(x = hazard, y = pct_hazard_exposure, label = scales::label_percent(accuracy = 1)(pct_hazard_exposure)),
    hjust = -0.1,  # Slightly beyond the bar's end
    size = 5,
    fontface = "bold"
  ) +


  geom_col(fill = "#00AEEF", color = "black", width = 0.6) +
  
  coord_flip() +
  
  labs(
    title = "% of children exposed to multiple hazards",
    x = "Total Students Affected",
    y = "Disaster Type"
  ) +
  
  theme_minimal() + 
  
  theme(
    plot.title =  element_blank(), # element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.margin = margin(t = 20, r = 60, b = 20, l = 10),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "none"
  ) +
  ylim(0, 1.1)
multihazard_exposure

# 2. Hazard exposure  -------------------------------------------------
# a. Define hazards to use -----------------------------------------------------

hazards_n <- c(
  "children_exposed_riverine_floods_n",
  "children_exposed_tropical_storms_n",
  "children_exposed_agri_droughts_n",
  "children_exposed_heatwave_freq_n",
  "children_exposed_fire_freq_n",
  "children_exposed_sand_dust_storm_n",
  "children_exposed_air_pollution_pm25_n",
  "children_exposed_malaria_pf_n",
  "children_exposed_coastal_floods_n"
)

# hazard names
hazard_names <- c(
  "Riverine Floods", "Tropical Storms", 
  "Agricultural Droughts", "Heatwave Frequency", "Fire Frequency",
  "Sand Dust Storm", "Air Pollution pm25", "Malaria PF", "Coastal Floods"
)

hazard_map <- setNames(hazard_names, hazards_n)

# b. Transform to long format --------------------------------------------------

df_hazard_long <- df_ccri_new %>%
  select(iso3, country_name, region, children_population, all_of(hazards_n)) %>%
  pivot_longer(cols = all_of(hazards_n), names_to = "hazard", values_to = "n_children") %>%
  mutate(hazard = as.character(hazard_map[hazard]))

# c. Graph
df_region <- df_hazard_long %>%
  dplyr::group_by(hazard) %>%
  dplyr::summarise(
    exposed_children = sum(n_children, na.rm = TRUE),
    total_children_pop = sum(children_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_hazard_exposure = round(exposed_children / total_children_pop,5)   # % of children in the region exposed
  )



hazards = ggplot(df_region, aes(y = pct_hazard_exposure, x = fct_reorder(hazard, pct_hazard_exposure))) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  
  geom_text(
    aes(
      label = ifelse(
        pct_hazard_exposure < 0.01,                   # very small values
        scales::label_percent(accuracy = 0.1)(pct_hazard_exposure), # show as 0.3 instead of 0%
        scales::percent(pct_hazard_exposure, accuracy = 1)         # normal percentages
      )
    ),
    hjust = -0.1,
    size = 5,
    fontface = "bold"
  ) +
  
  geom_col(fill = "#00AEEF", color = "black", width = 0.6) +
  
  coord_flip() +
  
  labs(
    title = "% of children exposed to major hazard",
    x = "Total Students Affected",
    y = "Disaster Type"
  ) +
  
  theme_minimal() + 
  
  theme(
    plot.title = element_blank(), # element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.margin = margin(t = 20, r = 60, b = 20, l = 10),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_blank(),
    legend.position = "none"
  )+
  ylim(0, 1.1)
hazards
# 3. Save graphs  ---------------------------------------------------------
# graph 1
ggsave(paste0(out_fld_hazard_maps,"/multihazard_exposure.png"), plot = multihazard_exposure, width = 8, height = 6, dpi = 300)
# graph 2
ggsave(paste0(out_fld_hazard_maps,"/hazards_prevalence.png"), plot = hazards, width = 8, height = 6, dpi = 300)
