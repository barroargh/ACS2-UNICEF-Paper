# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Sankey diagram    
# Author: Daniele Barro
# Date: 15th August 2025
# Editied by: DB
# Last edit: 
#  
#      
# Content
#     1. Define hazards to use
#     2. Transform to long format
#     3. Aggregate by region & hazard (population-adjusted)
#     4. Aggregate by hazard across all regions (population-adjusted)
#     5. Sankey diagram
#       a. Orders in the graph
#       b. Define nodes
#       c. Colors
#       d. Links
#       e. Color scale
#       f. Sankey plot
#       1. Save

# Input: shape-file of africa - look into "01.import_dta.R script" for details

# Output: There are 2 outputs
#       1. Map of Africa - african_subregions.png
#       2. Table of all country in each Africa sub-regions - word document: african_countries_per_subregions.docx



# 1. Define hazards to use -----------------------------------------------------

hazards_n <- c(
  "children_exposed_riverine_floods_n",
  "children_exposed_coastal_floods_n",
  "children_exposed_tropical_storms_n",
  "children_exposed_agri_droughts_n",
  "children_exposed_heatwave_freq_n",
  "children_exposed_fire_freq_n",
  "children_exposed_sand_dust_storm_n",
  "children_exposed_air_pollution_pm25_n",
  "children_exposed_malaria_pf_n"
)

# hazard names
hazard_names <- c(
  "Riverine Floods", "Costal Floods", "Tropical Storms", 
  "Agricultural Droughts", "Heatwave Frequency", "Fire Frequency",
  "Sand Dust Storm", "Air Pollution pm25", "Malaria PF"
)

hazard_map <- setNames(hazard_names, hazards_n)

# 2. Transform to long format --------------------------------------------------

df_hazard_long <- df_ccri_new %>%
  select(iso3, country_name, region, children_population, all_of(hazards_n)) %>%
  pivot_longer(cols = all_of(hazards_n), names_to = "hazard", values_to = "n_children") %>%
  mutate(hazard = as.character(hazard_map[hazard]))

# 3. Aggregate by region & hazard (population-adjusted) ------------------------

df_region <- df_hazard_long %>%
  dplyr::group_by(region, hazard) %>%
  dplyr::summarise(
    exposed_children = sum(n_children, na.rm = TRUE),
    total_children_pop = sum(children_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_region = exposed_children / total_children_pop    # % of children in the region exposed
  )

# 4. Aggregate by hazard across all regions (population-adjusted) --------------

df_hazard_totals <- df_hazard_long %>%
  dplyr::group_by(hazard) %>%
  dplyr::summarise(
    exposed_children = sum(n_children, na.rm = TRUE),
    total_children_pop = sum(children_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_global = exposed_children / total_children_pop * 100  # % of all children in Africa exposed
  )

# 5. Sankey diagram -----------------------------
# a. Orders
custom_order <- c(
  "Air Pollution pm25", "Malaria PF", "Heatwave Frequency",
  "Agricultural Droughts", "Fire Frequency",
  "Riverine Floods", "Sand Dust Storm", "Tropical Storms", "Costal Floods"
)

region_order <- df_region %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(total_pct = sum(pct_region), .groups = "drop") %>%
  arrange(total_pct) %>%  # ascending
  pull(region)

df_region <- df_region %>%
  mutate(hazard = factor(hazard, levels = custom_order))

# b. Define nodes
hazard_totals <- df_hazard_totals %>%
  mutate(hazard = as.character(hazard))

region_totals <- df_region %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(total_pct = sum(pct_region), .groups = "drop") %>%
  mutate(region = as.character(region))

nodes <- data.frame(
  name = c(custom_order, region_order),
  type = c(rep("hazard", length(custom_order)), rep("region", length(region_order))),
  stringsAsFactors = FALSE
) %>%
  mutate(
    label = ifelse(
      type == "hazard",
      paste0(name, " (", round(hazard_totals$pct_global[match(name, hazard_totals$hazard)], 1), "%)"),
      paste0(name, " (", round(region_totals$total_pct[match(name, region_totals$region)], 1), ")")
    )
  )

# c. Colors
hazard_colors <- c(
  "Malaria PF" = "#0047BB",
  "Riverine Floods" = "#005CE3",
  "Fire Frequency" = "#177DE7",
  "Air Pollution pm25" = "#49A1EE",
  "Agricultural Droughts" = "#2F8FEA",
  "Sand Dust Storm" = "#64B3F1",
  "Tropical Storms" = "#7EC4F4",
  "Costal Floods" = "#99D6F7",
  "Heatwave Frequency" = "#B3E7FA"
)

region_colors <- c(
  "North Africa" = "#FB8072",
  "East Africa" = "#FFFFB3",
  "West Africa" = "#FDB462",
  "Southern Africa" = "#80B1D3",
  "Central Africa" = "#8DD3C7",
  "Indian Ocean island countries" = "#BEBADA"
)

all_colors <- c(hazard_colors, region_colors)
nodes$color <- all_colors[nodes$name]
nodes$group <- nodes$name

# d.Links
links <- df_region %>%
  mutate(
    source = match(as.character(hazard), nodes$name) - 1,
    target = match(region, nodes$name) - 1,
    value = pct_region,
    group = hazard
  ) %>%
  select(source, target, value, group)

# e. Color scale
color_scale_js <- paste0(
  "d3.scaleOrdinal()",
  ".domain([", paste0('"', nodes$label, '"', collapse = ","), "])",
  ".range([", paste0('"', nodes$color, '"', collapse = ","), "])"
)

# f. Sankey plot
sankey_plot <- sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "label",
  NodeGroup = "group",
  LinkGroup = "group",
  fontSize = 16, nodeWidth = 30,
  colourScale = color_scale_js,
  iterations = 0,
  nodePadding = 10,
  width = 1000,
  height = 650
)

final_plot <- htmlwidgets::prependContent(
  sankey_plot,
  htmltools::tags$h3(
    "Breaking Down Children’s Hazard Exposure Across Africa",
    style = "text-align:center;"
  )
)

final_plot

# g. Save
saveWidget(final_plot, "temp.html", selfcontained = TRUE)
webshot("temp.html", file = file.path(out_fld, "sankey_graph.png"), cliprect = "viewport")
