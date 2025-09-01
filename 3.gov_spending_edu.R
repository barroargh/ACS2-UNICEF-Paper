
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Government expenditure data       
# Author: Daniele Barro
# Date: 14th July 2025
# Editied by: DB
# Last edit: 
# 
# Content
#   1. Import data
#   2. Clean data
#   3. Generate graphs
#   4. Save graphs 
#   5. Table of government expenditure in Education (paper appendix)


# Input: 1. Government expenditure in Education - UNSTAST: https://unstats.un.org/sdgs/dataportal/database

# Output: 1. Historical expenditure on education (% of gov. budget) for African sub-regions - gov_expenditure_regional.png
#         2. Historical expenditure on education (% of gov. budget): reference reference economies, Africa, Europe, World - gov_expenditure_refrence.png
#         3. Table with all % expenditures on education per year - country_gov_expenditure.docx 


# 1. Import data ----------------------------------------------------------
# Government spending

# gov spending in education
gov_spending <- file.path(gov_spending_fld,"SG_XPD_EDUC.xlsx")

gov_spending_sdgs_raw <- read_excel(gov_spending)

# check length is the same
length(north_africa)+ length(east_africa)+ length(west_africa)+ length(southern_africa)+ length(central_africa)+ length(islands) == length(african_m49_codes)

# 2. Clean data -----------------------------------------------------------
# select African countries
african_countries = gov_spending_sdgs_raw %>%
  filter(GeoAreaCode %in% c(african_m49_codes,europe_m49_codes,world)) %>%
  select(-c(Goal,Indicator,SeriesCode,SeriesDescription,`Observation Status`,`Reporting Type`,Units))


# reshape into long
african_long <- african_countries %>% # african_countries dataset comes from the R script 98.africa_sub_regions.R
  pivot_longer(
    cols = `2000`:`2023`,           # specify year columns
    names_to = "Year",              # new column name for years
    values_to = "Value"             # new column name for values
  ) %>%
  mutate(Year = as.integer(Year))   # make Year numeric


african_long$Value <- as.numeric(african_long$Value)

# Group countries into regions --------------------------------------------

africa_long_region = african_long %>%
  mutate(region = case_when(
    GeoAreaCode %in% north_africa ~ "North Africa",
    GeoAreaCode %in% east_africa ~ "East Africa",
    GeoAreaCode %in% west_africa ~ "West Africa",
    GeoAreaCode %in% southern_africa ~ "Southern Africa",
    GeoAreaCode %in% central_africa ~ "Central Africa",
    GeoAreaCode %in% islands ~ "Indian Ocean island countries",
    GeoAreaCode %in% world ~ "World",
    GeoAreaCode %in% europe_m49_codes ~ "Europe",
    TRUE ~ "other"
  )) %>%
  arrange(region)

unique(africa_long_region$region)

# Africa region summary ---------------------------------------------------

africa_only_long = african_long %>%
  mutate(region = case_when(
    GeoAreaCode %in% african_m49_codes ~ "Africa",
    TRUE ~ "other"
  )) %>%
  arrange(region) %>%
  filter(region == "Africa")

unique(africa_only_long$region)


# Combine Africa region with African Continent ----------------------------

regional_perc_gove_edu_exp_all <- bind_rows(
  africa_long_region,
  africa_only_long
)
# Regional median values by country and year ------------------------------

regional_perc_gov_edu_exp = regional_perc_gove_edu_exp_all %>%
  dplyr::group_by(region, Year) %>%
  dplyr::summarise(regional_value_median = median(Value,na.rm = TRUE),
                   regional_value_mean = mean(Value,na.rm = TRUE)) %>%
  mutate(regional_value_median = case_when(
    regional_value_median == 0 ~ NA_real_,
    TRUE ~ regional_value_median
  ),
  regional_value_mean = case_when(
    regional_value_mean == 0 ~ NA_real_,
    TRUE ~ regional_value_mean
  )) %>%
  ungroup()



# regional summaries  ---------------------------------------------------
region_medians <- regional_perc_gov_edu_exp %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(median_value = median(regional_value_median, na.rm = TRUE)) %>%
  # Place label at the last year (or customize as needed)
  dplyr::mutate(region_label = paste0(region, " (Median: ", round(median_value, 1), ")"))

regional_perc_gove_edu_exp_labeled <- regional_perc_gov_edu_exp %>%
  left_join(region_medians, by = "region") %>%
  mutate(region = region_label)

# Create a named vector for colors
region_colors <- c(
  "North Africa" = "#EA5622",
  "East Africa" = "#E5E016",
  "West Africa" = "#DA8D10",
  "Southern Africa" = "#80B1D3",
  "Central Africa" = "#8DD3C7",
  "Indian Ocean island countries" = "#BEBADA",
  "World" = "black",
  "Europe" = "#0047BB",
  "Africa" = "#D24018"
)

region_labels <- region_medians %>%
  mutate(label = paste0(region, " (Median: ", round(median_value, 1), "%)")) %>%
  select(region, label) %>%   # ✅ ensure only two columns
  deframe()                   # converts to named vector



# Separate legend into Africa sub-regions and the Comparison --------------
regional_perc_gov_edu_exp <- regional_perc_gov_edu_exp %>%
  mutate(legend_group = case_when(
    region %in% c("North Africa", "East Africa", "West Africa", "Southern Africa", "Central Africa", "Indian Ocean island countries") ~ "Sub-Region",
    region %in% c("Africa", "Europe", "World") ~ "Reference"
  ))

# 3. Generate graphs------------------------------------------------------------
# Education expenditures
# Sub-region plot
sub_region_plot <- ggplot() +
  geom_line(
    data = regional_perc_gov_edu_exp %>% filter(legend_group == "Sub-Region"),
    aes(x = Year, y = regional_value_median, color = region),
    linewidth = 1.5
  ) +
  scale_color_manual(
    name = "African\nSub-Regions",
    values = region_colors[names(region_colors) %in% c("North Africa", "East Africa", "West Africa", "Southern Africa", "Central Africa", "Indian Ocean island countries")],
    labels = region_labels[names(region_labels) %in% c("North Africa", "East Africa", "West Africa", "Southern Africa", "Central Africa", "Indian Ocean island countries")]
  ) +
  
  labs(
    title = "Median % of Government Expenditure on Education – African Sub-Regions",
    y = "% Government expenditure",
    x = "Year"
  ) +
  theme_minimal() +
  
  theme(
    plot.title = element_blank(), # element_text(hjust = 0, size =30, face = "bold"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 23),
    legend.text = element_text(size = 20),
  ) +
  
  guides(color = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")

# Save Government expenditure in Education - sub-regions
ggsave(paste0(out_fld_edu,"/gov_expenditure_regional.png"), plot = sub_region_plot, width = 15, height = 6, dpi = 300)


# Reference region plot
reference_plot <- ggplot() +
  geom_line(
    data = regional_perc_gov_edu_exp %>% filter(legend_group == "Reference"),
    aes(x = Year, y = regional_value_median, color = region),
    linewidth = 1.5
  ) +
  scale_color_manual(
    name = "Reference Regions",
    values = region_colors[c("Africa", "Europe", "World")],
    labels = region_labels[c("Africa", "Europe", "World")]
  ) +
  labs(
    title = "Median % of Government Expenditure on Education – Benchmark",
    y = "% Government expenditure",
    x = "Year"
  ) +
  theme_minimal() +
  
  theme(
    plot.title = element_blank(), # element_text(hjust = 0, size =30, face = "bold"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 23),
    legend.text = element_text(size = 20),
  ) +
  
  guides(color = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")

# Save government expenditure in Education - Reference plot: Africa, Europe, World
ggsave(paste0(out_fld_edu,"/gov_expenditure_refrence.png"), plot = reference_plot, width = 15, height = 6, dpi = 300)



# 5. Table of government expenditure in Education (paper appendix) --------

# Table with countries details  -------------------------------------------

country_list = regional_perc_gove_edu_exp_all %>%
  filter(!is.na(Value),region!="Africa", region!="World",region!="Europe") %>%
  mutate(Value = round(Value,2)) %>%
  arrange(region,GeoAreaName,Year,Value) %>%
  select(-Target) %>%
  dplyr::group_by(GeoAreaName) %>%
  dplyr::summarize(country_meadian = median(Value),
                   region = first(region),
                   years_available = paste0(Year, " (", Value, ")") %>%
                     paste(collapse = ", "),
                   .groups = "drop") %>%
  arrange(region,GeoAreaName) %>%
  select(GeoAreaName,region,country_meadian,years_available)
colnames(country_list)

# Print the table to word -------------------------------------------------
# Create a flextable
ft <- flextable(country_list)
ft <- set_header_labels(ft,
                        GeoAreaName = "Country Name",
                        region = "Sub-Region",
                        country_meadian = "Median %",
                        years_available = "Available year (% g.e.)"
) %>%
  
  # ✅ Set Specific Width for Each Column Instead of Full Table
  align(align = "center", part = "all") %>%
  #autofit() %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Calibri", part = "all") %>%      # 👈 Set font type here
  width(j = "GeoAreaName", width = 1.5) %>%
  width(j = "region", width = 1.5) %>%
  width(j = "country_meadian", width = 1.5) %>%
  width(j = "years_available", width = 3) %>%
  # Set Title
  set_caption(ft, caption = "Table X. Median Percentage of Government Expenditure by Country")

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste0(out_fld_edu,"/country_gov_expenditure.docx"))

