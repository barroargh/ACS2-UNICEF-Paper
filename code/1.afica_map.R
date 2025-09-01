
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Africa Map               
# Author: Daniele Barro
# Date: 19th June 2025
# Editied by: DB
# Last edit: 
#  
#      
# Content
#     1. Prepare shape file for Sub-Region map of Africa 
#     2. Plot map of Africa with sub-regions and average temperature  --------  source WMO 2024 Report: https://wmo.int/publication-series/state-of-global-climate-2024
#     3. Save plot 
#     4. Print list of all country in Africa sub-regions

# Input: shape-file of africa - look into "01.import_dta.R script" for details

# Output: There are 2 outputs
#       1. Map of Africa - african_subregions.png
#       2. Table of all country in each Africa sub-regions - word document: african_countries_per_subregions.docx


# 1. Prepare shape file for Sub-Region map of Africa ------------------------------

# Choose a coordinate reference system (CRS) suited for Africa
africa_crs <- "+proj=aea +lat_1=-18 +lat_2=21 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Merge geometries by sub-region
sub_regions <- shape_data %>%
  select(sub_region, geometry) %>%
  dplyr::group_by(sub_region) %>%
  dplyr::summarise(geometry = st_union(geometry), .groups = "drop")

# simplify the picture - it allows to process the graph faster
sub_regions_simplified <- ms_simplify(sub_regions, keep = 0.2, keep_shapes = TRUE)

# Re-project
sub_regions_proj <- st_transform(sub_regions_simplified, crs = africa_crs)

# Compute centroids and merge with temperature labels
centroids <- st_centroid(sub_regions_proj)
centroid_coords <- st_coordinates(centroids)

# Combine coordinates with temperature labels
temp_labels <- centroids %>%
  mutate(
    lon = centroid_coords[, 1],
    lat = centroid_coords[, 2]
  ) %>%
  select(sub_region, lon, lat) %>%
  left_join(
    data.frame(
      sub_region = c("North Africa", "West Africa", "Central Africa","East Africa", "Southern Africa", "Indian Ocean island countries"),
      mean_temp_label = c(
        "<b>2.14 °C</b><br><span style='font-size:12pt'>[1.99–2.26 °C]</span>",
        "<b>1.40 °C</b><br><span style='font-size:12pt'>[1.28–1.48 °C]</span>",
        "<b>1.37 °C</b><br><span style='font-size:12pt'>[1.09–1.54 °C]</span>",
        "<b>1.37 °C</b><br><span style='font-size:12pt'>[1.01–1.57 °C]</span>",
        "<b>1.43 °C</b><br><span style='font-size:12pt'>[1.27–1.59 °C]</span>",
        "<b>1.21 °C</b><br><span style='font-size:12pt'>[1.06–1.48 °C]</span>"
      )
    ),
    by = "sub_region"
  )


# 2. Plot map of Africa with sub-regions and average temperature  --------  source WMO 2024 Report: https://wmo.int/publication-series/state-of-global-climate-2024

africa_regions <- ggplot(data = sub_regions_proj) +
  geom_sf(aes(fill = sub_region), color = "black", linewidth = 0.3) +
  
  # Title and caption
  labs(
    title = "African Sub-Regions",
    caption = "\n\nSource shape file: ICPAC GeoPortal;\nSource mean sub-regional temperature: WMO 2024 - 2024 anomalies relative to 1961–1990",
    x = NULL,
    y = NULL
  ) +
  
  # Temperature labels on map
  geom_richtext(
    data = temp_labels,
    aes(x = lon, y = lat, label = mean_temp_label),
    size = 5,
    fill = NA,     # no box background
    label.color = NA,  # no border
    label.padding = grid::unit(rep(0, 4), "pt"),  # remove padding
    color = "black",
    text.colour = "black",
    hjust = 0.5,
    vjust = 0.5
  ) +
  
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
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 8),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) + 
  
  # Fill color of sub-regions
  scale_fill_manual(
    name = "Sub-Region",
    values = c(
      "North Africa" = "#FB8072",
      "East Africa" = "#FFFFB3",
      "West Africa" =  "#FDB462",
      "Southern Africa" = "#80B1D3",
      "Central Africa" = "#8DD3C7",
      "Indian Ocean island countries" ="#BEBADA" 
    )
  )


# 3. Save plot ------------------------------------------------------------
ggsave(out_path_africa_subregions_map, plot = africa_regions, width = 10, height = 8, dpi = 150, limitsize = FALSE)


# 4. Print list of all country in Africa sub-regions ----------------------

# use dataset "african_countries" generated from the script 98.afica_sub_regions

# Print countries grouped by sub-region
split(african_countries$Country, african_countries$Sub_Region)

# Assuming your long-format data frame is called african_countries
pivot_table <- african_countries %>%
  dplyr::mutate(row = row_number()) %>%
  pivot_wider(
    names_from = Sub_Region,
    values_from = Country
  ) %>%
  select(-row)

# Step 1: Apply na.omit to each column to remove NAs
clean_columns <- lapply(pivot_table, function(col) na.omit(col))

# Step 2: Find the longest column (to pad others)
max_length <- max(sapply(clean_columns, length))

# Step 3: Pad all columns to the same length (with NA at the bottom)
padded_columns <- lapply(clean_columns, function(col) {
  length(col) <- max_length
  return(col)
})

# Step 4: Combine back into a data frame
df_cleaned <- as.data.frame(padded_columns)

# Step 5: Rename columns (customize as needed)
colnames(df_cleaned) <- c(
  "North Africa",
  "East Africa",
  "West Africa",
  "Southern Africa",
  "Central Africa",
  "Indian Ocean island countries"
)

# remove NAs for the table
df_cleaned[is.na(df_cleaned)] <- ""

kable(df_cleaned, caption = "Countries by African Subregion", align = "l")

# Replace NAs
df_cleaned[is.na(df_cleaned)] <- ""

# Create a flextable
ft <- flextable(df_cleaned)
ft <- set_caption(ft, caption = "Table X. Countries by African Subregion") %>%
  
  # ✅ Set Specific Width for Each Column Instead of Full Table
  align(align = "center", part = "all") %>%
  width(j = "North Africa", width = 1.5) %>%
  width(j = "East Africa", width = 1.5) %>%
  width(j = "West Africa", width = 1.5) %>%
  width(j = "Southern Africa", width = 1.5) %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Calibri", part = "all")      # 👈 Set font type here

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste0(out_fld_maps,"/african_countries_per_subregions.docx"))
