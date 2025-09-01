
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Analysis EDUCATION data       
# Author: Daniele Barro
# Date: 9th July 2025
# Editied by: DB
# Last edit: 
# 
# Content
#   1. Import data
#       a. education dataset of major hazards in 2024 
#       b. global enrollment data
#   2. Adjust enrollment data
#   3. Clean data
#   4. Generate graphs
#       a. NBR of students affected by hazards
#       b. percentage of students affected by hazards
#   5. Save graphs 
#   6. Print the table of affected students

# Input: 1. Education data of major hazards as per the paper "Global snapshot climate related school disruption in 2024" - https://www.unicef.org/media/170626/file/Global-snapshot-climate-related-school-disruptions-2024.pdf
#        2. global enrollment data as per the UIS dataset - https://databrowser.uis.unesco.org/browser/EDUCATION/UIS-EducationOPRI/enrol-att/enrolment 

# Output: 1. Bar-charts of:
#           a. number of students affected by the hazards - std_affected_hazard.png
#           b. percentage of students affected by the hazards - pct_std_affected_hazard.png
#         2. List of all hazards for reference
#           a. hazard_edu_sector.docx

# 1. Import data ----------------------------------------------------------
# Hazard data for ESARO
num_var <- c("tot_stud_enrollment", "student_affected", "student_affected_if_counted", "per_affected_std")
education_dta <- file.path(edu_fld, "education_data.xlsx")

education_dta_raw <- read_excel(education_dta)
# Hazard data for Africa
hazard_raw = read_excel(
  education_dta,
  sheet = "hazards_countries"
)

# enrollment data as per UIS
enrollment_raw_path <- file.path(edu_fld, "uis_2024.dta")

enrollment_raw = read_dta("C:/Users/dbarro/UNICEF/ESARO-PPM Collaboration Site - Research/ESARO/8.Advocacy & Communication/2025/Climate Summit - data and evidence rep/Draft paper/data/sept2024data.dta")

# 2. Adjust enrollment data -------------------------------------------------------------

# select only indicator of interest - UNICEF target group
edu_indicator_list = c(
  "Enrolment in pre-primary education, both sexes (number)",
  "Enrolment in primary education, both sexes (number)",
  "Enrolment in lower secondary education, both sexes (number)",
  "Enrolment in upper secondary education, both sexes (number)"
)

# Define ESAR countries that are present in the dataset
# Process the data
enrollment_dta <- enrollment_raw %>%
  filter(indicator_label_en %in% edu_indicator_list) %>%
  filter(country_name_en %in% c(
    "Cameroon", "Chad", "Democratic Republic of the Congo", "Mali", "Mauritius",
    "Namibia", "Niger", "Nigeria", "Senegal", "Burundi", "Comoros", "Ethiopia",
    "Kenya", "Madagascar", "Malawi", "Mozambique", "Somalia", "South Africa",
    "South Sudan", "Uganda", "Tanzania", "Zimbabwe"
  )) 

# verify selection is correct
stopifnot(length(unique(enrollment_dta$indicator_label_en)) == 4)

# Get latest values and years per indicator per country
enrollment_info <- enrollment_dta %>%
  dplyr::group_by(country_name_en, indicator_label_en) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

# summarize total enrollment per country
enrollment_nbr <- enrollment_info %>%
  dplyr::group_by(country_name_en) %>%
  dplyr::summarise(
    enrollment = sum(value, na.rm = TRUE),
    
    # Create a note listing indicator and year pairs
    indicator_year_note = paste(
      paste(indicator_label_en, year, sep = ": "),
      collapse = "; "
    )
  )

# Non ESA country ---------------------------------------------------------
enrollment_nbr_updated = hazard_raw %>%
  filter(africa == "Yes", esa == "No") %>%
  select(-africa) %>%
  dplyr::rename(country_name_en = country,
                student_affected = nbr_std_affected,
                disaster = hazard)


# 2. Clean data  ----------------------------------------------------------

# Clean hazard esaro data
education_dta <- education_dta_raw %>%
  mutate(
    student_affected_if_counted = case_when(
      student_affected_if_counted == "N/A" ~ NA_real_,
      TRUE ~ as.numeric(student_affected_if_counted)
    ),
    per_affected_std = case_when(
      per_affected_std == "N/A" ~ NA_real_,
      TRUE ~ as.numeric(per_affected_std)
    )
  ) %>%
  mutate(across(all_of(num_var), as.numeric)) %>%
  dplyr::rename(country_name_en = cuntry)

# merge the hazard data with the new enrollment data
edu_enr_updated = full_join(education_dta,enrollment_nbr,by="country_name_en") %>%
  select(-tot_stud_enrollment) %>%
  mutate(per_affected_std = round((student_affected/enrollment)* 100,2)) %>%
  select(country_name_en,enrollment,disaster,desaster_detail,time,student_affected,student_affected_if_counted,per_affected_std,source,indicator_year_note)

# merge the hazard data for non esa country with the entire dataset
edu_enr_updated_africa = full_join(edu_enr_updated,enrollment_nbr_updated,by="country_name_en") %>%
  mutate(disaster = coalesce(disaster.x, disaster.y),
         student_affected = coalesce(student_affected.x,student_affected.y) )%>%
  select(-student_affected.x, -student_affected.y, disaster.x, disaster.y) %>%
  mutate(per_affected_std = round((student_affected/enrollment)* 100,2)) %>%
  select(country_name_en,enrollment,disaster,desaster_detail,time,student_affected,student_affected_if_counted,per_affected_std,source,indicator_year_note,esa) %>%
  mutate(esa = case_when(
    is.na(esa) ~ "Yes",
    TRUE ~ esa 
  )) %>%
  mutate(enrollment = case_when(
    enrollment< student_affected ~ student_affected,
    TRUE ~ enrollment
  )) %>%
  mutate(disaster = case_when(
    disaster == "Tropical cyclone" ~ "Tropical Cyclone",
    TRUE ~ disaster
  ))

# Summarize data for the Graphs -------------------------------------------
# filter out heatwave because it is not clear how this effected school participation
edu_enr_updated_africa = edu_enr_updated_africa %>%
  filter(disaster != "Heat Wave")

# Clean data: convert 'student_affected' to numeric if needed
education_dta_clean <- edu_enr_updated_africa %>%
  dplyr::mutate(student_affected = as.numeric(student_affected)) %>%
  dplyr::filter(!is.na(student_affected)) # Remove rows where student_affected is NA

# Summarize: total students affected by hazard
hazard_summary <- education_dta_clean %>%
  dplyr::group_by(disaster) %>%
  dplyr::summarise(total_affected = sum(student_affected, na.rm = TRUE),
                   total_enrolled = sum(enrollment, na.rm = TRUE)) %>%
  dplyr::arrange(desc(total_affected)) %>%
  dplyr::mutate(perc_total =  total_affected/total_enrolled  )

# Summarize countries where each disaster occurred and we have data
disaster_country_list <- education_dta_clean %>%
  select(disaster, country_name_en) %>%
  distinct() %>%  # remove duplicates
  dplyr::group_by(disaster) %>%
  dplyr::summarise(
    countries = paste(unique(country_name_en), collapse = ", ")
  ) %>%
  dplyr::arrange(disaster)

# 4. Generate graphs ------------------------------------------------------

# a. NBR of students affected by hazards
std_affected_hazard = ggplot(hazard_summary, aes(y = reorder(disaster, total_affected), x = total_affected)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  
  geom_text(
    aes(label = scales::label_comma(accuracy = 0.01)(total_affected)),
    vjust = -0.9,  # use hjust not vjust when coord_flip() is applied
    size = 5,
    fontface = "bold"
  ) +
  
  geom_col(fill = "#00AEEF", color = "black", width = 0.6) +
  
  coord_flip() +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add room on right
  
  labs(
    title = "Total Number of Students Affected by Each Hazard in 2024",
    x = "Total Students Affected",
    y = "Disaster Type"
  ) +
  scale_x_continuous(
    labels = scales::comma,  # <-- add commas here
    expand = expansion(mult = c(0, 0.2))
  ) +
  
  theme_minimal() + 
  
  theme(
    plot.title = element_blank(), # element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.margin = margin(t = 20, r = 60, b = 20, l = 10),  # Added space for labels
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1,face = "bold"),
    legend.position = "none"
  ) 


# b. percentage of students affected by hazards
perc_student_affect_hazard <- ggplot(hazard_summary, aes(y = reorder(disaster, perc_total), x = perc_total)) +
  geom_col(fill = "#00AEEF", color = "black", width = 0.6) +
  
  geom_text(
    aes(label = scales::label_percent(accuracy = 0.01)(perc_total)),
    vjust = -0.9,  # use hjust not vjust when coord_flip() is applied
    size = 5,
    fontface = "bold"
  ) +
  
  coord_flip() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add room on right
  
  labs(
    title = "Percentage of Students Affected by Each Hazard in 2024",
    x = "% of Students Affected",
    y = "Disaster Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(), # element_text(hjust = 0, size = 18, face = "bold"),
    plot.margin = margin(t = 20, r = 60, b = 20, l = 10),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1, face = "bold"),
    legend.position = "none"
  )


hazard_summary %>%
  dplyr::summarise(tot = sum(total_affected))


# 5. Save graphs  ---------------------------------------------------------
# graph 1
ggsave(paste0(out_fld_edu,"/std_affected_hazard.png"), plot = std_affected_hazard, width = 8, height = 6, dpi = 300)
# graph 2
ggsave(paste0(out_fld_edu,"/perc_std_affected_hazard.png"), plot = perc_student_affect_hazard, width = 8, height = 6, dpi = 300)

# 6. Print the table of affected students --------------------------------------
  
table = education_dta_clean %>%
  select(country_name_en,enrollment,disaster,student_affected) 

  # Create a flextable
  ft <- flextable(table)
  ft <- set_header_labels(ft,
                          country_name_en = "Country Name",
                          enrollment = "Enrollment",
                          disaster = "Main Hazard",
                          student_affected = "Students Affected"
  ) %>%
    
    # ✅ Set Specific Width for Each Column Instead of Full Table
    align(align = "center", part = "all") %>%
    autofit() %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Calibri", part = "all") %>%      # 👈 Set font type here
    # Set Title
    set_caption(ft, caption = "Table X. 2024 Education Hazards")
  
  # Export to Word
  doc <- read_docx()
  doc <- body_add_flextable(doc, value = ft)
  print(doc, target = paste0(out_fld_edu,"/hazard_edu_sector.docx"))
  
  
  