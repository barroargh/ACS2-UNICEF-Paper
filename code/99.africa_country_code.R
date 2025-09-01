
# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Define M49 Country code used in the analysis
# Author: Daniele Barro
# Date: 9th July 2025
# Editied by: DB
# Last edit: 
# 
# Content
#   1. Africa m49 countyr code
#   2. Europ m49 country code
#   3. Africa sub-regions m49 country code
#   4. World m49 country code

# Input: this script has no input
# Output: this script has no output

# African countries  ------------------------------------------------------
african_m49_codes <- c(
  012, # Algeria
  024, # Angola
  204, # Benin
  072, # Botswana
  854, # Burkina Faso
  108, # Burundi
  132, # Cabo Verde
  120, # Cameroon
  140, # Central African Republic
  148, # Chad
  174, # Comoros
  178, # Congo
  384, # Côte d’Ivoire
  180, # Democratic Republic of the Congo
  262, # Djibouti
  818, # Egypt
  226, # Equatorial Guinea
  232, # Eritrea
  748, # Eswatini
  231, # Ethiopia
  266, # Gabon
  270, # Gambia
  288, # Ghana
  324, # Guinea
  624, # Guinea-Bissau
  404, # Kenya
  426, # Lesotho
  430, # Liberia
  434, # Libya
  450, # Madagascar
  454, # Malawi
  466, # Mali
  478, # Mauritania
  480, # Mauritius
  504, # Morocco
  508, # Mozambique
  516, # Namibia
  562, # Niger
  566, # Nigeria
  # 638, # Réunion - excluded because it is a French oversse country
  646, # Rwanda
  678, # Sao Tome and Principe
  686, # Senegal
  690, # Seychelles
  694, # Sierra Leone
  706, # Somalia
  710, # South Africa
  728, # South Sudan
  729, # Sudan
  768, # Togo
  788, # Tunisia
  800, # Uganda
  834, # United Republic of Tanzania
  894, # Zambia
  716  # Zimbabwe
)
world = c(
  1    # World
)

europe_m49_codes <- c(
  008,  # Albania
  040,  # Austria
  056,  # Belgium
  070,  # Bosnia and Herzegovina
  100,  # Bulgaria
  191,  # Croatia
  196,  # Cyprus
  203,  # Czechia
  208,  # Denmark
  233,  # Estonia
  246,  # Finland
  250,  # France
  276,  # Germany
  300,  # Greece
  348,  # Hungary
  352,  # Iceland
  372,  # Ireland
  380,  # Italy
  428,  # Latvia
  440,  # Lithuania
  442,  # Luxembourg
  470,  # Malta
  498,  # Moldova (Republic of)
  499,  # Montenegro
  528,  # Netherlands
  616,  # Poland
  620,  # Portugal
  642,  # Romania
  643,  # Russian Federation
  688,  # Serbia
  703,  # Slovakia
  705,  # Slovenia
  724,  # Spain
  752,  # Sweden
  756,  # Switzerland
  792,  # Türkiye
  804,  # Ukraine
  807,  # North Macedonia
  826,  # United Kingdom
  833,  # Isle of Man
  832,  # Jersey
  831,  # Guernsey
  674,  # San Marino
  499,  # Montenegro
  462,  # Faroe Islands (self-governing under Danish sovereignty)
  248,  # Åland Islands (autonomous, part of Finland)
  336,  # Holy See (Vatican City State)
  807   # North Macedonia
)


# African regions by M49 codes -------------------------------------------

north_africa <- c(
  012, # Algeria
  818, # Egypt
  434, # Libya
  504, # Morocco
  788  # Tunisia
)

east_africa <- c(
  108, # Burundi
  174, # Comoros
  262, # Djibouti
  232, # Eritrea
  231, # Ethiopia
  404, # Kenya
  454, # Malawi
  646, # Rwanda
  706, # Somalia
  834, # United Republic of Tanzania
  800, # Uganda
  728, # South Sudan
  729  # Sudan
)

west_africa <- c(
  204, # Benin
  854, # Burkina Faso
  132, # Cabo Verde
  384, # Côte d’Ivoire
  270, # Gambia
  288, # Ghana
  324, # Guinea
  624, # Guinea-Bissau
  430, # Liberia
  466, # Mali
  478, # Mauritania
  562, # Niger
  566, # Nigeria
  686, # Senegal
  694, # Sierra Leone
  678, # Sao Tome and Principe
  768  # Togo
)

southern_africa <- c(
  072, # Botswana
  426, # Lesotho
  516, # Namibia
  710, # South Africa
  748, # Eswatini
  716, # Zimbabwe
  894, # Zambia
  508, # Mozambique
  024  # Angola
)

central_africa <- c(
  120, # Cameroon
  140, # Central African Republic
  148, # Chad
  178, # Congo
  180, # Democratic Republic of the Congo
  226, # Equatorial Guinea
  266  # Gabon
)

islands <- c(
  450,  # Madagascar
  480,  # Mauritius
  690   # Seychelles
)

world = c(
  1     # World
)
