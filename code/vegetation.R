library(tidyverse)
library(readxl)


# CHEMISTRY FUNCTIONS -------------------------------------------------------------------------

process_chemistry_foliage = function(foliage_chem){
  foliage_chem %>% 
    filter(!is.na(sample_ID)) %>% 
    filter(is.na(skip)) %>% 
    filter(material != "litter") %>% 
    # recode variables 
    mutate(watershed = recode(watershed, "West Bear" = "WB", "East Bear" = "EB"),
           species = case_when(
             species == "american beech"|species == "American beech" ~ "AB",
             species == "red maple"|species == "Red Maple" ~ "RM",
             species == "red spruce"|species == "Red Spruce" ~ "RS",
             species == "sugar maple"|species == "Sugar Maple"|species == "SM" ~ "SM",
             species == "yellow birch"|species == "Yellow birch" ~ "YB"),
           material = if_else(grepl("oliage", material), "foliage", material)) %>% 
    # calculate
    mutate(
           C_mg_g = C_amount_ug/amount_mg,
           C_mg_g = round(C_mg_g, 2),
           material = "foliage",
           forest = NA) %>% 
    dplyr::select(material, sample_ID, sample_number, watershed, forest, species, year, C_mg_g)
}
process_chemistry_root = function(root_chem){
  
  root_chem %>% 
    filter(!is.na(sample_ID)) %>% 
    filter(is.na(skip)) %>% 
    mutate(material = "roots",
           plot = as.character(plot),
           C_mg_g = C_amount_ug/amount_mg,
           C_mg_g = round(C_mg_g,2)) %>% 
    # fix horizon levels
    mutate(depth = recode(horizon,
                          "Oa" = "O", "Oe" = "O", "Oi" = "O",
                          "0-5" = "0-5cm", "5-25" = "5-25cm", "25-50" = "25-50cm",
                          "B05" = "0-5cm", "B525" = "5-25cm", "B2550" = "25-50cm"),
           horizon2 = if_else(grepl("-", depth), "B", depth)) %>% 
    dplyr::select(-horizon) %>% 
    dplyr::rename(horizon = horizon2,
                  forest = vegetation) %>% 
    dplyr::select(sample_ID, material, watershed, forest, year, horizon, depth, C_mg_g)
}
process_chemistry_wood = function(wood_chemistry, wood_chemistry_key){
  wood_chemistry_key %>% 
    dplyr::select(sample_ID, Year, watershed, year, species) %>% 
    left_join(wood_chemistry %>% dplyr::select(sample_ID, C_amount_ug, amount_mg), by = "sample_ID") %>% 
    drop_na() %>% 
    filter(amount_mg != "0") %>% 
    mutate(amount_mg = as.double(amount_mg),
           C_mg_g = C_amount_ug/amount_mg,
           C_mg_g = round(C_mg_g, 2),
           material = "wood",
           year = as.integer(year)) %>% 
    dplyr::select(material, sample_ID, watershed, species, year, C_mg_g)
  
}
process_chemistry_branch = function(litterfall){
  litterfall %>% 
    filter(lf_type == "woody") %>% 
    mutate(C_mg_g = C_amount_ug/amount_mg,
           material = "woody_litter") %>% 
    filter(!is.na(C_mg_g)) %>% 
    dplyr::select(material, sample_ID, watershed, forest, species, year, C_mg_g)
}
process_chemistry_looselitter = function(ll_chem, ll_chem_key){
  ll_chem_temp = 
    ll_chem %>% 
    mutate(sample_ID = str_replace(sample_ID, "LL ", "LL")) %>% 
    dplyr::select(sample_ID, C_amount_ug, amount_mg)
  
  ll_chem_key %>% 
    full_join(ll_chem_temp) %>% 
    drop_na() %>% 
    filter(p_trt == "noP") %>% 
    mutate(material = "loose_litter",
           species = NA,
           C_mg_g = C_amount_ug/amount_mg,
           C_mg_g = round(C_mg_g, 2),
           year = if_else(chase_day < 400, 2012, 2013)) %>% 
    dplyr::select(material, sample_ID, watershed, forest, species, year, C_mg_g)
}
process_chemistry_herb = function(herb_chem, herb_weights){
  herb_chem_temp = 
    herb_chem %>% 
    dplyr::select(sample_number, C_amount_ug, amount_mg) %>% 
    mutate(sample_number = as.numeric(sample_number))
  
  herb_weights %>% 
    full_join(herb_chem_temp, by = "sample_number") %>% 
    #drop_na() %>% 
    mutate(C_mg_g = C_amount_ug/amount_mg,
           C_mg_g = round(C_mg_g, 2),
           material = "herb",
           species = NA) %>% 
    filter(!is.na(C_mg_g) & !is.na(sample_number)) %>% 
    dplyr::select(material, sample_number, watershed, forest, species, year, C_mg_g) 
}


#
# BIOMASS FUNCTIONS -------------------------------------------------------------------------

calculate_biomass_young = function(young_coeff, dbh){
  # calculate biomass using young equation
  # ln (weight-lbs) = B0 + B1 (ln(DBH-inches))
  
  young2 = 
    young_coeff %>% 
    pivot_longer(-species) %>% 
    separate(name, sep = "_", into = c("tissue", "coefficient")) %>% 
    pivot_wider(names_from = "coefficient", values_from = "value")
  
  veg_biomass = 
    dbh %>% 
    dplyr::select(plotID, watershed, forest, species, dbh_cm) %>% 
    left_join(young2, by = "species") %>% 
    mutate(dbh_in = dbh_cm/2.54,
           ln_biomass_lbs = b0 + b1 * log(dbh_in),
           biomass_lbs = exp(ln_biomass_lbs),
           biomass_kg = round(biomass_lbs * 0.4536, 2)) %>% 
    dplyr::select(plotID, watershed, forest, species, tissue, biomass_kg)
  
        ##  # split stem into stem-wood and stem-bark
        ##  UPDATE: not doing this, because we do not have bark C chemistry
        ##  veg_biomass_stem = 
        ##    veg_biomass1 %>% 
        ##    filter(tissue == "stem") %>% 
        ##    mutate(stem_wood = if_else(species == "RS", biomass_kg * 0.87, biomass_kg * 0.88),
        ##           stem_bark = if_else(species == "RS", biomass_kg * 0.13, biomass_kg * 0.12)) %>% 
        ##    dplyr::select(plotID, watershed, forest, species, stem_wood, stem_bark) %>% 
        ##    pivot_longer(-c(plotID, watershed, forest, species), 
        ##                 names_to = "tissue", values_to = "biomass_kg") %>% 
        ##    mutate(biomass_kg = round(biomass_kg, 2))
      
        ##  # now add the stem-wood, stem_bark to the original file 
        ##    veg_biomass = bind_rows(veg_biomass1, veg_biomass_stem)  
  
  # calculate biomass per hectare
  veg_biomass_kgha = 
    veg_biomass %>% 
    group_by(watershed, forest, plotID, species, tissue) %>% 
    dplyr::summarise(biomass_kg_400m2 = sum(biomass_kg),
                     biomass_kg_ha = biomass_kg_400m2 * 100/4) %>% 
    dplyr::select(-biomass_kg_400m2)
  
  # summarize
  veg_biomass_kgha %>% 
    rename(sample_type = tissue) %>% 
    filter(sample_type != "total") %>% 
    group_by(watershed, forest, species, sample_type) %>% 
    dplyr::summarise(biomass_se = as.integer(sd(biomass_kg_ha, na.rm = TRUE)/sqrt(n())),
                     biomass_kg_ha = as.integer(mean(biomass_kg_ha))
                     ) 
}

calculate_biomass_looselitter = function(ll_weights){

  ll_weights_processed = 
    ll_weights %>% 
    filter(!is.na(od_weight_g) & sample_type == "LL" & forest != "RT") %>% 
    mutate(biomass_g_cm2 = od_weight_g / area_cm2,
           biomass_kg_ha = biomass_g_cm2 * 100000, # 1 ha = 10^8 cm2, 1 kg = 10^3 g
           biomass_kg_ha = as.integer(biomass_kg_ha)) %>% 
    dplyr::select(sample_type, sample_number, watershed, forest, plot, year, biomass_kg_ha)
  
  ll_weights_processed %>% 
    group_by(sample_type, watershed, forest) %>% 
    dplyr::summarise(biomass_se = as.integer(sd(biomass_kg_ha, na.rm = TRUE)/sqrt(n())),
                     biomass_kg_ha = as.integer(mean(biomass_kg_ha))
    ) 
  
}

calculate_biomass_herb = function(herb_weights){

  herb_weights_processed = 
    herb_weights %>% 
    filter(!is.na(sample_number)) %>% 
    group_by(code, watershed, forest, location, year) %>% 
    dplyr::summarise(wt_g = sum(wt_air_dry_g, na.rm = TRUE),
                     area_m2 = 25) %>% 
    ungroup() %>% 
    mutate(biomass_g_m2 = wt_g * area_m2,
           biomass_kg_ha = biomass_g_m2 * 10, # 1 ha = 10^4 m2, 1 kg = 10^3 g
           biomass_kg_ha  = as.integer(biomass_kg_ha),
           sample_type = "herb") %>% 
    dplyr::select(code, sample_type, watershed, forest, year, biomass_kg_ha)
  #  %>% 
  #    group_by(watershed, forest) %>% 
  #    dplyr::summarise(biomass_kg_ha = as.integer(mean(biomass_kg_ha)))
  
  herb_weights_processed %>% 
    group_by(sample_type, watershed, forest) %>% 
    dplyr::summarise(biomass_se = as.integer(sd(biomass_kg_ha, na.rm = TRUE)/sqrt(n())),
                     biomass_kg_ha = as.integer(mean(biomass_kg_ha))
    ) 
}








