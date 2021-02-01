library(tidyverse)
library(readxl)


# CHEMISTRY FUNCTIONS -------------------------------------------------------------------------




process_chemistry_foliage = function(){
  ## 1. foliage ----
  foliage = read_excel("data/veg_chem_foliage.xlsx")
  foliage_cleaned =
    foliage %>% 
    dplyr::rename(sample_id = `Sample ID`,
                  d13C_permil = d13C,
                  C_ug = `C Amount (ug)`,
                  d13C_comment = `d13C Comment`,
                  d15N_permil = d15N,
                  N_ug = `N Amount (ug)`,
                  d15N_comment = `d15N Comment`,
                  TrayName = `Tray Name`,
                  WellID = `Well Id`,
                  material = `Type of Material`,
                  enriched = `Enriched?`,
                  sample_wt_mg = `Amount (mg)`,
                  analysis_number = `Analysis Number`,
                  year = yr,
                  sample_number = `Sample #`,
                  watershed = Watershed,
                  species = Species
    ) %>% 
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
    mutate(sample_wt_mg = as.numeric(sample_wt_mg),
           C_mg_g = C_ug/sample_wt_mg) %>% 
    dplyr::select(skip, year, material, watershed, species, 
                  sample_wt_mg, C_ug, N_ug, C_mg_g, 
                  d13C_permil, d15N_permil,
                  TrayName, WellID, enriched, 
                  plot, tree, 
                  OurLabID, analysis_number, sample_id, analysis_number, sample_number, 
                  d13C_comment, d15N_comment,
                  source_file) %>% 
    filter(!is.na(material))
  
  
  
  foliage_cleaned %>% 
    filter(material == "foliage" & is.na(skip) & !is.na(species)) %>% 
    ggplot(aes(y = C_mg_g, x = species, shape = watershed, color = as.character(year), group = watershed))+
    geom_point(size = 2,
               position = position_dodge(width = 0.4))+
    facet_wrap(~year)+
    NULL
  
  
  
}
process_chemistry_root = function(){
  ## 2. roots ----
  roots = read_excel("data/veg_chem_roots.xlsx")
  
  roots_cleaned = 
    roots %>% 
    dplyr::rename(sample_id = `Sample ID`,
                  d13C_permil = d13C,
                  C_ug = `C Amount (ug)`,
                  d13C_comment = `d13C Comment`,
                  d15N_permil = d15N,
                  N_ug = `N Amount (ug)`,
                  d15N_comment = `d15N Comment`,
                  TrayName = `Tray Name`,
                  WellID = `Well Id`,
                  material = `Type of Material`,
                  enriched = `Enriched?`,
                  sample_wt_mg = `Amount (mg)`,
                  analysis_number = `Analysis Number`,
                  year = Yr,
                  sample_id2 = `SampleID`,
                  watershed = WS,
                  forest = VEG,
                  horizon = Horizon,
                  sample_identifier = `Sample Identifier`,
                  source_file = source,
                  plot = Plot
    ) %>% 
    mutate(material = "roots",
           plot = as.character(plot)) %>% 
    # fix horizon levels
    filter(horizon != "BLANK") %>% 
    mutate(depth = recode(horizon,
                          "Oa" = "O", "Oe" = "O", "Oi" = "O",
                          "0-5" = "0-5cm", "5-25" = "5-25cm", "25-50" = "25-50cm",
                          "B05" = "0-5cm", "B525" = "5-25cm"),
           horizon2 = if_else(grepl("-", depth), "B", depth)) %>% 
    dplyr::select(-horizon) %>% 
    dplyr::rename(horizon = horizon2) %>% 
    dplyr::select(skip, material, watershed, forest, year, 
                  plot, horizon, depth,
                  sample_wt_mg, C_ug, N_ug, d13C_permil, d15N_permil,
                  d13C_comment, d15N_comment,
                  TrayName, WellID, enriched, 
                  OurLabID, analysis_number, sample_id, analysis_number, 
                  sample_id2, sample_identifier, source_file) %>% 
    mutate(C_ug = as.numeric(C_ug),
           sample_wt_mg = as.numeric(sample_wt_mg),
           C_mg_g = C_ug/sample_wt_mg)
  
  
  
  
}
process_chemistry_wood = function(){
  ## 3. wood ----
  wood_chemistry = read.csv("data/veg_chem_wood.csv", na.strings = "")
  wood_chemistry_key = read.csv("data/veg_chem_wood_key.csv", na.strings = "")
  
  wood_chemistry_processed = 
    wood_chemistry_key %>% 
    dplyr::select(sample_ID, Year, watershed, year, species) %>% 
    left_join(wood_chemistry %>% dplyr::select(sample_ID, C_Amount_ug, amount_mg), by = "sample_ID") %>% 
    drop_na() %>% 
    filter(amount_mg != "0") %>% 
    mutate(amount_mg = as.double(amount_mg),
           C_mg_g = C_Amount_ug/amount_mg)
  
  
  
  wood_chemistry_processed %>% 
    ggplot(aes(x = species, y = C_mg_g, color = watershed))+ geom_point()
  
  #
  
}
process_chemistry_branch = function(){
  ## 4. woody litter ---- 
  litterfall = read.csv("data/veg_chem_litterfall.csv", na.strings = "")
  
  woody_litter = 
    litterfall %>% 
    dplyr::select(sample_ID, C_amount_ug, amount_mg, species, lf_type, watershed, forest) %>% 
    filter(lf_type == "woody") %>% 
    drop_na() %>% 
    mutate(C_mg_g = C_amount_ug/amount_mg)
  
  #
  
}
process_chemistry_looselitter = function(){
  ## 5. loose litter ----
  ll_chem = read.csv("data/veg_chem_ll.csv", na.strings = "")
  ll_chem_key = read.csv("data/veg_chem_ll_key.csv", na.strings = "")
  
  ll_chem_temp = 
    ll_chem %>% 
    mutate(sample_ID = str_replace(sample_ID, "LL ", "LL")) %>% 
    dplyr::select(sample_ID, C_amount_ug, amount_mg)
  
  ll_processed = 
    ll_chem_key %>% 
    #filter(is.na(skip)) %>% 
    #dplyr::select(sample_type, sample_number, watershed, forest, plot) %>% 
    #filter(sample_type == "LL") %>% 
    #mutate(sample_ID = paste0("LL", sample_number)) %>% 
    full_join(ll_chem_temp) %>% 
    drop_na() %>% 
    filter(p_trt == "noP") %>% 
    mutate(material = "loose litter",
           C_mg_g = C_amount_ug/amount_mg)
  
  #
  
}
process_chemistry_herb = function(){
  ## 6. herbaceous ----
  herb_chem = read.csv("data/veg_chem_herbaceous.csv", na.strings = "")
  herb_weights = read.csv("data/veg_herbaceous_weights.csv", na.strings = "")
  
  herb_chem_temp = 
    herb_chem %>% 
    dplyr::select(sample_number, C_amount_ug, amount_mg) %>% 
    mutate(sample_number = as.numeric(sample_number))
  
  herb_chem_processed = 
    herb_weights %>% 
    dplyr::select(sample_number, watershed, forest, location) %>% 
    full_join(herb_chem_temp, by = "sample_number") %>% 
    drop_na() %>% 
    mutate(C_mg_g = C_amount_ug/amount_mg)
  
  
  
  
  #
  
}



# BIOMASS FUNCTIONS -------------------------------------------------------------------------

calculate_biomass_tree = function(){
  dbh = read.csv("data/veg_dbh.csv")
  young = read.csv("data/veg_young_coeff.csv")
  
  # calculate biomass using young equation -------------------------------------------------------------------------
  # ln (weight-lbs) = B0 + B1 (ln(DBH-inches))
  
  young2 = 
    young %>% 
    pivot_longer(-species) %>% 
    separate(name, sep = "_", into = c("tissue", "coefficient")) %>% 
    pivot_wider(names_from = "coefficient", values_from = "value")
  
  veg_biomass1 = 
    dbh %>% 
    dplyr::select(plotID, watershed, forest, species, dbh_cm) %>% 
    left_join(young2, by = "species") %>% 
    mutate(dbh_in = dbh_cm/2.54,
           ln_biomass_lbs = b0 + b1 * log(dbh_in),
           biomass_lbs = exp(ln_biomass_lbs),
           biomass_kg = round(biomass_lbs * 0.4536, 2)) %>% 
    dplyr::select(plotID, watershed, forest, species, tissue, biomass_kg)
  
  # split stem into stem-wood and stem-bark
  veg_biomass_stem = 
    veg_biomass1 %>% 
    filter(tissue == "stem") %>% 
    mutate(stem_wood = if_else(species == "RS", biomass_kg * 0.87, biomass_kg * 0.88),
           stem_bark = if_else(species == "RS", biomass_kg * 0.13, biomass_kg * 0.12)) %>% 
    dplyr::select(plotID, watershed, forest, species, stem_wood, stem_bark) %>% 
    pivot_longer(-c(plotID, watershed, forest, species), 
                 names_to = "tissue", values_to = "biomass_kg") %>% 
    mutate(biomass_kg = round(biomass_kg, 2))
  
  # now add the stem-wood, stem_bark to the original file
  veg_biomass = bind_rows(veg_biomass1, veg_biomass_stem)  
  
  # summarize biomass
  veg_biomass_summary = 
    veg_biomass %>% 
    group_by(watershed, forest, plotID, species, tissue) %>% 
    dplyr::summarise(biomass_kg_400m2 = sum(biomass_kg),
                     biomass_kg_ha = biomass_kg_400m2 * 100/4)
  
  
  
  
}

calculate_biomass_looselitter = function(){
  ll_weights = read.csv("data/veg_cof_and_ll_weights.csv", na.strings = "")
  
  ll_weights_processed = 
    ll_weights %>% 
    filter(!is.na(od_weight_g) & sample_type == "LL" & forest != "RT") %>% 
    mutate(biomass_g_cm2 = od_weight_g / area_cm2,
           biomass_kg_ha = biomass_g_cm2 * 100000, # 1 ha = 10^8 cm2, 1 kg = 10^3 g
           biomass_kg_ha = as.integer(biomass_kg_ha)) %>% 
    dplyr::select(sample_type, sample_number, watershed, forest, plot, year, biomass_kg_ha)
}

calculate_biomass_herb = function(){
  herb_weights = read.csv("data/veg_herbaceous_weights.csv", na.strings = "")
  
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
  
  
}




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

## combined ----
combined_veg_chem = 
  bind_rows(foliage_cleaned, roots_cleaned) %>% 
  filter(sample_wt_mg > 0) %>% 
  filter(is.na(skip)) %>% 
  mutate(C_ug = round(C_ug, 3),
         N_ug = round(N_ug, 3),
         C_mg_g = round(C_mg_g, 3),
         d13C_permil = round(d13C_permil, 3),
         d15N_permil = round(d15N_permil, 3)) %>% 
  dplyr::select(material, year, watershed, forest, species, horizon, depth, 
                sample_wt_mg, C_ug, N_ug, C_mg_g, d13C_permil, d15N_permil,
                TrayName, WellID, enriched, plot, tree, 
                OurLabID, analysis_number, sample_id, d13C_comment, d15N_comment, source_file)



#
## output ----
foliage_cleaned %>% write.csv("data/processed/veg_chem_foliage_processed.csv", row.names = F)
roots_cleaned %>% write.csv("data/processed/veg_chem_roots_processed.csv", row.names = F)
combined_veg_chem %>% write.csv("data/processed/veg_concentrations.csv", row.names = F, na = "")


# plots -------------------------------------------------------------------

veg_biomass_summary %>% 
  filter(tissue == "total") %>% 
  mutate(compartment = str_sub(plotID, 1, 4)) %>% 
  ggplot(aes(y = biomass_kg_ha, x = watershed, fill = compartment, group = compartment))+
  #geom_point(position = position_dodge(width = 0.4))+
  geom_bar(stat = "identity", position = "stack", width = 0.2, color = 
             "black")


veg_biomass_summary %>% 
  filter(tissue == "total") %>% 
  mutate(compartment = str_sub(plotID, 1, 4)) %>%
  group_by(compartment, plotID) %>% 
  dplyr::summarise(biomass_kgha = sum(biomass_kg_ha, na.rm = T)) %>% 
  group_by(compartment) %>% 
  dplyr::summarise(biomass_kgha = mean(biomass_kgha, na.rm = T))

roots_cleaned %>% 
  ggplot(aes(x = forest, y = C_mg_g, color = watershed))+
  geom_point(position = position_dodge(width = 0.4))+
  facet_grid(horizon~.)

levels(as.factor(roots_cleaned$horizon))


#


