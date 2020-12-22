library(tidyverse)
library(readxl)

# load files --------------------------------------------------------------
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



# chemistry files ---------------------------------------------------------

## foliage
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


## roots
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
  


## combined
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
## output
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
  