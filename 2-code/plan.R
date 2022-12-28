
source("code/0-packages.R")
source("code/1-vegetation_functions.R")
source("code/2-soil_functions.R")
source("code/3-functions_graphs.R")
source("code/4-stream_functions.R")

budget_plan = drake_plan(
  # I. CHEMISTRY ------------------------------------------------------------
  ## Ia. LOAD FILES ----
  wood_chemistry = read.csv(file_in("data/veg_chem_wood.csv"), na.strings = ""),
  wood_chemistry_key = read.csv(file_in("data/veg_chem_wood_key.csv"), na.strings = ""),
  
  litterfall = read.csv(file_in("data/veg_chem_litterfall.csv"), na.strings = ""),
  
  ll_chem = read.csv(file_in("data/veg_chem_ll.csv"), na.strings = ""),
  ll_chem_key = read.csv(file_in("data/veg_chem_ll_key.csv"), na.strings = ""),
  
  herb_chem = read.csv("data/veg_chem_herbaceous.csv", na.strings = ""),
  herb_weights = read.csv("data/veg_herbaceous_weights.csv", na.strings = ""),
  
  foliage_chem = read.csv("data/veg_chem_foliage.csv", na.strings = ""),
  root_chem = read.csv("data/veg_chem_roots.csv", na.strings = ""),
  
  
  ## Ib. PROCESS DATA ----
  wood = process_chemistry_wood(wood_chemistry, wood_chemistry_key),
  branches = process_chemistry_branch(litterfall),
  loose_litter = process_chemistry_looselitter(ll_chem, ll_chem_key),
  herb = process_chemistry_herb(herb_chem, herb_weights),
  foliage = process_chemistry_foliage(foliage_chem),
  roots = process_chemistry_root(root_chem),
  vegetation_combined_chem_all = calculate_combined_chemistry(foliage, wood, branches, loose_litter, roots, herb)$all_values,
  vegetation_combined_chem = calculate_combined_chemistry(foliage, wood, branches, loose_litter, roots, herb)$average,
  
  ## Ic. PLOTS ----
    gg_veg_chemistry = plot_veg_chemistry(vegetation_combined_chem_all),
  ## Id. TABLES ----
  veg_chemistry_table = compute_tables_veg_chem(foliage, wood, branches, loose_litter, herb),
  
  
  
  ## Ie. OUTPUT ----
  wood  %>% write.csv("data/processed/veg_chem_wood.csv", row.names = FALSE, na = ""),
  branches %>% write.csv("data/processed/veg_chem_branches.csv", row.names = FALSE, na = ""),
  loose_litter %>% write.csv("data/processed/veg_chem_loose_litter.csv", row.names = FALSE, na = ""),
  herb %>% write.csv("data/processed/veg_chem_herb.csv", row.names = FALSE, na = ""),
  foliage %>% write.csv("data/processed/veg_chem_foliage.csv", row.names = FALSE, na = ""),
  roots %>% write.csv("data/processed/veg_chem_roots.csv", row.names = FALSE, na = ""),
  vegetation_combined_chem %>% write.csv("data/processed/veg_chem_combined.csv", row.names = FALSE, na = ""),
  
  #
  # II. BIOMASS -------------------------------------------------------------
  ## IIa. LOAD FILES ----
  dbh = read.csv("data/veg_dbh.csv"),
  young_coeff = read.csv("data/veg_young_coeff2.csv"),
  ll_weights = read.csv("data/veg_cof_and_ll_weights.csv", na.strings = ""),
  
  ## IIb. PROCESS BIOMASS ----
  biomass_trees = calculate_biomass_young(young_coeff, dbh),
  biomass_looselitter = calculate_biomass_looselitter(ll_weights),
  biomass_herb = calculate_biomass_herb(herb_weights),
  vegetation_combined_biomass = calculate_combined_biomass(biomass_trees, biomass_looselitter, biomass_herb),
  
  
  
  # III. CALCULATE CARBON STOCKS --------------------------------------------
  ## IIIa. calculations ----
  vegetation_carbon_stocks = calculate_biomass_carbon_stocks(vegetation_combined_biomass, vegetation_combined_chem)$compartment,
  vegetation_stocks_by_plot = calculate_biomass_carbon_stocks(vegetation_combined_biomass, vegetation_combined_chem)$plot_wise,
  
  ## IIIb. tables ----
  biomass_table = compute_tables_veg_biomass(vegetation_stocks_by_plot),
  carbon_stocks_table = compute_tables_veg_stocks(vegetation_stocks_by_plot),
  
  ## IIIc. FIGURES ----
  gg_veg_stocks = plot_veg_stocks(vegetation_carbon_stocks), 
  
  # -------------------------------------------------------------------------
  # IV. SOIL ----------------------------------------------------------------
  soil_data = read.csv(file_in("data/soil_budget.csv")),
  soil_data_cleaned = clean_soil_data(soil_data),
  soil_summary = compute_soil_summary(soil_data_cleaned),
  
  # -------------------------------------------------------------------------

  # V. STREAMS --------------------------------------------------------------
  stream_data = read.csv(file_in("data/bbwm_stream_monthly.csv")),
  stream_cleaned = clean_stream_data(stream_data),
  stream_annual = compute_stream_annual(stream_cleaned),
  streams_plot = make_stream_graphs(stream_annual, stream_cleaned),
  
  # REPORT ----
    report = rmarkdown::render(
      knitr_in("reports/c_budget_report.Rmd"),output_format = rmarkdown::github_document())
  
)

make(budget_plan)
