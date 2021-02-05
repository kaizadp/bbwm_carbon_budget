library(tidyverse)
library(drake)

source()


budget_plan = drake_plan(
  # LOAD FILES ----
  wood_chemistry = read.csv(file_in("data/veg_chem_wood.csv"), na.strings = ""),
  wood_chemistry_key = read.csv(file_in("data/veg_chem_wood_key.csv"), na.strings = ""),
  
  litterfall = read.csv(file_in("data/veg_chem_litterfall.csv"), na.strings = ""),
  
  ll_chem = read.csv(file_in("data/veg_chem_ll.csv"), na.strings = ""),
  ll_chem_key = read.csv(file_in("data/veg_chem_ll_key.csv"), na.strings = ""),
  
  herb_chem = read.csv("data/veg_chem_herbaceous.csv", na.strings = ""),
  herb_weights = read.csv("data/veg_herbaceous_weights.csv", na.strings = ""),
  
  foliage_chem = read.csv("data/veg_chem_foliage.csv", na.strings = ""),
  root_chem = read.csv("data/veg_chem_roots.csv", na.strings = ""),
  
  
  # PROCESS DATA ----
  wood = process_chemistry_wood(wood_chemistry, wood_chemistry_key),
  branches = process_chemistry_branch(litterfall),
  loose_litter = process_chemistry_looselitter(ll_chem, ll_chem_key),
  herb = process_chemistry_herb(herb_chem, herb_weights),
  foliage = process_chemistry_foliage(foliage_chem),
  roots = process_chemistry_root(root_chem),

  vegetation_combined_chem = bind_rows(foliage, wood, branches, loose_litter, roots, herb),

  # PLOTS ----
  gg_veg_chemistry = plot_veg_chemistry(vegetation_combined_chem),
  
  
  # OUTPUT ----
  wood  %>% write.csv("data/processed/veg_chem_wood.csv", row.names = FALSE, na = ""),
  branches %>% write.csv("data/processed/veg_chem_branches.csv", row.names = FALSE, na = ""),
  loose_litter %>% write.csv("data/processed/veg_chem_loose_litter.csv", row.names = FALSE, na = ""),
  herb %>% write.csv("data/processed/veg_chem_herb.csv", row.names = FALSE, na = ""),
  foliage %>% write.csv("data/processed/veg_chem_foliage.csv", row.names = FALSE, na = ""),
  roots %>% write.csv("data/processed/veg_chem_roots.csv", row.names = FALSE, na = ""),
  vegetation_combined_chem %>% write.csv("data/processed/veg_chem_combined.csv", row.names = FALSE, na = ""),

  # REPORT ----
  report = rmarkdown::render(
    knitr_in("reports/chemistry_report.Rmd"),output_format = rmarkdown::github_document())
  
    
  )

make(budget_plan)
