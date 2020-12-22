soil_data = read.csv("data/soil_budget.csv")

soil_data_cleaned = 
  soil_data %>% 
  rename(sample_num = `Sample.`,
         pit = Pit,
         increment = Increment,
         increment2 = `Increment.2`,
         increment3 = `Increment.3`,
         horizon = Horizon,
         soil_material = `Soil.material`,
         watershed = trt,
         forest = veg,
         OD_fine_earth_kgha = `OD.fine.earth.kg.ha`,
         OD_COF_kgha = `OD.COF.kg.ha`,
         total_earth_kgha = `Total.earth.kg.ha`,
         TN_perc = `TN..`,
         TC_perc = `TC..`,
         fine_TN_kgha = `fine.TN.kg.ha`,
         fine_TC_kgha = `fine.TC.kg.ha`,
         cof_TN_kgha = `cof.TN.kg.ha`,
         cof_TC_kgha = `cof.TC.kg.ha`,
         total_TN_kgha = `total.TN.kg.ha`,
         total_TC_kgha = `total.TC.kg.ha`,
         total_CN_ratio = `total.C.N.ratio`) %>% 
  mutate(pit = as.numeric(pit)) %>% 
  filter(pit <= 40)


soil_summary = 
  soil_data_cleaned %>% 
  dplyr::select(sample_num, pit, increment, horizon, soil_material,
                watershed, forest, total_TC_kgha, fine_TC_kgha, total_TN_kgha, fine_TN_kgha) %>% 
  mutate(TC_kgha = case_when(soil_material == "organic" ~ total_TC_kgha,
                             soil_material == "mineral" ~ fine_TC_kgha),
         TN_kgha = case_when(soil_material == "organic" ~ total_TN_kgha,
                             soil_material == "mineral" ~ fine_TN_kgha)) %>% 
  group_by(pit, soil_material, watershed, forest) %>% 
  dplyr::summarise(TC_kgha = sum(TC_kgha),
                   TN_kgha = sum(TN_kgha)) %>% 
  group_by(soil_material, watershed, forest) %>% 
  dplyr::summarise(avgTC_kgha = mean(TC_kgha),
                   avgTN_kgha = mean(TN_kgha))

