source("code/0-packages.R") 

soil_data = read.csv("data/soil_budget.csv")

clean_soil_data = function(soil_data){
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
    mutate(total_earth_kgha = str_replace_all(total_earth_kgha, ",", ""),
           total_earth_kgha = as.numeric(total_earth_kgha),
           pit = as.numeric(pit)) %>% 
    filter(pit <= 40) %>% 
    rowwise() %>% 
    mutate(
      total_TC_kgha = sum(fine_TC_kgha, cof_TC_kgha, na.rm = TRUE),
      total_TN_kgha = sum(fine_TN_kgha, cof_TN_kgha, na.rm = TRUE),
      total_CN_ratio = round(total_TC_kgha/total_TN_kgha, 2))
}

    ##    soil_summary = 
    ##      soil_data_cleaned %>% 
    ##      dplyr::select(sample_num, pit, increment, horizon, soil_material,
    ##                    watershed, forest, total_TC_kgha, fine_TC_kgha, total_TN_kgha, fine_TN_kgha) %>% 
    ##      mutate(TC_kgha = case_when(soil_material == "organic" ~ total_TC_kgha,
    ##                                 soil_material == "mineral" ~ fine_TC_kgha),
    ##             TN_kgha = case_when(soil_material == "organic" ~ total_TN_kgha,
    ##                                 soil_material == "mineral" ~ fine_TN_kgha)) %>% 
    ##      group_by(pit, soil_material, watershed, forest) %>% 
    ##      dplyr::summarise(TC_kgha = sum(TC_kgha),
    ##                       TN_kgha = sum(TN_kgha)) %>% 
    ##      group_by(soil_material, watershed, forest) %>% 
    ##      dplyr::summarise(avgTC_kgha = mean(TC_kgha),
    ##                       avgTN_kgha = mean(TN_kgha))    ##    




compute_soil_summary = function(soil_data_cleaned){
  
  fit_aov_tc = function(dat){
    a = aov(total_TC_kgha ~ watershed, data = dat)
    broom::tidy(a) %>% 
      filter(term == "watershed") %>% 
      rename(p_value = `p.value`) %>% 
      mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
      dplyr::select(p_value, label) %>% 
      mutate(watershed = "WB")
  }
  soil_stats_tc = 
    soil_data_cleaned %>% 
    group_by(horizon, forest) %>% 
    do(fit_aov(.))
  
  
  fit_aov_mass = function(dat){
    a = aov(total_earth_kgha ~ watershed, data = dat)
    broom::tidy(a) %>% 
      filter(term == "watershed") %>% 
      rename(p_value = `p.value`) %>% 
      mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
      dplyr::select(p_value, label) %>% 
      mutate(watershed = "WB")
  }
  soil_stats_mass = 
    soil_data_cleaned %>% 
    group_by(horizon, forest) %>% 
    do(fit_aov_mass(.))
  ## O horizon SW: WB > EB, p = 0.03
  
  soil_data_cleaned %>% 
    group_by(horizon, forest, watershed) %>% 
    dplyr::summarise(TC_kg_ha = mean(total_TC_kgha),
                     TC_se = sd(total_TC_kgha)/sqrt(n()),
                     mass_kg_ha = mean(total_earth_kgha),
                     mass_se = sd(total_earth_kgha)/sqrt(n()))

}
