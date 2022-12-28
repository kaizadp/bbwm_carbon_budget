
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

compute_soil_summary = function(soil_data_cleaned){

  # stats -------------------------------------------------------------------
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
    group_by(watershed, horizon, forest, pit) %>% 
    dplyr::summarise(total_TC_kgha = sum(total_TC_kgha)) %>% 
    group_by(forest, horizon) %>% 
    do(fit_aov_tc(.))
  
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
    group_by(watershed, horizon, forest, pit) %>% 
    dplyr::summarise(total_earth_kgha = sum(total_earth_kgha)) %>% 
    group_by(horizon, forest) %>% 
    do(fit_aov_mass(.))
  ## O horizon SW: WB > EB, p = 0.03
  
  fit_aov_tc_perc = function(dat){
    a = aov(TC_perc ~ watershed, data = dat)
    broom::tidy(a) %>% 
      filter(term == "watershed") %>% 
      rename(p_value = `p.value`) %>% 
      mutate(label = case_when(p_value <= 0.05 ~ "*")) %>% 
      dplyr::select(p_value, label) %>% 
      mutate(watershed = "WB")
  }
  soil_stats_tc = 
    soil_data_cleaned %>% 
    group_by(watershed, increment, forest, pit) %>% 
    dplyr::summarise(TC_perc = mean(TC_perc)) %>% 
    group_by(increment, forest) %>% 
    do(fit_aov_tc_perc(.))
  ## O horizon HW: WB > EB, p < 0.01 
  

  # tables ------------------------------------------------------------------
  soil_data_cleaned %>% 
    group_by(horizon, forest, watershed) %>% 
    dplyr::summarise(totalC_perc = mean(TC_perc),
                     TC_perc_se = sd(TC_perc)/sqrt(n()),
                     TC_kg_ha = mean(total_TC_kgha),
                     TC_se = sd(total_TC_kgha)/sqrt(n()),
                     mass_kg_ha = mean(total_earth_kgha),
                     mass_se = sd(total_earth_kgha)/sqrt(n()))

  summary_table_tc = 
    soil_data_cleaned %>% 
    group_by(watershed, horizon, forest, pit) %>% 
    dplyr::summarise(total_TC_kgha = sum(total_TC_kgha)) %>% 
    group_by(horizon, forest, watershed) %>% 
    dplyr::summarise(total_TC_kg_ha = mean(total_TC_kgha),
                     se = sd(total_TC_kgha)/sqrt(n())) %>% 
    mutate(total_TC_kg_ha = paste(as.integer(total_TC_kg_ha), "\u00b1", as.integer(se))) %>% 
    dplyr::select(horizon, forest, watershed, total_TC_kg_ha) %>% 
    pivot_wider(names_from = "watershed", values_from = "total_TC_kg_ha") %>% 
    mutate(horizon = factor(horizon, levels = c("O", "B", "C"))) %>% 
    arrange(forest, horizon) %>% 
    #knitr::kable() %>%  
    # gt(groupname_col = "forest") %>% 
    # tab_header(title = "soil TC, kg/ha") %>% 
    force()
  
  summary_table_mass = 
    soil_data_cleaned %>% 
    group_by(watershed, horizon, forest, pit) %>% 
    dplyr::summarise(total_earth_kgha = sum(total_earth_kgha)) %>% 
    group_by(horizon, forest, watershed) %>% 
    dplyr::summarise(total_earth_kg_ha = mean(total_earth_kgha),
                     se = sd(total_earth_kgha)/sqrt(n())) %>% 
    mutate(total_earth_kg_ha = paste(as.integer(total_earth_kg_ha), "\u00b1", as.integer(se))) %>% 
    dplyr::select(horizon, forest, watershed, total_earth_kg_ha) %>% 
    pivot_wider(names_from = "watershed", values_from = "total_earth_kg_ha") %>% 
    mutate(horizon = factor(horizon, levels = c("O", "B", "C")),
           stats = case_when(forest == "SW" & horizon == "O" ~ "*")) %>% 
    arrange(forest, horizon) %>% 
    # knitr::kable() %>%  
    # gt(groupname_col = "forest") %>% 
    # tab_header(title = "soil mass, kg/ha") %>% 
    force()
  
  summary_table_tc_perc = 
    soil_data_cleaned %>% 
    group_by(increment, forest, watershed) %>% 
    dplyr::summarise(totalC_perc = mean(TC_perc),
                    TC_perc_se = sd(TC_perc)/sqrt(n())) %>% 
    mutate(total_C_perc = paste(round(totalC_perc,2), "\u00b1", round(TC_perc_se,2))) %>% 
    dplyr::select(increment, forest, watershed, total_C_perc) %>% 
    pivot_wider(names_from = "watershed", values_from = "total_C_perc") %>% 
    mutate(increment = factor(increment, levels = c("O", "0-5cm", "5-25cm", "25-50cm", "50-C", "25-C", "C")),
           stats = case_when(forest == "HW" & increment == "O" ~ "*")) %>% 
    arrange(forest, increment) %>% 
    # knitr::kable() %>%  
    # gt(groupname_col = "forest") %>% 
    # tab_header(title = "total C, %") %>% 
    force()

  list(summary_table_tc = summary_table_tc,
       summary_table_mass = summary_table_mass,
       summary_table_tc_perc = summary_table_tc_perc)
  
}


compute_total_ecosystem_stocks = function(vegetation_carbon_stocks, soil_summary){
  loadd(soil_summary)$summary_table_tc
  
  soil_summary$summary_table_tc
  
  
}