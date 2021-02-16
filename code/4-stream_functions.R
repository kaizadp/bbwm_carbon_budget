
clean_stream_data = function(stream_data){
  #stream_cleaned = 
  stream_data %>% 
    dplyr::select(year, month, watershed, H2O_cm, DOC_umol_ha)
}

compute_stream_annual = function(stream_cleaned){
  #stream_annual = 
  stream_cleaned %>% 
    group_by(year, watershed) %>% 
    dplyr::summarise(DOC_umolha = sum(DOC_umol_ha, na.rm = TRUE)) %>% 
    filter(year > 1988) %>% 
    arrange(watershed, year) %>% 
    group_by(watershed) %>% 
    dplyr::mutate(cumul = cumsum(DOC_umolha)) %>% 
    force()

  ## nlme::lme(DOC_umolha ~ watershed, random = ~1|year, data = stream_annual) %>% anova()
  ## watershed significant, p < 0.0001
  
  }

make_stream_graphs = function(stream_annual, stream_cleaned){
  # annual
  gg_stream_annual = 
    stream_annual %>% 
    ggplot(aes(x = year, y = DOC_umolha, color = watershed, shape = watershed))+
    geom_point(size = 3, stroke = 1)+
    geom_path(size = 0.7, show.legend = FALSE)+
    scale_shape_manual(values = c(1, 19))+
    scale_color_manual(values = soil_palette("redox2", 2))+
    theme_kp()+
    NULL
  
  # cumulative
  gg_stream_cumulative = 
    stream_annual %>% 
    mutate(cumul_mgC_ha = cumul * 12/1000) %>% 
    #filter(year == 2014) %>% 
    ggplot(aes(x = year, y = cumul_mgC_ha, color = watershed, shape = watershed))+
    geom_point(size = 2.5, stroke = 1)+
    geom_path(size = 0.7, show.legend = FALSE)+
    annotate("segment", x = 2014, xend = 2014, y = 350, yend = 450, arrow = arrow(length = unit(0.2, "cm")))+
    annotate("text", label = "WB - EB = \n 59 mg C/ha \n DOC cumulative", x = 2014, y = 280, angle = 90, size = 4)+
    labs(y = "cumulative DOC export, mg C/ha")+
    scale_y_continuous(labels = scales::comma)+
    scale_shape_manual(values = c(1, 19))+
    scale_color_manual(values = soil_palette("redox2", 2))+
    theme_kp()+
    theme(legend.position = c(0.1, 0.8))+
    NULL
  
  # monthly
  gg_stream_monthly = 
    stream_cleaned %>% 
    mutate(year_group = case_when(year %in% c(1988, 1989) ~ "1989-1990",
                                  year >= 1990 & year <= 1994 ~ "1990-1994",
                                  year >= 2000 & year <= 2004 ~ "2000-2004",
                                  year >= 2010 & year <= 2014 ~ "2010-2014")) %>% 
    filter(!is.na(year_group)) %>% 
      mutate(month2 = recode(month, "1" = "Jan", "2" = "Feb", "3" = "Mar", 
                             "4" = "Apr", "5" = "May", "6" = "Jun", 
                             "7" = "Jul", "8" = "Aug", "9" = "Sep", 
                             "10" = "Oct", "11" = "Nov", "12" = "Dec"),
             month2 = factor(month2, levels = c("Oct", "Nov", "Dec", 
                                                "Jan", "Feb", "Mar", 
                                                "Apr", "May", "Jun", 
                                                "Jul", "Aug", "Sep"))) %>% 
    group_by(watershed, month2, year_group) %>% 
    dplyr::summarise(DOC_umol_ha = mean(DOC_umol_ha, na.rm = TRUE)) %>% 
    ggplot(aes(x = month2, y = DOC_umol_ha, color = year_group, shape = year_group))+
    geom_point(size = 3)+
    geom_path(aes(group = year_group), size = 1, show.legend = FALSE)+
      scale_shape_manual(values = c(16,18,17,15))+
      scale_color_manual(values = soilpalettes::soil_palette("redox2", 4))+
    labs(x = "",
         y = "DOC, μmol/ha")+
      facet_grid(. ~ watershed)+
    theme_kp()+
    NULL
    
  
  list(gg_stream_annual = gg_stream_annual,
       gg_stream_cumulative = gg_stream_cumulative,
       gg_stream_monthly = gg_stream_monthly)
}
