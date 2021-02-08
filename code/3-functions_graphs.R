theme_kp <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

plot_veg_chemistry = function(vegetation_combined_chem_all){
  (gg_species =
     vegetation_combined_chem_all %>% 
    filter(material %in% c("foliage", "wood")) %>% 
    ggplot(aes(x = species, y = C_mg_g, color = watershed, shape = watershed))+
    geom_point(size = 2, stroke = 1, position = position_dodge(width = 0.3))+
    scale_shape_manual(values = c(1, 19))+
    facet_wrap(~material, scales = "free_y")+
    theme_kp()+
    NULL)
  
  (gg_ws =
      vegetation_combined_chem_all %>% 
      filter(!material %in% c("foliage", "wood", "roots")) %>% 
      ggplot(aes(x = material, y = C_mg_g, color = watershed, shape = watershed))+
      geom_point(size = 2, stroke = 1, position = position_dodge(width = 0.3))+
      scale_shape_manual(values = c(1, 19))+
      labs(title = "watershed-level chemistry")+
      theme_kp()+
      NULL)
  
 # (gg_roots =
 #     vegetation_combined_chem_all %>% 
 #     filter(material %in% c("roots")) %>% 
 #     mutate(depth = factor(depth, 
 #                           levels = c("O", "E",  "B", "0-5cm", "5-25cm", 
 #                                      "25-50cm", "25-C", "50-C"))) %>% 
 #     ggplot(aes(x = depth, y = C_mg_g, color = watershed, shape = watershed))+
 #     geom_point(size = 2, stroke = 1, position = position_dodge(width = 0.3))+
 #     scale_shape_manual(values = c(1, 19))+
 #     labs(title = "roots")+
 #     theme_kp()+
 #     NULL)
  
  list(gg_species = gg_species,
       gg_ws = gg_ws)
  
}


plot_veg_stocks = function(vegetation_carbon_stocks){
  vegetation_carbon_stocks %>% 
    ggplot(aes(x = material, y = TC_kg_ha, fill = material))+
    geom_bar(stat = "identity")+
    facet_grid(forest ~ watershed)+
    scale_fill_brewer(palette = "Dark2")+
    labs(x = "", y = "total C, kg/ha",
         title = "vegetation carbon stocks")+
    scale_y_continuous(labels = scales::comma)+
    theme_kp()+
    theme(legend.position = "right")+
    NULL
  
}
