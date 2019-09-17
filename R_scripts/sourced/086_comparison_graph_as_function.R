# turning the star wars graph into a function

plot_rate_comps <- function(data, plot_type = 3, color_by_veg = FALSE, 
                            set_ids, rates, slr, slr_ci, veg){
    
    # plot_type: 1 = basic; points only; no confidence intervals
    #            2 = CIs for SET rates, but not sea level rise (SLR)
    #            3 = CIs for both SETs and SLR
    # default is the full plot with CIs, and with points all the same color
    
    # assemble the base plot, with axes and lines for 0 and SLR
    #####################################################################
    p <- ggplot() +
        geom_blank(data = data, 
                   aes(x = {{set_ids}}, 
                       y = {{rates}})) +
        geom_hline(aes(yintercept = {{slr}}), 
                   col = "navyblue", 
                   size = 1, 
                   alpha = 0.9) +
        geom_hline(aes(yintercept = 0), 
                   col = "gray70") +
        theme_classic() +
        coord_flip()
    
    
    # assemble each piece
    #####################################################################
    
    # points, not colored by veg
    points_same <- geom_point(data = data, 
                              aes(x = {{set_ids}}, 
                                  y = {{rates}}), 
                              size = 3, 
                              col = "red3")
    
    # labels, when CIs are included for both SETs and SLR
    labels_full <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                        subtitle = paste0("Local SLR in blue: ", {{slr}}, 
                                          " +/- ", {{slr_ci}}, " mm/yr"), 
                        x = "SET", 
                        y = "Rate of change (mm/yr)")
    
    # labels, when no CIs are included
    labels_minimal <- labs(title = "Elevation Change", 
                           subtitle = paste0("Local SLR in blue: ", {{slr}}, " mm/yr"), 
                           x = "SET", 
                           y = "Rate of change (mm/yr)")
    
    # labels, when CIs are included for SETs but not SLR
    labels_partial_setci <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                                 subtitle = paste0("Local SLR in blue: ", {{slr}}, " mm/yr"), 
                                 x = "SET", 
                                 y = "Rate of change (mm/yr)")
    
    # geom to include when CIs are included for SETs
    set_cis <- geom_errorbar(data = data, 
                             aes(x = {{set_ids}}, 
                                 ymin = CI_low, 
                                 ymax = CI_high), 
                             col = "gray55", 
                             size = 1) 
    
    # geom to include when CI is included for SLR
    slr_cis <- geom_ribbon(aes(x = 0:(nrow(data)+1), 
                               ymin = {{slr}}-{{slr_ci}}, 
                               ymax = {{slr}}+{{slr_ci}}), 
                           fill = "navyblue", 
                           alpha = 0.1)
    
    # geom and labels if points will be colored by dominant vegetation type
    if(color_by_veg){
        points_veg <- geom_point(data = data, 
                                 aes(x = {{set_ids}}, 
                                     y = {{rates}},
                                     col = {{veg}}), 
                                 size = 3) 
        colors_veg <- scale_color_brewer(type = "qual", palette = "Dark2") 
        labels_veg <- labs(color = "Dominant Vegetation")
    }
    
    

    ##### Assemble in different ways
    #####################################################################    
    
    
    ####################################################################
    ### minimal plot: points only; no confidence intervals
    ####################################################################
    
    # don't color by veg
    if(plot_type == 1 && !color_by_veg){
        p <- p +
            points_same +
            labels_minimal
    }
    
    # do color by veg
    if(plot_type == 1 && color_by_veg){
        p <- p +
            points_veg +
            colors_veg +
            labels_minimal +
            labels_veg
    }
    
    
    
    
    ####################################################################
    # Add in CIs for SETs
    ####################################################################
    # don't color by veg
    if(plot_type == 2 && !color_by_veg){
        p <- p +
            set_cis +
            points_same +
            labels_partial_setci
    }
    
    # do color by veg
    if(plot_type == 2 && color_by_veg){
        p <- p +
            set_cis +
            points_veg +
            colors_veg +
            labels_partial_setci +
            labels_veg
    }
    
    
    
    ####################################################################
    # CIs for both SETs and SLR
    ####################################################################
    # don't color by veg
    if(plot_type == 3 && !color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_same +
            labels_full
    }
    
    
    # do color by veg
    if(plot_type == 3 && color_by_veg){
        p <- p +
            set_cis +
            slr_cis +
            points_veg +
            colors_veg +
            labels_full +
            labels_veg
    }
    
    return(p)
    
} 