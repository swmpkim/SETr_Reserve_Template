q <- ggplot(filter(cumu_navd88, set_id %in% c("JURO_High-1", "JURO_Mid-1", "JURO_Low-1", "SPALT-1")), aes(x = date, y = mean_cumu, color = set_id)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "SET cumulative change",
         subtitle = "first sample date set to that SET's NAVD88 elevation",
         y = "NAVD88 elevation (m)",
         color = "SET")
q
out_file <- here::here("R_output", "figures", "cumu_change_plots", "cumu_change_NAVD88_subset.png")
ggsave(out_file, width = 5.8, height = 8.2, units = "in")


# Tidal datums in meters, relative to MLLW:  
#     
#     +  MHHW 0.485  
# +  MHW 0.456  
# +  MSL 0.236  
# +  MLW 0.039  
# +  MLLW 0.000  
# +  NAVD88 0.183
# 
# 
# ### Tidal Datums relative to NAVD88  
# 
# These are obtained by subtracting 0.183 from all the other values, and these will be used in the graphs.  
# 
# +  MHHW 0.302  
# +  MHW 0.273  
# +  MSL 0.053  
# +  MLW -0.144  
# +  MLLW -0.183  
# +  NAVD88 0.000 

# to relate everything to mean sea level: subtract 0.053 from all my NAVD88 elevations

cumu_msl <- cumu_navd88 %>% 
    mutate(mean_cumu = mean_cumu - 0.053)

msl_int <- 0
mhw_int <- 0.273-0.053
mlw_int <- -0.144-0.053

q <- ggplot(filter(cumu_msl, set_id %in% c("JURO_High-1", "JURO_Mid-1", "JURO_Low-1", "SPALT-1")), aes(x = date, y = mean_cumu, color = set_id)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "SET cumulative change",
         subtitle = "first sample date set to that SET's elevation relative to mean sea level",
         y = "elevation above MSL (m)",
         color = "SET")
q


# add lines for msl, mhw, mlw
q +
    geom_abline(slope = slr_for_graph,
                intercept = msl_int,
                type = "dashed") +
    geom_abline(slope = slr_for_graph,
                intercept = mhw_int,
                type = "dashed") +
    geom_abline(slope = slr_for_graph,
                intercept = mlw_int,
                type = "dashed")


    