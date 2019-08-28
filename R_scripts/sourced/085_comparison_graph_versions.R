# ALL OF THESE USE FRIENDLY SET NAMES AND A USER-SPECIFIED ORDER FOR THEM TO SHOW UP ALONG THE AXIS


# base plot:
# SLR line (long-term)
# 0 line
# points

# additions:
# 1  CIs - SETs only
# 2  CIs - SETs and SLR

# short-term water level change
# Veg colors for points






if(sum(is.na(mdat$numerical_order)) + sum(is.na(mdat$user_friendly_set_name)) != 0){stop("this won't work because some metadata is missing")}


# pull dominant veg into the model data frame
lmm_out <- left_join(lmm_out, select(mdat, unique_set_id, co_dominant_species1), by = c("set_id" = "unique_set_id")) %>% 
    rename(veg = co_dominant_species1)


# make the set id a factor and order it according to metadata
lmm_out_ordered <- mdat %>% 
    select(unique_set_id, user_friendly_set_name, numerical_order) %>%
    left_join(lmm_out, ., by = c("set_id" = "unique_set_id")) 

a <- forcats::fct_reorder(lmm_out_ordered$user_friendly_set_name,
                          desc(lmm_out_ordered$numerical_order))
b <- forcats::fct_reorder(lmm_out_ordered$set_id,
                          desc(lmm_out_ordered$numerical_order))

lmm_out_ordered$user_friendly_set_name <- a
lmm_out_ordered$set_id2 <- b


#################### make plot



###### SET UP THE PIECES

# base plot with axes and lines for 0 and SLR
p <- ggplot() +
    geom_blank(data = lmm_out_ordered, 
               aes(x = user_friendly_set_name, 
                   y = rate)) +
    geom_hline(aes(yintercept = slr), 
               col = "navyblue", 
               size = 1, 
               alpha = 0.9) +
    geom_hline(aes(yintercept = 0), 
               col = "gray70") +
    theme_classic() +
    coord_flip()



points_same <- geom_point(data = lmm_out_ordered, 
               aes(x = user_friendly_set_name, 
                   y = rate), 
               size = 3, 
               col = "red3")


points_veg <- geom_point(data = lmm_out_ordered, 
                         aes(x = user_friendly_set_name, 
                             y = rate,
                             col = veg), 
                         size = 3) 
colors_veg <- scale_color_brewer(type = "qual", palette = "Dark2") 
labels_veg <- labs(color = "Dominant Vegetation")



labels_full <- labs(title = "Elevation Change with 95% Confidence Intervals", 
         subtitle = paste0("Local SLR in blue: ", slr, " +/- ", slr_ci, " mm/yr"), 
         x = "SET", 
         y = "Rate of change (mm/yr)")


labels_minimal <- labs(title = "Elevation Change", 
                             subtitle = paste0("Local SLR in blue: ", slr, " mm/yr"), 
                             x = "SET", 
                             y = "Rate of change (mm/yr)")


labels_partial_setci <- labs(title = "Elevation Change with 95% Confidence Intervals", 
                       subtitle = paste0("Local SLR in blue: ", slr, " mm/yr"), 
                       x = "SET", 
                       y = "Rate of change (mm/yr)")


set_cis <- geom_errorbar(data = lmm_out_ordered, 
                          aes(x = user_friendly_set_name, 
                              ymin = CI_low, 
                              ymax = CI_high), 
                          col = "gray55", 
                          size = 1) 


slr_cis <- geom_ribbon(aes(x = 0:(nrow(lmm_out_ordered)+1), 
                           ymin = slr-slr_ci, 
                           ymax = slr+slr_ci), 
                       fill = "navyblue", 
                       alpha = 0.1)



##### Assemble in different ways:

# Points only
p +
    points_same +
    labels_minimal

# Points only; colored by veg
p +
    points_veg +
    colors_veg +
    labels_minimal +
    labels_veg

# Add in CIs for SETs
p +
    set_cis +
    points_same +
    labels_partial_setci

# CIs for SETs; points colored by veg
p +
    set_cis +
    points_veg +
    colors_veg +
    labels_partial_setci +
    labels_veg


# CIs for both SETs and SLR
p +
    set_cis +
    slr_cis +
    points_same +
    labels_full


# CIs for both SETs and SLR; points colored by veg
p +
    set_cis +
    slr_cis +
    points_veg +
    colors_veg +
    labels_full +
    labels_veg
