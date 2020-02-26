# run this at the end of the Word report script (after 002)
library(leaflet)
library(mapview)

# save a log file just in case
log_name <- paste0(Sys.Date(), "_004mapsstatic_logfile.txt")
writeLines(capture.output(sessionInfo()), here::here("R_output", "log_files", log_name))

# map differences
# dir_0 is comparison to 0
# dir_slr is comparison to SLR
# map_lab is label column for hovering over points
# arrow for direction; color for whether CIs overlap (gray if they do; red/blue if differences are "significant")
# join coordinates with the rate results, and categorize the rates
to_map <- rates_slr_all %>%
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg) %>% 
    filter(!is.na(lat),
           !is.na(long))


# read in images to use as map icons
icon_inc_sig_path <- here::here("img", "blue_up_arrow.png")
icon_dec_sig_path <- here::here("img", "red_down_arrow.png")
icon_inc_nonsig_path <- here::here("img", "gray_up_arrow.png")
icon_dec_nonsig_path <- here::here("img", "gray_down_arrow.png")
icon_nonsig_path <- here::here("img", "yel_not_enough_info.png")

# turn them into icons
icon_inc_siga <- icon_inc_sigb <- makeIcon(iconUrl = icon_inc_sig_path, 
                                           iconWidth = 30, iconHeight = 35)
icon_dec_siga <- icon_dec_sigb <- makeIcon(iconUrl = icon_dec_sig_path, 
                                           iconWidth = 30, iconHeight = 35)
icon_inc_nonsiga <- icon_inc_nonsigb <- makeIcon(iconUrl = icon_inc_nonsig_path, 
                                                 iconWidth = 30, iconHeight = 35)
icon_dec_nonsiga <- icon_dec_nonsigb <- makeIcon(iconUrl = icon_dec_nonsig_path, 
                                                 iconWidth = 30, iconHeight = 35)
icon_nonsiga <- icon_nonsigb <- makeIcon(iconUrl = icon_nonsig_path, 
                                         iconWidth = 20, iconHeight = 20)


# specify what these colors are, for the legends
map_pal <- c("#c00000", "#2f5597", "#7f7f7f", "#fffacd")





# build the map - comparison to 0
m0 <- leaflet(to_map,
              options = leafletOptions(zoomControl = FALSE)) %>%
    ### base layer options
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
    ### Compared to 0 
    addMarkers(icon = icon_nonsiga,
               lng = ~long[to_map$dir_0 == "nonsig"],
               lat = ~lat[to_map$dir_0 == "nonsig"]) %>%
    addMarkers(icon = icon_inc_siga,
               lng = ~long[to_map$dir_0 == "inc_sig"],
               lat = ~lat[to_map$dir_0 == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_siga,
               lng = ~long[to_map$dir_0 == "dec_sig"],
               lat = ~lat[to_map$dir_0 == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsiga,
               lng = ~long[to_map$dir_0 == "inc_nonsig"],
               lat = ~lat[to_map$dir_0 == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsiga,
               lng = ~long[to_map$dir_0 == "dec_nonsig"],
               lat = ~lat[to_map$dir_0 == "dec_nonsig"]) %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(title = "Compared to 0",
              position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap", "not enough info"),
              opacity = 0.8) 

# save it out
file_path1 <- here::here("R_output", "figures", "maps", "map_0.png")
mapview::mapshot(m0, file = file_path1)



# build the map - comparison to SLR
mSLR <- leaflet(to_map,
                options = leafletOptions(zoomControl = FALSE)) %>%
    ### base layer options
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
    ### Compared to SLR 
    addMarkers(icon = icon_nonsiga,
               lng = ~long[to_map$dir_slr == "nonsig"],
               lat = ~lat[to_map$dir_slr == "nonsig"]) %>%
    addMarkers(icon = icon_inc_siga,
               lng = ~long[to_map$dir_slr == "inc_sig"],
               lat = ~lat[to_map$dir_slr == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_siga,
               lng = ~long[to_map$dir_slr == "dec_sig"],
               lat = ~lat[to_map$dir_slr == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsiga,
               lng = ~long[to_map$dir_slr == "inc_nonsig"],
               lat = ~lat[to_map$dir_slr == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsiga,
               lng = ~long[to_map$dir_slr == "dec_nonsig"],
               lat = ~lat[to_map$dir_slr == "dec_nonsig"]) %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(title = "Compared to Sea Level Rise",
              position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap", "not enough info"),
              opacity = 0.8) 

# save it out
file_path2 <- here::here("R_output", "figures", "maps", "map_SLR.png")
mapview::mapshot(mSLR, file = file_path2)





# build the map - comparison to 19-year water level change
m19yr <- leaflet(to_map,
                options = leafletOptions(zoomControl = FALSE)) %>%
    ### base layer options
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>% 
    ### Compared to SLR 
    addMarkers(icon = icon_nonsiga,
               lng = ~long[to_map$dir_19yr == "nonsig"],
               lat = ~lat[to_map$dir_19yr == "nonsig"]) %>%
    addMarkers(icon = icon_inc_siga,
               lng = ~long[to_map$dir_19yr == "inc_sig"],
               lat = ~lat[to_map$dir_19yr == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_siga,
               lng = ~long[to_map$dir_19yr == "dec_sig"],
               lat = ~lat[to_map$dir_19yr == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsiga,
               lng = ~long[to_map$dir_19yr == "inc_nonsig"],
               lat = ~lat[to_map$dir_19yr == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsiga,
               lng = ~long[to_map$dir_19yr == "dec_nonsig"],
               lat = ~lat[to_map$dir_19yr == "dec_nonsig"]) %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(title = "Compared to 19-yr \nwater level change",
              position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap", "not enough info"),
              opacity = 0.8) 

# save it out
file_path2 <- here::here("R_output", "figures", "maps", "map_19yr.png")
mapview::mapshot(m19yr, file = file_path2)