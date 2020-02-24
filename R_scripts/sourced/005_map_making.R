# script that has the muscle for making interactive maps
# sourced by R_scripts/04_interact_maps.R

# libraries leaflet, dplyr, and here loaded in parent script
# file_in also generated in parent script

dat <- read.csv(file_in) %>%
    mutate_at(c("reserve", "set_id",   
                "user_friendly_set_name",
                "co_dominant_species1"),
              as.character)


# map differences
# dir_0 is comparison to 0
# dir_slr is comparison to SLR
# map_lab is label column for hovering over points
# arrow for direction; color for whether CIs overlap (gray if they do; red/blue if differences are "significant")
# join coordinates with the rate results, and categorize the rates
to_map <- dat %>%
    mutate(map_lab = paste0(set_id, ": ", user_friendly_set_name, "; ",
                            round(rate, 2), " mm/yr")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)


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



# build the map
m <- leaflet(to_map,
             options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
    ### base layer options
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, 
                     group = "Esri World Gray Canvas") %>% 
    addProviderTiles(leaflet::providers$Esri.WorldTopoMap, 
                     group = "Esri World Topo Map") %>% 
    addProviderTiles(leaflet::providers$Esri, 
                     group = "Esri default")%>% 
    ### Compared to 0 
    addMarkers(icon = icon_nonsiga,
               lng = ~long[to_map$dir_0 == "nonsig"],
               lat = ~lat[to_map$dir_0 == "nonsig"],
               group = "Compared to 0",
               popup = ~map_lab[to_map$dir_0 == "nonsig"]) %>%
    addMarkers(icon = icon_inc_siga,
               lng = ~long[to_map$dir_0 == "inc_sig"],
               lat = ~lat[to_map$dir_0 == "inc_sig"],
               group = "Compared to 0",
               popup = ~map_lab[to_map$dir_0 == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_siga,
               lng = ~long[to_map$dir_0 == "dec_sig"],
               lat = ~lat[to_map$dir_0 == "dec_sig"],
               group = "Compared to 0",
               popup = ~map_lab[to_map$dir_0 == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsiga,
               lng = ~long[to_map$dir_0 == "inc_nonsig"],
               lat = ~lat[to_map$dir_0 == "inc_nonsig"],
               group = "Compared to 0",
               popup = ~map_lab[to_map$dir_0 == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsiga,
               lng = ~long[to_map$dir_0 == "dec_nonsig"],
               lat = ~lat[to_map$dir_0 == "dec_nonsig"],
               group = "Compared to 0",
               popup = ~map_lab[to_map$dir_0 == "dec_nonsig"]) %>% 
    ### Compared to SLR 
    addMarkers(icon = icon_nonsigb,
               lng = ~long[to_map$dir_slr == "nonsig"],
               lat = ~lat[to_map$dir_slr == "nonsig"],
               group = "Compared to SLR",
               popup = ~map_lab[to_map$dir_slr == "nonsig"]) %>%
    addMarkers(icon = icon_inc_sigb,
               lng = ~long[to_map$dir_slr == "inc_sig"],
               lat = ~lat[to_map$dir_slr == "inc_sig"],
               group = "Compared to SLR",
               popup = ~map_lab[to_map$dir_slr == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_sigb,
               lng = ~long[to_map$dir_slr == "dec_sig"],
               lat = ~lat[to_map$dir_slr == "dec_sig"],
               group = "Compared to SLR",
               popup = ~map_lab[to_map$dir_slr == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsigb,
               lng = ~long[to_map$dir_slr == "inc_nonsig"],
               lat = ~lat[to_map$dir_slr == "inc_nonsig"],
               group = "Compared to SLR",
               popup = ~map_lab[to_map$dir_slr == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsigb,
               lng = ~long[to_map$dir_slr == "dec_nonsig"],
               lat = ~lat[to_map$dir_slr == "dec_nonsig"],
               group = "Compared to SLR",
               popup = ~map_lab[to_map$dir_slr == "dec_nonsig"]) %>%
    ### Compared to 19-year water level change 
    addMarkers(icon = icon_nonsigb,
               lng = ~long[to_map$dir_19yr == "nonsig"],
               lat = ~lat[to_map$dir_19yr == "nonsig"],
               group = "Compared to 19yr change",
               popup = ~map_lab[to_map$dir_19yr == "nonsig"]) %>%
    addMarkers(icon = icon_inc_sigb,
               lng = ~long[to_map$dir_19yr == "inc_sig"],
               lat = ~lat[to_map$dir_19yr == "inc_sig"],
               group = "Compared to 19yr change",
               popup = ~map_lab[to_map$dir_19yr == "inc_sig"]) %>%
    addMarkers(icon = icon_dec_sigb,
               lng = ~long[to_map$dir_19yr == "dec_sig"],
               lat = ~lat[to_map$dir_19yr == "dec_sig"],
               group = "Compared to 19yr change",
               popup = ~map_lab[to_map$dir_19yr == "dec_sig"]) %>% 
    addMarkers(icon = icon_inc_nonsigb,
               lng = ~long[to_map$dir_19yr == "inc_nonsig"],
               lat = ~lat[to_map$dir_19yr == "inc_nonsig"],
               group = "Compared to 19yr change",
               popup = ~map_lab[to_map$dir_19yr == "inc_nonsig"]) %>%  
    addMarkers(icon = icon_dec_nonsigb,
               lng = ~long[to_map$dir_19yr == "dec_nonsig"],
               lat = ~lat[to_map$dir_19yr == "dec_nonsig"],
               group = "Compared to 19yr change",
               popup = ~map_lab[to_map$dir_19yr == "dec_nonsig"]) %>%
    ### control which layers can be shown
    addLayersControl(
        baseGroups = c("Esri World Gray Canvas", "Esri World Topo Map", "Esri default"),
        overlayGroups = c("Compared to 0", "Compared to SLR", "Compared to 19yr change"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup(c("Compared to SLR", "Compared to 19yr change")) %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap", "not enough info"),
              opacity = 0.8) 

# print the map
# actually will do this in the calling script so it shows up
# m

