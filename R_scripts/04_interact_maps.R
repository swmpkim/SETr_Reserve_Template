## map-making

## TO-DOs:
# put titles on the maps
# make legends for the maps
# figure out how to make this easy for an end-user
# (maybe the guts of this script get sourced?)
# see if information can pop up when an arrow is clicked on


#### INSTRUCTIONS ####


library(leaflet)
library(here)
library(dplyr)


dat <- read.csv(here::here("data", "intermediate", "rate_summary.csv"))


# map differences
# dir_0 is comparison to 0
# dir_slr is comparison to SLR
# arrow for direction; color for whether CIs overlap (gray if they do; red/blue if differences are "significant")
# join coordinates with the rate results, and categorize the rates
to_map <- dat %>%
    mutate(dir_0 = case_when(CI_high < 0 ~ "dec_sig",
                               CI_low > 0  ~ "inc_sig",
                               rate < 0 ~ "dec_nonsig",
                               rate > 0 ~ "inc_nonsig",
                               TRUE ~ "nonsig"),
           dir_slr = case_when(CI_high < slr_CI_low ~ "dec_sig",
                               CI_low > slr_CI_high  ~ "inc_sig",
                               rate < slr_rate ~ "dec_nonsig",
                               rate > slr_rate ~ "inc_nonsig",
                               TRUE ~ "nonsig")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)


# read in images to use as map icons
icon_inc_sig_path <- here::here("img", "blue_up_arrow.png")
icon_dec_sig_path <- here::here("img", "red_down_arrow.png")
icon_inc_nonsig_path <- here::here("img", "gray_up_arrow.png")
icon_dec_nonsig_path <- here::here("img", "gray_down_arrow.png")
icon_nonsig_path <- here::here("img", "gray_dash.png")

# turn them into icons
icon_inc_sig <- makeIcon(iconUrl = icon_inc_sig_path, 
                         iconWidth = 30, iconHeight = 35)
icon_dec_sig <- makeIcon(iconUrl = icon_dec_sig_path, 
                         iconWidth = 30, iconHeight = 35)
icon_inc_nonsig <- makeIcon(iconUrl = icon_inc_nonsig_path, 
                            iconWidth = 30, iconHeight = 35)
icon_dec_nonsig <- makeIcon(iconUrl = icon_dec_nonsig_path, 
                            iconWidth = 30, iconHeight = 35)
icon_nonsig <- makeIcon(iconUrl = icon_nonsig_path, 
                        iconWidth = 25, iconHeight = 12)

# specify what these colors are, for the legends
map_pal <- c("red", "blue", "gray")



# build the map
m <- leaflet(to_map,
             options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
    ### base layer options
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, 
                     group = "ESRI World Gray Canvas") %>% 
    addProviderTiles(leaflet::providers$Esri.WorldTopoMap, 
                     group = "ESRI World Topo Map") %>% 
    addProviderTiles(leaflet::providers$Esri, 
                     group = "ESRI default")%>% 
    ### Compared to 0 
    addMarkers(icon = icon_nonsig,
               lng = ~long[to_map$dir_0 == "nonsig"],
               lat = ~lat[to_map$dir_0 == "nonsig"],
               group = "Compared to 0") %>%
    addMarkers(icon = icon_inc_sig,
               lng = ~long[to_map$dir_0 == "inc_sig"],
               lat = ~lat[to_map$dir_0 == "inc_sig"],
               group = "Compared to 0") %>%
    addMarkers(icon = icon_dec_sig,
               lng = ~long[to_map$dir_0 == "dec_sig"],
               lat = ~lat[to_map$dir_0 == "dec_sig"],
               group = "Compared to 0") %>% 
    addMarkers(icon = icon_inc_nonsig,
               lng = ~long[to_map$dir_0 == "inc_nonsig"],
               lat = ~lat[to_map$dir_0 == "inc_nonsig"],
               group = "Compared to 0") %>%  
    addMarkers(icon = icon_dec_nonsig,
               lng = ~long[to_map$dir_0 == "dec_nonsig"],
               lat = ~lat[to_map$dir_0 == "dec_nonsig"],
               group = "Compared to 0") %>% 
    ### Compared to SLR 
    addMarkers(icon = icon_nonsig,
               lng = ~long[to_map$dir_slr == "nonsig"],
               lat = ~lat[to_map$dir_slr == "nonsig"],
               group = "Compared to SLR") %>%
    addMarkers(icon = icon_inc_sig,
               lng = ~long[to_map$dir_slr == "inc_sig"],
               lat = ~lat[to_map$dir_slr == "inc_sig"],
               group = "Compared to SLR") %>%
    addMarkers(icon = icon_dec_sig,
               lng = ~long[to_map$dir_slr == "dec_sig"],
               lat = ~lat[to_map$dir_slr == "dec_sig"],
               group = "Compared to SLR") %>% 
    addMarkers(icon = icon_inc_nonsig,
               lng = ~long[to_map$dir_slr == "inc_nonsig"],
               lat = ~lat[to_map$dir_slr == "inc_nonsig"],
               group = "Compared to SLR") %>%  
    addMarkers(icon = icon_dec_nonsig,
               lng = ~long[to_map$dir_slr == "dec_nonsig"],
               lat = ~lat[to_map$dir_slr == "dec_nonsig"],
               group = "Compared to SLR") %>%
    ### control which layers can be shown
    addLayersControl(
        baseGroups = c("ESRI World Gray Canvas", "ESRI World Topo Map", "ESRI default"),
        overlayGroups = c("Compared to 0", "Compared to SLR"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Compared to SLR") %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("significantly lower", "significantly higher", "nonsignificant"))

# print the map
m


