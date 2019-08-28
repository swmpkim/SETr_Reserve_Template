# script that has the muscle for making interactive maps
# sourced by R_scripts/04_interact_maps.R






library(leaflet)
library(here)
library(dplyr)

# where to look for the input file
file_in <- here::here("data", "intermediate", "rate_summary.csv")



# libraries leaflet, dplyr, and here loaded in parent script
# file_in also generated in parent script

dat <- read.csv(file_in)


# map differences
# dir_0 is comparison to 0
# dir_slr is comparison to SLR
# map_lab is label column for hovering over points
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
                               TRUE ~ "nonsig"),
           icon_0 = case_when(dir_0 == "dec_sig" ~ "caret-down",
                              dir_0 == "inc_sig" ~ "caret-up",
                              TRUE ~ "circle"),
           icon_slr = case_when(dir_slr == "dec_sig" ~ "caret-down",
                                dir_slr == "inc_sig" ~ "caret-up",
                                TRUE ~ "circle"),
           color_0 = case_when(dir_0 == "dec_sig" ~ "#c00000",
                               dir_0 == "inc_sig" ~ "#2f5597",
                               TRUE ~ "#7f7f7f"),
           color_slr = case_when(dir_slr == "dec_sig" ~ "#c00000",
                               dir_slr == "inc_sig" ~ "#2f5597",
                               TRUE ~ "#7f7f7f"),
           map_lab = paste0(set_id, ": ", user_friendly_set_name, "; ",
                            round(rate, 2), " mm/yr")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)


### actually make the list of icons
icons0 <- awesomeIcons(icon = to_map$icon_0,
                       iconColor = to_map$color_0,
                       markerColor = "lightgray",
                       library = "fa")

iconsslr <- awesomeIcons(icon = to_map$icon_slr,
                       iconColor = to_map$color_slr,
                       markerColor = "lightgray",
                       library = "fa")



# specify what these colors are, for the legends
# lower (c00000)- higher (2f5597) - neutral (7f7f7f)
map_pal <- c("#c00000", "#2f5597", "#7f7f7f")



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
    addAwesomeMarkers(icon = icons0,
                      lng = ~long,
                      lat = ~lat,
                      group = "Compared to 0",
                      popup = ~map_lab) %>%
    ### Compared to SLR
    addAwesomeMarkers(icon = iconsslr,
                      lng = ~long,
                      lat = ~lat,
                      group = "Compared to SLR",
                      popup = ~map_lab) %>%
    ### control which layers can be shown
    addLayersControl(
        baseGroups = c("Esri World Gray Canvas", "Esri World Topo Map", "Esri default"),
        overlayGroups = c("Compared to 0", "Compared to SLR"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Compared to SLR") %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap"),
              opacity = 0.8) 

# print the map
# actually will do this in the calling script so it shows up
m









#####################################################
# regular icons
# make the list of icons


## svg files were downloaded from https://ionicons.com/
## and colors were changed by opening the svg in a text editor (notepad)
## and adding style="fill:#7f7f7f;" (or whatever hext color) inside the 
## 'path' portion of the text

# here we read in my color-modified svgs and turn them into leaflet icons

# carets and plus/minus signs are both present because i'm still not happy

map_icons <- iconList(
    caret_up = makeIcon(iconUrl = here::here("img", "caret_up_blue.svg"),
                        iconWidth = 40, iconHeight = 40),
    caret_down = makeIcon(iconUrl = here::here("img", "caret_down_red.svg"),
                           iconWidth = 40, iconHeight = 40),
    plus_sign = makeIcon(iconUrl = here::here("img", "plus_blue.svg"),
                          iconWidth = 35, iconHeight = 35),
    minus_sign = makeIcon(iconUrl = here::here("img", "minus_red.svg"),
                           iconWidth = 30, iconHeight = 30),
    neutral = makeIcon(iconUrl = here::here("img", "square_open.svg"),
                        iconWidth = 22, iconHeight = 22)
)

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
                               TRUE ~ "nonsig"),
           icon_0 = case_when(dir_0 == "dec_sig" ~ "minus_sign",
                              dir_0 == "inc_sig" ~ "plus_sign",
                              TRUE ~ "neutral"),
           icon_slr = case_when(dir_slr == "dec_sig" ~ "minus_sign",
                                dir_slr == "inc_sig" ~ "plus_sign",
                                TRUE ~ "neutral"),
           map_lab = paste0(set_id, ": ", user_friendly_set_name, "; ",
                            round(rate, 2), " mm/yr")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)




# specify what these colors are, for the legends
# lower (c00000)- higher (2f5597) - neutral (7f7f7f)
map_pal <- c("#c00000", "#2f5597", "#7f7f7f")



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
    addMarkers(icon = ~map_icons[icon_0],
                      lng = ~long,
                      lat = ~lat,
                      group = "Compared to 0",
                      popup = ~map_lab) %>%
    ### Compared to SLR
    addMarkers(icon = ~map_icons[icon_slr],
                      lng = ~long,
                      lat = ~lat,
                      group = "Compared to SLR",
                      popup = ~map_lab) %>%
    ### control which layers can be shown
    addLayersControl(
        baseGroups = c("Esri World Gray Canvas", "Esri World Topo Map", "Esri default"),
        overlayGroups = c("Compared to 0", "Compared to SLR"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup("Compared to SLR") %>% 
    ### dress up the map
    addScaleBar() %>%
    addLegend(position = "bottomright",
              colors = map_pal,
              values = c(1:length(map_pal)),
              labels = c("lower; CIs don't overlap", "higher; CIs don't overlap", "CIs overlap"),
              opacity = 0.8) 

# print the map
# actually will do this in the calling script so it shows up
m


