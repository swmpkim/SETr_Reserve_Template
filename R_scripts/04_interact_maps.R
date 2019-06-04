## map-making


#### INSTRUCTIONS ####


library(leaflet)
library(here)
library(dplyr)


dat <- read.csv(here::here("data", "intermediate", "rate_summary.csv"))


# map difference from 0
# arrow for direction; color for whether CI excludes 0?
# join coordinates with the rate results, and categorize the rates
# if confidence intervals don't overlap 0, it's a significant increase or decrease
# if CIs do overlap 0, it's nonsig increase or decrease based on sign of rate
to_map <- dat %>%
    mutate(dir_sig = case_when(CI_high < 0 ~ "dec_sig",
                               CI_low > 0  ~ "inc_sig",
                               rate < 0 ~ "dec_nonsig",
                               rate > 0 ~ "inc_nonsig",
                               TRUE ~ "nonsig")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)

# set up the indices of which SET gets which icon, based on diff or not
inc_sig_index <- which(to_map$dir_sig == "inc_sig")
dec_sig_index <- which(to_map$dir_sig == "dec_sig")
inc_nonsig_index <- which(to_map$dir_sig == "inc_nonsig")
dec_nonsig_index <- which(to_map$dir_sig == "dec_nonsig")
nonsig_index <- which(to_map$dir_sig == "nonsig")


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


# make the map template
m <- leaflet(to_map,
             options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
        addScaleBar() %>%
        addCircleMarkers(lng = ~long[nonsig_index],
                     lat = ~lat[nonsig_index],
                     radius = 8,
                     color = "gray40") %>%
    addMarkers(icon = icon_inc_sig,
               lng = ~long[inc_sig_index],
               lat = ~lat[inc_sig_index]) %>%
    addMarkers(icon = icon_dec_sig,
               lng = ~long[dec_sig_index],
               lat = ~lat[dec_sig_index]) %>% 
    addMarkers(icon = icon_inc_nonsig,
               lng = ~long[inc_nonsig_index],
               lat = ~lat[inc_nonsig_index]) %>%  
    addMarkers(icon = icon_dec_nonsig,
               lng = ~long[dec_nonsig_index],
               lat = ~lat[dec_nonsig_index])


## print the map with some different backgrounds:

# Esri World Gray Canvas tiles
m %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) 


# Esri topo map (kim's favorite, but might not work everywhere)
m %>%
    addProviderTiles(leaflet::providers$Esri.WorldTopoMap)

####################################################################

# leaflet's default tiles
m %>%
    addTiles()


# default Esri tiles
m %>%
    addProviderTiles(leaflet::providers$Esri)

# NatGeo tiles
m %>%
    addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap)

m %>%
    addProviderTiles(leaflet::providers$OpenTopoMap)
