## map-making

## first run 005_rate_calculations.Rmd to get all the objects into the environment
## (eventually this will be tacked on to the end of that Rmd)

library(leaflet)
library(here)

# read in icons to use on maps
icon_incr_path <- here::here("img", "blue_up_arrow.png")
icon_decr_path <- here::here("img", "red_down_arrow.png")
icon_nonsig_path <- here::here("img", "gray_dash.png")

icon_incr <- makeIcon(iconUrl = icon_incr_path, iconWidth = 30, iconHeight = 40)

# funs from SWMPrExtension::res_sk_map
###############################################################################
if (exists("inc_icons")) {
    ico_loc <- system.file("extdata", "arrow_inc.png", package = "SWMPrExtension")
    icon_img <- makeIcon(iconUrl = ico_loc, iconWidth = 30, 
                         iconHeight = 40, iconAnchorX = 15, iconAnchorY = 15)
    m <- m %>% addMarkers(lng = ~Longitude[inc_icons] * 
                              -1, lat = ~Latitude[inc_icons], icon = icon_img)
}
if (exists("dec_icons")) {
    ico_loc <- system.file("extdata", "arrow_dec.png", package = "SWMPrExtension")
    icon_img <- makeIcon(iconUrl = ico_loc, iconWidth = 30, 
                         iconHeight = 40, iconAnchorX = 15, iconAnchorY = 15)
    m <- m %>% addMarkers(lng = ~Longitude[dec_icons] * 
                              -1, lat = ~Latitude[dec_icons], icon = icon_img)
}
if (exists("insig_icons")) {
    ico_loc <- system.file("extdata", "bar_insig.png", package = "SWMPrExtension")
    icon_img <- makeIcon(iconUrl = ico_loc, iconWidth = 30, 
                         iconHeight = 15, iconAnchorX = 15, iconAnchorY = 7)
    m <- m %>% addMarkers(lng = ~Longitude[insig_icons] * 
                              -1, lat = ~Latitude[insig_icons], icon = icon_img)
}
################################################################################

# pull together rates, p-values, and lat/long coords for mapping
# include reserve and set_id as identifiers
# weed out everything else
to_map1 <- modelcoef2 %>%
    select(reserve, set_id, rate_mm.yr, se_mm.yr, p_value)

## MESS WITH THE DATASET TO MAKE SURE ALL DIRECTIONS and different p values APPEAR
## REMOVE THIS BEFORE USING ON REAL DATA!!!!!!!!!
to_map1 <- to_map1 %>%
    mutate(rate_mm.yr = case_when(set_id %in% c("SPALT-1", "SPALT-2", "SPALT-3") ~ -rate_mm.yr,
                                  TRUE ~ rate_mm.yr),
           p_value = case_when(set_id %in% c("JURO_Mid-1", "JURO_High-1") ~ 0.08,
                               TRUE ~ p_value))
    
to_map1 <- to_map1 %>%
    mutate(rate_mm.yr = case_when(set_id %in% c("SPALT-1", "SPALT-2", "SPALT-3") ~ -rate_mm.yr,
                                  TRUE ~ rate_mm.yr),
           p_value = case_when(set_id %in% c("JURO_Mid-1", "JURO_High-1") ~ 0.08,
                               TRUE ~ p_value))

# pull coordinates from the metadata
coords <- mdat %>%
    select(reserve,
           set_id = unique_set_id,
           lat = latitude_dec_deg,
           long = longitude_dec_deg)

# make cutoffs for the size classes (split dataset into 3)
cut_low <- quantile(abs(to_map1$rate_mm.yr), probs = 0.33)
cut_high <- quantile(abs(to_map1$rate_mm.yr), probs = 0.67)

# join coordinates with the rate results, and categorize the rates
to_map <- left_join(to_map1, coords) %>%
    mutate(dir_sig = case_when(rate_mm.yr < 0 & p_value <= 0.05 ~ "red",
                               rate_mm.yr > 0 & p_value <= 0.05 ~ "blue",
                               TRUE ~ "gray80"),
           circle_size = case_when(abs(rate_mm.yr) < cut_low ~ 5,
                                   abs(rate_mm.yr) > cut_high ~ 14,
                                   TRUE ~ 10))



# make the map template
m <- leaflet(to_map,
             options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
    addCircleMarkers(lng = ~long, 
                     lat = ~lat,
                     radius = ~circle_size,
                     color = ~dir_sig,
                     fill = FALSE) %>%
    addScaleBar()




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



################# jitter the coords
to_map_jittered <- to_map %>%
    mutate(lat_jitter = jitter(lat, factor = 0.001),
           long_jitter = jitter(long, factor = 0.001))

mj <- leaflet(to_map_jittered,
              options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
    addCircleMarkers(lng = ~long_jitter, 
                     lat = ~lat_jitter,
                     radius = ~circle_size,
                     color = ~dir_sig,
                     fill = FALSE) %>%
    addScaleBar()

mj %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) 


