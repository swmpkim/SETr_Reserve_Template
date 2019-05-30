## map-making


#### INSTRUCTIONS ####
# Don't use this right now; it doesn't sync up with any other scripts

library(leaflet)
library(here)



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
    


# pull coordinates from the metadata
coords <- mdat %>%
    select(reserve,
           set_id = unique_set_id,
           lat = latitude_dec_deg,
           long = longitude_dec_deg) %>%
    mutate(set_id = as.character(set_id))

# make cutoffs for the size classes (split dataset into 3)
cut_low <- quantile(abs(to_map1$rate_mm.yr), probs = 0.33)
cut_high <- quantile(abs(to_map1$rate_mm.yr), probs = 0.67)

# join coordinates with the rate results, and categorize the rates
to_map <- left_join(to_map1, coords) %>%
    mutate(dir_sig = case_when(rate_mm.yr < 0 & p_value <= 0.05 ~ "dec",
                               rate_mm.yr > 0 & p_value <= 0.05 ~ "inc",
                               TRUE ~ "nonsig"),
           circle_size = case_when(abs(rate_mm.yr) < cut_low ~ 5,
                                   abs(rate_mm.yr) > cut_high ~ 14,
                                   TRUE ~ 10))
inc_index <- which(to_map$dir_sig == "inc")
dec_index <- which(to_map$dir_sig == "dec")
nonsig_index <- which(to_map$dir_sig == "nonsig")


# read in images to use as map icons
icon_incr_path <- here::here("img", "blue_up_arrow.png")
icon_decr_path <- here::here("img", "red_down_arrow.png")
icon_nonsig_path <- here::here("img", "gray_dash.png")


# turn them into icons
icon_incr <- makeIcon(iconUrl = icon_incr_path, 
                      iconWidth = 30, iconHeight = 35)
icon_decr <- makeIcon(iconUrl = icon_decr_path, 
                      iconWidth = 30, iconHeight = 35)
icon_nonsig <- makeIcon(iconUrl = icon_nonsig_path, 
                        iconWidth = 25, iconHeight = 12)


# make the map template
m <- leaflet(to_map,
             options = leafletOptions(minZoom = 0, maxZoom = 25)) %>%
    # addCircleMarkers(lng = ~long, 
    #                  lat = ~lat,
    #                  radius = ~circle_size,
    #                  color = ~dir_sig,
    #                  fill = FALSE) %>%
    addScaleBar() %>%
    # addMarkers(icon = icon_nonsig,
    #            lng = ~long[nonsig_index],
    #            lat = ~lat[nonsig_index]) %>%
    addCircleMarkers(lng = ~long[nonsig_index],
               lat = ~lat[nonsig_index],
               radius = 8,
               color = "gray40") %>%
    addMarkers(icon = icon_incr,
               lng = ~long[inc_index],
               lat = ~lat[inc_index]) %>%
    addMarkers(icon = icon_decr,
               lng = ~long[dec_index],
               lat = ~lat[dec_index])





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
