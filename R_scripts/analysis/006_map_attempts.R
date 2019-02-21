## map-making

## first run 005_rate_calculations.Rmd to get all the objects into the environment
## (eventually this will be tacked on to the end of that Rmd)

library(leaflet)


# pull together rates, p-values, and lat/long coords for mapping
# include reserve and set_id as identifiers
# weed out everything else
to_map1 <- modelcoef2 %>%
    select(reserve, set_id, rate_mm.yr, se_mm.yr, p_value)

## MESS WITH THE DATASET TO MAKE SURE ALL DIRECTIONS APPEAR
## REMOVE THIS BEFORE ACTUALLY RUNNING!!!!!!!!!
to_map1[to_map1$set_id %in% c("SPALT-1", "SPALT-2", "SPALT-3"), ]$rate_mm.yr <- -5
to_map1[to_map1$set_id %in% c("JURO_Low-1", "JURO_Low-2", "JURO_Low-3"), ]$rate_mm.yr <- 0

coords <- mdat %>%
    select(reserve,
           set_id = unique_set_id,
           lat = latitude_dec_deg,
           long = longitude_dec_deg)

to_map <- left_join(to_map1, coords) %>%
    mutate(direction = case_when(rate_mm.yr < 0 ~ "neg",
                                 rate_mm.yr > 0 ~ "pos",
                                 TRUE ~ "none"),
           dir_col = case_when(direction == "neg" ~ "red",
                               direction == "pos" ~ "blue",
                               direction == "none" ~ "gray80",
                               TRUE ~ "black"),
           significance = case_when(p_value > 0.05 ~ "non-sig",
                                    p_value <= 0.05 & p_value > 0.01 ~ "p < 0.05",
                                    p_value <= 0.01 ~ "p < 0.01",
                                    TRUE ~ "oops"))



m <- leaflet(to_map) %>%
    addCircleMarkers(radius = 8,
                     color = ~dir_col) %>%
    addScaleBar()

# Esri World Gray Canvas tiles
m %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas)


# default tiles
m %>%
    addTiles()

# Esri World Gray Canvas tiles
m %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas)

# default Esri tiles
m %>%
    addProviderTiles(leaflet::providers$Esri)

# Esri topo map
# i think this is my favorite
m %>%
    addProviderTiles(leaflet::providers$Esri.WorldTopoMap)

# NatGeo tiles
m %>%
    addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap)

m %>%
    addProviderTiles(leaflet::providers$OpenTopoMap)


