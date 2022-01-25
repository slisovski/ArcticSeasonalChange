# Load packages
library(rgee)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)
library(cptcity)

ee_Initialize()


map <- read_sf("My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_land/ne_50m_land.shp") %>%
  st_difference(read_sf("My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_lakes/ne_50m_lakes.shp") %>%
                  mutate(area = st_area(geometry)) %>%
                  filter(area >= quantile(area, probs = 0.9))) %>% st_union()


sub_roi <- st_bbox(c(xmin =  126-2.5, ymin = 72, xmax =  127+2.5, ymax = 74)) %>% st_as_sfc() %>% st_set_crs(4326) %>% st_cast("LINESTRING") %>% st_sample(2000, type = "regular") %>% st_cast("POLYGON")

area <- map %>% st_intersection(sub_roi)


ecoreg   <- read_sf("My Drive/GeoDat/Ecoregions2017/Ecoregions2017.shp") %>% 
  filter(BIOME_NAME %in% c("Tundra", "Boreal Forests/Taiga")) %>% 
  group_by(BIOME_NAME) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup() %>%
  st_intersection(area)

plot(ecoreg) 

st_write(ecoreg, "Data/StudyAreas/roi/sub_roi.shp")

sf_sub_roi <- st_read("Data/StudyAreas/roi/sub_roi.shp") 

## Buffer for GEE selection
sf_roi <- sf_sub_roi %>% st_buffer(0.2) %>% st_union()
ee_roi <- sf_as_ee(sf_roi)

center <- st_centroid(area) %>% st_coordinates()

## Modis water mask
water <- ee$Image("MODIS/MOD44W/MOD44W_005_2000_02_24")$select("water_mask")$Not()

Mod09ga <- ee$ImageCollection("MODIS/006/MOD09GA")$
  filterDate("2018-07-01", "2018-08-31")$
  filterBounds(ee_roi)$
  map(function(image) {
    return(image$mask(water))
  })$
  map(function(image) {
    ndvi = image$normalizedDifference(c('sur_refl_b02', 'sur_refl_b01'))$rename('ndvi')
    return(image$addBands(ndvi))
  })


## cloud masking (using Modis MOD10A1 cloud mask)
clouds_ndsi <- ee$ImageCollection('MODIS/006/MOD10A1')$
  select('NDSI_Snow_Cover')$
  map(function(image) {
    mask = image$lt(-1)$Not()$rename("CloudMask")
    return(image$addBands(mask))
  })

##############################
### match image collection ###
##############################
## create filter
filter = ee$Filter$equals(
  leftField = "system:time_start",
  rightField = "system:time_start"
)
## Create the join
simpleJoin <- ee$Join$inner()
## Inner join
innerJoin <- ee$ImageCollection(simpleJoin$apply(Mod09ga, clouds_ndsi, filter))

## Join, select, mask
Mod09ga_masked <- innerJoin$map(function(feature) {
  return(ee$Image$cat(feature$get('primary'), feature$get('secondary')))
})$map(function(image) {
  ndvi = image$select("ndvi")
  ndsi = image$select("NDSI_Snow_Cover")$rename("ndsi")
  mask = image$select("CloudMask")
  return(image$select(c('sur_refl_b01', 'sur_refl_b04', 'sur_refl_b03'))$addBands(ndvi)$addBands(ndsi)$updateMask(mask))
})


ee_as_raster()