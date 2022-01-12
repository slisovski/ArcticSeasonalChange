# Load packages
library(rgee)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)

ee_Initialize()

# # Load the Study Areas and convert them to ee Object
# sf_roi <- c(st_bbox(c(xmin =  126-7.5, ymin = 50, xmax =  126+7.5, ymax = 90)) %>% st_as_sfc(),
#             st_bbox(c(xmin = -134-7.5, ymin = 50, xmax = -134+7.5, ymax = 90)) %>% st_as_sfc()) %>% st_set_crs(4326) %>%
#           st_cast("LINESTRING") %>% st_sample(2000, type = "regular") %>% st_cast("POLYGON")
# 
# 
# ## Intersect with land, habitat (tundra, taiga)
# ### get biome data
# ### https://www.gislounge.com/terrestrial-ecoregions-gis-data/
# ecoreg   <- read_sf("~/Google Drive/My Drive/GeoDat/Ecoregions2017/Ecoregions2017.shp") %>% 
#   filter(BIOME_NAME %in% c("Tundra", "Boreal Forests/Taiga")) %>% 
#   group_by(BIOME_NAME) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup() %>%
#   st_intersection(sf_roi) %>% mutate(roi = c("roi1", "roi1", "roi2", "roi2"))
# 
# st_write(ecoreg, "Data/StudyAreas/roi/roi_1_2.shp")
sf_roi_all <- st_read("Data/StudyAreas/roi/roi_1_2.shp") 

## Buffer for GEE selection
sf_roi <- sf_roi_all %>% filter(roi == "roi1") %>% st_buffer(0.2) %>% st_union()
ee_roi <- sf_as_ee(sf_roi)

center <- st_centroid(sf_roi) %>% st_coordinates()

##########################
## MODIS dataset in GEE ##
##########################

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


  


###### test

testPlot <- Mod09ga_masked$
  filterDate("2018-07-25", "2018-08-15")
  
ndvi_vis <- list(
  min = 0.05,
  max = 1,
  palette = c("yellow", "green")
)


Map$setCenter(center[1], center[2], 2)
Map$addLayer(ee_roi, {}, "ROI", FALSE) +
  Map$addLayer(testPlot$select(c('sur_refl_b01', 'sur_refl_b04', 'sur_refl_b03'))$first()$clip(ee_roi), list(
    min = -100.0,
    max = 8000.0), 'RGB', FALSE)+
  Map$addLayer(testPlot$select('ndvi')$first()$clip(ee_roi), 
               ndvi_vis, 'NDVI_first', TRUE) +
  Map$addLayer(testPlot$select('ndvi')$median()$clip(ee_roi), 
               ndvi_vis, 'NDVI_median', TRUE)

  



