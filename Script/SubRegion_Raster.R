# Load packages
library(rgee)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)
library(cptcity)

ee_Initialize()


# map <- read_sf("~/Google Drive/My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_land/ne_50m_land.shp") %>%
#   st_difference(read_sf("~/Google Drive/My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_lakes/ne_50m_lakes.shp") %>%
#                   mutate(area = st_area(geometry)) %>%
#                   filter(area >= quantile(area, probs = 0.9))) %>% st_union()
# 
# ecoreg   <- read_sf("My Drive/GeoDat/Ecoregions2017/Ecoregions2017.shp") %>% 
#   filter(BIOME_NAME %in% c("Tundra", "Boreal Forests/Taiga")) %>% 
#   group_by(BIOME_NAME) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup() %>%
#   st_intersection(area)
# area <- map %>% st_intersection(sub_roi)

ecoreg <- st_read("Data/StudyAreas/roi/roi_1_2.shp")

sub_roi <- st_bbox(c(xmin =  126-2.5, ymin = 72, xmax =  127+2.5, ymax = 74)) %>% 
  st_as_sfc() %>% st_set_crs(4326)

plot(roi)
plot(ecoreg %>% st_intersection(sub_roi), add = T)

## Buffer for GEE selection
ee_roi <- sf_as_ee(sub_roi)
center <- st_centroid(sub_roi) %>% st_coordinates()

## Modis water mask
water <- ee$Image("MODIS/MOD44W/MOD44W_005_2000_02_24")$select("water_mask")$Not()

Mod09ga <- ee$ImageCollection("MODIS/006/MOD09GA")$
  filterDate("2018-07-01", "2018-08-01")$
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
    mask = image$gt(190)$Not()$rename("CloudMask")
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


# Map$setCenter(center[1], center[2], 9)
# Map$addLayer(Mod09ga_masked$select(c('sur_refl_b01', 'sur_refl_b04', 'sur_refl_b03'))$first() , list(
#   min = -100.0,
#   max = 8000.0), "rgb") +
# Map$addLayer(Mod09ga_masked$first()$select("ndsi"), list(palette = c("red", "green"), opacity = 0.4), "cloud", FALSE)+
# Map$addLayer(Mod09ga_masked$first()$clip(ee_roi)$select("ndsi"), list(palette = c("grey", "blue")), "ndsi", FALSE)


# test <- ee_as_raster(Mod09ga_masked$first()$select("ndsi")$unmask(-9999),
#                      region = ee_roi,
#                      scale = Mod09ga_masked$select("ndsi")$first()$projection()$nominalScale()$getInfo(),
#                      crs = "EPSG:4326")
# # test[test[]<9000] <- NA
# raster::plot(test)
# plot(sub_roi %>% st_transform(proj4string(test)), add = T)

## ee_as_raster is super inefficient - one raster per time lots of time

# ee_image_to_drive() ## saves raster in you google drive
                    ## loop throught the image.Collection and save each band with a meaningful name

dates <- ee_get_date_ic(Mod09ga_masked)

for(i in 1:nrow(dates)) {
  
  tmp <- Mod09ga_masked$filterDate(format(dates$time_start[i], "%Y-%m-%d"), format(dates$time_start[i]+24*60*60, "%Y-%m-%d"))$first()
  
  task1 <- ee_image_to_drive(tmp$select("ndsi"),
                             region = ee_roi,                            
                             timePrefix = FALSE,
                             fileNamePrefix = glue::glue('MODIS_ndsi_{format(dates$time_start[i], "%Y-%m-%d")}'),
                             fileFormat = "GeoTIFF",
                             folder = "rasters1")
  
  task1$start()
  ## check if you can save images with multiple bands
  
  task2 <- ee_image_to_drive(tmp$select("ndvi"),
                             region = ee_roi,
                             timePrefix = FALSE,
                             fileNamePrefix = glue::glue('MODIS_ndvi_{format(dates$time_start[i], "%Y-%m-%d")}'),
                             fileFormat = "GeoTIFF",
                             folder = "rasters1")
  task2$start()
  
  ## define folder in Drive etc.
  ## Description and name of file needs to contain e.g. "2001-12-11.tif"
  
  ## test within a loop (DON'T RUN IF YOU RUN THE LOOP)
  # test <- raster("path/tif")
  # plot(test)
  
  
}



######
### you've got all the tifs
##
## what you want

# Step2
## For example looping through all raster files (both NDVI and NDSI) and get Data into an Array
## array[pixels, dates, index]


plot(dataset$date, dataset$ndvi)
points(dataset$date, dataset$ndsi)






