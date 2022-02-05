
# Load packages
library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

## Location
proj   <- "+proj=laea +lon_0=98.54 +lat_0=76.08"
roi    <- st_point(c(98.54, 76.08)) %>% st_sfc() %>% st_set_crs(4326) %>%
  st_transform(proj) %>% st_buffer(5000)
bbox   <- st_bbox(roi %>% st_transform(4326)) %>% st_as_sfc() %>% st_transform(4326)


# flsMod (get dates for NDVI, since it shoudl be the same for NDSI). I changed the drive location to my drive here
drive         <- "My Drive/Master Thesis/rasters/"
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*")   # I got a correct list with 365 tifs for the year 2018
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif")))) ## that may need to be adjusted depending on your file name. This looked OK as well

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
r0   <- raster(paste0(drive, flsModis[1])); proj4string(r0) <- rast_proj
crds <- r0 %>% projectRaster(crs = CRS("+proj=longlat")) %>% coordinates()

modisArray <- array(dim = c(nrow(crds), length(c(flsModis)), 2)) ## array[pixels, dates, c(ndsi, ndvi)]

for(i in 1:length(flsModis)) {
  
  ndvi <- raster(paste0(drive, flsModis[i])); proj4string(r0) <- rast_proj
  ndsi <- raster(gsub("MODIS_ndvi", "MODIS_ndsi", paste0(drive, flsModis[i]))); proj4string(r0) <- rast_proj
  
  modisArray[,i,1] <- ndvi[] 
  modisArray[,i,2] <- ndsi[] 
  
}

modisArray # Here I get the NA dataframe

matplot(flsModis_date, modisArray[,,1], pch = 16, cex = 0.2, col = "cornflowerblue", type = "o")
## if that does not work
matplot(flsModis_date, t(modisArray[,,1]), pch = 16, cex = 0.2, col = "cornflowerblue", type = "o")

## Not is the best time to get into ggplot and see how you can make a nice graph of this