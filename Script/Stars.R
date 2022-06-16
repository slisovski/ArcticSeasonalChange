library(tidyverse)
library(sp)
library(raster)
library(stars)
library(lubridate)
library(ggplot2)
library(tidyr)
library(sf); sf::sf_use_s2(FALSE)


# Work with all the grids now
drive         <- "/Users/tasos/Documents/20_rasters/"
# drive         <- "~/Google Drive/My Drive/rasters/"

flsModis      <- tibble(path = list.files(drive, pattern = "MODIS_ndvi*", recursive = T)) %>%   
  mutate(date = as.POSIXct(sapply(strsplit(path, "_"), function(x) unlist(strsplit(x[3], ".tif"))))) %>%
  filter(as.numeric(format(date, "%m"))%in%c(4:11))

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

r0   <- read_stars(paste0(drive, flsModis$path[1])) %>% st_set_crs(rast_proj)
plot(r0, axes = T)


grid_bbox <- r0 %>% st_bbox() %>% st_as_sfc() %>% st_make_grid(n = c(2,1)) 
text(grid_bbox %>% st_centroid() %>% st_coordinates(), as.character(1:length(grid_bbox))) 

for(i in 1:length(grid_bbox)) {
  
  pol        <- grid_bbox[i]
  tmp        <- r0 %>% st_set_crs(rast_proj) %>%
                st_crop(pol %>% st_transform(st_crs(rast_proj)))
  
  modisArray <- array(dim = c(length(c(tmp[[1]])), nrow(flsModis), 2))
  for(dts in 1:nrow(flsModis)) {
    
    ndvi <- read_stars(paste0(drive, flsModis$path[i])) %>% 
      st_set_crs(rast_proj) %>%
      st_crop(pol %>% st_transform(st_crs(rast_proj)))
    
    ndsi <- read_stars(gsub("MODIS_ndvi", "MODIS_ndsi", paste0(drive, flsModis$path[i]))) %>%
      st_warp(ndvi)
    
    modisArray[,dts,1] <- ifelse(c(ndvi[[1]]) < -1000, NA, c(ndvi[[1]])) ## at this stage you can remove -9999 to NAs
    modisArray[,dts,2] <- ifelse(c(ndsi[[1]]) < -1000, NA, c(ndsi[[1]]))
    
  }
  
  chunckOut <- list(rast        = tmp %>% setNames("template"),
                    date        = flsModis$date,
                    modisArray  = modisArray)
  
  save(chunckOut, file = glue::glue("/Users/tasos/Documents/chunckOut_chnk{i}.rda"))
  
}


