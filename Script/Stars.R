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
plot(r0)

grid_bbox <- r0 %>% st_bbox() %>% st_as_sfc() %>% st_make_grid(n = c(2,1))
text(grid_bbox %>% st_centroid() %>% st_coordinates(), as.character(1:length(grid_bbox))) 

chunck_01    <- grid_bbox[1]
chunck_02    <- grid_bbox[2]

crds01 <- 


# Create pixel dataframes for each grid cell




# Get the arrays of data for each grid

modisArray01 <- array(dim = c(nrow(crds01), nrow(flsModis), 2)) ## array[pixels, dates, c(ndsi, ndvi)]
modisArray02 <- array(dim = c(nrow(crds02), nrow(flsModis), 2))



for(i in 1:nrow(flsModis)) {
  
  ndvi <- raster(paste0(drive, flsModis$path[i]))
  ndsi <- raster(gsub("MODIS_ndvi", "MODIS_ndsi", paste0(drive, flsModis$path[i])))
  
  modisArray01[,i,1] <- ifelse(ndvi[][crds01$index] < -1000, NA, ndvi[][crds01$index]) ## at this stage you can remove -9999 to NAs
  modisArray01[,i,2] <- ifelse(ndsi[][crds01$index] < -1000, NA, ndsi[][crds01$index])
  
}

chunckOut01 <- list(rasterIndex = indexRast_01,
                  date        = flsModis$date,
                  modisArray  = modisArray01)

chunckOut02 <- list(rasterIndex = indexRast_02,
                    date        = flsModis$date,
                    modisArray  = modisArray02)


save(chunckOut, file = "C:/Users/tasos/Desktop/Master Thesis Project!/chunkOut.rda")