
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
## Simeon drive
# drive         <- "~/Google Drive/My Drive/rasters/"
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*")   
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif")))) ## that may need to be adjusted depending on your file name. This looked OK as well

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
r0   <- raster(paste0(drive, flsModis[1])); proj4string(r0) <- rast_proj

##### This line is making problems, since the projectRaster creates a different resolution
##### and thus more crs than the original raster
# crds <- r0 %>% projectRaster(crs = CRS("+proj=longlat")) %>% coordinates()

#### Solution
crds <- r0 %>% coordinates() %>% as.data.frame() %>% st_as_sf(coords = c("y", "y")) %>%
  st_set_crs(rast_proj) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble() %>% 
  rownames_to_column(var = "index") %>% filter(!is.na(r0[])) %>% mutate(index = as.numeric(index))

modisArray <- array(dim = c(nrow(crds), length(c(flsModis)), 2)) ## array[pixels, dates, c(ndsi, ndvi)]

for(i in 1:length(flsModis)) {
  
  ndvi <- raster(paste0(drive, flsModis[i]))
  ndsi <- raster(gsub("MODIS_ndvi", "MODIS_ndsi", paste0(drive, flsModis[i])))
  
  modisArray[,i,1] <- ifelse(ndvi[][crds$index] < -1000, NA, ndvi[][crds$index]) ## at this stage you can remove -9999 to NAs
  modisArray[,i,2] <- ifelse(ndsi[][crds$index] < -1000, NA, ndsi[][crds$index])
  
}

opar <- par(mar = c(4,4,1,4), las = 1)
matplot(flsModis_date-6*60*60, t(modisArray[,,1]), pch = 16, cex = 1, 
        col = adjustcolor("darkgreen", alpha.f = 0.4), type = "p",
        ylim = c(-0.25, 1), xlim = range(flsModis_date), xlab = "", ylab = "NDVI")
par(new = T)
matplot(flsModis_date+6*60*60, t(modisArray[,,2]), pch = 16, cex = 1, 
        col = adjustcolor("cornflowerblue", alpha.f = 0.4), type = "p",
        ylim = c(0,100), xlim = range(flsModis_date), xlab = "", ylab = "", yaxt  = "n", xaxt = "n")
axis(4)
mtext("NDSI", 4, line = 3, las = 3)
par(opar)

## Not is the best time to get into ggplot and see how you can make a nice graph of this