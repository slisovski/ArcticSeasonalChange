<<<<<<< HEAD
=======

>>>>>>> 4975f9d3f866e2b5210e52bd30213364559e0e0f
# Load packages
library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

<<<<<<< HEAD
# Load the array and the df
load("/Users/tasos/Documents/array2001_2012.rda")
load("/Users/tasos/Documents/array2001_2012df.rda")
=======
# flsMod (get dates for NDVI, since it shoudl be the same for NDSI). I changed the drive location to my drive here
drive         <- "~/Documents/20_rasters/"
## Simeon drive 
# drive         <- "~/Google Drive/My Drive/rasters/"
flsModis      <- tibble(path = list.files(drive, pattern = "MODIS_ndvi*", recursive = T)) %>%   
  mutate(date = as.POSIXct(sapply(strsplit(path, "_"), function(x) unlist(strsplit(x[3], ".tif"))))) %>%
  filter(as.numeric(format(date, "%m"))%in%c(4:11))

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
r0   <- raster(paste0(drive, flsModis$path[1])); proj4string(r0) <- rast_proj

grid_bbox <- as(extent(r0), "SpatialPolygons") %>% st_as_sf() %>% st_make_grid(cellsize = 25000)
text(grid_bbox %>% st_centroid() %>% st_coordinates(), as.character(1:length(grid_bbox)))
chunck    <- grid_bbox[15]

indexRast <- rasterize(as(chunck, "Spatial"), r0)
>>>>>>> 4975f9d3f866e2b5210e52bd30213364559e0e0f


##### This line is making problems, since the projectRaster creates a different resolution
##### and thus more crs than the original raster
# crds <- r0 %>% projectRaster(crs = CRS("+proj=longlat")) %>% coordinates()

#### Solution
crds <- indexRast %>% coordinates() %>% as.data.frame() %>% st_as_sf(coords = c("y", "y")) %>%
  mutate(chunck = indexRast[]) %>% as_tibble() %>% 
  rownames_to_column(var = "index") %>% filter(!is.na(chunck)) %>% mutate(index = as.numeric(index))


<<<<<<< HEAD
dim(chunckOut$modisArray)

# Calculate the median of NDVI and NDVI for each year
NDVImedian <- t(apply(chunckOut$modisArray[,,1], 1, function(x) {
  chunckOut$modisArray[chunckOut$modisArray[,,1] < 0.05] <- NA # Remove the NDVI values below 0.05
  tapply(x, format(chunckOut$date, "%Y"), median, na.rm = T)
}))

NDSImedian <- t(apply(chunckOut$modisArray[,,2], 1, function(x) {
  chunckOut$modisArray[,,2] <- chunckOut$modisArray[,,2] /100 #Convert NDSI values to 0 - 1 range
  tapply(x, format(chunckOut$date, "%Y"), median, na.rm = T)
=======
# Load the Chunk
load("/Users/tasos/Documents/chunk.rda")



dim(chunkOut$modisArray)

# Calculate the median of NDVI and NDVI for each year
NDVImedian <- t(apply(chunkOut$modisArray[,,1], 1, function(x) {
  chunkOut$modisArray[chunkOut$modisArray[,,1] < 0.05] <- NA # Remove the NDVI values below 0.05
  tapply(x, format(chunkOut$date, "%Y"), median, na.rm = T)
}))

NDSImedian <- t(apply(chunkOut$modisArray[,,2], 1, function(x) {
  chunkOut$modisArray[,,2] <- chunkOut$modisArray[,,2] /100 #Convert NDSI values to 0 - 1 range
  tapply(x, format(chunkOut$date, "%Y"), median, na.rm = T)
>>>>>>> 4975f9d3f866e2b5210e52bd30213364559e0e0f
}))


# Make dataframe
<<<<<<< HEAD
yearMedian <- t(apply(chunckOut$modisArray[,,1], 1, function(x) {
  # tapply(x, format(flsModis_date, "%Y"), median, na.rm = T)
  sapply(split(data.frame(x, joy = as.numeric(format(chunckOut$date, "%j"))), format(chunckOut$date, "%Y")), function(y) y[min(which(y[,1]<0.1)),2])
=======
yearMedian <- t(apply(chunkOut$modisArray[,,1], 1, function(x) {
  # tapply(x, format(flsModis_date, "%Y"), median, na.rm = T)
  sapply(split(data.frame(x, joy = as.numeric(format(chunkOut$date, "%Y"))), format(chunkOut$date, "%Y")), function(y) y[min(which(y[,1]<0.1)),2])
>>>>>>> 4975f9d3f866e2b5210e52bd30213364559e0e0f
}))

yearMedian


medPxl <- apply(chunkOut$modisArray, c(1,3), median, na.rm = T)

tbl <- tibble(date = rep(chunkOut$date, each = dim(chunkOut$modisArray[,,1])[1]),
       snow = c(chunkOut$modisArray[,,1]))

plot(tbl)



<<<<<<< HEAD
pxlQuants <- apply(chunckOut$modisArray, c(2,3), median, na.rm = T)

plot(chunckOut$date, pxlQuants[,1])
=======
pxlQuants <- apply(chunkOut$modisArray, c(2,3), median, na.rm = T)
plot(chunkOut$date, pxlQuants[,1])
>>>>>>> 4975f9d3f866e2b5210e52bd30213364559e0e0f


r0 <- chunkOut$rasterIndex
r0[!is.na(r0[])] <- medPxl[,1]
plot(r0)


#Plot of frequency for NDVI
matplot(chunckOut$date-6*60*60, t(chunckOut$modisArray[,,1]), pch = 16, cex = 1, 
        col = adjustcolor("darkgreen", alpha.f = 0.4), type = "l",
        ylim = c(-0.25, 1), xlim = range(chunckOut$date), xlab = "", ylab = "NDVI")

# Quantiles difference to get amplitude
quants <- t(apply(chunckOut$modisArray[,,1], 1, function(z) {
    diffQuants <- quantile(chunckOut$modisArray[,,1], prob = 0.975, na.rm = T) - quantile(chunckOut$modisArray[,,1], prob = 0.025, na.rm = T)
  tapply(z, format(chunckOut$date, "%Y")
}))

opar <- par(mar = c(4,4,1,4), las = 1)
matplot(chunkOut$date-6*60*60, t(chunkOut$modisArray[,,1]), pch = 16, cex = 1, 
        col = adjustcolor("darkgreen", alpha.f = 0.4), type = "p",
        ylim = c(-0.25, 1), xlim = range(chunkOut$date), xlab = "", ylab = "NDVI")
par(new = T)
matplot(chunkOut$date+6*60*60, t(chunkOut$modisArray[,,2]), pch = 16, cex = 1, 
        col = adjustcolor("cornflowerblue", alpha.f = 0.4), type = "p",
        ylim = c(0,1), xlim = range(chunkOut$date), xlab = "", ylab = "", yaxt  = "n", xaxt = "n")
axis(4)
mtext("NDSI", 4, line = 3, las = 3)
par(opar)

