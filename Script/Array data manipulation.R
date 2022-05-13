
# Load packages
library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)
library(mapview)


# Load the chunk output
load("/Users/tasos/Documents/chunkOut.rda")
# flsMod (get dates for NDVI, since it shoudl be the same for NDSI). I changed the drive location to my drive here
drive         <- "G:/My Drive/20_rasters/"
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



##### This line is making problems, since the projectRaster creates a different resolution
##### and thus more crs than the original raster
# crds <- r0 %>% projectRaster(crs = CRS("+proj=longlat")) %>% coordinates()

#### Solution
crds <- indexRast %>% coordinates() %>% as.data.frame() %>% st_as_sf(coords = c("y", "y")) %>%
  mutate(chunck = indexRast[]) %>% as_tibble() %>% 
  rownames_to_column(var = "index") %>% filter(!is.na(chunck)) %>% mutate(index = as.numeric(index))



dim(chunckOut$modisArray)




# Calculate the median of NDVI and NDVI for each year
NDVImedian <- t(apply(chunckOut$modisArray[,,1], 1, function(x) {
  chunckOut$modisArray[chunckOut$modisArray[,,1] < 0.05] <- NA # Remove the NDVI values below 0.05
  tapply(x, format(chunckOut$date, "%Y"), median, na.rm = T)
}))

NDSImedian <- t(apply(chunckOut$modisArray[,,2], 1, function(x) {
  chunckOut$modisArray[,,2] <- chunckOut$modisArray[,,2] /100 #Convert NDSI values to 0 - 1 range
  tapply(x, format(chunckOut$date, "%Y"), median, na.rm = T)
}))


# Make dataframe

yearMedian <- t(apply(chunckOut$modisArray[,,1], 1, function(x) {
  tapply(x, format(chunckOut$date, "%Y"), median, na.rm = T)
  sapply(split(data.frame(x, joy = as.numeric(format(chunckOut$date, "%j"))), format(chunckOut$date, "%Y")), function(y) y[min(which(y[,1]<0.1)),2])
}))

yearMedian


medPxl <- apply(chunckOut$modisArray, c(1,3), median, na.rm = T)

tbl <- tibble(date = rep(chunckOut$date, each = dim(chunckOut$modisArray[,,1])[1]),
       snow = c(chunckOut$modisArray[,,2]) / 100) 

plot(tbl)


pxlQuants <- apply(chunckOut$modisArray, c(2,3), median, na.rm = T)
plot(chunckOut$date, pxlQuants[,1])

r0 <- chunckOut$rasterIndex
r0[!is.na(r0[])] <-medAmp -sdAmp
plot(r0)


# Quantiles difference to get amplitude
quants <- t(apply(chunckOut$modisArray[,,1], 1, function(z) {
  # z <- chunckOut$modisArray[1,,1]
  dat <- ifelse(z<0.05, NA, z)
  tapply(dat, format(chunckOut$date, "%Y"), function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T)))
}))

medAmp <- apply(quants, 1, median, na.rm = T)
sdAmp  <- apply(quants, 1, sd, na.rm = T)


#### Amplitude
## 1) map with median amplitude and sd amplitude (also plot of amplitude and some quantiles across years)
r0 <- chunckOut$rasterIndex
r0[!is.na(r0[])] <- medAmp
plot(r0)
r0 <- as.data.frame(r0, xy = T, na.rm = T)

r1 <- chunckOut$rasterIndex
r1[!is.na(r1[])] <- sdAmp
plot(r1)
r1 <- as.data.frame(r1, xy = T, na.rm = T)


ggplot(r0, aes(x = x, y = y, fill = layer)) + geom_raster() + coord_quickmap()
ggplot(r1, aes(x = x, y = y, fill = layer)) + geom_raster() + coord_quickmap()

# Plot of amplitude 2001
r2 <- chunckOut$rasterIndex
r2[!is.na(r2[])] <- quants[,1]
plot(r2)
r2 <- as.data.frame(r2, xy = T, na.rm = T)

ggplot(r2, aes(x = x, y = y, fill = layer)) + geom_raster() + coord_quickmap()

#95th percentile 
quant95 <- t(apply(chunckOut$modisArray[,,1], 1, function(z) {
  # z <- chunckOut$modisArray[1,,1]
  dat <- ifelse(z<0.05, NA, z)
  tapply(dat, format(chunckOut$date, "%Y"), function(f) quantile(f, probs = 0.95, na.rm = T))
}))


## 2) map of change over years - apply linear model of amplitude over years
t(quants)
diff(t(quants))
diffQuants <- t(diff(t(quants)))




## 3) create a map with change (slope of linear model) and a plot for the sub region with overall trend and confidence intervals of change/slope

# Slope of linear model
df <- data.frame(year = as.numeric(colnames(quants)), amplitude = quants[1,])
lm(amplitude ~ year, data = df)

amplitudeSlopes <- apply(quants, 1, function(pxl) {
  if(all(is.na(pxl))){
    return(NA)
  }
  df <- data.frame(year = as.numeric(colnames(quants)), amplitude = pxl)
  model <- lm(amplitude ~ year, data = df)
  coef(model)[2]
})

plot(amplitudeSlopes)

# Map of change (slope)
r3 <- chunckOut$rasterIndex
r3[!is.na(r3[])] <- amplitudeSlopes
plot(r3)
r3 <- as.data.frame(r3, xy = T, na.rm = T)

ggplot(r3, aes(x = x, y = y, fill = layer)) + geom_raster() + coord_quickmap()

# Get confidence intervals
t.test(amplitudeSlopes)




#Plot of NDVI and NDSI values
opar <- par(mar = c(4,4,1,4), las = 1)
matplot(chunckOut$date-6*60*60, t(chunckOut$modisArray[,,1]), pch = 16, cex = 1, 
        col = adjustcolor("darkgreen", alpha.f = 0.4), type = "p",
        ylim = c(-0.25, 1), xlim = range(chunckOut$date), xlab = "", ylab = "NDVI")
par(new = T)
matplot(chunckOut$date+6*60*60, t(chunckOut$modisArray[,,2]), pch = 16, cex = 1, 
        col = adjustcolor("cornflowerblue", alpha.f = 0.4), type = "p",
        ylim = c(0,1), xlim = range(chunckOut$date), xlab = "", ylab = "", yaxt  = "n", xaxt = "n")
axis(4)
mtext("NDSI", 4, line = 3, las = 3)
par(opar)





