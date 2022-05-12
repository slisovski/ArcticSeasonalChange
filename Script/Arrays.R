
# Load packages
library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

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

modisArray <- array(dim = c(nrow(crds), nrow(flsModis), 2)) ## array[pixels, dates, c(ndsi, ndvi)]

for(i in 1:nrow(flsModis)) {
  
  ndvi <- raster(paste0(drive, flsModis$path[i]))
  ndsi <- raster(gsub("MODIS_ndvi", "MODIS_ndsi", paste0(drive, flsModis$path[i])))
  
  modisArray[,i,1] <- ifelse(ndvi[][crds$index] < -1000, NA, ndvi[][crds$index]) ## at this stage you can remove -9999 to NAs
  modisArray[,i,2] <- ifelse(ndsi[][crds$index] < -1000, NA, ndsi[][crds$index])
  
}

chunckOut <- list(rasterIndex = indexRast,
                  date        = flsModis$date,
                  modisArray  = modisArray)

save(chunckOut, file = "C:/Users/tasos/Desktop/Master Thesis Project!/chunkOut.rda")




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

## To use ggplot we need to convert the array into a dataframe

meltArray <- reshape2::melt(chunckOut$modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)
meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value))

#Plot

plotArray <- ggplot(meltArray, aes(x = DOY, y = Value, color = Index)) + geom_point() +
# Custom the Y scales:
scale_y_continuous(
  
  # Features of the first axis
  name = "NDSI", 
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(trans = ~.* 1 , name = "NDVI")
)

# Add palette and theme
library(hrbrthemes)
plotArray + scale_color_manual(breaks = c("NDSI", "NDVI"),values = c("blue", "green")) + theme_ipsum()



## Replace negative NDVI values with NA (NDSI values can not go below 0, so we can just set all negative values to NA)

meltArray <- reshape2::melt(chunckOut$modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)
meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value)) %>%
  mutate(Value = ifelse(Value < 0.05, NA, Value))

meltArray$DOY <- as.numeric(meltArray$DOY)

# Time Series Plot                                 
plotArray <- ggplot(meltArray, aes(x = DOY, y = Value, color = Index)) + geom_point() +
  # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "NDSI",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~.* 1 , name = "NDVI")
  )


library(hrbrthemes)
plotArray + scale_color_manual(breaks = c("NDSI", "NDVI"),values = c("blue", "green")) + theme_ipsum()




## Get the first day of the year when NDSI falls below 0.3
meltArray <- reshape2::melt(chunckOut$modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)
meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value)) %>%
  group_split(Index)

snowmelt <- meltArray[[1]] %>%  
  filter(Value < 0.4) %>% 
  slice(1)

snowmelt




## Get the 90th percentile values of Indices
meltArray <- reshape2::melt(modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)
meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value)) %>%
  group_by(Index) %>%
  summarise(percent90 = quantile(Value, probs =0.9, na.rm = T))
meltArray


# Get statistics of Indices
meltArray <- reshape2::melt(modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)
meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value)) %>%
  group_split(Index)

# e.g. NDSI statistics
library(doBy)
summaryBy(Value ~ Index, data = meltArray[[1]], 
          FUN = list(mean, max, min, median, sd), na.rm = T)







