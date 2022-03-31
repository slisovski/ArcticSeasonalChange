library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

# Load Driver and Array
drive         <- "/Users/tasos/Documents/rasters_final/"
load("Array2014-2017.rda")
## Simeon drive
# drive         <- "~/Google Drive/My Drive/rasters_final/"
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*", recursive = T)   
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif")))) ## that may need to be adjusted depending on your file name. This looked OK as well

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
r0   <- raster(paste0(drive, flsModis[1])); proj4string(r0) <- proj

##### This line is making problems, since the projectRaster creates a different resolution
##### and thus more crs than the original raster
# crds <- r0 %>% projectRaster(crs = CRS("+proj=longlat")) %>% coordinates()

#### Solution
crds <- r0 %>% coordinates() %>% as.data.frame() %>% st_as_sf(coords = c("y", "y")) %>%
  st_set_crs(rast_proj) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble() %>% 
  rownames_to_column(var = "index") %>% filter(!is.na(r0[])) %>% mutate(index = as.numeric(index))


dim(modisArray)


# Plot of median 
medPxl <- apply(modisArray, c(1,3), median, na.rm = T)
medNDVI <- r0
medNDVI[!is.na(r0[])] <- medPxl[,1]
plot(medNDVI)


# Plot of mean
meanPxl <- apply(modisArray, c(1,3), mean, na.rm = T)
meanNDVI <- r0
medNDVI[!is.na(r0[])] <- meanPxl[,1]
plot(meanNDVI)


# Create dataframe
meltArray <- reshape2::melt(modisArray)
colnames(meltArray) <- c("Pixel", "DOY", "Index", "Value")
meltArray$Index <- as.character(meltArray$Index)

meltArray <- meltArray %>% mutate(Index = recode(Index, '1' = 'NDVI', '2' = 'NDSI'),
                                  Value = ifelse(Index == 'NDSI', Value / 100, Value),
                                  Year  = case_when(DOY <= 365 ~ "2014",
                                                    DOY <= 730 ~ "2015",
                                                    DOY <= 1095 ~ "2016",
                                                    DOY >= 1096 ~ "2017")) %>%
                           group_split(Index)

Median <- median(meltArray$Value, na.rm = T)
sd <- sd(meltArray$Value, na.rm = T)

ggplot(meltArray[[1]], aes(x = Year, y = Median)) + 
  geom_point() + geom_errorbar(aes(ymin = Median-sd, ymax = Median+sd), width = .2)
