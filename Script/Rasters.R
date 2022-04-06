library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

# Load Driver and Array
drive         <- "/Users/tasos/Documents/rasters_final/"
load("Array2014-2017.rda")

# Get rasters
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*", recursive = T)   
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif")))) ## that may need to be adjusted depending on your file name. This looked OK as well

### template of rasters
rast_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
r0   <- raster(paste0(drive, flsModis[1])); proj4string(r0) <- proj


crds <- r0 %>% coordinates() %>% as.data.frame() %>% st_as_sf(coords = c("y", "y")) %>%
  st_set_crs(rast_proj) %>% st_transform(4326) %>% st_coordinates() %>% as_tibble() %>% 
  rownames_to_column(var = "index") %>% filter(!is.na(r0[])) %>% mutate(index = as.numeric(index))


dim(modisArray)


# Plot of median 
medPxl <- apply(modisArray, c(1,3), median, na.rm = T)
medNDVI <- r0
medNDVI[!is.na(r0[])] <- medPxl[,1]

plot(medNDVI)


# Plot of median minus standard deviation
sdPxl <- apply(modisArray, c(1,3), sd, na.rm = T)
sdNDVI <- r0
sdNDVI[!is.na(r0[])] <- sdPxl[,1]


plot(medNDVI-sdNDVI)






# Create dataframe
meltArray <- reshape2::melt(modisArray) %>% setNames(c("Pixel", "indexDate", "Index", "Value")) %>%
  filter(!is.na(Value)) %>%
  mutate(date = flsModis_date[indexDate],
         year = as.numeric(format(date, "%Y")),
         Value = ifelse(Index == 1 & Value<0.05, NA, Value),
         Value = ifelse(Index == 2, Value / 100, Value))

sumArray <- meltArray %>% group_by(Index, year) %>% summarise(Median = median(Value, na.rm = T), 
                                                              q20 = quantile(Value, probs = 0.2, na.rm = T),
                                                              q80 = quantile(Value, probs = 0.8, na.rm = T))

ggplot(sumArray %>% filter(Index==2), aes(x = year, y = Median)) + 
  geom_point() + geom_errorbar(aes(ymin = q20, ymax = q80), width = .2)


# 1st day of snowmelt each year
snowmelt <- meltArray %>% group_by(year) %>%  
  filter(Index == 2 & Value < 0.3) %>% 
  slice(1)

snowmelt
