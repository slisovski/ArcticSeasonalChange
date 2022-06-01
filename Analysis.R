library(tidyverse)
library(sp)
library(raster)
library(lubridate)
library(ggplot2)
library(tidyr)


# DATA LOADING
# ------------
# Load the chunk
load("chunkOut.rda")

rasterIndex = chunckOut$rasterIndex
modisData = chunckOut$modisArray
data.dates = chunckOut$date



# DATA PREPARATION
# ----------------

modisData[,,2] = modisData[,,2] / 100

# DATA PROCESSING
# --------------
#
#   a) amplitude of prefiltered NDVI
#
# Step 1
# We need to clean up our data by filtering out ndvi values below 0.05 and values that co-exist with ndsi values larger than 0.4
# in order to get the amplitude of ndvi.
ndvi.filtered = ifelse(modisData[,,2] > 0.4 | modisData[,,1] < 0.05, NA, modisData[,,1] )
# for plotting later we *also* extract the NDVI values which *are* filtered, i.e. the NDVI
# value during snowcover
ndvi.snowcover = ifelse(modisData[,,2] > 0.4 | modisData[,,1] < 0.05, modisData[,,1], NA )
#
# recode the date-vector to its year to be able to aggregate the data by the years
cell.labels.year = format(data.dates, "%Y")
# calculate the amplitude for each pixel 
amplitude = apply(ndvi.filtered, 1, function(pxl.values) {
  tapply(pxl.values, cell.labels.year, function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T))  )
} )

#
#
#   b) binary coded NDSI data
#
#
#    1 -> NDSI >  0.4
#    0 -> NDSI <= 0.4
ndsi.binary = ifelse(modisData[,,2] > 0.4, 1, 0)

#
#
#
# TEST/RESULT VISUALIZATION
# -------------------------
# plot the data of one pixel in one year and compare with amplitude of filtered NDVI:
test.pixel.index = 888
test.year = 2001

# data frame of all the values from that pixel of that year:
indices.df = data.frame(
  NDVI = ndvi.filtered[test.pixel.index,],
  NDVI.snowcover = ndvi.snowcover[test.pixel.index,],
  NDSI = modisData[test.pixel.index,,2],
  NDSI.binary = ndsi.binary[test.pixel.index,],
  data.date = data.dates
) %>% dplyr::filter(year(data.date) == test.year)

# get the calculated amplitude of the selecetd pixel/year
test.amplitude = amplitude[as.character(test.year),test.pixel.index]
# calculate the percentiles based on the selected data
test.percentiles = quantile(indices.df$NDVI, probs=c(0.05, 0.95), na.rm = T)
test.percentiles

# prepare data frame for ggplotting:
plot.data = indices.df  %>%
  pivot_longer(cols = -data.date, names_to="Index")

# plot that:
ggplot(mapping=aes(x=data.date, y=value)) +
  geom_point(aes(color = Index), data = plot.data, size = 1.1, alpha = 0.7) + # all the points
  geom_point( # mark snowcovered NDVI with red x
    data=dplyr::filter(plot.data, Index == "NDVI.snowcover"),
    shape=4, size=1.4, color = "red"
  ) +
  geom_hline(yintercept = test.percentiles, color = "red") + # percentile lines
  scale_color_manual( # adjust legend
    labels = c("NDVI", "NDVI (during snovcover)", "NDSI", "NDSI (binary)"),
    values=c(NDVI = "green", NDVI.snowcover = "darkgreen", NDSI = "blue", NDSI.binary = "purple")) +
  labs(
    title = sprintf("Pixel# %d - %d", test.pixel.index, test.year),
    subtitle = sprintf("Amplitude filtered: %0.3f", test.amplitude)
  ) +
  theme_minimal()

# amplitude <- lapply(chunckOut$modisArray[,,1], function(pxl) {
#   chunckOut$modisArray[,,2] <- chunckOut$modisArray[,,2] /100 | ifelse(chunckOut$modisArray[,,2] > 0.4, 1, 0)# convert ndsi values to range 0-1 and make them binary
#   chunckOut$modisArray[,,1] <- ifelse(chunckOut$modisArray[,,1] < 0.05 | chunckOut$modisArray[,,2] > 0.4, NA, chunckOut$modisArray[,,1]) # apply the filter on ndvi values
#   dat <- chunckOut$modisArray[pxl,,]
#   tapply(dat, format(rep(chunckOut$date, each = dim(chunckOut$modisArray[,,1])[1]), "$Y"), function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T))) # get the amp
# })




