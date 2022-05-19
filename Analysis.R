library(tidyverse)


# Load the chunk
load("/Users/tasos/Documents/chunkOut.rda")

# Step 1
# We need to clean up our data by filtering out ndvi values below 0.05 and values that co-exist with ndsi values larger than 0.4
# in order to get the amplitude of ndvi.


amplitude <- lapply(chunckOut$modisArray[,,1], function(pxl) {
  chunckOut$modisArray[,,2] <- chunckOut$modisArray[,,2] /100 | ifelse(chunckOut$modisArray[,,2] > 0.4, 1, 0)# convert ndsi values to range 0-1 and make them binary
  chunckOut$modisArray[,,1] <- ifelse(chunckOut$modisArray[,,1] < 0.05 | chunckOut$modisArray[,,2] > 0.4, NA, chunckOut$modisArray[,,1]) # apply the filter on ndvi values
  dat <- chunckOut$modisArray[pxl,,]
  tapply(dat, format(rep(chunckOut$date, each = dim(chunckOut$modisArray[,,1])[1]), "$Y"), function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T))) # get the amp
})




