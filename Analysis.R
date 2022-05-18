library(tidyverse)


# Load the chunk
load("/Users/tasos/Documents/chunkOut.rda")

# Step 1
# We need to clean up our data by filtering out ndvi values below 0.05 and values that co-exist with ndsi values larger than 0.4
# in order to get the amplitude


amplitude <- t(apply(chunckOut$modisArray, 1,  function(z) {
  chunckOut$modisArray[,,2] <- chunckOut$modisArray[,,2] /100 # convert ndsi values to 0-1
  dat <- ifelse(chunckOut$modisArray[,,1] < 0.05 | chunckOut$modisArray[,,2] > 0.4, chunckOut$modisArray[,,1], NA) # apply the filter
  tapply(dat, format(chunckOut$date, "%Y"), function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T))) # get the amp
}))





