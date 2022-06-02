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



allAtOnce <- lapply(1:dim(modisData)[1], function(pxl){
  ndsi <- modisData[,,2] / 100 | ifelse(modisData[,,2] > 0.4, 1, 0) # transform ndsi values to range 0-1 and make them binary
  ndvi <- ifelse(modisData[,,2] > 0.4 | modisData[,,1] < 0.05, NA, modisData[,,1] ) # filter ndvi values by snow
  
  })



  lapply(split(modisData, year(data.dates), function(year){
    
    
    
  }
})
