# Load packages
library(sf); sf::sf_use_s2(FALSE)
library(raster)
library(rgee)
library(tidyverse)

# Load the array and the df
load("/Users/tasos/Documents/array2001_2012.rda")
load("/Users/tasos/Documents/array2001_2012df.rda")


# Load the rasters and their dates
drive         <-"/Users/tasos/Documents/20_rasters/"
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*", recursive = T)   
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif"))))


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
  # tapply(x, format(flsModis_date, "%Y"), median, na.rm = T)
  sapply(split(data.frame(x, joy = as.numeric(format(chunckOut$date, "%j"))), format(chunckOut$date, "%Y")), function(y) y[min(which(y[,1]<0.1)),2])
}))

yearMedian


medPxl <- apply(chunckOut$modisArray, c(1,3), median, na.rm = T)

tbl <- tibble(date = rep(chunckOut$date, each = dim(chunckOut$modisArray[,,1])[1]),
       snow = c(chunckOut$modisArray[,,1]))

plot(tbl)



pxlQuants <- apply(chunckOut$modisArray, c(2,3), median, na.rm = T)

plot(chunckOut$date, pxlQuants[,1])


r0 <- chunckOut$rasterIndex
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



