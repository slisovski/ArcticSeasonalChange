# Load the array and the df
load("/Users/tasos/Documents/array2001_2012.rda")
load("/Users/tasos/Documents/array2001_2012df.rda")


# Load the rasters and their dates
drive         <-"/Users/tasos/Documents/20_rasters/"
flsModis      <- list.files(drive, pattern = "MODIS_ndvi*", recursive = T)   
flsModis_date <- as.POSIXct(sapply(strsplit(flsModis, "_"), function(x) unlist(strsplit(x[3], ".tif"))))


dim(modisArray)

# Calculate the median of NDVI and NDVI for each year
NDVImedian <- t(apply(chunckOut$modisArray[,,1], 1, function(x) {
  chunckOut$modisArray[chunckOut$modisArray[,,1] < 0.05] <- NA # Remove the NDVI values below 0.05
  tapply(x, format(flsModis[2], "%Y"), median, na.rm = T)
}))

NDSImedian <- t(apply(modisArray[,,2], 1, function(x) {
  modisArray[,,2] <- modisArray[,,2] /100 #Convert NDSI values to 0 - 1 range
  tapply(x, format(flsModis_date, "%Y"), median, na.rm = T)
}))


# Make dataframe
yearMedian <- t(apply(modisArray[,,1], 1, function(x) {
  # tapply(x, format(flsModis_date, "%Y"), median, na.rm = T)
  sapply(split(data.frame(x, joy = as.numeric(format(flsModis_date, "%j"))), format(flsModis_date, "%Y")), function(y) y[min(which(y[,1]<0.1)),2])
}))

yearMedian


medPxl <- apply(chunckOut$modisArray, c(1,3), median, na.rm = T)

tbl <- tibble(date = rep(chunckOut$date, each = dim(chunckOut$modisArray[,,1])[1]),
       snow = c(chunckOut$modisArray[,,1]))

plot(tbl)



pxlQuants <- 
  
  apply(chunckOut$modisArray, c(2,3), median, na.rm = T)
plot(chunckOut$date, pxlQuants[,1])


r0 <- chunckOut$rasterIndex
r0[!is.na(r0[])] <- medPxl[,1]
plot(r0)






