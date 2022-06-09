library(tidyverse)
library(sp)
library(raster)
library(lubridate)
library(ggplot2)
library(tidyr)
library(abind)
library(MASS)
#------------------------------------------------------------
# Try to create a function for all pixels and all years

# Work in it with the raw data

phenArray <- abind::abind(parallel::mclapply(1:dim(chunckOut$modisArray)[1], function(pxl) {
  
  dat <- suppressMessages(bind_cols(tibble(as.numeric(format(chunckOut$date, "%Y")), 
                          as.numeric(format(chunckOut$date, "%j"))), 
                          chunckOut$modisArray[pxl,,]) %>% setNames(c("year", "doy", "ndvi", "ndsi"))) %>%
         filter(any(c(!is.na(ndsi), !is.na(ndvi)))) %>% mutate(ndsi = ifelse(ndsi>40, 1, 0),
                                         ndvi = ifelse(ndsi==1, NA, ndvi))
  
  ### plot
  # plot(dat$doy, dat$ndvi, pch = 16, type = "o", ylim = c(0,1))
  # par(new = T)
  # plot(dat$doy, dat$ndsi, pch = 15, type = "o", xaxt = "n", yaxt = "n")
  # axis(4)
  
  if(nrow(dat)>0) {
    abind::abind(lapply(group_split(dat, year), function(y) {
      
      #### Example plot
      # y <- group_split(dat, year)[[1]]
      # plot(y$doy, y$ndvi)
      
      ## 1. Amplitude
      amp <- diff(quantile(y$ndvi, probs = c(0.05, 0.975), na.rm = T))
      
      ## 2. NDVI smooth
      if(sum(!is.na(y$ndvi))>5) {
        ndvi.loess = loess(ndvi ~ doy, data = dat)
        dat$prediction.NDVI = predict(ndvi.loess, dat$doy)
        max = dat$doy[which.max(dat$prediction.NDVI)]
      } else max <- NA
      
      ## 3. snowmelt
      ndsi.binomial = glm(ndsi ~ doy, family = "binomial", data = dat)
      dat$prediction.NDSI = predict(ndsi.binomial, newdata = dat, type="response")
      target.probability = 0.5
      NDSI.prob.day = as.numeric(dose.p(ndsi.binomial, p = target.probability))
      
      
      matrix(c(amp, max, max-NDSI.prob.day), nrow = 1)
      
    }), along = 3)
  } else array(dim = c(1, 3, 21))
  
}, mc.cores = parallel::detectCores()), along = 1)


# Median of amplitude across years

ampMed <- apply(phenArray[,1,], 1, median, na.rm = T)
## into raster
raster.df = as.data.frame(chunckOut$rasterIndex, xy = T, na.rm = T)
raster.df$layer = as.numeric(ampMed)

plot(rasterFromXYZ(raster.df))


# Median minus standard deviation

ampSd = apply(phenArray[,1,], 1, sd, na.rm = T)
raster.df$layer = as.numeric(ampMed-ampSd)






