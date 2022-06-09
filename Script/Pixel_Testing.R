library(tidyverse)
library(sp)
library(raster)
library(lubridate)
library(ggplot2)
library(tidyr)
library(abind)
library(MASS)


# DATA LOADING
# ------------
# Load the chunk
load("chunkOut.rda")

rasterIndex = chunckOut$rasterIndex
modisData = chunckOut$modisArray
data.dates = chunckOut$date




# DATA PREPARATION
modisData[,,2] = modisData[,,2] / 100


# DATA PROCESSING

## NDVI
ndvi.filtered = ifelse(modisData[,,2] > 0.4 | modisData[,,1] < 0.05, NA, modisData[,,1] )

ndvi.snowcover = ifelse(modisData[,,2] > 0.4 | modisData[,,1] < 0.05, modisData[,,1], NA )


# recode the date-vector to its year to be able to aggregate the data by the years
cell.labels.year = format(data.dates, "%Y")
# calculate the amplitude for each pixel (the "apply" function with MARGIN=1 )
# by each year ( the "tapply()" call within )
amplitude = apply(ndvi.filtered, MARGIN=1, FUN=function(pxl.values) {
  tapply(pxl.values, cell.labels.year, function(f) diff(quantile(f, probs = c(0.05, 0.95), na.rm = T))  )
} )


## NDSI
ndsi.binary = ifelse(modisData[,,2] > 0.4, 1, 0)

# Test 
# plot the data of one pixel in one year and compare with amplitude of filtered NDVI:
test.pixel.index= 888
test.year = 2013

data.label = sprintf("Pixel #%d of %d", test.pixel.index, test.year)

# data frame of all the values from that pixel of that year:
testdata.df = data.frame(
  NDVI = ndvi.filtered[test.pixel.index,],
  NDVI.snowcover = ndvi.snowcover[test.pixel.index,],
  NDSI = modisData[test.pixel.index,,2],
  NDSI.binary = ndsi.binary[test.pixel.index,],
  data.date = data.dates
) %>% dplyr::filter(year(data.date) == test.year)





# get the calculated amplitude of the selected pixel/year
test.amplitude = amplitude[as.character(test.year),test.pixel.index]
# calculate the percentiles based on the selected data
test.percentiles = quantile(testdata.df$NDVI, probs=c(0.05, 0.95), na.rm = T)

# prepare data frame for ggplotting:
plot.data = testdata.df  %>%
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
    values=c(NDVI = "green", NDVI.snowcover = "darkgreen", NDSI = "blue", NDSI.binary = "purple")
  ) +
  labs(
    title = sprintf("Pixel# %d - %d", test.pixel.index, test.year),
    subtitle = sprintf("Amplitude filtered: %0.3f", test.amplitude)
  ) +
  theme_minimal()



## Loess/Binomial Modelling
modelling.df = testdata.df[,c(1,4,5)]
modelling.df$day = yday(modelling.df$data.date)
# cutoff NDSI after 15.08. (day 232)
modelling.df$NDSI.binary[modelling.df$day > 232] = NA




### NDVI with Loess
ndvi.loess = loess(NDVI ~ day, data = modelling.df)
modelling.df$prediction.NDVI = predict(ndvi.loess, modelling.df$day)



max.modelled.NDVI.day = modelling.df$day[which.max(modelling.df$prediction.NDVI)]




modelling.df %>% ggplot(aes(x=day)) +
  geom_point(aes(y=NDVI)) + geom_line(aes(y=prediction.NDVI)) +
  geom_vline(xintercept = max.modelled.NDVI.day, color = "red") +
  lims(x=c(0,365), y=c(0.0,NA)) + labs(title="Filtered NDVI", subtitle=data.label  )



### NDSI binomial model
ndsi.binomial = glm(NDSI.binary ~ day, family = "binomial", data = modelling.df)
modelling.df$prediction.NDSI = predict(ndsi.binomial, newdata = modelling.df, type="response")




# to find predictor values where this model predicts certain probabilities we use the library "MASS::does.p()"
library(MASS)
target.probability = 0.4
NDSI.prob.day = as.numeric(dose.p(ndsi.binomial, p = target.probability))




modelling.df %>% ggplot(aes(x=day)) +
  geom_point(aes(y=NDSI.binary)) + geom_line(aes(y=prediction.NDSI)) +
  geom_vline(xintercept=NDSI.prob.day, color = "red") +
  lims(x=c(0,365), y=c(0.0,NA))


#Putting it all together

greenup.period = max.modelled.NDVI.day - NDSI.prob.day

modelling.df %>% ggplot(aes(x=day)) +
  geom_point(aes(y=NDSI.binary), color = "blue", alpha = 0.3) + geom_line(aes(y=prediction.NDSI), color = "blue", alpha = 0.3) +
  geom_vline(xintercept=NDSI.prob.day, color = "blue", size=0.3) +
  geom_point(aes(y=NDVI), color = "darkgreen", alpha = 0.3) + geom_line(aes(y=prediction.NDVI), color = "darkgreen", alpha = 0.3) +
  geom_vline(xintercept=max.modelled.NDVI.day, color = "limegreen", size=0.3) +
  annotate("segment", x=NDSI.prob.day, xend = max.modelled.NDVI.day, y =0.8, yend=0.8, color = "red" ) +
  annotate("text", x=mean(c(NDSI.prob.day,max.modelled.NDVI.day)), y=0.85, label=sprintf("greenup: %.2f", greenup.period), hjust="center",color="darkred" ) +
  lims(x=c(0,365), y=c(0.0,NA))