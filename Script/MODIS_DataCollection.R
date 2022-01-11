# Load packages
library(rgee)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)

ee_Initialize()

# Load the Study Areas and convert them to ee Object
ee_roi <- st_read("Data/StudyAreas/pols.shp") %>% 
          st_geometry() %>% 
          sf_as_ee()


# Get MODIS Dataset 
modis <- ee$ImageCollection("MODIS/006/MOD09GA")

#Filtering
modis_500m <- ee$ImageCollection("MODIS/006/MOD09GA") %>%
              ee$ImageCollection$filterDate("2000-02-24", "2021-12-31") %>%
              ee$ImageCollection$filterBounds(ee_roi) %>%
              ee$ImageCollection$select("sur_refl_b01", "sur_refl_b02","sur_refl_b03", "sur_refl_b04", "sur_refl_b05", "sur_refl_b06", "sur_refl_b07", "QC_500m")



# Quality Control

from <- c(1,3,11,14)
to   <- c(2,3,11,14)
reject <- c("01,10", "1", "1", "1")
qa_bits <- cbind(from, to, reject)
qa_bits
##      from to   reject
## [1,] "1"  "2"  "01,10"  "cloud state"
## [2,] "3"  "3"  "1"      "cloud shadow"
## [3,] "11" "11" "1"      "cloud flag"
## [4,] "14" "14" "1"      "is adjacent to cloud"



