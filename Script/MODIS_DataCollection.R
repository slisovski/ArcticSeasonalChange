# Load packages
library(rgee)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)
library(sp)

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
              ee$ImageCollection$select("sur_ref1_b01", "sur_ref1_b02", "sur_ref1_b03", "sur_ref1_b04", "sur_ref1_b05", "sur_ref1_b06", "sur_ref1_b07")
  






