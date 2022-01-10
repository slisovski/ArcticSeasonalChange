# Load packages
library(rgee)
library(dplyr) ## not nessesary if you load tidyverse
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)

ee_Initialize()

# Load the Study Areas
ee_roi <- st_read("Data/StudyAreas/pols.shp") %>%
  st_geometry() %>%
  sf_as_ee()


# Get MODIS Dataset
surface500m <- ee$ImageCollection("MODIS/006/MOD09GA") %>%
  ee$ImageCollection$filterDate("2000-02-24", "2021-12-31") %>%
  ee$ImageCollection$filterBounds(ee_roi)


