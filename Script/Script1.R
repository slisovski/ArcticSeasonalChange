## 1st script

library(rgee)
library(dplyr)
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf); sf::sf_use_s2(FALSE)
library(tidyverse)

## U use downloaded data from the natural earth website
## and include the largest lakes

map <- read_sf("~/Google Drive/My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_land/ne_50m_land.shp") %>%
  st_difference(read_sf("~/Google Drive/My Drive/GeoDat/NaturalEarth/50m_physical/ne_50m_lakes/ne_50m_lakes.shp") %>%
                  mutate(area = st_area(geometry)) %>%
                  filter(area >= quantile(area, probs = 0.9))) %>% st_union()

### create bounding box with many points
bbox   <- st_bbox(c(xmin = -180, ymin = 50, xmax = 180, ymax = 90)) %>% st_as_sfc() %>% st_set_crs(4326) %>%
  st_cast("LINESTRING") %>% st_sample(1000, type = "regular") %>% st_cast("POLYGON")


### intersection and projection
proj   <- "+proj=stere +lat_0=90 +lat_ts=75"
arctis <- map %>% st_intersection(bbox) %>% st_transform(proj)
# plot(arctis, col = "grey90", border = "grey60")

### get biome data
### https://www.gislounge.com/terrestrial-ecoregions-gis-data/
ecoreg   <- read_sf("~/Google Drive/My Drive/GeoDat/Ecoregions2017/Ecoregions2017.shp") %>% 
  filter(BIOME_NAME %in% c("Tundra", "Boreal Forests/Taiga")) %>% 
  group_by(BIOME_NAME) %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup() %>%
  st_intersection(bbox) %>% st_transform(proj)


### Study regions
pols <- c(st_bbox(c(xmin =  126-7.5, ymin = 50, xmax =  126+7.5, ymax = 90)) %>% st_as_sfc(),
          st_bbox(c(xmin = -134-7.5, ymin = 50, xmax = -134+7.5, ymax = 90)) %>% st_as_sfc()) %>% st_set_crs(4326) %>%
  st_cast("LINESTRING") %>% st_sample(2000, type = "regular") %>% st_cast("POLYGON") %>%
  st_transform(proj)


### plot
p1 <-   ggplot() + 
  geom_sf(data = arctis, fill = "grey90", col = "grey50", size = 0.3) + 
  geom_sf(data = pols, fill = NA, col = "grey30", size = 0.4) + 
  geom_sf(data = pols %>% st_intersection(arctis), fill = "grey60", col = NA, size = 0.6) +
  geom_label(data = st_centroid(pols) %>% st_coordinates() %>% as.data.frame(), aes(x=X, y=Y, label = c("A1", "A2"))) +
  scale_y_continuous(breaks = seq(50, 90, by = 10)) +
  theme_bw() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())

prj1 <- glue::glue("+proj=stere +lat_0=90 +lon_0={(st_centroid(pols)[1] %>% st_transform(4326) %>% st_coordinates())[1,1]}")
p2 <- ggplot() +
  geom_sf(data = pols[1] %>% st_intersection(arctis) %>% st_transform(prj1), fill = "grey60", col = NA, size = 0.6) +
  geom_sf(data = ecoreg %>% st_intersection(pols[1] %>% st_intersection(arctis)) %>% st_transform(prj1), 
          mapping = aes(geometry = geometry, fill = BIOME_NAME), col = NA, show.legend = F) +
  scale_fill_manual(values = c("#40A090", "#608080")) +
  labs(tag = "A1") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.3),
        plot.margin = margin(t = 56,
                             b = 56))

prj2 <- glue::glue("+proj=stere +lat_0=90 +lon_0={(st_centroid(pols)[2] %>% st_transform(4326) %>% st_coordinates())[1,1]}")
p3 <- ggplot() +
  geom_sf(data = pols[2] %>% st_intersection(arctis) %>% st_transform(prj2), fill = "grey60", col = NA, size = 0.6) +
  geom_sf(data = ecoreg %>% st_intersection(pols[2] %>% st_intersection(arctis)) %>% st_transform(prj2), 
          mapping = aes(geometry = geometry, fill = BIOME_NAME), col = NA, show.legend = T) +
  scale_fill_manual(values = c("#40A090", "#608080")) +
  labs(tag = "A2") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.3))

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

