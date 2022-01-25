# installing the libraries
library(sf)
library(data.table)
library(tidyverse)
library(maptiles)
library(cartography)
library(geogrid)
library(tmap)
library(geogrid)

# loading all the shape files
list.files("C://Users//ivkot//Downloads//shape_files")

# the points of interest multipolygons
a_ber_poi_multipolygon <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_pois_a_free_1.shp")

# the points of interest polygons
a_ber_poi_polygon <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_pois_free_1.shp")

# the landuse(parks) polygons
b_ber_landuse_multipolygon <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_landuse_a_free_1.shp")

# the transport polygons
c_ber_transport_polygon <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_transport_free_1.shp")

# the water objects multipolygons
d_ber_water_multipolygons <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_water_a_free_1.shp")

# the Berlin city map
e_ber_map <- read_sf(dsn = "C://Users//ivkot//Downloads//shape_files//gis_osm_places_a_free_1.shp")
berlin_countour <- filter(e_ber_map, fclass == "suburb")
berlin_countour <- berlin_countour[, 5:6]

# A documentation of the layers in this shape file is available here:
# http://download.geofabrik.de/osm-data-in-gis-formats-free.pdf


# cleaning data
# the a_ files
# picking out the right "codes" for items: 21xx, 22xx, 23xx, 25xx, 27xx
a_ber_poi_polygon <- a_ber_poi_polygon %>% filter(((code >= 2100) & (code < 2400))|((code >= 2500) & (code < 2600))|((code >= 2700) & (code < 2800)))
a_ber_poi_multipolygon <- a_ber_poi_multipolygon %>% filter(((code >= 2100) & (code < 2400))|((code >= 2500) & (code < 2600))|((code >= 2700) & (code < 2800)))

# the b_ files
# picking out the right "codes" for items: 7201, 7202
b_ber_landuse_multipolygon <- b_ber_landuse_multipolygon %>% filter((code == 7201) | (code == 7202))

# the c_ files
# picking out the right "codes" for items: 5601, 5602, 5603, 5621, 5622
c_ber_transport_polygon <- c_ber_transport_polygon %>% filter((code == 5601) | (code == 5602) | (code == 5603) | (code == 5621) | (code == 5622))

# the c_ files
# picking out the right "codes" for items: 5601, 5602, 5603, 5621, 5622
d_ber_water_multipolygons <- d_ber_water_multipolygons %>% filter((code == 8200) | (code == 8202))


# 40 HEC cutoff for polygons seems reasonable after testing
plot(a_ber_poi_multipolygon %>%
       filter(as.integer(st_area(a_ber_poi_multipolygon$geometry)) >= 400000) %>% select(geometry))


# testing out how to work with polygons, distances and overlaps
st_distance(a_ber_poi_multipolygon[13354, 5], a_ber_poi_polygon[1,5])
a_ber_poi_polygon[1, ]

st_intersects(a_ber_poi_multipolygon[13354, 5], berlin_countour)
berlin_countour[c(15, 30, 74, 87, 88, 89), ]

ggplot()+
  geom_sf(data = berlin_countour)+
  geom_sf(data = a_ber_poi_multipolygon[13354, 5], color = "red")+
  geom_sf(data = a_ber_poi_polygon[1,5], color = "blue")


