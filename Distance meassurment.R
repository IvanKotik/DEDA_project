# Load packages
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)


model <- read.csv("/Users/ivankotik/Documents/DEDA_project/miscellaneous/data after modelling.csv")
geocoding <- read.csv("/Users/ivankotik/Documents/DEDA_project/miscellaneous/coordinates_osm_v2.csv")

# Getting the shapefiles
{
# for mac
a_ber_poi_multipolygon <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_pois_a_free_1.shp")
a_ber_poi_polygon <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_pois_free_1.shp")
b_ber_landuse_multipolygon <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_landuse_a_free_1.shp")
c_ber_transport_polygon <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_transport_free_1.shp")
d_ber_water_multipolygons <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_water_a_free_1.shp")
e_ber_map <- read_sf(dsn = "/Users/ivankotik/Documents/shape_files/gis_osm_places_a_free_1.shp")
berlin_countour <- filter(e_ber_map, fclass == "suburb")
berlin_countour <- berlin_countour[, 5:6]
berlin_countour <- arrange(berlin_countour, name)
rm(e_ber_map)

# A documentation of the layers in this shape file is available here:
# http://download.geofabrik.de/osm-data-in-gis-formats-free.pdf


# cleaning data
# the a_ files
# picking out the right "codes" for items: 21xx, 22xx, 23xx, 25xx, 27(2+)x
a_ber_poi_polygon <- a_ber_poi_polygon %>% filter(((code >= 2100) & (code < 2400))|((code >= 2500) & (code < 2600))|((code >= 2720) & (code < 2800)))
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


# subgrouping the df's
# poi polygon
a_ber_poi_polygon$group <- NA
a_ber_poi_polygon[((a_ber_poi_polygon$code == 2201) | (a_ber_poi_polygon$code == 2202) | (a_ber_poi_polygon$code == 2203)), 6] <- "entertainment"
a_ber_poi_polygon[(a_ber_poi_polygon$code == 2204), 6] <- "park"
a_ber_poi_polygon[(a_ber_poi_polygon$code == 2205), 6] <- "kids"
a_ber_poi_polygon[((a_ber_poi_polygon$code > 2205) & (a_ber_poi_polygon$code < 2300)), 6] <- "activities"
a_ber_poi_polygon[((a_ber_poi_polygon$code >= 2100) & (a_ber_poi_polygon$code < 2200)), 6] <- "health"
a_ber_poi_polygon[((a_ber_poi_polygon$code >= 2300) & (a_ber_poi_polygon$code < 2400)), 6] <- "catering"
a_ber_poi_polygon[((a_ber_poi_polygon$code >= 2500) & (a_ber_poi_polygon$code < 2600)), 6] <- "shopping"
a_ber_poi_polygon[((a_ber_poi_polygon$code >= 2700) & (a_ber_poi_polygon$code < 2800)), 6] <- "destinations"
table(a_ber_poi_polygon$group)

# poi multipolygon
a_ber_poi_multipolygon$group <- NA
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code == 2201) | (a_ber_poi_multipolygon$code == 2202) | (a_ber_poi_multipolygon$code == 2203)), 6] <- "entertainment"
a_ber_poi_multipolygon[(a_ber_poi_multipolygon$code == 2204), 6] <- "park"
a_ber_poi_multipolygon[(a_ber_poi_multipolygon$code == 2205), 6] <- "kids"
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code > 2205) & (a_ber_poi_multipolygon$code < 2300)), 6] <- "activities"
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code >= 2100) & (a_ber_poi_multipolygon$code < 2200)), 6] <- "health"
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code >= 2300) & (a_ber_poi_multipolygon$code < 2400)), 6] <- "catering"
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code >= 2500) & (a_ber_poi_multipolygon$code < 2600)), 6] <- "shopping"
a_ber_poi_multipolygon[((a_ber_poi_multipolygon$code >= 2700) & (a_ber_poi_multipolygon$code < 2800)), 6] <- "destinations"
table(a_ber_poi_multipolygon$group)

# landuse multipolygons
table(b_ber_landuse_multipolygon$fclass)
b_ber_landuse_multipolygon$group <- "park"

# transport polygons
table(c_ber_transport_polygon$fclass)
c_ber_transport_polygon$group <- "transport"

# water multipolygons
table(d_ber_water_multipolygons$fclass)
d_ber_water_multipolygons$group <- "water"


# creating an empty dataframe
berlin_counter <- data.frame(matrix(ncol = 0, nrow = nrow(berlin_countour)))
berlin_counter$bezirk <- berlin_countour$name

# checking the groups and assigning the count-value
# poi multipolygon counts (parks by area, rest by intersection)
table(a_ber_poi_multipolygon$group)
berlin_counter$activities <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "activities")), length)
berlin_counter$catering <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "catering")), length)
berlin_counter$destinations <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "destinations")), length)
berlin_counter$entertainment <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "entertainment")), length)
berlin_counter$health <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "health")), length)
berlin_counter$kids <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "kids")), length)
test <- data.frame(bezirk = st_intersection(filter(a_ber_poi_multipolygon, group == "park"), berlin_countour)$name.1,
                   area = st_area(st_intersection(filter(a_ber_poi_multipolygon, group == "park"), berlin_countour)))
  test2 <- aggregate(test$area, by = list(test$bezirk), FUN = sum)
  test2 <- rename(test2, bezirk = Group.1)
  berlin_counter <- left_join(berlin_counter, test2, by = "bezirk")
  berlin_counter <- rename(berlin_counter, park = x)
berlin_counter$shopping <- sapply(st_intersects(berlin_countour, filter(a_ber_poi_multipolygon, group == "shopping")), length)


# poi polygon count (done)
table(a_ber_poi_polygon$group)
berlin_counter$activities <- berlin_counter$activities + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "activities")), length)
berlin_counter$catering <- berlin_counter$catering + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "catering")), length)
berlin_counter$destinations <- berlin_counter$destinations + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "destinations")), length)
berlin_counter$entertainment <- berlin_counter$entertainment + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "entertainment")), length)
berlin_counter$health <- berlin_counter$health + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "health")), length)
berlin_counter$kids <- berlin_counter$kids + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "kids")), length)
berlin_counter$shopping <- berlin_counter$shopping + sapply(st_intersects(berlin_countour, filter(a_ber_poi_polygon, group == "shopping")), length)

# adding up more parks from landuse polygons by intersection (done)
table(b_ber_landuse_multipolygon$group)
test <- data.frame(bezirk = st_intersection(filter(b_ber_landuse_multipolygon, group == "park"), berlin_countour)$name.1,
                   area = st_area(st_intersection(filter(b_ber_landuse_multipolygon, group == "park"), berlin_countour)))
  test2 <- aggregate(test$area, by = list(test$bezirk), FUN = sum)
  test2 <- rename(test2, bezirk = Group.1)
  berlin_counter <- left_join(berlin_counter, test2, by = "bezirk")
  berlin_counter$park <- berlin_counter$park + berlin_counter$x
  berlin_counter$x <- NULL

# transport polygon count (done)
table(c_ber_transport_polygon$group)
berlin_counter$transport <- sapply(st_intersects(berlin_countour, filter(c_ber_transport_polygon, group == "transport")), length)

# water multipolygon count (done)
table(d_ber_water_multipolygons$group)
test <- data.frame(bezirk = st_intersection(d_ber_water_multipolygons, berlin_countour)$name.1,
                   area = st_area(st_intersection(d_ber_water_multipolygons, berlin_countour)))
test2 <- aggregate(test$area, by = list(test$bezirk), FUN = sum)
test2 <- rename(test2, bezirk = Group.1)
berlin_counter <- left_join(berlin_counter, test2, by = "bezirk")
berlin_counter <- rename(berlin_counter, water = x)
head(berlin_counter)
rm(test)
rm(test2)
# counter done

# combining the one file
x_berlin <- left_join(rename(berlin_countour, bezirk = name), berlin_counter, by = "bezirk")

x_distance_polygon <- bind_rows(a_ber_poi_polygon, c_ber_transport_polygon)
x_distance_multipolygon <- bind_rows(a_ber_poi_multipolygon, b_ber_landuse_multipolygon, d_ber_water_multipolygons)
rm(a_ber_poi_multipolygon, a_ber_poi_polygon, b_ber_landuse_multipolygon, berlin_countour, berlin_counter,
   c_ber_transport_polygon, d_ber_water_multipolygons)
berlin <- x_berlin
rm(x_berlin)
multipolygon <- x_distance_multipolygon
rm(x_distance_multipolygon)
polygon <- x_distance_polygon
rm(x_distance_polygon)
}

