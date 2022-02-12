# Load packages
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)

model <- read.csv("/Users/ivankotik/Documents/DEDA_project/files and graphs/data after modelling.csv")
geocoding <- read.csv("/Users/ivankotik/Documents/DEDA_project/files and graphs/coordinates_osm_v2.csv")

# Getting the shapefiles from previous files
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

# Deleting an empty row
geocoding <- geocoding[-1, ]
# Get the full adress
model$fulladress <- paste(model$regio1, model$geo_plz, model$streetPlain, model$houseNumber)

# Checking dimention compatability
length(model$fulladress)
length(geocoding$query)

# Checking matches
match(model$fulladress, geocoding$query)
geocoding <- rename(geocoding, "fulladress" = "query")

# Deleting a useless column
model$X <- NULL

# Joining the tables
left_join(model, geocoding, by = "fulladress") -> apartments
distinct(apartments) -> apartments

# Checking empty coordinates
length(apartments[(is.na(apartments[, 32])), 29])
# Leaving only correct coordinates
apartments[is.na(apartments[, 32]) == FALSE, ] -> apartments_clean

# Creating the SF object
st_as_sf(apartments_clean, coords = c("lat","lon")) -> apartments_clean_points
berlin  # the CRS in use is WGS 84
st_as_sf(apartments_clean, coords = c("lon", "lat"), crs = "WGS84") -> apartments_clean_points

# Plotting the solutions
{
ggplot()+
  geom_sf(data = (multipolygon %>% filter(group == "park")), color = "green", fill = "lightgreen", alpha = 0.2)+
  geom_sf(data = apartments_clean_points, color = "gold", size = 0.25)
ggplot()+
  geom_sf(data = (multipolygon %>% filter(group == "water")), color = "blue", fill = "lightblue", alpha = 0.2)+
  geom_sf(data = apartments_clean_points, color = "gold", size = 0.25)

ggplot()+
  geom_sf(data = berlin, fill = "NA", color = "white", alpha = 1)+
  geom_sf(data = (multipolygon %>% filter(group == "park")), color = "green", fill = "lightgreen", alpha = 0.2)+
  geom_sf(data = (multipolygon %>% filter(group == "water")), color = "blue", fill = "lightblue", alpha = 0.2)+
  geom_sf(data = apartments_clean_points, color = "gold", size = 0.25)+
  {theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("apartments.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin, fill = "NA", color = "white", alpha = 1)+
  geom_sf(data = multipolygon, aes(color = group, fill = group), alpha = 0.5)+
  geom_sf(data = polygon, aes(color = group), size = 0.2, alpha = 0.5)+
  geom_sf(data = apartments_clean_points, color = "gold", size = 0.25)+
  {theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("apartments3.png", dpi = 320, scale = 1)
}

# Meassuring distance
rm(apartments_clean)
rm(apartments)

# Getting rid of scientific notation
options(scipen=999)

# The distance calculation
apartments_clean_points %>% st_distance((polygon %>% filter(group == "catering"))) %>%
  `^`(-1) %>% apply(MARGIN = 1, sum) -> apartments_clean_points$dist_catering
# there are some Inf's in the data
apartments_clean_points[is.infinite(apartments_clean_points$dist_catering), ]  # 15 is inf
# Trying to solve the inf's
apartments_clean_points %>% filter(fulladress == "Berlin 12047 Weserstr. 204") %>%
  st_distance((polygon %>% filter(group == "catering"))) %>% sub(pattern = 0, replacement = 10) %>%
  as.numeric() %>% sort(decreasing = TRUE)
# The inf's are comming from close proximity (0), we could say that there are 10 meters between the closest objects
apartments_clean_points %>% filter(fulladress == "Berlin 12047 Weserstr. 204") %>%
  st_distance((polygon %>% filter(group == "catering"))) %>% sort(decreasing = TRUE)

st_distance(apartments_clean_points[1, ], polygon[1, ])  # units are m

# TESTING:
apartments_clean_points[1:2, ] %>% st_distance((polygon %>% filter(group == "catering"))) %>%
  sub(pattern = 0, replacement = 10) %>% as.numeric() %>% `^`(-1)

# need to change 0's to 10's
library(units)
apartments_clean_points[13:15, ] %>% st_distance((polygon %>% filter(group == "catering"))) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum)

# Calculating the distance measurment score between each apartment
table(polygon$group)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "catering"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$catering
summary(apartments_clean_points$catering)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "activities"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$activities
summary(apartments_clean_points$activities)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "destinations"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$destinations
summary(apartments_clean_points$destinations)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "entertainment"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$entertainment
summary(apartments_clean_points$entertainment)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "health"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$health
summary(apartments_clean_points$health)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "kids"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$kids
summary(apartments_clean_points$kids)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "shopping"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$shopping
summary(apartments_clean_points$shopping)

apartments_clean_points %>% st_distance((polygon %>% filter(group == "transport"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$transport
summary(apartments_clean_points$transport)

# Normalizing data
apartments_clean_points$catering <- (apartments_clean_points$catering - min(apartments_clean_points$catering))/(max(apartments_clean_points$catering) - min(apartments_clean_points$catering))
apartments_clean_points$activities <- (apartments_clean_points$activities - min(apartments_clean_points$activities))/(max(apartments_clean_points$activities) - min(apartments_clean_points$activities))
apartments_clean_points$destinations <- (apartments_clean_points$destinations - min(apartments_clean_points$destinations))/(max(apartments_clean_points$destinations) - min(apartments_clean_points$destinations))
apartments_clean_points$entertainment <- (apartments_clean_points$entertainment - min(apartments_clean_points$entertainment))/(max(apartments_clean_points$entertainment) - min(apartments_clean_points$entertainment))
apartments_clean_points$health <- (apartments_clean_points$health - min(apartments_clean_points$health))/(max(apartments_clean_points$health) - min(apartments_clean_points$health))
apartments_clean_points$kids <- (apartments_clean_points$kids - min(apartments_clean_points$kids))/(max(apartments_clean_points$kids) - min(apartments_clean_points$kids))
apartments_clean_points$shopping <- (apartments_clean_points$shopping - min(apartments_clean_points$shopping))/(max(apartments_clean_points$shopping) - min(apartments_clean_points$shopping))
apartments_clean_points$transport <- (apartments_clean_points$transport - min(apartments_clean_points$transport))/(max(apartments_clean_points$transport) - min(apartments_clean_points$transport))
# Total score calculation
apartments_clean_points$total_score <- apartments_clean_points$catering+apartments_clean_points$activities+apartments_clean_points$destinations+
  apartments_clean_points$entertainment+apartments_clean_points$health+apartments_clean_points$kids+
  apartments_clean_points$shopping+apartments_clean_points$transport
# Total score normalized
apartments_clean_points$total_score_normalized <- (apartments_clean_points$total_score - min(apartments_clean_points$total_score))/(max(apartments_clean_points$total_score) - min(apartments_clean_points$total_score))

# Area calculations example
ggplot()+
  geom_sf(data = multipolygon)+
  geom_sf(data = st_buffer(apartments_clean_points[1:10,], set_units(2000, m)), color = "red", alpha = 0.5)+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("buffers.png", dpi = 320, scale = 1)


table(multipolygon$group)
seq_len(nrow(apartments_clean_points))

# Setting up id's to later identify correct addresses
apartments_clean_points$id <- seq_len(nrow(apartments_clean_points))

# Calcualte the distance between the multipolygon and points for all the groups except park and water objects
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "activities"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$activities_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "catering"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$catering_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "destinations"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$destinations_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "entertainment"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$entertainment_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "health"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$health_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "kids"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$kids_m
apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "shopping"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$shopping_m

# normalizing
apartments_clean_points$activities_m <- (apartments_clean_points$activities_m - min(apartments_clean_points$activities_m))/
  (max(apartments_clean_points$activities_m) - min(apartments_clean_points$activities_m))
apartments_clean_points$catering_m <- (apartments_clean_points$catering_m - min(apartments_clean_points$catering_m))/
  (max(apartments_clean_points$catering_m) - min(apartments_clean_points$catering_m))
apartments_clean_points$destinations_m <- (apartments_clean_points$destinations_m - min(apartments_clean_points$destinations_m))/
  (max(apartments_clean_points$destinations_m) - min(apartments_clean_points$destinations_m))
apartments_clean_points$entertainment_m <- (apartments_clean_points$entertainment_m - min(apartments_clean_points$entertainment_m))/
  (max(apartments_clean_points$entertainment_m) - min(apartments_clean_points$entertainment_m))
apartments_clean_points$health_m <- (apartments_clean_points$health_m - min(apartments_clean_points$health_m))/
  (max(apartments_clean_points$health_m) - min(apartments_clean_points$health_m))
apartments_clean_points$kids_m <- (apartments_clean_points$kids_m - min(apartments_clean_points$kids_m))/
  (max(apartments_clean_points$kids_m) - min(apartments_clean_points$kids_m))
apartments_clean_points$shopping_m <- (apartments_clean_points$shopping_m - min(apartments_clean_points$shopping_m))/
  (max(apartments_clean_points$shopping_m) - min(apartments_clean_points$shopping_m))

# Creating
apartments_clean_points_narrow <- select(apartments_clean_points, id, geometry)
apartments_clean_points_narrow$buffer <- st_buffer(apartments_clean_points_narrow, set_units(2000, m))


ggplot()+
  geom_sf(data = (multipolygon %>% filter(group == "water")))+
  geom_sf(data = apartments_clean_points_narrow[1:1000, 3])

apartments_clean_points_narrow <- apartments_clean_points_narrow$buffer

# Calculating the intersection
placeholder1 <- st_intersection(apartments_clean_points_narrow[1, ],
                               (multipolygon %>% filter(group == "water")))
for (i in 2:7034){
  placeholder2 <- st_intersection(apartments_clean_points_narrow[i, ],
                               (multipolygon %>% filter(group == "water")))
  placeholder1 <- bind_rows(placeholder1, placeholder2)
  print(Sys.time())
  print(i)
}

write_csv(placeholder1, file = "/Users/ivankotik/Documents/shape_files/intersection_water_apartments.csv")

# Could not execute this code, not enought memory:
# placeholder3 <- st_intersection(apartments_clean_points_narrow[1, ],
#                                (multipolygon %>% filter(group == "park")))
# for (i in 2:7034){
#   placeholder4 <- st_intersection(apartments_clean_points_narrow[i, ],
#                                (multipolygon %>% filter(group == "park")))
#   placeholder3 <- bind_rows(placeholder3, placeholder4)
#   print(Sys.time())
#   print(i)
# }
#
# placeholder3

placeholder$area <- st_area(placeholder)
placeholder <- aggregate(placeholder$area, by = list(id = placeholder$id), FUN = sum)
left_join(apartments_clean_points, placeholder, by = "id")


placeholder <- st_intersection(apartments_clean_points_narrow$buffer, multipolygon %>% filter(group == "activities"))

# Installing the good palettes
   library(jcolors)
display_all_jcolors_contin()

# Plotting the results
ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = total_score_normalized))+
  scale_color_jcolors_contin("pal2")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_total.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = catering))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_catering.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = activities))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_activities.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = destinations))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_destinations.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = entertainment))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_entertainment.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = health))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_health.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = kids))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_kids.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = shopping))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_shopping.png", dpi = 320, scale = 1)

ggplot()+
  geom_sf(data = berlin)+
  geom_sf(data = apartments_clean_points, aes(color = transport))+
  scale_color_jcolors_contin("pal4")+
{theme(
    panel.background = element_rect(fill = "#222222",
                                  colour = "#222222",
                                  size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "white"),
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "#222222"),
    plot.background = element_rect(fill = "#222222"),
    legend.background = element_rect(fill = "#222222"),
    legend.title = element_text(colour = "#cacaca"),
    legend.text = element_text(colour = "#545454")
  )
  }
ggsave("score_transport.png", dpi = 320, scale = 1)

# Working on the weights
summary(apartments_clean_points$total_score)
summary(apartments_clean_points$total_score_normalized)
apartments_clean_points$weights <- apartments_clean_points$total_score_normalized + 0.5

# The landlord premium model
