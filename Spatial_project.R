# installing the libraries
library(sf)
library(data.table)
library(tidyverse)
library(maptiles)
library(cartography)
library(geogrid)
library(tmap)
library(geogrid)
# options(device = "windows") # for pycharm 2021.3.1

# PART 1
# importing OSM data from daten.berlin.de
# the points of interest in multipolygon format
berlin_mp <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_a_free_1.shp")
# the points of interest in point format
berlin_po <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_free_1.shp")
# ------------------------------------------------------------------------------------------------
# => these are the main dataframes that will give us the points of interest for further evaluation
# ------------------------------------------------------------------------------------------------


# PART 2
# picking out the classes to be used
table(berlin_mp$fclass)
table(berlin_po$fclass)
classes_used <- list(food_and_drinks = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant"),
                   shops_consumables = c("butcher", "convenience", "department_store", "kiosk", "market_place", "supermarket"),
                   entertaining = c("arts_centre", "cinema", "community_centre", "library", "nightclub", "theatre", "zoo"),
                   other_shops = c("bank", "beauty_shop", "bicycle_shop", "bookshop", "clothes", "florist", "hairdresser", "mall", "shoe_shop"),
                   landmarks = c("artwork", "attraction", "fountain", "monument", "observation_tower", "tower"),
                   kids = c("kindergarten", "playground"),
                   parks = "park",
                   pets = c("dog_park", "veterinary"),
                   sport = c("pitch", "sports_centre", "stadium", "swimming_pool", "track"))

classes_vector <- c("food_and_drinks", "shops_consumables", "entertaining", "other_shops",
                    "landmarks", "kids", "parks", "pets", "sport")
# ------------------------------------------------------------------------------------------------
# => these are the all the categories that can have a significant influence on the borough rating
# ------------------------------------------------------------------------------------------------


# PART 3
# checking points and polygins for uniqueness (to exclude overlaps)
table(as.data.table(table(berlin_mp$osm_id))[, 2])
# => 18091 x 1 times present, 76 x 2 times present, 1 x 3 times present // 0.42 % duplicates in id's

table(as.data.table(table(berlin_po$osm_id))[, 2])
# 75434 x 1 times present, 211 x 2 times present, 1 x 3 times present // 0.28% duplicates in id's

table(data.table(table(rbind(berlin_po, berlin_mp)$osm_id))[, 2])
# 93525 x 1 times present, 287 x 2 times present, 2 x 3 times present // 0.31% duplicates
# ------------------------------------------------------------------------------------------------
# => no reason to believe that the data is overlapped or duplicated in any significant way
# ------------------------------------------------------------------------------------------------


# PART 4
# filtering the multipolygon and point data
classes_used <- data.table(fclass = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant",
                                      "butcher", "convenience", "department_store", "kiosk", "market_place", "supermarket",
                                      "arts_centre", "cinema", "community_centre", "library", "nightclub", "theatre", "zoo",
                                      "bank", "beauty_shop", "bicycle_shop", "bookshop", "clothes", "florist", "hairdresser", "mall", "shoe_shop",
                                      "artwork", "attraction", "fountain", "monument", "observation_tower", "tower",
                                      "kindergarten", "playground",
                                      "parks",
                                      "dog_park", "veterinary",
                                      "pitch", "sports_centre", "stadium", "swimming_pool", "track"),
                         category = c(rep("food_and_drinks", times = 8),
                                      rep("shops_consumables", times = 6),
                                      rep("entertaining", times = 7),
                                      rep("other_shops", times = 9),
                                      rep("landmarks", times = 6),
                                      rep("kids", times = 2),
                                      rep("park", times = 1),
                                      rep("pets", times = 2),
                                      rep("sports", times = 5)
                                    ),
                         number = rep(1, times = 46))

# Adding the categories to the source data and filtering out what is not useful
# berlin_mp <- berlin_mp[classes_used, on = .(fclass == V1)]
# berlin_po <- berlin_po[classes_used, on = .(fclass == V1)]

# Restructuring the data, the atempt above did not work due to data.table conflict
berlin_mp_filtered <- inner_join(berlin_mp, classes_used, by = "fclass")
berlin_po_filtered <- inner_join(berlin_po, classes_used, by = "fclass")
# ------------------------------------------------------------------------------------------------
# => now the points of interest are filtered out and we are left with the needed points/polygons
# ------------------------------------------------------------------------------------------------


# Get the OSMmap [depreciated]
# tiles <- get_tiles(x = berlin_po_filtered, provider = "OpenStreetMap")
# tilesLayer(tiles)

# two working plots
plot(st_geometry(berlin_mp_filtered))
plot(st_geometry(filter(berlin_po_filtered, category == "kids")))


# PART 5 (skippable?)
# regex to extract extracting the coordinates from the sf file if needed
data.frame(x = as.numeric(str_extract(as.character(berlin_po_filtered$geometry), "\\d.\\.\\d.*(?=,)")),
           y = as.numeric(str_extract(as.character(berlin_po_filtered$geometry), "\\d*.\\d*(?=\\))")))


# PART 6
# importing and checking the countor map
berlin_countour <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_places_a_free_1.shp")
berlin_countour <- filter(berlin_countour, fclass == "suburb")
berlin_countour <- berlin_countour[, 5:6]
plot(berlin_countour)
ggplot(berlin_countour)+
  geom_sf()
# ------------------------------------------------------------------------------------------------
# => this is the main division of the city by area used further
# ------------------------------------------------------------------------------------------------


# PART 7
# making a hex-fill based on data and map
plot_hex_map <- function(i) {
ggplot() +
  geom_hex(data = filter(berlin_po_filtered, category == classes_vector[i]),
           mapping = aes(x = as.numeric(str_extract(as.character(filter(berlin_po_filtered, category == classes_vector[i])$geometry), "\\d.\\.\\d.*(?=,)")),
                         y = as.numeric(str_extract(as.character(filter(berlin_po_filtered, category == classes_vector[i])$geometry), "\\d*.\\d*(?=\\))"))),
           bins = 50) +
  scale_fill_viridis_c() +
  geom_sf(data = berlin_countour, fill = "transparent", color = "black")+
  labs(title = classes_vector[i])
}

# plots with various categories
plot_hex_map(1)
plot_hex_map(2)
plot_hex_map(3)
plot_hex_map(4)
plot_hex_map(5)
plot_hex_map(6)
plot_hex_map(7)
plot_hex_map(8)
plot_hex_map(9)


# plot with borough names
ggplot()+
  geom_sf(data = berlin_countour )+
  geom_sf_text(data = berlin_countour, aes(label = name), colour = "black", size = 2.5)+
  geom_hex(data = filter(berlin_po_filtered, category == classes_vector[1]),
           mapping = aes(x = as.numeric(str_extract(as.character(filter(berlin_po_filtered, category == classes_vector[1])$geometry), "\\d.\\.\\d.*(?=,)")),
                         y = as.numeric(str_extract(as.character(filter(berlin_po_filtered, category == classes_vector[1])$geometry), "\\d*.\\d*(?=\\))"))),
           bins = 50, alpha = 0.5)+
  scale_fill_viridis_c()


# PART 8
# geogrid plot for further use, will be used only for story telling
# after testing different seeds 2 seems to be the most good looking
newgrid <- calculate_grid(shape = berlin_countour, grid_type = "hexagonal", seed = 2)
resulthex <- assign_polygons(berlin_countour, newgrid)

# fixing names to check how it fits
berlin_countour$short_name <- str_extract(berlin_countour$name, "[[:upper:]].{2,5}")

# the hex plot
tm_shape(resulthex) +
  tm_polygons() +
  tm_text("short_name")

# ggolot map, multipolygons are red and points are green
ggplot() +
  geom_sf(data = berlin_countour) +
  geom_sf_text(data = berlin_countour, aes(label = name), colour = "black", size = 2.5) +
  scale_fill_viridis_c() +
  geom_sf(data = berlin_mp_filtered, color = "#cc1c3d", fill = NA) +
  geom_sf(data = berlin_po_filtered, color = "#499c54", size = 0.1)

# multipolygons must be inspected whether they can be simplified to points
# creating the centers of the multipolygons for analysis
st_point_on_surface(berlin_mp_filtered$geometry)

# checking multipolygon sizes
options(scipen=999)
polysize <- st_area(berlin_mp_filtered$geometry)

length(polysize)
sort(polysize, FALSE)
# all multipolygons
summary(sort(polysize, TRUE)[24:11828])
# only greater than 1 hectar
summary(polysize[as.numeric(polysize) > 100*100])
# only 58 are greater than 5 hectars
length(polysize[as.numeric(polysize) > 100*100*5])


# here you can see the biggest multipolygons in the dataset
test <- berlin_mp_filtered
test$area <- as.numeric(polysize)
ggplot() +
  geom_sf(data = arrange(test, desc(area))[1:20,]) +
  geom_sf(data = berlin_countour, color = "#cc1c3d", fill = NA)
rm(test)

# simplifying down polygons to points
berlin_mp_filtered$geometry <- st_centroid(berlin_mp_filtered$geometry)

# binding the now two point dataframes
berlin_poi <- rbind(berlin_po_filtered, berlin_mp_filtered)

# the nice plot of all the points of interest by categories
ggplot() +
  geom_sf(data = berlin_poi, size = 0.1, aes(color = category)) +
  geom_sf(data = berlin_countour, color = "#cc1c3d", fill = NA, alpha = 0.5, size = 0.5)

# calculating the number of points of interest that are contained in the polygons
lengths(st_intersects(berlin_countour[19,], filter(berlin_poi, category == "food_and_drinks")))
lengths(st_intersects(berlin_countour[19,], berlin_poi))

berlin_countour <- arrange(berlin_countour, name)

# the category matrix
counter <-
data.frame(name = berlin_countour$name,
           all = lengths(st_intersects(berlin_countour, berlin_poi)),
          food_and_drinks = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "food_and_drinks"))),
          shops_consumables = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "shops_consumables"))),
          entertaining = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "entertaining"))),
          other_shops = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "other_shops"))),
          landmarks = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "landmarks"))),
          kids = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "kids"))),
          parks = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "parks"))),
          pets = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "pets"))),
          sport = lengths(st_intersects(berlin_countour, filter(berlin_poi, category == "sport"))))


# Hexplot with the finalized data
resulthex <- arrange(resulthex, name)
resulthex2 <- counter %>% arrange(name) %>% select(all) %>% bind_cols(resulthex, .)

tm_shape(resulthex2) +
  tm_polygons(col = "all", midpoint = 1000, pal = c("#8db3be", "#499c54")) +
  tm_text(text = "short_name", size = 1, col = "white", ymod = 0.25) +
tm_shape(resulthex2) +
  tm_text(text = "all", size = 1, col = "white", ymod = -0.25)


# [WORK IN PROGRESS]