# initiating the packages
install.packages("tidyverse")
library(tidyverse)
install.packages("data.table")
library(data.table)
install.packages("ggmap")
library(ggmap)
install.packages("sf")
library(sf)
install.packages("cartography")
library(cartography)
install.packages("osmdata")
library(osmdata)
install.packages("maptiles")
library(maptiles)
library(ggplot2)
install.packages("hexbin")
library(hexbin)
library(RColorBrewer)
options(device = "windows") # for buggy build

# importing OSM data from daten.berlin.de
berlin_mp <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_a_free_1.shp")
berlin_po <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_free_1.shp")

# picking out the classes to be used
table(berlin_mp$fclass)
classes_mp <- list(food_and_drinks = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant"),
                   shops_consumables = c("butcher", "convenience", "department_store", "kiosk", "market_place", "supermarket"),
                   entertaining = c("arts_centre", "cinema", "community_centre", "library", "nightclub", "theatre", "zoo"),
                   other_shops = c("bank", "beauty_shop", "bicycle_shop", "bookshop", "clothes", "florist", "hairdresser", "mall", "shoe_shop"),
                   landmarks = c("artwork", "attraction", "fountain", "monument", "observation_tower", "tower"),
                   kids = c("kindergarten", "playground"),
                   parks = "park",
                   pets = c("dog_park", "veterinary"),
                   sport = c("pitch", "sports_centre", "stadium", "swimming_pool", "track"))

table(berlin_po$fclass)

classes_po <- list(food_and_drinks = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant"),
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

a <- as.data.table(table(berlin_mp$osm_id))
table(a[,2])
# 18091 x 1 times present, 76 x 2 times present, 1 x 3 times present // 0.42 % duplicates in id's
rm(a)

a <- as.data.table(table(berlin_po$osm_id))
table(a[,2])
# 75434 x 1 times present, 211 x 2 times present, 1 x 3 times present // 0.28% duplicates in id's
rm(a)

a <- as.data.table(table(rbind(berlin_mp[, 1], berlin_po[, 1])))
table(a[,2])
# 93525 x 1 times present, 287 x 2 times present, 2 x 3 times present // 0.31% duplicates
# Conclusion: no need to clear up the data within polygon and points all observations are unique

# for merging with the tables
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

# Get the map out
tiles <- get_tiles(x = berlin_po_filtered, provider = "OpenStreetMap")
tilesLayer(tiles)

# Two working plots
plot(st_geometry(berlin_mp_filtered))
plot(st_geometry(filter(berlin_po_filtered, category == "kids")))

# trying out with ggplot2
as.character(berlin_po_filtered$geometry[1])
library(stringr)

#  extracting the coordinates
coord <- data.frame(x = as.numeric(str_extract(as.character(berlin_po_filtered$geometry), "\\d.\\.\\d.*(?=,)")),
                    y = as.numeric(str_extract(as.character(berlin_po_filtered$geometry), "\\d*.\\d*(?=\\))")))

berlin_po_filtered <- cbind(berlin_po_filtered, coord)

# importing and checking the border map
berlin_countour <- read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_places_a_free_1.shp")
berlin_countour <- berlin_countour[, 5:6]
plot(berlin_countour)

# ggplot version, will use further
ggplot(berlin_countour)+
  geom_sf()+
  coord_sf()

# trying to combine the borders with the hex

plot_hex_map <- function(i) {
ggplot() +
  geom_hex(data = filter(berlin_po_filtered, category == classes_vector[i]), mapping = aes(x, y), bins = 50) +
  scale_fill_viridis_c() +
  geom_sf(data = berlin_countour, fill = "transparent", color = "white")+
  labs(title = classes_vector[i])
}
plot_hex_map(2)