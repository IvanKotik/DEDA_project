# initiating the packages
install.packages("tidyverse")
library(tidyverse)
install.packages("data.table")
library(data.table)
install.packages("ggmap")
library(ggmap)
install.packages("sf")
library(sf)
install.packages("osmdata")
library(osmdata)

# importing OSM data from daten.berlin.de
berlin_mp <- as.data.table(read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_a_free_1.shp"))
berlin_po <- as.data.table(read_sf(dsn = "C://Users//ivkot//Downloads//berlin-latest-free.shp//gis_osm_pois_free_1.shp"))

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

classes_зщ <- list(food_and_drinks = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant"),
                   shops_consumables = c("butcher", "convenience", "department_store", "kiosk", "market_place", "supermarket"),
                   entertaining = c("arts_centre", "cinema", "community_centre", "library", "nightclub", "theatre", "zoo"),
                   other_shops = c("bank", "beauty_shop", "bicycle_shop", "bookshop", "clothes", "florist", "hairdresser", "mall", "shoe_shop"),
                   landmarks = c("artwork", "attraction", "fountain", "monument", "observation_tower", "tower"),
                   kids = c("kindergarten", "playground"),
                   parks = "park",
                   pets = c("dog_park", "veterinary"),
                   sport = c("pitch", "sports_centre", "stadium", "swimming_pool", "track"))