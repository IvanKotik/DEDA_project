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

classes_po <- list(food_and_drinks = c("bakery", "bar", "biergarten", "cafe", "fast_food", "food_court", "pub", "restaurant"),
                   shops_consumables = c("butcher", "convenience", "department_store", "kiosk", "market_place", "supermarket"),
                   entertaining = c("arts_centre", "cinema", "community_centre", "library", "nightclub", "theatre", "zoo"),
                   other_shops = c("bank", "beauty_shop", "bicycle_shop", "bookshop", "clothes", "florist", "hairdresser", "mall", "shoe_shop"),
                   landmarks = c("artwork", "attraction", "fountain", "monument", "observation_tower", "tower"),
                   kids = c("kindergarten", "playground"),
                   parks = "park",
                   pets = c("dog_park", "veterinary"),
                   sport = c("pitch", "sports_centre", "stadium", "swimming_pool", "track"))

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

123