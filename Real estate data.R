library(tidyverse)

# Data taken from: https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany/version/6
# Importing data
data <-   read_csv("C://Users//ivkot//Downloads//archive//immo_data.csv")

# Checking for berlin
table(data$regio1)

# Extracting Berlin
data %>% filter(regio1 == "Berlin") -> data_berlin

# Deleting columns that would not be used
data_berlin$telekomTvOffer <- NULL
data_berlin$telekomHybridUploadSpeed <- NULL
data_berlin$telekomUploadSpeed <- NULL
data_berlin$picturecount <- NULL
data_berlin$pricetrend <- NULL
data_berlin$scoutId <- NULL
data_berlin$noParkSpaces <- NULL
data_berlin$firingTypes <- NULL
data_berlin$geo_bln <- NULL
data_berlin$geo_krs <- NULL
data_berlin$petsAllowed <- NULL
data_berlin$street <- NULL
data_berlin$thermalChar <- NULL
data_berlin$livingSpaceRange <- NULL
data_berlin$baseRentRange <- NULL
data_berlin$yearConstructedRange <- NULL
data_berlin$noRoomsRange <- NULL
data_berlin$energyEfficiencyClass <- NULL
data_berlin$lastRefurbish <- NULL
data_berlin$electricityBasePrice <- NULL
data_berlin$electricityKwhPrice <- NULL
table(data_berlin$electricityBasePrice)

# Moving around the columns
data_berlin <- relocate(data_berlin, regio2, regio3, geo_plz, streetPlain, houseNumber, .after = regio1)
data_berlin <- relocate(data_berlin, totalRent, baseRent, serviceCharge, heatingCosts, .after = houseNumber)
data_berlin <- relocate(data_berlin, livingSpace, noRooms, hasKitchen, balcony, facilities, description, typeOfFlat, garden, interiorQual, condition, cellar,  .after = heatingType)
data_berlin <- relocate(data_berlin, lift,  .after = numberOfFloors)
data_berlin <- relocate(data_berlin, yearConstructed,  .after = cellar)

# Checking the "facilities" column for more useful characteristics
# Guest WC's
str_extract(data_berlin$facilities, "([zZ]wei.{0,4}B.d)|(ste.WC)")
sort(str_extract(data_berlin$facilities, "([zZ]wei.{0,4}B.d)|(ste.WC)"))  # all good
data_berlin$extrawc <- as.integer(sub("([zZ]wei.{0,4}B.d)|(ste.WC)", 1, (str_extract(data_berlin$facilities, "([zZ]wei.{0,4}B.d)|(ste.WC)"))))
data_berlin <- relocate(data_berlin, extrawc, .after = hasKitchen)

# dishwashing machine
sort(str_extract(data_berlin$facilities, "Geschirrsp.lmaschine"))
data_berlin$geschirrsp <- as.integer(sub("Geschirrsp.lmaschine", 1, (str_extract(data_berlin$facilities, "Geschirrsp.lmaschine"))))

# washing machine
sort(str_extract(data_berlin$facilities, "Waschmaschine"))
data_berlin$washingm <- as.integer(sub("Waschmaschine", 1, (str_extract(data_berlin$facilities, "Waschmaschine"))))

# sorting data
data_berlin <- relocate(data_berlin, geschirrsp, washingm, .after = balcony)
data_berlin <- relocate(data_berlin, description, facilities, .after = date)

# checking the true-false columns for compatability
sum(data_berlin$hasKitchen)
sum(data_berlin$balcony)
sum(data_berlin$garden)
sum(data_berlin$cellar)
sum(data_berlin$newlyConst)
sum(data_berlin$lift)  # all good

# making the NA-1 to true-false
data_berlin$extrawc <- as.logical(replace_na(data_berlin$extrawc, 0))
data_berlin$geschirrsp <- as.logical(replace_na(data_berlin$geschirrsp, 0))
data_berlin$washingm <- as.logical(replace_na(data_berlin$washingm, 0))

# Checking whether heatingTypes have a influence on the price
ggplot(data_berlin[data_berlin[, "totalRent"]<5000, ], aes(x = livingSpace, y = totalRent, color = heatingType))+
  geom_point()
ggsave("heatingtypewithoutlines.png", dpi = 320, scale = 1)

ggplot(data_berlin[data_berlin[, "totalRent"]<5000, ], aes(x = livingSpace, y = totalRent, color = heatingType))+
  geom_point(size = 0.75, alpha = 0.2)+
  geom_smooth(se = FALSE, method = lm, size = 0.7)  # based on the graps it can be seen that there is no heavy difference between the types
ggsave("heatingtypewithlines.png", dpi = 320, scale = 1)

