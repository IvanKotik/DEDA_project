library(tidyverse)

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

