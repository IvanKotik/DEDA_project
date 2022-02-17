library(tidyverse)

# Data taken from: https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany/version/6
# Importing data
data <- read_csv("C://Users//ivkot//Downloads//archive//immo_data.csv")
# for mac
data <- read.csv("/Users/ivankotik/Documents/shape_files/immo_data.csv")

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
  geom_point()+
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
ggsave("heatingtypewithoutlines.png", dpi = 320, scale = 1)

ggplot(data_berlin[data_berlin[, "totalRent"]<5000, ], aes(x = livingSpace, y = totalRent, color = heatingType))+
  geom_point(size = 0.75, alpha = 0.2)+
  geom_smooth(se = FALSE, method = lm, size = 0.7)+
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
  }  # based on the graps it can be seen that there is no heavy difference between the types
ggsave("heatingtypewithlines.png", dpi = 320, scale = 1)

# making the model
# checking differences in the behavior of the various rent parameters
ggplot()+
  geom_density(data = data_berlin, aes(x = data_berlin$serviceCharge))
ggplot()+
  geom_density(data = data_berlin, aes(x = data_berlin$baseRent))
ggplot()+
  geom_density(data = data_berlin, aes(x = data_berlin$totalRent))

# model without house parameters
model <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + geschirrsp + washingm + garden + cellar + lift + floor + yearConstructed, data_berlin)
plot(model)
# from the diagnostic plots we can see that:
# 1. RvsL: observation nr.183, 7130, 8626 are externalities and should be excluded
# 2. S-L: 183, 7130, 7909 are externalities, data has some heteroscedasticity present
# 3. Q-Q: 183, 7130, 7909 are externalities, data is not normal on the tails
# 4. RvsF: 2169, 7130, 7909 are externalities, data is almost linear
# deleting these observations
data_berlin <- data_berlin[-c(183, 2169, 7130, 7909),]

# remodelling
model <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + geschirrsp + washingm + garden + cellar + lift + floor + yearConstructed, data_berlin)
plot(model)
data_berlin <- data_berlin[-c(8622, 6265, 9313),]
model <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + geschirrsp + washingm + garden + cellar + lift + floor + yearConstructed, data_berlin)
plot(model)
data_berlin <- data_berlin[-c(8753, 7369, 8513),]
model <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + geschirrsp + washingm + garden + cellar + lift + floor + yearConstructed, data_berlin)
plot(model)
data_berlin <- data_berlin[-c(3562, 4987, 9013),]
model <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + geschirrsp + washingm + garden + cellar + lift + floor + yearConstructed, data_berlin)
plot(model)

# trying out a Breusch-Pagan test to check for homoscedasticity
library(lmtest)
options(scipen=999)
bptest(model)  # p-value < 0.00000000000000022, heteroscedasticity present
summary(model)

# looking for parameters to exclude
summary(lm(totalRent ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + washingm + lift + yearConstructed, data_berlin))
summary(lm(totalRent ~ livingSpace + noRooms + hasKitchen + balcony + lift + yearConstructed, data_berlin))
model2 <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + balcony + lift + yearConstructed, data_berlin)

# calibrated model with only impactful parameters left
plot(model2)
data_berlin <- data_berlin[-c(6612, 6046, 9497),]
model2 <- lm(totalRent ~ livingSpace + noRooms + hasKitchen + balcony + lift + yearConstructed, data_berlin)
summary(model2)

# change NA's to 0 and calculate the prices for all apartments
data_berlin$baseRent <- replace_na(data_berlin$baseRent, 0)
data_berlin$serviceCharge <- replace_na(data_berlin$serviceCharge, 0)
data_berlin$heatingCosts <- replace_na(data_berlin$heatingCosts, 0)
data_berlin$price <- data_berlin$baseRent + data_berlin$serviceCharge + data_berlin$heatingCosts
data_berlin <- relocate(data_berlin, price, .after = houseNumber)

sum(is.na(data_berlin$yearConstructed))  # among all parameters except yearsConstructed there are none NA's, there is no reason to replace NA with anything so we jsut delete the NA rows
data_berlin %>% filter(yearConstructed != "NA") -> data_berlin_final


model3 <- lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_final)
summary(model3)
plot(model3)

plot(sort(abs(model3$residuals / model3$fitted.values)))
sort(abs(model3$residuals / model3$fitted.values))
data_berlin_final <- data_berlin_final[-c(7560, 6172, 3933, 1587, 8837, 467, 2911, 3760, 7693, 4727, 6511, 4171, 614, 4548, 2812, 3562, 5018),]

weight <- 1/(model3$fitted.values - data_berlin_final$price)^2
plot(lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_final, weights = weight))
summary(lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_final, weights = weight))
model4 <- lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_final, weights = weight)
# this model looses all of its normality which I don't like
rm(model4)
rm(wt)
rm(wls_model)
rm(model2)
rm(model)


plot(model3)
summary(model3)

data_berlin_final$fitted <- model3$fitted.values

ggplot(data_berlin_final)+
  geom_point(aes(x = livingSpace, y = price, color = "current rent prices"), size = 0.4)+
  geom_point(aes(x = livingSpace, y = fitted, color = "fitted values"), alpha = 0.5, size = 0.4)+
  geom_smooth(aes(x = livingSpace, y = fitted, color = "model"), method = "lm", alpha = 0.5)+
  scale_color_manual(values = c("#ffffff", "#fedb1b", "#cf0303"))+
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
ggsave("modelingtheprices.png", dpi = 320, scale = 1)

predict.lm(model3, data.frame(livingSpace = 75, noRooms = 2, hasKitchen = TRUE, balcony = TRUE, lift = FALSE))
predict.lm(model3, data.frame(livingSpace = 80, noRooms = 3, hasKitchen = TRUE, balcony = TRUE, lift = FALSE))
predict.lm(model3, data.frame(livingSpace = 60, noRooms = 2, hasKitchen = TRUE, balcony = TRUE, lift = FALSE))

# calibrating
summary(model3)
ggplot(data_berlin_final, aes(y = price, fill = factor(noRooms)))+
  geom_boxplot()
ggplot(data_berlin_final, aes(y = price, fill = factor(balcony)))+
  geom_boxplot()

data_berlin_final %>% select(price, livingSpace, noRooms, hasKitchen, balcony, lift) %>% plot
data_berlin_final %>% select(livingSpace, noRooms, hasKitchen, balcony, lift) %>% cor

model_x <- lm(data = data_berlin, formula = price ~ livingSpace + noRooms + hasKitchen + extrawc + balcony + typeOfFlat + garden + cellar + yearConstructed + newlyConst + floor + numberOfFloors + lift)
summary(model_x)
plot(model_x)


# Converting adresses to coordinates
paste(data_berlin$regio1[5], data_berlin$geo_plz[5], data_berlin$streetPlain[5], data_berlin$houseNumber[5])
data_berlin$full_adress <- paste(data_berlin$regio1, data_berlin$geo_plz, data_berlin$streetPlain, data_berlin$houseNumber)
data_berlin <- relocate(data_berlin, full_adress, .before = regio1)

sum(str_detect(data_berlin$full_adress, "NA"))
data_berlin[10369, 1]
data_berlin$full_adress

# Fixing the model
# The initial last model
model3 <- lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_final)
summary(model3)
plot(model3)

ggplot(data = data_berlin_final, aes(x = livingSpace, y = price))+
  geom_point()

# inspecting how the price data distribution looks like
ggplot(data = data_berlin_final, aes(x = price))+
  geom_density()

# deleting the tail
summary(data_berlin_final$price)
quantile(data_berlin_final$price, 0.90)
quantile(data_berlin_final$price, 0.80)
data_berlin_final %>% filter(price <= 2500) -> data_berlin_filtered
data_berlin_final %>% filter(price <= 2000) -> data_berlin_filtered
data_berlin_filtered <- data_berlin_filtered[-c(5882, 4690, 922), ]
data_berlin_filtered <- data_berlin_filtered[-c(5882, 419, 6074, 4150), ]
data_berlin_filtered <- data_berlin_filtered[-5832, ]
# recalibrating the model
model3_filter <- lm(price ~ livingSpace + noRooms + hasKitchen + balcony + lift, data_berlin_filtered)
summary(model3_filter)
# deleting noRooms
model3_filter <- lm(price ~ livingSpace + hasKitchen + balcony + lift, data_berlin_filtered)
summary(model3_filter)
plot(model3_filter)

ggplot(data = data_berlin_filtered, aes(x = livingSpace, y = price))+
  geom_point()
ggplot(data = data_berlin_filtered, aes(x = price))+
  geom_density()

install.packages("ggfortify")
library(ggfortify)
autoplot(lm(price ~ livingSpace + hasKitchen + balcony + lift, data_berlin_filtered), label.size = 3)
data_berlin_filtered$fitted <- NULL
