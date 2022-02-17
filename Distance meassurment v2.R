# Load packages
install.packages("sf")
library(sf)
install.packages("tidyverse")
library(tidyverse)
library(units)
library(jcolors)
options(scipen=999)

# Import data
model <- read.csv("/Users/ivankotik/Documents/DEDA_project/files and graphs/data after modelling.csv")
geocoding <- read.csv("/Users/ivankotik/Documents/DEDA_project/files and graphs/coordinates_osm_v2.csv")
polygon <- read_sf("/Users/ivankotik/Documents/DEDA_project/files and graphs/polygon.shp")
multipolygon <- read_sf("/Users/ivankotik/Documents/DEDA_project/files and graphs/multipolygon.shp")
berlin <- read_sf("/Users/ivankotik/Documents/DEDA_project/files and graphs/berlin.shp")


### Data cleaning
# Deleting an empty row
geocoding <- geocoding[-1, ]

# Changing column names
geocoding <- rename(geocoding, "fulladress" = "query")

# Get the full adress
model$fulladress <- paste(model$regio1, model$geo_plz, model$streetPlain, model$houseNumber)

# Deleting a useless column
model$X <- NULL

# Joining the tables
left_join(model, geocoding, by = "fulladress") -> apartments
distinct(apartments) -> apartments

# Leaving only correct coordinates
apartments[is.na(apartments[, 32]) == FALSE, ] -> apartments_clean

# Creating the SF object out of
st_as_sf(apartments_clean, coords = c("lon", "lat"), crs = "WGS84") -> apartments_clean_points
rm(apartments_clean)
rm(apartments)


### Calaulating distance polygons
# Calculating the distance
apartments_clean_points %>% st_distance((polygon %>% filter(group == "catering"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$catering_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "activities"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$activities_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "destinations"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$destinations_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "entertainment"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$entertainment_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "health"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$health_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "kids"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$kids_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "shopping"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$shopping_p

apartments_clean_points %>% st_distance((polygon %>% filter(group == "transport"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$transport_p

# Normalizing data
apartments_clean_points$catering_p <- (apartments_clean_points$catering_p - min(apartments_clean_points$catering_p))/
  (max(apartments_clean_points$catering_p) - min(apartments_clean_points$catering_p))

apartments_clean_points$activities_p <- (apartments_clean_points$activities_p - min(apartments_clean_points$activities_p))/
  (max(apartments_clean_points$activities_p) - min(apartments_clean_points$activities_p))

apartments_clean_points$destinations_p <- (apartments_clean_points$destinations_p - min(apartments_clean_points$destinations_p))/
  (max(apartments_clean_points$destinations_p) - min(apartments_clean_points$destinations_p))

apartments_clean_points$entertainment_p <- (apartments_clean_points$entertainment - min(apartments_clean_points$entertainment))/
  (max(apartments_clean_points$entertainment_p) - min(apartments_clean_points$entertainment))

apartments_clean_points$health_p <- (apartments_clean_points$health_p - min(apartments_clean_points$health_p))/
  (max(apartments_clean_points$health_p) - min(apartments_clean_points$health_p))

apartments_clean_points$kids_p <- (apartments_clean_points$kids_p - min(apartments_clean_points$kids_p))/
  (max(apartments_clean_points$kids_p) - min(apartments_clean_points$kids_p))

apartments_clean_points$shopping_p <- (apartments_clean_points$shopping_p - min(apartments_clean_points$shopping_p))/
  (max(apartments_clean_points$shopping_p) - min(apartments_clean_points$shopping_p))

apartments_clean_points$transport_p <- (apartments_clean_points$transport_p - min(apartments_clean_points$transport_p))/
  (max(apartments_clean_points$transport_p) - min(apartments_clean_points$transport_p))

# Total score calculation
apartments_clean_points$total_score_p <- apartments_clean_points$catering_p + apartments_clean_points$activities_p +
  apartments_clean_points$destinations_p + apartments_clean_points$entertainment_p + apartments_clean_points$health_p +
  apartments_clean_points$kids_p + apartments_clean_points$shopping_p + apartments_clean_points$transport_p

# Total score normalized
apartments_clean_points$total_score_normalized_p <- (apartments_clean_points$total_score_p - min(apartments_clean_points$total_score_p))/
  (max(apartments_clean_points$total_score_p) - min(apartments_clean_points$total_score_p))


### Calaulating distance multipolygons
# Calcualte the distance between the multipolygon and points for all the groups
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

apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "park"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$park_m

apartments_clean_points %>% st_distance((multipolygon %>% filter(group == "water"))) %>%
  + set_units(10, m) %>% `^`(-1) %>% apply(MARGIN = 1, FUN = sum) -> apartments_clean_points$water_m

# normalizing the data
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

apartments_clean_points$park_m <- (apartments_clean_points$park_m - min(apartments_clean_points$park_m))/
  (max(apartments_clean_points$park_m) - min(apartments_clean_points$park_m))

apartments_clean_points$water_m <- (apartments_clean_points$water_m - min(apartments_clean_points$water_m))/
  (max(apartments_clean_points$water_m) - min(apartments_clean_points$water_m))

# Total score calculation
apartments_clean_points$total_score_m <- apartments_clean_points$catering_m+apartments_clean_points$activities_m +
  apartments_clean_points$destinations_m + apartments_clean_points$entertainment_m + apartments_clean_points$health_m +
  apartments_clean_points$kids_m + apartments_clean_points$shopping_m + apartments_clean_points$park_m + apartments_clean_points$water_m

# Total score normalized
apartments_clean_points$total_score_normalized_m <- (apartments_clean_points$total_score_m - min(apartments_clean_points$total_score_m))/
  (max(apartments_clean_points$total_score_m) - min(apartments_clean_points$total_score_m))


### The landlord premium model
# Calculater the total weight scores
apartments_clean_points$score_total <- apartments_clean_points$total_score_normalized_m + apartments_clean_points$total_score_normalized_p

# Normalize the total scores combined
apartments_clean_points$total_score_normalized <- (apartments_clean_points$score_total - min(apartments_clean_points$score_total))/
  (max(apartments_clean_points$score_total) - min(apartments_clean_points$score_total)) + 0.5

# Calcualter the premium
apartments_clean_points$premium <- apartments_clean_points$price - (apartments_clean_points$total_score_normalized *
  apartments_clean_points$model2_2)

summary(apartments_clean_points$premium)
b_shape <- st_union(berlin)

### PLOTS
# Plotting the results
{
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
}

# Unfulfilled multipolygon idea
{
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
}

# Premiums visualized
ggplot()+
  geom_sf(data = berlin, fill = "white", alpha = 0.5)+
  geom_sf(data = apartments_clean_points, aes(color = premium))+
  scale_color_jcolors_contin("pal4")+
  labs(title = "LANDLORD PREMIUM", color = "PREMIUM")+
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
    legend.text = element_text(colour = "#cacaca"),
    title = element_text(colour = "#cacaca")
  )
  }
ggsave("Premiums.png", dpi = 320, scale = 1)

# Total scores normalized
ggplot()+
  geom_sf(data = berlin, fill = "white", alpha = 0.5)+
  geom_sf(data = apartments_clean_points, aes(color = total_score_normalized))+
  scale_color_jcolors_contin("pal2")+
  labs(title = "TOTAL SCORE, NORMALIZED", color = "SCORE")+
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
    legend.text = element_text(colour = "#cacaca"),
    title = element_text(colour = "#cacaca")
  )
  }
ggsave("Scores_normalized.png", dpi = 320, scale = 1)