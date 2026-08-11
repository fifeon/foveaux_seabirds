
rm(list = ls())
#processed <- read.csv("~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/data-processed/all_data.csv")
processed <- read.csv("./data-processed/all_data.csv")


## Libraries ##
library(plyr)
library(dplyr)
library(tidyr)
library(janitor)
library(tidyverse)
library(stringr)
library(readr)
library(data.table)
library(circular)
library(patchwork)
library(sf)## Spatial analysis setup ####

# Create a new spatial object using dataframe

data = sf::st_as_sf(processed, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Read georeferenced polygon of NZ incl. South Isl and Rakiura into R env.

nz_polygon <- sf::st_read("gis/coast_and_islands/nz-coastlines-and-islands-polygons-topo-150k.shp")
mainland_polygon <- sf::st_read("gis/coast/nz-coastlines-topo-150k.shp")
# Bluff Marker = -46.6056996 168.3644488

# Transform df to match polygon CRS

data <- sf::st_transform(data, sf::st_crs(nz_polygon))

# Calculate minimum distance from each sighting to any polygon feature

data <- data %>%
  dplyr::mutate(dist_to_coast = apply( sf::st_distance(data, nz_polygon), 
                                        1,        # 1 = row-wise (i.e. per sighting)
                                        min       # take the nearest polygon feature
  ))%>%
  
  # Calculate minimum distance from each sighting to any mainland polygon features (does not factor in small islands)
  dplyr::mutate(dist_to_mainland = apply( sf::st_distance(data, mainland_polygon), 
                                       1,        # 1 = row-wise (i.e. per sighting)
                                       min       # take the nearest polygon feature
  ))

# Create a geometry point in sf for the bluff survey marker
bluff_point <- sf::st_sfc(
  sf::st_point(c(168.3644488, -46.6056996)),
  crs = 4167
)

# Calculate minimum distance from each sighting from Bluff Survey Marker
data <- data %>%
  dplyr::mutate(
    foveaux_south = as.numeric(sf::st_distance(., bluff_point))
  )
## -- Sort into categories ####

#turn metres to kilometres
data <- data %>%
  dplyr::mutate(dist_to_mainland = dist_to_mainland / 1000) %>%
  dplyr::mutate(dist_to_coast = dist_to_coast / 1000) %>%
  dplyr::mutate(foveaux_south = foveaux_south / 1000)

# Add section column
data <- data %>%
dplyr::mutate(region_mainland = dplyr::case_when(
  dist_to_mainland < 8 ~ 'Coastal',
  dist_to_mainland >= 8 ~ 'Offshore',
)) %>%
  dplyr::mutate(region_islands = dplyr::case_when(
    dist_to_coast < 4 ~ 'Coastal',
    dist_to_coast >= 4 ~ 'Offshore',
  ))

data <- data %>%
  dplyr::mutate(distance_offshore = dplyr::case_when(
    dist_to_mainland <= 4 ~ "DistOffshore0.4km",
    dist_to_mainland > 4 & dist_to_mainland <= 8 ~ "DistOffshore4.8km",
    dist_to_mainland > 8 & dist_to_mainland <= 12 ~ "DistOffshore8.12km",
    dist_to_mainland > 12 & dist_to_mainland <= 16 ~ "DistOffshore12.16km",

  ), .before = "dist_to_mainland")

# Sort levels
data$distance_offshore <-
  factor(data$distance_offshore, levels = c("DistOffshore0.4km",
                                           "DistOffshore4.8km",
                                           "DistOffshore8.12km",
                                           "DistOffshore12.16km"))

## Create Segments of Strait from Bluff > Halfmoon Bay
# m to km
data <- data %>%
  dplyr::mutate(foveaux_south = dplyr::case_when(
    foveaux_south <= 5 ~ "FoveauxSouth0-5km",
    foveaux_south > 5 & foveaux_south <= 10 ~ "FoveauxSouth5-10km",
    foveaux_south > 10 & foveaux_south <= 15 ~ "FoveauxSouth10-15km",
    foveaux_south > 15 & foveaux_south <= 20 ~ "FoveauxSouth15-20km",
    foveaux_south > 20 & foveaux_south <= 25 ~ "FoveauxSouth20-25km",
    foveaux_south > 25 & foveaux_south <= 30 ~ "FoveauxSouth25-30km",
    foveaux_south > 3 & foveaux_south <= 36 ~ "FoveauxSouth30-35km",
    
  ), .before = "dist_to_mainland")

# Sort levels
data$foveaux_south <-
  factor(data$foveaux_south, levels = c("FoveauxSouth5",
                                        "FoveauxSouth5-10km",
                                        "FoveauxSouth10-15km",
                                        "FoveauxSouth15-20km",
                                        "FoveauxSouth20-25km",
                                        "FoveauxSouth25-30km",
                                        "FoveauxSouth30-35km"))

## Write file


data <-
  data %>%
  sf::st_drop_geometry(df) %>%
  arrange(date, year, month, day, time) 

write.csv(data, "./data-processed/all_data.csv", row.names = FALSE)
