
rm(list = ls())
processed <- read.csv("~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/data-processed/all_data.csv")

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


## -- Sort into categories ####

# Add section column
data <- data %>%
dplyr::mutate(region_mainland = dplyr::case_when(
  dist_to_mainland < 7186.898 ~ 'Coastal',
  dist_to_mainland >= 7186.898 ~ 'Offshore',
)) %>%
  dplyr::mutate(region_islands = dplyr::case_when(
    dist_to_coast < 3803.99 ~ 'Coastal',
    dist_to_coast >= 3803.99 ~ 'Offshore',
  ))


## Write file


data <-
  data %>%
  sf::st_drop_geometry(df) %>%
  arrange(date, year, month, day, time) 

write.csv(data, "./data-processed/all_data.csv", row.names = FALSE)
