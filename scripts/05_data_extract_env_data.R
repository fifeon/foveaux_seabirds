##
## Environmental data extraction
##

## This code extracts all the environmental data that will be used in future analyses.

## Libraries ####

library(readr)
library(dplyr)
library(stringr)
library(sp)
library(sf)
library(mapview)
library(rnaturalearth)
library(raster)
library(terra)
library(rerddap)
library(rerddapXtracto)
library(remotes)
# remotes::install_github("jebyrnes/hadsstR")
library(hadsstr)
library(tmap)
library(CopernicusMarine)
library(CFtime)
library(stars)
library(cubelyr)

## Read Data ####

rm(list = ls())

data <- read.csv("./data-processed/all_data.csv")

## Get Parameters for downloading this data

summary_data <-
  data %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(date_start = min(date),
                   date_end = max(date),
                   lat_min = min(lat),
                   lat_max =max(lat),
                   lon_min = min(lon),
                   lon_max = max(lon),
                   n_records = n()) %>%
  dplyr::arrange(date_start)

  # Note: I downloaded data manually from CyberTracker

## Spatialize seabird data & get NZ Polygon
data_sf = sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

nz_polygon <- sf::st_read("gis/coast_and_islands/nz-coastlines-and-islands-polygons-topo-150k.shp")
mainland_polygon <- sf::st_read("gis/coast/nz-coastlines-topo-150k.shp")

## Read Copernicus Weather Data ####
env <- rast("./data-raw/data.grib")
# Can save this as: saveRDS(env,file = "./data-processed/env.rds")
# And read as: env <- readRDS("./data-processed/env.rds")

## Describes env data
terra::describe(env)
# 1 = 10 metre u wind component [m/s]
# 2 = 10 metre v wind component [m/s]
# 3 = Mean sea level prssure [Pa]
# 4 = Sea surface temperature (absolute) [C]
# 5 = Significant wave height [m]
# 6 = Surface pressure [Pa]
# 7 = Total precipitation [m]
# 8 = Total cloud covr (0-1) [-]
# Each group of 8 bands is the same 8 variables, but 2 hours later.

desc <- terra::describe("./data-raw/data.grib")

# Tighter match: only match real block headers, not stray "Band" mentions elsewhere
band_starts <- grep("^Band [0-9]+ Block=", desc)
band_ends <- c(band_starts[-1] - 1, length(desc))

length(band_starts)        # sanity check
terra::nlyr(env)            # should match now


## Download Copernicus Marine Data ####
cms_login(username = Sys.getenv("CMS_USERNAME"), password = Sys.getenv("CMS_PASSWORD"))

# cms_product_details("GLOBAL_ANALYSISFORECAST_PHY_001_024") to get variable names

data_env_sst <- ## Get SST
  CopernicusMarine::cms_download_subset(
    product = "GLOBAL_ANALYSISFORECAST_PHY_001_024",
    layer = "cmems_mod_glo_phy_anfc_0.083deg_PT1H", #hourly data
    variable = c("thetao"), #thetao = sea water potential temperature?
    region = c(168.1424, -46.60382, 168.3664, -46.98094),
    timerange = c("2025-07-25", "2026-06-22"),
    verticalrange = c(0, -0.5),
    progress = TRUE,
    username = Sys.getenv("CMS_USERNAME"),
    password = Sys.getenv("CMS_PASSWORD")
  )

data_env_wind <-
  CopernicusMarine::cms_download_subset(
    product = "WIND_GLO_PHY_L4_NRT_012_004",
    layer = "cmems_obs-wind_glo_phy_nrt_l4_0.125deg_PT1H", #hourly data
    variable = c("eastward_wind", "northward_wind"), #thetao = sea water potential temperature?
    region = c(168.1424, -46.60382, 168.3664, -46.98094),
    timerange = c("2025-07-25", "2026-06-22"),
    verticalrange = c(0, -0.5),
    progress = TRUE,
    username = Sys.getenv("CMS_USERNAME"),
    password = Sys.getenv("CMS_PASSWORD")
  )

## Extract SST (thetao) from Copernicus Marine data - manual nearest-neighbor version ####

time_vals <- stars::st_get_dimension_values(data_env_sst, "time")
lon_vals  <- stars::st_get_dimension_values(data_env_sst, "longitude")
lat_vals  <- stars::st_get_dimension_values(data_env_sst, "latitude")

## Extract wind from data_env_wind ####
wind_2d <- stars::st_apply(data_env_wind, c("longitude", "latitude", "time"), FUN = identity)

wind_time_vals <- stars::st_get_dimension_values(wind_2d, "time")
wind_lon_vals  <- stars::st_get_dimension_values(wind_2d, "longitude")
wind_lat_vals  <- stars::st_get_dimension_values(wind_2d, "latitude")

# stars stores lon as intervals - get midpoints for distance matching
lon_mid <- sapply(lon_vals, function(iv) mean(as.numeric(iv)))

# Check if lon is stored as intervals like before, or as plain values
wind_lon_mid <- if (is.list(wind_lon_vals)) {
  sapply(wind_lon_vals, function(iv) mean(as.numeric(iv)))
} else {
  wind_lon_vals
}

data_sf$date <- as.POSIXct(data_sf$date, tz = "UTC")

coords <- sf::st_coordinates(data_sf)   # column 1 = X (lon), column 2 = Y (lat)
# Squeeze out the singleton elevation dimension using st_apply

data_env_2d <- stars::st_apply(data_env_sst, c("longitude", "latitude", "time"), FUN = identity)

## Extract SST (thetao) - direct, no st_apply ####

arr_thetao <- data_env_sst[["thetao"]]
dim(arr_thetao)   # check dimension order/count - likely 4D: lon, lat, elevation, time

## Extract wind - direct, no st_apply ####

arr_u_wind <- data_env_wind[["eastward_wind"]]
arr_v_wind <- data_env_wind[["northward_wind"]]
dim(arr_u_wind)   # check if wind has an elevation/height dimension too

data_sf$sst <- NA_real_
data_sf$wind_u <- NA_real_
data_sf$wind_v <- NA_real_

data_sf$sst <- NA_real_
data_sf$wind_u <- NA_real_
data_sf$wind_v <- NA_real_

for (i in seq_len(nrow(data_sf))) {
  lon_idx  <- which.min(abs(lon_mid - coords[i, "X"]))
  lat_idx  <- which.min(abs(lat_vals - coords[i, "Y"]))
  time_idx <- which.min(abs(as.numeric(time_vals) - as.numeric(data_sf$date[i])))
  
  wind_lon_idx  <- which.min(abs(wind_lon_mid - coords[i, "X"]))
  wind_lat_idx  <- which.min(abs(wind_lat_vals - coords[i, "Y"]))
  wind_time_idx <- which.min(abs(as.numeric(wind_time_vals) - as.numeric(data_sf$date[i])))
  
  data_sf$sst[i]    <- arr_thetao[lon_idx, lat_idx, 1, time_idx]
  data_sf$wind_u[i] <- arr_u_wind[wind_lon_idx, wind_lat_idx, wind_time_idx]
  data_sf$wind_v[i] <- arr_v_wind[wind_lon_idx, wind_lat_idx, wind_time_idx]
}

summary(data_sf$sst)
summary(data_sf$wind_u)
summary(data_sf$wind_v)

## Fill NA values with nearest same-day sighting ####

fill_same_day_nearest <- function(data_sf, var_name) {
  data_sf$day <- as.Date(data_sf$date)
  
  has_val     <- data_sf[!is.na(data_sf[[var_name]]), ]
  missing_val <- data_sf[is.na(data_sf[[var_name]]), ]
  
  if (nrow(missing_val) == 0) return(data_sf[[var_name]])
  
  filled_values <- sapply(seq_len(nrow(missing_val)), function(i) {
    row <- missing_val[i, ]
    candidates <- has_val[has_val$day == row$day, ]
    
    if (nrow(candidates) == 0) {
      return(NA_real_)
    }
    
    time_diffs <- abs(as.numeric(candidates$date) - as.numeric(row$date))
    candidates[[var_name]][which.min(time_diffs)]
  })
  
  data_sf[[var_name]][is.na(data_sf[[var_name]])] <- filled_values
  data_sf[[var_name]]
}

## Create a same-day fill function for each variable
env_vars <- c("sst", "wind_u", "wind_v")

data_sf$day <- as.Date(data_sf$date)

for (v in env_vars) {
  data_sf[[v]] <- fill_same_day_nearest(data_sf, v)
}

sapply(data_sf[env_vars], function(x) sum(is.na(x)))

## FINAL CALCULATIONS ####

## Figure out Wind Speed and Direction using u-component of eastward and v-component of northward winds in m/s
data_sf <-
  data_sf %>%
  dplyr::mutate(wind_direction = (270-atan2(wind_u, wind_v)*180/pi)%%360) %>%
  dplyr::mutate(wind_speed = sqrt(wind_u^2+wind_v^2)) %>%
  dplyr::mutate(wind_direction = round(wind_direction/45)*45)

# Group Wind directions to nearest 8-point compass direction
data_sf <-
  data_sf %>%
  dplyr::mutate(wind_direction = case_when(
    wind_direction == 45 ~ "NE",
    wind_direction == 90 ~ "E",
    wind_direction == 135 ~ "SE",
    wind_direction == 180 ~ "S",
    wind_direction == 225 ~ "SW",
    wind_direction == 270 ~ "W",
    wind_direction == 315 ~ "NW",
    wind_direction == 360 ~ "N"
  ))



## SAVE ####
data_sf <- sf::st_drop_geometry(data_sf)
write.csv(data_sf, "./data-processed/all_data.csv", row.names = FALSE)


