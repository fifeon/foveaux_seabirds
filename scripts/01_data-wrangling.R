##
## Raw data to a standardised format
##
## --------------------------------------------------------##

rm(list = ls())

## Libraries ##
library(plyr)
library(dplyr)
library(tidyr)
library(janitor)
library(tidyverse)
library(stringr)
library(readr)
library(data.table)

## Raw data notes ##
{
  # data from the 27th of July 2025 has been edited in excel only to add sightings done as part of a test to determine if audio recordings were a valid method for the study.
}

## New Zealand Base Map ##

#--------------------------------------#
## Read data raw and standardise
#--------------------------------------#

# Get file names
df_birds <- dir(path = "./data-raw", pattern = "*.csv", ignore.case = TRUE, full.names = TRUE) %>% 
purrr::map_df(~readr::read_csv(., col_types = cols(.default = "c")))

# Reorganise data set for birds IN-TRANSECT
df_birds <- df_birds %>% 
  dplyr::select(date, time, lat, lon, swell, beaufort, home_screen, effort, albatross, mollymawk_sooty, shearwater, petrel, gadfly, diving_petrel, prion, gull, tern, gannet, shag, skua, penguin, age, count, behaviour, distance_band, stewart_island_shag_phase, note) %>%
  dplyr::rename(shag_plumage = stewart_island_shag_phase)


#--------------------------------------#
## Tidying up columns ##
#--------------------------------------#

# Sets columns to lowercase
names(df_birds) <- tolower(names(df_birds))

# Replaces 'spaces' with 'underscore'
names(df_birds) <- gsub(" ", "_", names(df_birds))

#--------------------------------------#
## Establishes correct column classes ##
#--------------------------------------#

# Date & time
df_birds$date <- lubridate::dmy(df_birds$date)
df_birds$time <- format(strptime(c(df_birds$time), "%I:%M:%S %p"), "%H:%M:%S")
df_birds$time <- lubridate::hms(df_birds$time) #not sure about the time? Will ask Nico abt it
df_birds < df_birds %>% dplyr::rename(hour = time)

df_birds <-
  df_birds %>%
  arrange(date, time)

# Factor
factor_cols <- c("swell", "beaufort", "home_screen", "effort", "albatross", "mollymawk_sooty", "shearwater", "petrel", "gadfly", "diving_petrel", "prion", "gull", "tern", "gannet", "shag", "skua", "penguin", "age", "behaviour", "distance_band", "shag_plumage")
df_birds[factor_cols] <- lapply(df_birds[factor_cols], as.factor)

# Numerics
numeric_cols <- c("lat", "lon", "count")
df_birds[numeric_cols] <- lapply(df_birds[numeric_cols], as.numeric)

#--------------------------------------#
## The raw dataset contains some input errors so this fixes them ##
#--------------------------------------#

# Deletes wrong data inputs (e.g. double effort "ON/"OFF" when testing)
df_birds <-
  df_birds %>%
  dplyr::filter(!c(date == "2025-07-27" & time == "7H 58M 37S")) %>% #Double effort ON
  dplyr::filter(!c(date == "2025-09-28" & time == "15H 16M 34S")) %>% #Recorded sparrows seen in the middle of the strait
  dplyr::filter(!c(date == "2026-05-19" & time == "16H 51M 58S")) #Double effort ON

#--------------------------------------#
## Column filtering and sorting ##
#--------------------------------------#

## Fill conditions (swell, beaufort, effort) for the whole 'df'
df_birds <-
  df_birds %>%
  tidyr::fill(swell, beaufort, .direction = "updown") %>%
  dplyr::mutate(effortid = effort) %>%
  tidyr::fill(effort, .direction = "down")

## Filter just seabird information and weather conditions, drop unused levels
df_birds <-
  df_birds %>%
  dplyr::filter(home_screen == "seabirds" |
                home_screen == "note" |
                home_screen == "weather_conditions" |
                home_screen == "effort") %>%
  droplevels(.)

## Create an ID number for each seabird count ('id'),
## which is between (including) every 'effort ON' and 'effort oFF'
## from 'home_screen' variable
df_birds <-
  df_birds %>%
  dplyr::mutate(id = ifelse(effortid == "ON", seq(1:n()), NA)) %>%
  dplyr::mutate(id = cumsum(!is.na(id))) %>%
  dplyr::mutate(id = ifelse(home_screen == "note" | home_screen =="weather_conditions", NA, id)) %>%
  dplyr::relocate(id, .before = home_screen)

# Clears ID numbers for each seabird count outside of effort 'ON' and effort 'OFF'
df_birds <- df_birds %>%
  dplyr::mutate(id = dplyr::case_when(
    effort == "OFF"  ~ NA_integer_,
    TRUE ~ id
  )) %>% # Removes the ID from each seabird count where effort == 'OFF'
  dplyr::mutate(effort = effortid) %>%
  dplyr::select(-effortid)


#--------------------------------------#
## Gather all species under a couple of columns ##
#--------------------------------------#

# Separate seabird rows and non-seabird rows
df_birds <- bind_rows(
  df_birds %>%
  dplyr::filter(home_screen == "seabirds") %>%
  tidyr::pivot_longer(
    cols = c(albatross, mollymawk_sooty, shearwater, petrel, gadfly,
             diving_petrel, prion, gull, tern, gannet, shag, skua, penguin),
    names_to = "group",
    values_to = "species" ## Compacts all the columns under two: "group + species"
  ) %>%
  dplyr::filter(!is.na(species) & species != "") %>%
  dplyr::select(date, time, lat, lon, swell, beaufort, id, home_screen, effort,
         age, count, behaviour, distance_band, shag_plumage,
         note, group, species), ## Removes the old species rows 

  df_birds %>% 
  dplyr::filter(home_screen != "seabirds") %>% ## Keeps non-seabird rows as is
  dplyr::select(date, time, lat, lon, swell, beaufort, id, home_screen, effort,
         age, count, behaviour, distance_band, shag_plumage,
         note) %>%
  dplyr::mutate(group = NA, species = NA) ## Adds in NA to non-'seabird' home_screen values
)

#--------------------------------------#
## Amends some notes such as direction of flight ##
#--------------------------------------#

# Checks notes (to deal with if needed)
df_notes <-
  df_birds %>% dplyr::filter(note != 'N/A') %>% dplyr::group_by(id, species, date, time, lat, lon, swell, beaufort, count) %>% dplyr::summarise(notes = unique(note)) %>% print(n=100)
write.csv(df_notes, "./data-processed/notes.csv", row.names = FALSE)

# Adds changes that were put in notes
{
df_birds <-
  df_birds %>%
    # Adds in the direction of flight/phase of shag before they were added as an option to CyberTracker
  dplyr::mutate(species = case_when(date == "2025-07-25" & time == "17H 0M 6S" ~ "black_shag", TRUE ~ species)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-25" & time == "17H 0M 6S" ~ "flying_E", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-25" & time == "17H 0M 40S" ~ "flying_E", TRUE ~ behaviour)) %>%
  dplyr::mutate(species = case_when(date == "2025-07-25" & time == "17H 15M 38S" ~ "black_shag", TRUE ~ species)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-25" & time == "17H 15M 38S" ~ "flying_W", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-25" & time == "17H 24M 58S" ~ "flying_N", TRUE ~ behaviour)) %>%
  dplyr::mutate(shag_plumage = case_when(date == "2025-07-26" & time == "15H 47M 10S" ~ "pied", TRUE ~ shag_plumage)) %>%
  dplyr::mutate(shag_plumage = case_when(date == "2025-07-26" & time == "16H 47M 6S" ~ "bronze", TRUE ~ shag_plumage)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-26" & time == "16H 49M 55S" ~ "flying_W", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-26" & time == "16H 51M 4S" ~ "flying_S", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-26" & time == "16H 51M 59S" ~ "flying_E", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-26" & time == "16H 54M 39S" ~ "flying_W", TRUE ~ behaviour)) %>%
  dplyr::mutate(behaviour = case_when(date == "2025-07-26" & time == "17H 8M 48S" ~ "flying_E", TRUE ~ behaviour)) %>%
  dplyr::mutate(shag_plumage = case_when(date == "2025-07-26" & time == "17H 12M 19S" ~ "pied", TRUE ~ shag_plumage)) %>%
    dplyr::mutate(shag_plumage = case_when(date == "2025-07-27" & time == "16H 56M 3S" ~ "bronze", TRUE ~ shag_plumage)) %>%
    dplyr::mutate(species = case_when(date == "2025-07-27" & time == "17H 26M 8S" ~ "little_shag", TRUE ~ species)) %>% # "Possibly a little shag""
    
    ## Changes sitting distances measured after sighting was recorded
    dplyr::mutate(distance_band = case_when(date == "2025-08-16" & time == "16H 51M 36S" ~ "50-100m", TRUE ~ distance_band)) %>%
  
     ## Changes sightings that are likely to be this species - may remove later if this isn't accurate?
    dplyr::mutate(species = case_when(date == "2025-07-26" & time == "17H 8M 48S" ~ "southern_skua", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-07-27" & time == "16H 56M 3S" ~ "stewart_island_shag", TRUE ~ species)) %>% # "Likely" Stewart Island shag
    dplyr::mutate(species = case_when(date == "2025-08-15" & time == "16H 37M 44S" ~ "pied_shag", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-08-16" & time == "9H 58M 34S" ~ "northern_royal_albatross", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-08-17" & time == "15H 7M 4S" ~ "shearwater_sp", TRUE ~ species)) %>% # Think that this was a shearwater, not sure sp.
    dplyr::mutate(species = case_when(date == "2025-08-17" & time == "10H 19M 56S" ~ "black-browed_mollymawk", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-09-27" & time == '8H 34M 18S' ~ 'grey-headed_mollymawk', TRUE ~ species)) %>% # Likely - grey underwings, juveniles visit NZ coast in winter.
    dplyr::mutate(species = case_when(date == "2025-09-29" & time == '8H 34M 46S' ~ 'prion_sp', TRUE ~ species)) %>% #retroactively likely due to seeing prions seconds later
    dplyr::mutate(species = case_when(date == "2025-09-29" & time == "8H 43M 26S" ~ "westland_petrel", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-10-29" & time == "8H 31M 20S" ~ "sooty_shearwater", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-12-03" & time == "17H 51M 8S" ~ "brown_skua", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2025-12-05" & time == "12H 23M 49S" ~ "white-capped_mollymawk", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2026-01-17" & time == "14H 33M 23S" ~ "westland_petrel", TRUE ~ species)) %>%
    dplyr::mutate(species = case_when(date == "2026-03-22" & time == "8H 19M 26S" ~ "westland_petrel", TRUE ~ species)) %>%

    # fixes group of prion sighting, mira recorded 'tern_sp' but says it looked like a prion
    dplyr::mutate(group = case_when(species == 'prion_sp' & group == 'tern' ~ 'prion', TRUE ~ group))
    }


## Fix old elements to newer inputs

df_birds <-
  df_birds %>%
  dplyr::mutate(distance_band = dplyr::case_when( # I changed the distance bands to have a '-' instead of an '_'.
    distance_band %in% c("0_50m", "0-50m") ~ "0-50m",
    distance_band %in% c("50_100m", "50-100m") ~ "50-100m",
    distance_band %in% c("100_200m", "100-200m") ~ "100-200m",
    distance_band %in% c("200_plus", "200-plus") ~ "200+", TRUE ~ distance_band
  ))

#--------------------------------------#
## Adds English names without '_' to ease data plotting ##
#--------------------------------------#
{
df_birds <-
  df_birds %>%
  dplyr::mutate(species_nice_name = dplyr::case_when(
    species == "wandering_royal_complex" ~ "Wandering albatross",
    species == "northern_royal_albatross" ~ "Northern royal albatross",
    species == "southern_royal_albatross" ~ "Southern royal albatross",
    species == "royal_albatross_sp" ~ "Unknown (royal) albatross",
    species == "Diomedea_sp" ~ "Unknown (Diomedea) albatross",
    species == "other_albatross" ~ "Unknown (great) albatross",
    species == "Buller's_mollymawk" ~ "Buller's mollymawk",
    species == "white-capped_mollymawk" ~ "White-capped mollymawk",
    species == "Salvin's_mollymawk" ~ "Salvin's' mollymawk",
    species == "black-browed_mollymawk" ~ "Black-browed mollymawk",
    species == "Campbell_mollymawk" ~ "Campbell black-browed mollymawk",
    species == "grey-headed_mollymawk" ~ "Grey-headed mollymawk",
    species == "Chatham_mollymawk" ~ "Chatham Island mollymawk",
    species == "sooty_albatross" ~ "Sooty albatross",
    species == "light-mantled_sooty_albatross" ~ "Light-mantled sooty albatross",
    species == "Thalassarche_sp" ~ "Unknown (Thalassarche) mollymawk",
    species == "Phoebetria_sp" ~ "Unknown (Sooty) albatross",
    species == "other_mollymawk" ~ "Unknown mollymawk",
    species == "northern_giant_petrel" ~ "Northern giant petrel",
    species == "southern_giant_petrel" ~ "Southern giant petrel",
    species == "giant_petrel_sp" ~ "Unknown (giant) petrel",
    species == "Antarctic_fulmar" ~ "Antarctic fulmar",
    species == "Cape_pigeon" ~ "Cape petrel",
    species == "Antarctic_petrel" ~ "Antarctic petrel",
    species == "black_petrel" ~ "Black petrel",
    species == "grey_petrel" ~ "Grey petrel",
    species == "white-chinned_petrel" ~ "White-chinned petrel",
    species == "blue_petrel" ~ "Blue petrel",
    species == "other_petrel" ~ "Unknown petrel",
    species == "black-winged_petrel" ~ "Black-winged petrel",
    species == "Cook's_petrel" ~ "Cook's petrel",
    species == "grey-faced_petrel" ~ "Grey-faced petrel",
    species == "mottled_petrel" ~ "Mottled petrel",
    species == "soft-plumaged_petrel" ~ "Soft-plumaged petrel",
    species == "white-headed_petrel" ~ "White-headed petrel",
    species == "gadfly_sp" ~ "Unknown (gadfly) petrel",
    species == "subantarctic_little_shearwater" ~ "Subantarctic little shearwater",
    species == "Buller's_shearwater" ~ "Buller's shearwater",
    species == "flesh-footed_shearwater" ~ "Flesh-footed shearwater",
    species == "sooty_shearwater" ~ "Sooty shearwater",
    species == "fluttering_Hutton's_shearwater" ~ "Fluttering/Hutton's shearwater",
    species == "shearwater_sp" ~ "Unknown shearwater",
    species == "white-faced_storm_petrel" ~ "White-faced storm petrel",
    species == "Kermadec_storm_petrel" ~ "Kermadec storm petrel",
    species == "black-bellied_storm_petrel" ~ "Black-bellied storm petrel",
    species == "white-bellied_storm_petrel" ~ "White-bellied storm petrel",
    species == "grey-backed_storm_petrel" ~ "Grey-backed storm petrel",
    species == "NZ_storm_petrel" ~ "New Zealand storm petrel",
    species == "Wilson's_storm_petrel" ~ "Wilson's storm petrel",
    species == "Fregetta_Pealeornis_sp" ~ "Unknown (Fregetta/Pealeornis) storm petrel",
    species == "fairy_prion" ~ "Fairy prion",
    species == "broad-billed_prion" ~ "Broad-billed prion",
    species == "Antarctic_prion" ~ "Antarctic prion",
    species == "fulmar_prion" ~ "Fulmar prion",
    species == "thin-billed_prion" ~ "Thin-billed prion",
    species == "Salvin's_prion" ~ "Salvin's prion",
    species == "prion_sp" ~ "Unknown prion",
    species == "common_diving_petrel" ~ "Common diving petrel",
    species == "Australasian_gannet" ~ "Australasian gannet",
    species == "Arctic_skua" ~ "Arctic skua",
    species == "pomarine_skua" ~ "Pomarine skua",
    species == "long-tailed_skua" ~ "Long-tailed skua",
    species == "brown_skua" ~ "Brown skua",
    species == "southern_skua" ~ "Brown skua",
    species == "south_polar_skua" ~ "South polar skua",
    species == "skua_sp" ~ "Unknown skua",
    species == "black-backed_gull" ~ "Black-backed gull",
    species == "red-billed_gull" ~ "Red-billed gull",
    species == "black-billed_gull" ~ "Black-billed gull",
    species == "Franklin's_gull" ~ "Franklin's gull",
    species == "gull_sp" ~ "Unknown gull",
    species == "white-fronted_tern" ~ "White-fronted tern",
    species == "white_fronted_tern" ~ "White-fronted tern",
    species == "black-fronted_tern" ~ "Black-fronted tern",
    species == "Caspian_tern" ~ "Caspian tern",
    species == "grey_ternlet" ~ "Grey ternlet",
    species == "antarctic_tern" ~ "Antarctic tern",
    species == "tern_sp" ~ "Unknown tern",
    species == "stewart_island_shag" ~ "Stewart Island shag",
    species == "spotted_shag" ~ "Spotted shag",
    species == "black_shag" ~ "Black shag",
    species == "pied_shag" ~ "Pied shag",
    species == "little_shag" ~ "Little shag",
    species == "shag_sp" ~ "Unknown shag",
    species == "other" & group == "shag" ~ "Unknown shag",  
    species == "blue_penguin" ~ "Blue penguin",
    species == "yellow-eyed_penguin" ~ "Yellow-eyed penguin",
    species == "crested_penguin_spp" ~ "Crested penguin species",
    species == "eastern_rockhopper_penguin" ~ "Eastern rockhopper penguin",
    species == "other_penguin" ~ "Other penguin",
    species == "westland_petrel" ~ "Westland petrel",
    species == "fulmarine_sp" ~ "Unknown (fulmarine) petrel",
    species == "Procellaria_sp" ~ "Unknown (Procellaria) petrel",
    species == "fiordland_crested_penguin" ~ "Fiordland crested penguin",
    TRUE ~ species
   ))
}

#--------------------------------------#
## Adds Scientific names ## --- DOUBLE CHECK THIS, AI helped me out but they got a couple wrong 
#--------------------------------------#
{
  df_birds <-
    df_birds %>%
    dplyr::mutate(scientific_name = dplyr::case_when(
      species == "wandering_royal_complex" ~ "Diomedea exulans or Diomedea antipodensis",
      species == "northern_royal_albatross" ~ "Diomedea sanfordi",
      species == "southern_royal_albatross" ~ "Diomedea epomophora",
      species == "royal_albatross_sp" ~ "Diomedea spp.",
      species == "Diomedea_sp" ~ "Diomedea spp.",
      species == "other_albatross" ~ "Diomedeidae spp.",
      species == "Buller's_mollymawk" ~ "Thalassarche bulleri",
      species == "white-capped_mollymawk" ~ "Thalassarche cauta steadi",
      species == "Salvin's_mollymawk" ~ "Thalassarche salvini",
      species == "black-browed_mollymawk" ~ "Thalassarche melanophris",
      species == "Campbell_mollymawk" ~ "Thalassarche impavida",
      species == "grey-headed_mollymawk" ~ "Thalassarche chrysostoma",
      species == "Chatham_mollymawk" ~ "Thalassarche eremita",
      species == "sooty_albatross" ~ "Phoebetria fusca",
      species == "light-mantled_sooty_albatross" ~ "Phoebetria palpebrata",
      species == "Thalassarche_sp" ~ "Thalassarche spp.",
      species == "Phoebetria_sp" ~ "Phoebetria spp.",
      species == "northern_giant_petrel" ~ "Macronectes halli",
      species == "southern_giant_petrel" ~ "Macronectes giganteus",
      species == "giant_petrel_sp" ~ "Macronectes spp.",
      species == "Antarctic_fulmar" ~ "Fulmarus glacialoides",
      species == "Cape_pigeon" ~ "Daption capense",
      species == "Antarctic_petrel" ~ "Thalassoica antarctica",
      species == "black_petrel" ~ "Procellaria parkinsoni",
      species == "grey_petrel" ~ "Procellaria cinerea",
      species == "white-chinned_petrel" ~ "Procellaria aequinoctialis",
      species == "blue_petrel" ~ "Halobaena caerulea",
      species == "other_petrel" ~ "Procellariidae (petrel) spp.",
      species == "black-winged_petrel" ~ "Pterodroma nigripennis",
      species == "Cook's_petrel" ~ "Pterodroma cookii",
      species == "grey-faced_petrel" ~ "Pterodroma macroptera gouldi",
      species == "mottled_petrel" ~ "Pterodroma inexpectata",
      species == "soft-plumaged_petrel" ~ "Pterodroma mollis",
      species == "white-headed_petrel" ~ "Pterodroma lessonii",
      species == "gadfly_sp" ~ "Pterodroma spp.",
      species == "subantarctic_little_shearwater" ~ "Puffinus elegans",
      species == "Buller's_shearwater" ~ "Ardenna bulleri",
      species == "flesh-footed_shearwater" ~ "Ardenna carneipes",
      species == "sooty_shearwater" ~ "Ardenna grisea",
      species == "fluttering_Hutton's_shearwater" ~ "Puffinus gavia/huttoni",
      species == "shearwater_sp" ~ "Procellariidae (shearwater) spp.",
      species == "white-faced_storm_petrel" ~ "Pelagodroma marina",
      species == "Kermadec_storm_petrel" ~ "Pelagodroma albiclunis",
      species == "black-bellied_storm_petrel" ~ "Fregetta tropica",
      species == "white-bellied_storm_petrel" ~ "Fregetta grallaria",
      species == "grey-backed_storm_petrel" ~ "Garrodia nereis",
      species == "NZ_storm_petrel" ~ "Fregetta maoriana",
      species == "Wilson's_storm_petrel" ~ "Oceanites oceanicus",
      species == "Fregetta_Pealeornis_sp" ~ "Fregetta/Pealeornis spp.",
      species == "fairy_prion" ~ "Pachyptila turtur",
      species == "broad-billed_prion" ~ "Pachyptila vittata",
      species == "Antarctic_prion" ~ "Pachyptila desolata",
      species == "fulmar_prion" ~ "Pachyptila crassirostris",
      species == "thin-billed_prion" ~ "Pachyptila belcheri",
      species == "Salvin's_prion" ~ "Pachyptila salvini",
      species == "prion_sp" ~ "Pachyptila spp.",
      species == "common_diving_petrel" ~ "Pelecanoides urinatrix",
      species == "Australasian_gannet" ~ "Morus serrator",
      species == "Arctic_skua" ~ "Stercorarius parasiticus",
      species == "pomarine_skua" ~ "Stercorarius pomarinus",
      species == "long-tailed_skua" ~ "Stercorarius longicaudus",
      species == "brown_skua" ~ "Stercorarius antarcticus",
      species == "southern_skua" ~ "Stercorarius antarcticus",
      species == "south_polar_skua" ~ "Stercorarius maccormicki",
      species == "skua_sp" ~ "Stercorarius spp.",
      species == "black-backed_gull" ~ "Larus dominicanus",
      species == "red-billed_gull" ~ "Chroicocephalus novaehollandiae scopulinus",
      species == "black-billed_gull" ~ "Chroicocephalus bulleri",
      species == "Franklin's_gull" ~ "Leucophaeus pipixcan",
      species == "gull_sp" ~ "Laridae spp.",
      species == "white-fronted_tern" ~ "Sterna striata",
      species == "white_fronted_tern" ~ "Sterna striata",
      species == "black-fronted_tern" ~ "Chlidonias albostriatus",
      species == "Caspian_tern" ~ "Hydroprogne caspia",
      species == "grey_ternlet" ~ "Procelsterna cerulea",
      species == "antarctic_tern" ~ "Sterna vittata",
      species == "tern_sp" ~ "Sternidae spp.",
      species == "stewart_island_shag" ~ "Leucocarbo chalconotus",
      species == "spotted_shag" ~ "Stictocarbo punctatus",
      species == "black_shag" ~ "Phalacrocorax carbo",
      species == "pied_shag" ~ "Phalacrocorax varius",
      species == "little_shag" ~ "Microcarbo melanoleucos",
      species == "shag_sp" ~ "Phalacrocoracidae spp.",
      species == "other" & group == "shag" ~ "Phalacrocoracidae spp.",  
      species == "blue_penguin" ~ "Eudyptula minor",
      species == "yellow-eyed_penguin" ~ "Megadyptes antipodes",
      species == "crested_penguin_spp" ~ "Eudyptes spp.",
      species == "eastern_rockhopper_penguin" ~ "Eudyptes filholi",
      species == "other_penguin" ~ "Spheniscidae spp.",
      species == "westland_petrel" ~ "Procellaria westlandica",
      species == "fulmarine_sp" ~ "Procellariidae (fulmarine) spp.",
      species == "Procellaria_sp" ~ "Procellaria spp.",
      species == "fiordland_crested_penguin" ~ "Eudyptes pachyrynchus",
      TRUE ~ species
    ))
}

## Check
# sort(unique(data$species)) # --- OK!


df_birds <-
  df_birds %>%
# Add year and month column
  dplyr::mutate(year = as.numeric(stringr::str_sub(date, start = 1, end = -7)),
              month = as.numeric(stringr::str_sub(date, start = 6, end =-4)),
              day = as.numeric(stringr::str_sub(date, start = 9, end =-1))) %>%
# Add season column
  dplyr::mutate(season = dplyr::case_when(
    month < 03 ~ 'Summer',
    month == 12 ~ 'Summer',
    month >= 03 & month < 06 ~ 'Autumn',
    month >= 6 & month < 09 ~ 'Winter',
    month >= 9 & month < 12 ~ 'Spring'
  )) %>%
  # Add month names after seasons
  dplyr::mutate(month = dplyr::case_when(
    month == 01 ~ 'January',
    month == 2 ~ 'February',
    month == 3 ~ 'March',
    month == 4 ~ 'April',
    month == 5 ~ 'May',
    month == 6 ~ 'June',
    month == 7 ~ 'July',
    month == 8 ~ 'August',
    month == 9 ~ 'September',
    month == 10 ~ 'October',
    month == 11 ~ 'November',
    month == 12 ~ 'December')) %>%
# Add period of day column
dplyr::mutate(time_of_day = dplyr::case_when(
  lubridate::hour(time) <= 12 ~ 'morning',
  lubridate::hour(time) >12 ~ 'afternoon'
)) %>% 
  # Replace NA values with 0, and filter them away
  dplyr::mutate(count = tidyr::replace_na(as.numeric(count), 0)) %>% 
  dplyr::filter(! count == 0)

# Remove date and note columns
df_birds <-
  df_birds %>%
  dplyr::select(-c(note))

#--------------------------------------#
## Deletes rows that have id = N/a (i.e. recorded after of before effort ON) v ##
#--------------------------------------#
df_birds <-
  df_birds %>%
  dplyr::mutate(id = tidyr::replace_na(as.numeric(id), 0)) %>% 
  dplyr::filter(! id == 0)

#--------------------------------------#
## Exports the df as .csv ##
#--------------------------------------#

## Move new columns
df_birds <-
  df_birds %>%
  dplyr::relocate(group, .before = age) %>%
  dplyr::relocate(species, .before = age) %>%
  dplyr::relocate(id, year, month, day, season, time_of_day, .before = time) %>%
  dplyr::relocate(species, species_nice_name, group, count,behaviour,distance_band, .before = shag_plumage)


df_birds <-
  df_birds %>%
  arrange(date, year, month, day, time)

write.csv(df_birds, "./data-processed/all_data.csv", row.names = FALSE)
