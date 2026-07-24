##
## EDA
##

## This script is for Exploratory Data Analysis (EDA)
## e.g. summaries and descriptive stats

rm(list = ls())

## Libraries ####
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
library(sf)

## Colour Palette ####
# Jan - #a0ca78 / Light green
# Feb - #429130 / Dark green
# Mar - #96ced3 / Light blue
# Apr - #006eae / Dark blue
# May - #d3a9ce / Light purple
# Jun - #a54891 / Dark purple
# Jul - #e9a0a5 / Light red
# Aug - #c5373d / Red
# Sept - #fbbc7d / light orange
# Oct - #e96900 / Orange
# Nov - #f5db86 / Light yellow
# Dec - #ca9b23 / Gold

palette <- c("#a0ca78", "#429130", "#96ced3", "#006eae", "#d3a9ce", "#a54891", "#e9a0a5", "#c5373d", "#fbbc7d", "#e96900", "#f5db86", "#ca9b23")
palette <- c("#ffc32c", "#D19600", "#FF9E5E", "#e96900", "#C24400", "#CDE7EA", "#96ced3", "#4BABB4", "#a0ca78", "#7EB847", "#4C8514", "#FFD15C")
seas_palette <- c("#ffc32c", "#e96900", "#96ced3", "#a0ca78")
region_palette <- c("#8DD9D2", "#515D91")

# Read data ####

data <- read.csv("~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/data-processed/all_data.csv")

# Formatting columns

data <- data %>%
  #Month
  dplyr::mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>%
  #Season
  dplyr::mutate(season = factor(season, levels = c('Summer', 'Autumn', 'Winter', 'Spring'))) %>%
  #Time of day
  dplyr::mutate(time_of_day = factor(time_of_day, levels = c("morning", "midday", "afternoon")))


#--------------------------------------#
# quick function for counting sightings of aeach species per month
sp_count <-
  data %>%
  dplyr::group_by(species_nice_name, month) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(species = species_nice_name)
write.csv(sp_count, "./data-processed/species_count.csv", row.names = FALSE)

##  ---- Transform the data set from long to (simplified) wide format  ---- ##
data_wide <-
  data %>%
  tidyr::pivot_wider(names_from = species_nice_name,
                     values_from = count,
                     values_fill = 0)

# Get spp and sp-only column names
spp_cols <- colnames(data_wide[,c(24:ncol(data_wide))]) ## All seabirds
sp_only_cols <- spp_cols[! grepl(pattern = "Unknown", x = spp_cols)] ## Only species-level

# Calculate total number of birds per sample (total_birds)
data_wide <- 
  data_wide %>% 
  dplyr::mutate(total_birds = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::select(id, season, year, month, all_of(sp_only_cols), total_birds) %>% 
  dplyr::group_by(id, season, year, month) %>% 
  dplyr::summarise(across(everything(), list(sum)))

colnames(data_wide) <- c("id", "season", "year", "month", sp_only_cols, "total_birds")


## Summary of Each Effort
#--------------------------------------#

effort_summary <-
  data_wide %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(number_of_voyages = n_distinct(id),
                   number_of_species = sum(colSums(across(all_of(sp_only_cols))) > 0),
                   number_of_individuals = sum(total_birds))



# write.csv(effort_summary,
#           "./results/seasonal-effort-summary.csv",
#           row.names = FALSE)

rm("effort_summary")

# Get species names and number of occurrences
num_occ_all <- 
  # Get species names and number of occurrences
  data.frame(
    species = sp_only_cols,
    num_occ = apply(data_wide[sp_only_cols], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL)

rm(birds_today, group_list, species_list, num_occ_all)



## Summaries ####
#--------------------------------------#

# Print per-species breakdown for today
date_today <- Sys.Date()

birds_today <-
  data %>%
  filter(date == date_today, !is.na(species)) %>%
  group_by(species_nice_name) %>%
  summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_count))

print(birds_today)

## --- Creates a list of species seen and how many individuals --- ##

species_list <- 
  data %>%
  dplyr::group_by(species_nice_name, scientific_name, group) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(total_count))

print(species_list, n = 200)
write.csv(species_list, "./data-processed/species_list.csv", row.names = FALSE)

group_list <- 
  data %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(desc(total_count))

print(group_list, n = 200)
write.csv(group_list, "./data-processed/group_list.csv", row.names = FALSE)

# quick function for counting sightings of aeach species per month
sp_count <-
  data %>%
  dplyr::group_by(species_nice_name, month) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(species = species_nice_name)
write.csv(sp_count, "./data-processed/species_count.csv", row.names = FALSE)

##  ---- Transform the data set from long to (simplified) wide format  ---- ##
data_wide <-
  data %>%
  tidyr::pivot_wider(names_from = species_nice_name,
                     values_from = count,
                     values_fill = 0)

# Get spp and sp-only column names
spp_cols <- colnames(data_wide[,c(24:ncol(data_wide))]) ## All seabirds
sp_only_cols <- spp_cols[! grepl(pattern = "Unknown", x = spp_cols)] ## Only species-level

# Calculate total number of birds per sample (total_birds)
data_wide <- 
  data_wide %>% 
  dplyr::mutate(total_birds = rowSums(across(all_of(spp_cols)))) %>% 
  dplyr::select(id, season, year, month, all_of(sp_only_cols), total_birds) %>% 
  dplyr::group_by(id, season, year, month) %>% 
  dplyr::summarise(across(everything(), list(sum)))

colnames(data_wide) <- c("id", "season", "year", "month", sp_only_cols, "total_birds")


## Summary of Each Effort
#--------------------------------------#

effort_summary <-
  data_wide %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(number_of_voyages = n_distinct(id),
                   number_of_species = sum(colSums(across(all_of(sp_only_cols))) > 0),
                   number_of_individuals = sum(total_birds))



# write.csv(effort_summary,
#           "./results/seasonal-effort-summary.csv",
#           row.names = FALSE)

rm("effort_summary")

# Get species names and number of occurrences
num_occ_all <- 
  # Get species names and number of occurrences
  data.frame(
    species = sp_only_cols,
    num_occ = apply(data_wide[sp_only_cols], MARGIN = 2, function(x) sum(x >= 1)),
    row.names = NULL)

rm(birds_today, group_list, species_list, num_occ_all)



## Summaries of number of species, total number of birds, and maximum group sizes by month (overall) ####
#------------------------------------------------------------------------------#

## SPECIES RICHNESS
num_spp <- data %>%
  dplyr::filter(species_nice_name %in% sp_only_cols) %>%
  dplyr::group_by(id, region_mainland, region_islands, month, season) %>%
  dplyr::summarise(n = n_distinct(species_nice_name))

overall_mean_num_spp <- floor(mean(num_spp$n))                   

## TOTAL NUMBER OF BIRDS
num_birds_month <-
  data %>%
  dplyr::filter(species_nice_name %in% sp_only_cols) %>%
  dplyr::group_by(id, region_mainland, region_islands,  month, season) %>%
  dplyr::summarise(n = sum(count)) %>%
  dplyr::mutate(log10_n = log10(n))

overall_log10mean_num_birds_month <- round(mean(num_birds_month$log10_n), digits = 2)

## PLOT 1: Species richness --
gg_num_spp <-
  ggplot(data = num_spp, 
         aes(x = month, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
gg_num_spp

ggsave(gg_num_spp,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summaries/gg_num_spp.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 2: Number of birds  --
gg_num_birds_month <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_num_birds_month

ggsave(gg_num_birds_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summaries/gg_num_birds_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 3: maximum group size \n(log10)

gg_max_group_size_month <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = log10_n, fill = month)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_max_group_size_month

ggsave(gg_num_birds_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summaries/gg_max_group_size_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)


## OVERALL NUM BIRDS PER MONTH

raw_num_birds_month <-
  data %>%
  dplyr::filter(species_nice_name %in% sp_only_cols) %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(n = sum(count))

overall_raw_num_birds_month <- floor(mean(raw_num_birds_month$n))    

## PLOT 4: Number of birds overall per month
gg_raw_num_birds_month <- 
  ggplot(data = raw_num_birds_month, aes(x = month, y = n)) +
  geom_bar(aes(fill = month), stat = "identity", size = 4) +
  geom_line(aes(group = 1), linewidth = 1) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  xlab("") + ylab("Number of birds") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        legend.background = element_rect(colour = "black"))

gg_raw_num_birds_month

ggsave(gg_raw_num_birds_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summaries/gg_raw_num_birds_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## Summaries of Number of species by MONTH and REGION ####

## PLOT 5A: Species richness by month --
sp_richness_region_mainland_by_month <-
  ggplot(data = num_spp, 
         aes(x = month, y = n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
sp_richness_region_mainland_by_month

ggsave(sp_richness_region_mainland_by_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/sp_richness_region_mainland_by_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 5B: Species richness by region --
sp_richness_region_mainland_by_region <-
  ggplot(data = num_spp, 
         aes(x = region_mainland, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
sp_richness_region_mainland_by_region

ggsave(sp_richness_region_mainland_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/sp_richness_region_mainland_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 5C: Species richness by month factoring islands --
sp_richness_region_islands_by_month <-
  ggplot(data = num_spp, 
         aes(x = month, y = n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
sp_richness_region_islands_by_month

ggsave(sp_richness_region_islands_by_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/sp_richness_region_islands_by_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 5D: Species richness by region factoring islands --
sp_richness_region_islands_by_region <-
  ggplot(data = num_spp, 
         aes(x = region_islands, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
sp_richness_region_islands_by_region

ggsave(sp_richness_region_islands_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/sp_richness_region_islands_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# ---------------------------------------------------------------------- #

## --Stitched together for comparison -- ##

gg_spprichness_month_region <-
  (sp_richness_region_mainland_by_month | sp_richness_region_islands_by_month) / (sp_richness_region_mainland_by_region | sp_richness_region_islands_by_region) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
gg_spprichness_month_region

ggsave(gg_spprichness_month_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/sp_richness_by_month_and_region.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

rm(gg_max_group_size_month, gg_num_birds_month, gg_num_spp, gg_raw_num_birds_month, gg_spprichness_month_region, sp_richness_region_islands_by_month, sp_richness_region_islands_by_region, sp_richness_region_mainland_by_month, sp_richness_region_mainland_by_region)

## Summaries of Total number of birds by MONTH and REGION ####
## PLOT 6A: Number of birds by month --
gg_num_birds_region_mainland_by_month <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

gg_num_birds_region_mainland_by_month

ggsave(gg_num_birds_region_mainland_by_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/gg_num_birds_region_mainland_by_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 6B: Number of birds by region --
gg_num_birds_region_mainland_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_mainland, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

gg_num_birds_region_mainland_by_region

ggsave(gg_num_birds_region_mainland_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/gg_num_birds_region_mainland_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 6C: Number of birds by month factoring islands --
gg_num_birds_region_islands_by_month <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_num_birds_region_islands_by_month

ggsave(gg_num_birds_region_islands_by_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/gg_num_birds_region_islands_by_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 6D: Number of birds by region factoring islands --
gg_num_birds_region_islands_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_islands, y = n, fill = month)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_num_birds_region_islands_by_region

ggsave(gg_num_birds_region_islands_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/gg_num_birds_region_islands_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# ---------------------------------------------------------------------- #

## --Stitched together for comparison -- ##

gg_num_birds_month_region <-
  (gg_num_birds_region_mainland_by_month | gg_num_birds_region_islands_by_month) / ( gg_num_birds_region_mainland_by_region | gg_num_birds_region_islands_by_region) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
gg_num_birds_month_region

ggsave(gg_num_birds_month_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/total_number_of_birds_by_month_and_region.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

rm(gg_num_birds_region_mainland_by_region, gg_num_birds_region_mainland_by_month, gg_num_birds_region_islands_by_region, gg_num_birds_region_islands_by_month, gg_num_birds_month_region)

## Summaries of Maximum group sizes by MONTH and REGION ####

# PLOT 7A: maximum group size by month \n(log10)

gg_max_group_size_region_by_month <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = log10_n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +
  scale_fill_manual(values = region_palette, name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

gg_max_group_size_region_by_month

ggsave(gg_max_group_size_region_by_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/gg_max_group_size_region_by_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 7B: maximum group size by region \n(log10)

gg_max_group_size_region_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_mainland, y = log10_n, fill = month)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

gg_max_group_size_region_by_region

ggsave(gg_max_group_size_region_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/gg_max_group_size_region_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 7C: maximum group size by month factoring islands \n(log10)

gg_max_group_size_region_by_month_islands <- 
  ggplot(data = num_birds_month,
         aes(x = month, y = log10_n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +
  scale_fill_manual(values = region_palette, name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_max_group_size_region_by_month_islands

ggsave(gg_max_group_size_region_by_month_islands,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/gg_max_group_size_region_by_month_islands.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 7D: maximum group size by region \n(log10)

gg_max_group_size_region_by_region_islands <- 
  ggplot(data = num_birds_month,
         aes(x = region_islands, y = log10_n, fill = month)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

gg_max_group_size_region_by_region_islands

ggsave(gg_max_group_size_region_by_region_islands,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/gg_max_group_size_region_by_region_islands.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

# ---------------------------------------------------------------------- #

## --Stitched together for comparison -- ##

gg_max_group_sizes_month_region <-
  (gg_max_group_size_region_by_month | gg_max_group_size_region_by_month_islands) / (gg_max_group_size_region_by_region | gg_max_group_size_region_by_region_islands) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
gg_max_group_sizes_month_region

ggsave(gg_max_group_sizes_month_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/max_group_size_by_region_and_month.jpg",
       width = 25, height = 20, units = "cm", dpi = 300)

rm(gg_max_group_size_region_by_month, gg_max_group_size_region_by_month_islands, gg_max_group_size_region_by_region, gg_max_group_size_region_by_region_islands, gg_max_group_sizes_month_region)


## Summaries of Number of species by SEASON and REGION ####

## PLOT 8A: Species richness by season --
seas_sp_richness_region_mainland_by_season <-
  ggplot(data = num_spp, 
         aes(x = season, y = n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
seas_sp_richness_region_mainland_by_season

ggsave(seas_sp_richness_region_mainland_by_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/seas_sp_richness_region_mainland_by_season.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 8B: Species richness by region --
seas_sp_richness_region_mainland_by_region <-
  ggplot(data = num_spp, 
         aes(x = region_mainland, y = n, fill = season)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = seas_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
seas_sp_richness_region_mainland_by_region

ggsave(seas_sp_richness_region_mainland_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/seas_sp_richness_region_mainland_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 8C: Species richness by season factoring islands --
seas_sp_richness_region_islands_by_season <-
  ggplot(data = num_spp, 
         aes(x = season, y = n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
seas_sp_richness_region_islands_by_season

ggsave(seas_sp_richness_region_islands_by_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/seas_sp_richness_region_islands_by_season.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 8D: Species richness by region factoring islands --
seas_sp_richness_region_islands_by_region <-
  ggplot(data = num_spp, 
         aes(x = region_islands, y = n, fill = season)) +
  geom_boxplot(width = 0.6) +
  scale_fill_manual(values = seas_palette, 
                    name = NULL) +
  geom_hline(yintercept = overall_mean_num_spp,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Number of species per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")
seas_sp_richness_region_islands_by_region

ggsave(seas_sp_richness_region_islands_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/species_richness/seas_sp_richness_region_islands_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## --Stitched together for comparison -- ##

gg_spprichness_seas_region <-
  (seas_sp_richness_region_mainland_by_season | seas_sp_richness_region_islands_by_season) / (seas_sp_richness_region_mainland_by_region | seas_sp_richness_region_islands_by_region) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
gg_spprichness_seas_region

ggsave(gg_spprichness_seas_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/sp_richness_by_season_and_region.jpg.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

rm(gg_spprichness_seas_region, seas_sp_richness_region_islands_by_region, seas_sp_richness_region_islands_by_season, seas_sp_richness_region_mainland_by_region, seas_sp_richness_region_mainland_by_season)

## Summaries of Total number of birds by SEASON and REGION ####
## PLOT 9A: Number of birds by season --
seas_gg_num_birds_region_mainland_by_season <- 
  ggplot(data = num_birds_month,
         aes(x = season, y = n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

seas_gg_num_birds_region_mainland_by_season

ggsave(seas_gg_num_birds_region_mainland_by_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/seas_gg_num_birds_region_mainland_by_season.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 9B: Number of birds by region --
seas_gg_num_birds_region_mainland_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_mainland, y = n, fill = season)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = seas_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

seas_gg_num_birds_region_mainland_by_region

ggsave(seas_gg_num_birds_region_mainland_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/seas_gg_num_birds_region_mainland_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 9C: Number of birds by season factoring islands --
seas_gg_num_birds_region_islands_by_season <- 
  ggplot(data = num_birds_month,
         aes(x = season, y = n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = region_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

seas_gg_num_birds_region_islands_by_season

ggsave(seas_gg_num_birds_region_islands_by_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/seas_gg_num_birds_region_islands_by_season.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## PLOT 9D: Number of birds by region factoring islands --
seas_gg_num_birds_region_islands_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_islands, y = n, fill = season)) +
  geom_boxplot(width = 0.6) +
  scale_y_log10(
    breaks = c(1, 5, 10, 50, 100, 500, 1000,1500,2000),
    labels = scales::comma
  ) +
  scale_fill_manual(values = seas_palette, 
                    name = NULL) +
  
  xlab("") + ylab("Number of birds per effort") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

seas_gg_num_birds_region_islands_by_region

ggsave(seas_gg_num_birds_region_islands_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/total_number_of_birds/seas_gg_num_birds_region_islands_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)
# ---------------------------------------------------------------------- #

## --Stitched together for comparison -- ##

num_birds_seas_region <-
  (seas_gg_num_birds_region_mainland_by_season | seas_gg_num_birds_region_islands_by_season) / (seas_gg_num_birds_region_mainland_by_region | seas_gg_num_birds_region_islands_by_region) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
num_birds_seas_region

ggsave(num_birds_seas_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/total_number_of_birds_by_season_and_region.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

rm(num_birds_seas_region, seas_gg_num_birds_region_islands_by_region, seas_gg_num_birds_region_islands_by_season, seas_gg_num_birds_region_mainland_by_region, seas_gg_num_birds_region_mainland_by_season)

## Summaries of Maximum group sizes by SEASON and REGION ####

# PLOT 10A: maximum group size by season \n(log10)

seas_gg_max_group_size_region_by_season <- 
  ggplot(data = num_birds_month,
         aes(x = season, y = log10_n, fill = region_mainland)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = region_palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

seas_gg_max_group_size_region_by_season

ggsave(seas_gg_max_group_size_region_by_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/seas_gg_max_group_size_region_by_season.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 10B: maximum group size by region \n(log10)

seas_gg_max_group_size_region_by_region <- 
  ggplot(data = num_birds_month,
         aes(x = region_mainland, y = log10_n, fill = season)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = seas_palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

seas_gg_max_group_size_region_by_region

ggsave(seas_gg_max_group_size_region_by_region,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/seas_gg_max_group_size_region_by_region.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 10C: maximum group size by season factoring islands \n(log10)

seas_gg_max_group_size_region_by_season_islands <- 
  ggplot(data = num_birds_month,
         aes(x = season, y = log10_n, fill = region_islands)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = region_palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

seas_gg_max_group_size_region_by_season_islands

ggsave(seas_gg_max_group_size_region_by_season_islands,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/maximum_group_size/seas_gg_max_group_size_region_by_season_islands.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

# PLOT 10D: maximum group size by region \n(log10)

seas_gg_max_group_size_region_by_region_islands <- 
  ggplot(data = num_birds_month,
         aes(x = region_islands, y = log10_n, fill = season)) +
  geom_boxplot(width = 0.6) +
  geom_hline(yintercept = overall_log10mean_num_birds_month, linetype = "longdash", colour = "grey50") +  scale_fill_manual(values = seas_palette, 
                                                                                                                            name = NULL) +
  geom_hline(yintercept = overall_log10mean_num_birds_month,
             linetype = "longdash", colour = "grey50") +
  xlab("") + ylab("Maximum group size\n(log10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none")

seas_gg_max_group_size_region_by_region_islands

ggsave(seas_gg_max_group_size_region_by_region_islands,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/seas_gg_max_group_size_region_by_region_islands.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

## --------------------------------------------------------------- ##

## --Stitched together for comparison -- ##

max_group_size_by_region_and_season <-
  (seas_gg_max_group_size_region_by_season | seas_gg_max_group_size_region_by_season_islands) / (seas_gg_max_group_size_region_by_region |seas_gg_max_group_size_region_by_region_islands ) +
  patchwork::plot_annotation(tag_levels = c('A', '1'),
                             tag_sep = '.',
                             title = "Coastal vs. offshore species richness")
max_group_size_by_region_and_season

ggsave(max_group_size_by_region_and_season,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summary_comparisons/max_group_size_by_region_and_season.jpg",
       width = 32, height = 24, units = "cm", dpi = 300)

rm(max_group_size_by_region_and_season, seas_gg_max_group_size_region_by_region, seas_gg_max_group_size_region_by_region_islands, seas_gg_max_group_size_region_by_season, seas_gg_max_group_size_region_by_season_islands)
