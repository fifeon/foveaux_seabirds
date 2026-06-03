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
                     values_fill = 0,
                     values_fn= sum)

# Get spp and sp-only column names
spp_cols <- colnames(data_wide[,c(26:ncol(data_wide))]) ## All seabirds
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



write.csv(effort_summary,
           "./results/seasonal-effort-summary.csv",
           row.names = FALSE)

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
  theme(axis.text.x = element_text(size = 10, colour = "black", vjust = 1, hjust = 1, angle = 45),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        legend.background = element_rect(colour = "black"))

gg_raw_num_birds_month

ggsave(gg_raw_num_birds_month,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/summaries/gg_raw_num_birds_month.jpg",
       width = 16, height = 12.5, units = "cm", dpi = 300)

rm(gg_max_group_size_month, gg_num_birds_month, gg_num_spp, gg_raw_num_birds_month, overall_log10mean_num_birds_month, overall_mean_num_spp, overall_raw_num_birds_month)

## Frequency of occurrence and numeric frequency, by MONTH --- ####
# ---------------------------------------------------------------- #

# specify functions to calculate frequency of occurrence (freq_occ) and numeric frequency (freq_num)
funs <- list(freq_occ = ~ sum(.x >= 1)/n() *100,
             freq_num = ~ sum(.x)/sum(dplyr::pick(total_birds)) *100)

data_species_fo_nf  <-
  data_wide %>% 
  dplyr::group_by(month) %>%
  dplyr::summarise(across(all_of(sp_only_cols), .fns = funs)) %>%
  tidyr::pivot_longer(cols = !month, 
                      names_to = "species_freq",
                      values_to = "value") %>%
  dplyr::mutate(value = round(value, digits = 2)) %>% 
  # split name into variables
  tidyr::separate(species_freq, 
                  into = c("species", "freq"),
                  sep = -8) %>% 
  # remove an extra underline
  dplyr::mutate(species = stringr::str_sub(species, end = -2))

  # quick function for getting occurrence of each species per month
  sp_freq_occ <-
    data_wide %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(across(all_of(sp_only_cols), 
                            ~ sum(.x >= 1) / n() * 100)) %>%
    tidyr::pivot_longer(cols = -month,
                        names_to = "species",
                        values_to = "freq_occ")
  write.csv(sp_count, "./data-processed/species_freq_occ.csv", row.names = FALSE)
  
## Freq Occ / Num Freq Plots ##
## --------------------------##

# Order months in df for plotting
data_species_fo_nf <-
  data_species_fo_nf %>%
  dplyr::mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

# Calculate overall ordering (mean across all months)
species_order <- data_species_fo_nf %>%
  dplyr::filter(freq == "freq_occ") %>%  # or "freq_num" - choose one
  dplyr::group_by(species) %>%
  dplyr::summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  dplyr::arrange(mean_value) %>%
  dplyr::pull(species)

# Apply this order to both plots
data_species_fo_nf <- data_species_fo_nf %>%
  dplyr::mutate(species = factor(species, levels = species_order))

## Frequency of occurrence
plot_freq_occ <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_occ") %>% 
  ggplot(., aes(x = species,  # Just use species directly now
                y = value, 
                fill = month)) + 
  geom_col() +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  facet_grid(~ month) +
  ylab("Frequency of occurrence (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))
plot_freq_occ

## Relative abundance
plot_freq_num <-
  data_species_fo_nf %>% 
  dplyr::filter(freq == "freq_num") %>% 
  ggplot(., aes(x = species,  # Just use species directly now
                y = value, 
                fill = month)) + 
  geom_col() +
  scale_fill_manual(values = palette, 
                    name = NULL) +
  facet_grid(~ month) +
  ylab("Relative abundance (%)") + xlab ("") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        axis.title.x = element_text(size = 8),
        strip.text = element_text(size = 8))
plot_freq_num

## Patchwork these plots and save it
#freqs_occ_num <-
 # plot_freq_occ / plot_freq_num +
 #patchwork::plot_annotation(tag_levels = "A")
#freqs_occ_num

#ggsave(freqs_occ_num,
      # filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/EDA_sp_frqs-occ-num-month.jpeg",
      # width = 16, height = 25, units = "cm", dpi = 300)

## Replot with a grid to fit reports ### ALTHOUGHT It'll probably just be grouped by season in the end.
plot_freq_occ_grid <-
  data_species_fo_nf %>% 
    dplyr::filter(freq == "freq_occ") %>% 
    ggplot(., aes(x = species,  # Just use species directly now
                  y = value, 
                  fill = month)) + 
    geom_col() +
    scale_fill_manual(values = palette, 
                      name = NULL) +
    facet_wrap(~ month,
               nrow = 4, ncol = 3) +
    ylab("Frequency of occurrence (%)") + xlab ("") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 7, colour = "black"),
          axis.text.y = element_text(size = 6, colour = "black"),
          axis.title.x = element_text(size = 8),
          strip.text = element_text(size = 8))
  #plot_freq_occ_grid
  
## Frequency of occurrence and numeric frequency, by SEASON ####
  
  seas_data_species_fo_nf <-
    data_wide %>% 
    dplyr::group_by(season) %>%
    dplyr::summarise(
      across(all_of(sp_only_cols), .fns = funs),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = -season, 
      names_to = "species_freq",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = round(value, digits = 2)) %>% 
    tidyr::separate(
      species_freq, 
      into = c("species", "freq"),
      sep = -8
    ) %>% 
    dplyr::mutate(
      species = stringr::str_sub(species, end = -2)
    ) %>%
    # Global ordering of species (shared across all seasons) #
  dplyr::group_by(species) %>%
    dplyr::mutate(mean_value = mean(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      species = reorder(species, mean_value)
    )
  
  # Order seasons in df for plotting
  seas_data_species_fo_nf <-
    seas_data_species_fo_nf %>%
    dplyr::mutate(season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring")))
  
  # Seasonal freq occ
  plot_seas_freq_occ <-
    seas_data_species_fo_nf %>% 
    dplyr::filter(freq == "freq_occ") %>% 
    ggplot(., aes(x = species,
                  y = value, 
                  fill = season)) + 
    geom_col() +
    scale_fill_manual(values = seas_palette, 
                      name = NULL) +
    facet_grid(~ season) +
    ylab("Frequency of occurrence (%)") + xlab ("") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 7, colour = "black"),
          axis.text.y = element_text(size = 6, colour = "black"),
          axis.title.x = element_text(size = 8),
          strip.text = element_text(size = 8))
  plot_seas_freq_occ
  
  ggsave(plot_seas_freq_occ,
         filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/EDA_sp_frqs-occ-num-season.jpg",
         width = 16, height = 12.5, units = "cm", dpi = 300)
  
  rm("plot_freq_occ_grid", "plot_freq_occ", "plot_freq_num", "freqs_occ_num", "data_species_fo_nf")
  
  ## Seasonal relative abundance
  plot_freq_num <-
    seas_data_species_fo_nf %>% 
    dplyr::filter(freq == "freq_num") %>% 
    ggplot(., aes(x = species,  # Just use species directly now
                  y = value, 
                  fill = season)) + 
    geom_col() +
    scale_fill_manual(values = seas_palette, 
                      name = NULL) +
    facet_grid(~ season) +
    ylab("Relative abundance (%)") + xlab ("") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "none",
          axis.text = element_text(size = 7, colour = "black"),
          axis.text.y = element_text(size = 6, colour = "black"),
          axis.title.x = element_text(size = 8),
          strip.text = element_text(size = 8))
  plot_freq_num
  ggsave(plot_freq_num,
         filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/plot_freq_num.jpg",
         width = 16, height = 12.5, units = "cm", dpi = 300)
  
  
  
rm(funs, num_bird_month, num_spp, raw_num_birds_month, seas_data_species_fo_nf, sp_freq_occ, gg_num_birds_month, gg_num_spp, gg_raw_num_birds_month, overall_log10mean_num_birds_month, overall_mean_num_spp_season, overall_raw_num_birds_month, plot_freq_num, plot_seas_freq_occ)


## Circular Stats ####

sp_abun <- read.csv("~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/data-processed/species_count.csv")
sp_abun <-
  sp_abun %>%
    dplyr::mutate(month = factor(month, levels = c("January", "February", "March", "April", 
                                                   "May", "June", "July", "August", 
                                                   "September", "October", "November", "December")))

## Statistics for abundance
{
  abundance_summary <- sp_abun %>%
  dplyr::select(species, month) %>%
  split(.$species) %>%
  purrr::map(pull, month)
  abundance_summary
    }

# Add months for when there were no sightings to create circular column plot with every month
sp_abun <-
  sp_abun %>%
  tidyr::complete(species, month, fill = list(total_count = 0)) %>%
  tidyr::replace_na(list(total_count = 0)) %>%
  dplyr::mutate(month_abbr = factor(month.abb[as.integer(month)], 
                                    levels = month.abb)) %>%
  dplyr::mutate(month_num = as.integer(month))

{
  abundance_means <- sp_abun %>%
    dplyr::filter(total_count > 0) %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(
      mean_month_num = as.numeric(
        mean(circular(as.integer(month) * (2 * pi / 12))) * 12 / (2 * pi)
      ) %% 12,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mean_month_num = round(mean_month_num),
      mean_month_num = ifelse(mean_month_num == 0, 12, mean_month_num)
    ) %>%
    dplyr::mutate(
      mean_month = dplyr::if_else(
        is.na(mean_month_num), 
        NA_character_, 
        month.name[mean_month_num]
      )
    )
  }

# Defining a function to create a circular column plot
circ_abun_plot <- function(data, sp, x, y, fill) {
  mean_abundance_time <-
  data %>%
    dplyr::filter(species == sp) %>%
    ggplot(aes({{x}}, {{y}})) +
    geom_col(width = 1, colour = "white", fill = fill, linewidth = 0.4) +
    geom_text(aes(label = {{y}}), 
              position = position_stack(vjust = 1.05),
              size = 3, colour = "grey30") +
    scale_y_continuous() +
    coord_polar() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 8, colour = "black"),
          panel.grid.major = element_line(colour = "grey90", linewidth = 0.3))
        #+ geom_vline(xintercept = abundance_means$mean_month_num[abundance_means$species == sp],linewidth = 1)
}

## -- CIRCULAR PLOTS -- ##
theme_set(theme_minimal() + theme(panel.grid.minor = element_blank()))

## -- Diving Petrel -- ##
diving_petrel_circ_abun <- circ_abun_plot(sp_abun, "common_diving_petrel", month_abbr, total_count, "#FF674D")
diving_petrel_circ_abun #Create plot


### Circular plot 12x for most commonly seen spp. ####
sp_abun_with_means <- sp_abun %>%
  dplyr::left_join(abundance_means %>% 
                     dplyr::select(species, mean_month_num), 
                   by = "species") %>%
  dplyr::filter(species %in% c("Common diving petrel","Stewart Island shag","White-fronted tern", "Sooty shearwater", "White-capped mollymawk", 'Red-billed gull', "Buller's mollymawk", "Black-backed gull", "Blue penguin", "Spotted shag", "Pied shag", "Cape petrel"))

breeding_times <- data.frame(
  species = c("Common diving petrel", "Sooty shearwater"),
  breed_start = c(8, 11),  # month numbers
  breed_end = c(12, 5)
)

sp_abun_with_means <- sp_abun_with_means %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(sp_max = max(total_count, na.rm = TRUE)) %>%
  dplyr::ungroup()  %>%
  dplyr::left_join(breeding_times, by = "species")

testplot <-
  sp_abun_with_means %>%
  ggplot(aes(month_abbr, total_count, fill = species)) +
  geom_col(width = 1, colour = "white", linewidth = 0.4, alpha = 0.8) +
  geom_text(aes(label = total_count),   position = position_stack(vjust = 1.15), size = 2.2, colour = "black") +
  scale_y_continuous() +
  facet_wrap(~species, scales = "free_y") +
  coord_polar() +
  #+ geom_vline(aes(xintercept = mean_month_num), linewidth = 1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = 'bold', size = 9, colour = "black"),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3))

testplot
testplot +
  geom_rect(aes(xmin = breed_start, xmax = breed_end,
                ymin = sp_max * 0.55,
                ymax = sp_max * 0.85),
            fill = "#91C1D9", alpha = 0.5)

ggsave(testplot,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/test_circ.svg",
       width = 30, height = 25, units = "cm", dpi = 300)




## CIRCULAR PLOT for % of individuals observed per month within each species
sp_abun_pct <- sp_abun %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(
    species_total = sum(total_count, na.rm = TRUE),
    pct = round((total_count / species_total) * 100, digits = 1)
  ) %>%
  dplyr::ungroup()

sp_abun_pct_filtered <- sp_abun_pct %>%
  dplyr::filter(species %in% c("Common diving petrel", 
                               "Sooty shearwater", 
                               "Buller's mollymawk")) %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(max_pct = max(pct, na.rm = TRUE)) %>%
  dplyr::ungroup()

circ_pct_plot <-
  sp_abun_pct_filtered %>%
  ggplot(aes(month_abbr, pct, fill = species)) +
  geom_col(width = 1, colour = "white", linewidth = 0.1, alpha = 1) +
  geom_text(aes(y = max_pct * 1.2,
                label = paste0(pct, "%")),
            size = 2.8, colour = "grey30") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~ species, scales = "free_y") +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 9, colour = "black"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    legend.position = "none"
  )
circ_pct_plot

ggsave(circ_pct_plot,
       filename = "~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/results/circ_abundances.svg",
       width = 30, height = 25, units = "cm", dpi = 300)
