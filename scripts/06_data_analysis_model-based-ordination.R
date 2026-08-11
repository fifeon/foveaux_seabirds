##
## Model-based ordinations
##
## ------------------------------------- ##

## Script runs model-based ordinations using the {gllvm} framework

## ------------------------------------- ##

## Libraries ##
library(gllvm)
library(dplyr)
library(ggExtra)
library(ggplot2)


rm(list = ls())

data <- read.csv("./data-processed/all_data.csv")


## Create a continuous version of 'distance_offshore' & 'foveaux_south'####
data <-
  data %>%
  dplyr::mutate(dist = case_when(
    distance_offshore == "DistOffshore0.4km" ~ 2,
    distance_offshore == "DistOffshore4.8km" ~ 6,
    distance_offshore == "DistOffshore8.12km" ~ 10,
    distance_offshore == "DistOffshore12.16km" ~ 14,
    .default = TRUE
  ), .after = distance_offshore) %>%
  dplyr::mutate(segment = case_when(
    foveaux_south == "FoveauxSouth0-5km" ~ 2.5,
    foveaux_south == "FoveauxSouth5-10km" ~ 7.5,
    foveaux_south == "FoveauxSouth10-15km" ~ 12.5,
    foveaux_south == "FoveauxSouth15-20km" ~ 17.5,
    foveaux_south == "FoveauxSouth20-25km" ~ 22.5,
    foveaux_south == "FoveauxSouth25-30km" ~ 27.5,
    foveaux_south == "FoveauxSouth30-35km" ~ 32.5,
    .default = TRUE
  ), .after = dist)
  
## Format some columns
data$distance_offshore <-
  factor(data$distance_offshore, levels = c("DistOffshore0.4km",
                                           "DistOffshore4.8km",
                                           "DistOffshore8.12km",
                                           "DistOffshore12.16km"),
         labels = c('0-4 km', '4-8 km', '8-12 km', '12-16 km'))

data$foveaux_south <-
  factor(data$foveaux_south, levels = c("FoveauxSouth0-5km",
                                        "FoveauxSouth5-10km",
                                        "FoveauxSouth10-15km",
                                        "FoveauxSouth15-20km",
                                        "FoveauxSouth20-25km",
                                        "FoveauxSouth25-30km",
                                        "FoveauxSouth30-35km"))

data$season <-
  factor(data$season,
         levels = c("Summer", "Autumn", "Winter", "Spring"))

# Round Wind Speed to nearest whole value
data <-
  data %>%
  dplyr::mutate(wind_speed = round(wind_speed/1)*1)

data$count <- as.numeric(data$count)

## Prepare data for modelling
spp_cols_all <- unique(data$species)
sp_cols_only <- spp_cols_all[! grepl("unknown", spp_cols_all)]

## First, transform it from long to wide format
data_wide <-
  data %>%
  # need to delete this column becuase it otherwise messes up with the 'pivot_wide' results
  dplyr::select(-species_nice_name) %>%
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0)
  # Nico matched up SST/SALINITY by modelling only on way back - consider maybe for later?

## Second, identify 'rare' species (i.e. less than 3 occurrences)

sp_rare_cols <-
  #gets species names and number of occurrences
  data.frame(
    species = sp_cols_only,
    n_occ = apply(data_wide[sp_cols_only], MARGIN = 2, function(x) sum(x>=1)),
    row.names = NULL) %>%
  # Filters and pulls species names
  dplyr::filter(n_occ <3) %>%
  dplyr::pull(species)

# Create vector of filtered cols
sp_cols_filtered <- 
  sp_cols_only[!sp_cols_only %in% sp_rare_cols]

## Get Seabird data
data_wide <-
  data %>%
  # Select species rows
  dplyr::filter(species %in% sp_cols_filtered) %>%
  dplyr::group_by(id, distance_offshore, foveaux_south, season, time_of_day, swell, beaufort, species, sst, wind_speed, wind_direction) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::mutate(sst = mean(sst, na.rm = TRUE)) # Ask nico about this in more detail

data_wide <-
  data_wide %>%
  tidyr::pivot_wider(names_from = species,
                     values_from = count,
                     values_fill = 0)

#cols 8-39
spp_matrix <-
  data_wide[, c(11:42)]

# GLLVM, purely biological (a NULL model) ####

### Describes the model log likelihood, residual dofs, AIC, AICc, BIC
# default printout includes information criteria

# Test to see if negative binomial or poisson fits better
#fitp <- gllvm(spp_matrix, family = poisson())
#fitp

### Run NULL models with 1 and 2 LV, respectively

# if I leave "method" blank it goes to default method.LA (Laplace Approximation; VA (variational approximation)

gllvm_null_model_lv0 <-
  gllvm::gllvm(y = spp_matrix,
               row.eff = "fixed",
               num.lv = 0,
               family = "negative.binomial",
               method = "VA", 
               seed = 726
  )
### Save the model
# saveRDS(gllvm_null_model_lv1,file = "./results/models/gllvm_null-model_lv1_model.rds")


gllvm_null_model_lv1 <-
  gllvm::gllvm(y = spp_matrix,
               row.eff = "fixed",
               num.lv = 1,
               disp.formula = rep(1, 32),
               family = "negative.binomial",
               method = "VA",
               seed = 726
  )

gllvm_null_model_lv2 <-
  gllvm::gllvm(y = spp_matrix,  
               row.eff = "fixed",
               num.lv = 2, 
               disp.formula = rep(1, 32),
               family = "negative.binomial",
               method = "VA",
               seed = 726)


### Based on BIC, choose the best model

BIC(gllvm_null_model_lv1, gllvm_null_model_lv2) #parsimonious
AIC(gllvm_null_model_lv1, gllvm_null_model_lv2) #mix of complexity but takes into account predictive power


### Residual Plots

par(mfrow = c(2,2))
plot(gllvm_null_model_lv1, which = 1:4)
# each color is a species (bottom right)
# they've got the same range on the scatterplots
# lv_1 & lv_2 / lv_1 is better as less parsimonious (less complex) but they're pretty much the same

### You can load the files back instead of running the models again
gllvm_null_model_lv1 <- readRDS("./results/models/gllvm_null-model_lv1_model.rds")

# PLOT ####


### Get the Latent Variable values and arrange it in a dataframe to plot

df_plot_null_model <-
  cbind((data_wide %>% dplyr::select(season, distance_offshore, sst, wind_speed, wind_direction)),
        as.data.frame(gllvm::getLV.gllvm(gllvm_null_model_lv1)))

## Plot colour-coded by 'distance_offshore'
plot_null_model_season <-
  ggplot(
    data = df_plot_null_model,
    aes(x = LV1, y = rep(0, times = nrow(df_plot_null_model)),
        color = season)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("Summer" = "#ffc32c", "Autumn" = "#e96900", "Winter" = "#96ced3", "Spring" = "#a0ca78")) +
  xlab("Latent Variable 1") + ylab("") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
plot_null_model_season <-
  ggExtra::ggMarginal(plot_null_model_season,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)
plot_null_model_season

## Plot colour-coded by 'distance_offshore'
plot_null_model_distance_offshore <-
  ggplot(
    data = df_plot_null_model,
    aes(x = LV1, y = rep(0, times = nrow(df_plot_null_model)),
        color = distance_offshore)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Dark2") +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
plot_null_model_distance_offshore <-
  ggExtra::ggMarginal(plot_null_model_distance_offshore,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)
plot_null_model_distance_offshore

## Plot colour-coded by 'sst' and Facet by Season
plot_null_model_sst <-
  ggplot(
    data = df_plot_null_model,
    aes(x = LV1, y = rep(0, times = nrow(df_plot_null_model)),
        color = sst)) +
  scale_color_gradient(low = "#ffcf0c", high = "#ff1f00", name = "SST (ºC)") +
  geom_point(alpha = 0.6, size = 2) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  facet_wrap(~ season, scales = "free") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
plot_null_model_sst

## Plot colour-coded by 'wind_speed'
plot_null_model_wind_speed <-
  ggplot(
    data = df_plot_null_model,
    aes(x = LV1, y = rep(0, times = nrow(df_plot_null_model)),
        color = wind_speed)) +
  scale_color_gradient(low = "lightblue", high = "blue", name = "Wind Speed (m/s)") +
  geom_point(alpha = 0.6, size = 2) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
plot_null_model_wind_speed

## Plot colour-coded by 'wind_direction'
plot_null_model_wind_direction <-
  ggplot(
    data = df_plot_null_model,
    aes(x = LV1, y = rep(0, times = nrow(df_plot_null_model)),
        color = wind_direction)) +
  scale_color_brewer(palette = "Spectral") +
  geom_point(alpha = 0.6, size = 2) +
  xlab("Latent Variable 1") + ylab("Latent Variable 2") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom")
plot_null_model_wind_direction <-
  ggExtra::ggMarginal(plot_null_model_wind_direction,
                      type = "density",
                      groupColour = TRUE,
                      groupFill = TRUE)
plot_null_model_wind_direction
