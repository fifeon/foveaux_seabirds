##
## Model-based ordinations
##
## ------------------------------------- ##

## Script runs model-based ordinations using the {gllvm} framework

## ------------------------------------- ##

## Libraries ##
library(gllvm)
library(dplyr)


rm(list = ls())

data <- read.csv("~/uniwersity/Postgrad Otago/Project/Data/foveaux_seabirds/data-processed/all_data.csv")

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

## Get Seabird data
spp_matrix <-
  data_wide %>%
  # Select species columns
  dplyr::select(all_of(sp_cols_only)) %>%
  # Removes rare species columns (they will be more noisy than explanatory)
  dplyr::select(-all_of(sp_rare_cols))

# GLLVM, purely biological (a NULL model) ####

### Describes the model log likelihood, residual dofs, AIC, AICc, BIC
# default printout includes information criteria

# Test to see if negative binomial or poisson fits better
#fitp <- gllvm(spp_matrix, family = poisson())
#fitp

### Run NULL models with 1 and 2 LV, respectively

gllvm_null_model_lv0 <-
  gllvm::gllvm(y = spp_matrix,
               row.eff = "fixed",
               num.lv = 0,
               family = "negative.binomial",
               disp.formula = rep(1, 35),
               method = "LA",
               seed = 321
  )

gllvm_null_model_lv1 <-
  gllvm::gllvm(y = spp_matrix,
               row.eff = "fixed",
               num.lv = 1,
               family = "negative.binomial",
               disp.formula = rep(1, 35),
               method = "LA",
               seed = 321
  )

gllvm_null_model_lv2 <-
  gllvm::gllvm(y = spp_matrix, 
               row.eff = "fixed",
               num.lv = 2, 
               family = "negative.binomial",
               disp.formula = rep(1, 35),
               method = "LA",
               seed = 321)

### Based on BIC, choose the best model

BIC(gllvm_null_model_lv0, gllvm_null_model_lv1, gllvm_null_model_lv2)

#                       df      BIC
#gllvm_null_model_lv0 4059 81537.94
#gllvm_null_model_lv1 4094 80220.02
#gllvm_null_model_lv2 4094 80220.02