library(gllvm)
library(mvabund) # contains example ant data
library(corrplot)
library(gclus)
library(lattice)

rm(list = ls())
dev.off()  # closes the current plotting device and resets everything

data("antTraits")

y <- as.matrix(antTraits$abund)
x <- scale(as.matrix(antTraits$env))
x <- x[, c("Bare.ground", "Shrub.cover", "Volume.lying.CWD")]
TR <- antTraits$traits

fitp <- gllvm(y, family = poisson())
fitp # describes the model, log likelihood, residual dofs, AIC, AICc, BIC
# default printout includes information criteria

fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord

# Negative Binomial is better choice than poisson for modelling the response (lower information criteria = better fit?)

# now that we have an appropriate model, we can construct an ordination as a scatter  plot
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3,3), ylim = c(-2, 1.6))

# Environmental variables can be included in the model, whether to study their effects on assemblages or to study patterns of species co‐occurrence after controlling for environmental variables.

fit_env <- gllvm(y, x, family = "negative.binomial", num.lv = 3, 
                  formula = ~ Bare.ground + Shrub.cover + Volume.lying.CWD,
                  n.init = 22, seed = 123)
fit_env
dev.off()
# The estimated coefficients for predictors and their confidence intervals can be plotted using the coefplot() function, in order to study the nature of effects of environmental variables on species.
pdf("coefplot_output.pdf", width = 8, height = 12)
coefplot(fit_env, cex.ylab = 0.7, mar = c(4,9,2,1), 
         xlim = rep(list(c(-4, 4)), 3))
 #cex.ylab controls size of axis titles, mar controls margin sizes
dev.off()
dev.off()
# creates plot of points estimates for coefficients of the environment variables

cr <- getResidualCor(fit_env)

corrplot(cr[order.single(cr), order.single(cr)], diag = FALSE, type = "lower", method = "square", tl.cex = 0.8, tl.srt = 45, tl.col = 'red')

fit_4th <- gllvm(y, x, TR, family = "negative.binomial", num.lv = 3, formula = y ~ (Bare.ground + Shrub.cover + Volume.lying.CWD) + (Bare.ground + Shrub.cover + Volume.lying.CWD): (Pilosity + Polymorphism + Webers.length), n.init = 22, seed = 123)

levelplot()

## GLLVM 2.0
rm(list = ls())
library(devtools)
library(gllvm)
library(mvabund)
require(graphics)

# Ground Beetle dataset consists of measured counts from m=68 species, collected from n=87 sites.
# The data has k=17 primary environmental covariates.
# While you could fit a GLM, but a GLLVM is preferred when the num of covariates is non-negligible and/or when a low dimensional visual presentation of the species-environment is preferred.

data("beetle")
data("beetleEnv")

X <- scale(beetleEnv)
Y <- scale(beetle$Y)

meanvar.plot(mvabund(Y), xlab = "mean", ylab = "var") #Visualise data first to make sure there are no outliers, errors or abnormalities

#This function plots the variance of each species against it's mean, giving us an impression of the model we might want to fit.
