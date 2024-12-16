library(stringr)

# Radon is a radioactive gas that enters homes through contact points with the ground. 
# It is a carcinogen that is the primary cause of lung cancer in non-smokers. 
# Radon levels vary greatly from household to household. 
# This dataset contains measured radon levels in U.S homes by county and state. 
# The 'activity' label is the measured radon concentration in pCi/L. 
# Important predictors are 'floor' (the floor of the house in which the measurement was taken), 'county' (the U.S. county in which the house is located), and 'Uppm' (a measurement of uranium level of the soil by county).
# https://www.tensorflow.org/probability/examples/Multilevel_Modeling_Primer

# Download data from http://www.stat.columbia.edu/~gelman/arm/examples/radon/

# read in data
cty <- read.table("cty.dat", sep = ",", header = TRUE)
srrs2 <- read.table("srrs2.dat", sep = ",", header = TRUE)

# Clean data
# See https://mc-stan.org/users/documentation/case-studies/radon.html
srrs2$fips <- 1000 * srrs2$stfips + srrs2$cntyfips
cty$fips <- 1000 * cty$stfips + cty$ctfips
srrs2 <- merge(srrs2, cty[, c("fips", "Uppm")], by = "fips")
srrs2 <- srrs2[!duplicated(srrs2$idnum),]
srrs2_mn <- srrs2[srrs2$state == "MN",]
srrs2$log_radon <- log(srrs2$activity + 0.1)

# Finalize datasets
log_radon <- srrs2$log_radon
floor_measure <- srrs2$floor
log_uppm <- log(srrs2$Uppm + 0.1)
county <- stringr::str_trim(as.character(srrs2$county))
mn_bool <- srrs2$state == "MN" # Minnesota subset

radon_all <-  list(N = length(log_radon),
                   J = length(levels(as.factor(county))),
                   floor_measure = floor_measure,
                   log_radon = log_radon,
                   log_uppm = log_uppm,
                   county_idx = as.integer(as.factor(county)))

radon_mn <-  list(N = length(log_radon[mn_bool]),
                  J = length(levels(as.factor(county[mn_bool]))),
                  floor_measure = floor_measure[mn_bool],
                  log_radon = log_radon[mn_bool],
                  log_uppm = log_uppm[mn_bool],
                  county_idx = as.integer(as.factor(county[mn_bool])))


library(cmdstanr)
model_complete_pooling <- cmdstan_model("radon/radon_pooled.stan")
model_unpooled <- cmdstan_model("radon/radon_county_intercept.stan")
model_partially_pooled <- cmdstan_model("radon/radon_partially_pooled_noncentered.stan")
model_varying_intercepts <- cmdstan_model("radon/radon_variable_intercept_noncentered.stan")
model_varying_slopes <- cmdstan_model("radon/radon_variable_slope_noncentered.stan")
model_varying_both <- cmdstan_model("radon/radon_variable_intercept_slope_noncentered.stan")
model_hierarchical_intercept <- cmdstan_model("radon/radon_hierarchical_intercept_noncentered.stan")

fit_complete_pooling <- model_complete_pooling$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_unpooled <- model_unpooled$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_partially_pooled <- model_partially_pooled$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_varying_intercepts <- model_varying_intercepts$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_varying_slopes <- model_varying_slopes$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_varying_both <- model_varying_both$sample(radon_mn, seed = 0, parallel_chains = 4)
fit_hierarchical_intercept <- model_hierarchical_intercept$sample(radon_mn, seed = 0, parallel_chains = 4)

# Different models let us answer different questions
mean(fit_complete_pooling$draws("lp__"))
mean(fit_unpooled$draws("lp__"))
mean(fit_partially_pooled$draws("lp__"))
mean(fit_varying_intercepts$draws("lp__"))
mean(fit_varying_slopes$draws("lp__"))
mean(fit_varying_both$draws("lp__"))
mean(fit_hierarchical_intercept$draws("lp__"))

