install.packages("mlbench")
library(mlbench)

# Harrison and Rubinfield: Hedonic housing prices and the demand for clean air (1978)
# https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# By predicting median house value in $1000s for different towns, identify which variables contribute to price the most/least

data("BostonHousing")

house_price <- BostonHousing$medv
features <- cbind(
  BostonHousing$crim,    # per capita crime rate by town
  BostonHousing$indus,   # proportion of non-retail business acres per town
  BostonHousing$nox,     # nitric oxides concentration
  BostonHousing$rm,      # average number of rooms per dwelling
  BostonHousing$age,     # proportion of owner-occupied units built prior to 1940
  BostonHousing$dis,     # weighted distances to five Boston employment centres
  BostonHousing$rad,     # index of accessibility to radial highways
  BostonHousing$tax,     # full-value property-tax rate per $10,000
  BostonHousing$ptratio, # pupil-teacher ratio by town
  BostonHousing$lstat    # % lower status of the population
)

sum(is.na(features))  # no missing values

# scale to equalize effects of each feature ... sampling is also MUCH faster
features <- scale(features)


library(cmdstanr)
model <- cmdstan_model("housing.stan")
fit <- model$sample(
  data = list(
    N = nrow(features),
    M = ncol(features),
    y = house_price,
    features = features
  ),
  seed = 0,
  chains = 4,
  parallel_chains = 4
)

fit$cmdstan_diagnose()
fit$summary()

bayesplot::mcmc_areas(fit$draws("beta"))

# Most positively contributing variables: 4 (number of rooms) and 6 (distance to employment center)
# Most negatively contributing variable: 10 (% lower status of the population)
# Least contributing variable (closest to zero): 2 (proportion of non-retail business acres per town) and 5 (proportion of owner-occupied units built prior to 1940)
