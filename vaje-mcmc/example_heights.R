x <- c(1.91, 1.94, 1.68, 1.75, 1.81, 1.83, 1.91, 1.95, 1.77, 1.98,
       1.81, 1.75, 1.89, 1.89, 1.83, 1.89, 1.99, 1.65, 1.82, 1.65,
       1.73, 1.73, 1.88, 1.81, 1.84, 1.83, 1.84, 1.72, 1.91, 1.63)

# Likelihood definition
sigma.likelihood <- 0.1
log_likelihood <- function(theta) {
  sum(dnorm(x, mean = theta, sd = sigma.likelihood, log = TRUE))
}

# Prior definition
mu.prior <- 1.78
sigma.prior <- 0.2
log_prior <- function(theta) {
  dnorm(theta, mean = mu.prior, sd = sigma.prior, log = TRUE)
}

# Unnormalized posterior definition
log_posterior <- function(theta) {
  log_likelihood(theta) + log_prior(theta)
}

# True posterior parameters (we do not know this & philosophically it does not exist)
sigma.posterior <- sqrt(1 / (1 / sigma.prior^2 + length(x) / sigma.likelihood^2))
mu.posterior <- (
  mu.prior * (1 / sigma.prior^2) / (1 / sigma.prior^2 + length(x) / sigma.likelihood^2)
  + mean(x)  * (length(x) / sigma.likelihood^2) / (1 / sigma.prior^2 + length(x) / sigma.likelihood^2)
)

source("metropolis_hastings.R")
source("metrics.R")

# Draw the initial state from the prior
set.seed(0)
x0 <- rnorm(1, mean = mu.prior, sd = sigma.prior)

# Run MH
set.seed(0)
x_mh <- metropolis_hastings_1d(log_posterior, x0, n_iter = 1000, proposal_sd = 1)
# Check effective sample size
compute_ess(x_mh)  # "we have effectively drawn 18 samples"
plot(x_mh, type='l')  # not a hairy catterpillar

# What if we reduce the proposal SD?
set.seed(0)
x_mh <- metropolis_hastings_1d(log_posterior, x0, n_iter = 1000, proposal_sd = 0.1)
compute_ess(x_mh)  # "we have effectively drawn 28 samples" - somewhat better
plot(x_mh, type='l')  # not quite a hairy catterpillar

# Go a bit higher (lower is not helpful)
set.seed(0)
x_mh <- metropolis_hastings_1d(log_posterior, x0, n_iter = 1000, proposal_sd = 0.05)
compute_ess(x_mh)  # "we have effectively drawn 34 samples" - good enough, it won't get better than this apparently
plot(x_mh, type='l')  # much better

# How can we get even more effective samples?
# -> run MH for much longer given this proposal scale
set.seed(0)
x_mh <- metropolis_hastings_1d(log_posterior, x0, n_iter = 10000, proposal_sd = 0.05)
compute_ess(x_mh)  # "we have effectively drawn 107 samples" - this will probably be enough for very high accuracy of mu
plot(x_mh, type='l')  # this is what we want :D

# We could also run for longer if we wanted to.

# Let's also check the acceptance rate - how many transitions were accepted (rerun the algorithm with the flag)
set.seed(0)
out_mh <- metropolis_hastings_1d(log_posterior, x0, n_iter = 10000, proposal_sd = 0.05, return_accept_rate = TRUE)
compute_ess(out_mh$samples)
out_mh$acceptance_rate  # 40% of transitions were accepted. The ideal for Metropolis-Hastings is around 55%. This is fine.

# What happens to ESS if we discard an initial "burn-in" portion of the samples? Say we discard 10 samples.
compute_ess(out_mh$samples[10:length(out_mh$samples)])  # We get a bigger ESS. That's because the initial samples have some autocorrelation. For more complicated models, we drop more samples, e.g. 100 or 1000.
# Let's set our samples to the post burn-in ones
x_mh <- out_mh$samples[10:length(out_mh$samples)]

# What is the expected mean given MH draws and the true mean?
mean(x_mh)  # each draw in x_mh is a different mu; taking the mean gives us the expected value
mu.posterior # we were very accurate!

# What if we went with our initial proposal scale of 1, 1000 iterations, and no burn-in cutoff?
set.seed(0)
out_basic <- metropolis_hastings_1d(log_posterior, x0, n_iter = 1000, proposal_sd = 1, return_accept_rate = TRUE)
compute_ess(out_basic$samples)  # we are estimating the average mu with only 18 samples...
mean(out_basic$samples)
# Turns out it's still good, though the error appears on the second decimal already.
out_basic$acceptance_rate  # terrible acceptance rate, but it was enough considering we made enough iterations

# What if we only made 100 iterations?
set.seed(0)
out_short <- metropolis_hastings_1d(log_posterior, x0, n_iter = 100, proposal_sd = 1, return_accept_rate = TRUE)
compute_ess(out_short$samples)  # we are estimating the average mu with only 12 samples...
mean(out_short$samples)  # bigger error on the second decimal
# Turns out it's still good, though the error appears on the second decimal already.
out_short$acceptance_rate  # terrible acceptance rate, just like before

# Let's package all these into a dataframe
data.frame(
  mean_estimation_strategy = c("Prior", "Data", "Tuned MH", "Basic MH", "Short MH"),
  absolute_error = abs(c(mu.prior, mean(x), mean(x_mh), mean(out_basic$samples), mean(out_short$samples)) - mu.posterior),
  ess = c(NA, NA, compute_ess(x_mh), compute_ess(out_basic$samples), compute_ess(out_short$samples)),
  acceptance_rate = c(NA, NA, out_mh$acceptance_rate, out_basic$acceptance_rate, out_short$acceptance_rate)
)

# Only the tuned MH run is better than the data estimate
# Worth noting that the data estimate is not always available (if we are estimating more complex quantities)

# Plot the true posterior density and the MH density
library(ggplot2)
x_vals <- seq(min(x_mh), max(x_mh), length.out = 1000)
true_density <- data.frame(
  x = x_vals,
  density = dnorm(x_vals, mean = mu.posterior, sd = sigma.posterior)
)

# Plot using ggplot2
ggplot(data.frame(x_mh = x_mh), aes(x = x_mh)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue", alpha = 0.6, color = "black") +
  geom_line(data = true_density, aes(x = x, y = density), color = "red", size = 1.2) +
  labs(
    title = "Metropolis-Hastings Samples vs. True Posterior",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal()

# We can also compute 95% confidence intervals for mu
install.packages("HDInterval")
library(HDInterval)

quantile(x_mh, c(0.025, 0.975))  # quantile
c(hdi(x_mh, credMass = 0.95)) # HDI
