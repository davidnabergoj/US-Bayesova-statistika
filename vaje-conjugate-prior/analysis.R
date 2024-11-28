# Beta-binomial model for customer satisfaction

# Download the airline customer satisfaction dataset and extract it

dataset_url <- "https://www.kaggle.com/api/v1/datasets/download/teejmahal20/airline-passenger-satisfaction"
dataset_zip_file <- "airline-passenger-satisfaction.zip"
download.file(dataset_url, dataset_zip_file, method = "curl", extra = "-L")

dataset_directory <- "airline-passenger-satisfaction"
unzip(dataset_zip_file, exdir = dataset_directory)


# Load the data; we can ignore the test set
df_train <- read.csv(file.path(dataset_directory, "train.csv"))
# df_test <- read.csv(file.path(dataset_directory, "test.csv"))

# Convert the target to binary values (0s and 1s)
df_train$satisfaction <- as.numeric(as.factor(df_train$satisfaction)) - 1
# df_test$satisfaction <- as.numeric(as.factor(df_test$satisfaction)) - 1


# We first poll 10 customers in our preliminary research
first_poll.df_train <- df_train[1:10,]

# We then poll 10 additional customers
second_poll.df_train <- df_train[11:20,]

# We finally poll 50 additional customers, which should be enough to estimate customer satisfaction
third_poll.df_train <- df_train[21:70,]

# Define the likelihood
log_likelihood <- function(theta, n_successes, n_trials) {
  dbinom(n_successes, size = n_trials, prob = theta, log = TRUE)
}
likelihood <- function(...) {
  exp(log_likelihood(...))
}

# Define the prior
log_prior <- function(parameters, alpha, beta) {
  dbeta(parameters, alpha, beta, log = TRUE)
}
prior <- function(...) {
  exp(log_prior(...))
}

# Derive the posterior
log_posterior <- function(parameters, alpha, beta, n_successes, n_trials) {
  n_failures <- n_trials - n_successes
  dbeta(parameters, n_successes + alpha, n_failures + beta, log = TRUE)
}
posterior <- function(...) {
  exp(log_posterior(...))
}

# What is the probability theta that the customer will be satisfied?

# Prior opinion: we are neither optimistic, nor pessimistic. We will assign equal probability to all values of theta. In other words, we have a uniform prior. This occurs with alpha = 1, beta = 1 for the beta distribution.

# Plot the prior, likelihood and posterior.
install.packages("ggplot2")
library(ggplot2)

linewidth <- 0.6
stat_function_n <- 1000
opacity <- 0.05

# Number of successes and trials
ns_1 <- sum(first_poll.df_train$satisfaction)
nt_1 <- nrow(first_poll.df_train)
nf_1 <- nt_1 - ns_1

ggplot() +
  xlim(0, 1) +
  ylim(0, 4) +
  stat_function(fun = prior, args = c(1, 1), aes(color = 'Prior density'), geom = "area", alpha = opacity, linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = posterior, args = c(1, 1, ns_1, nt_1), aes(color = 'Posterior density'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = likelihood, args = c(ns_1, nt_1), aes(color = 'Unnormalized likelihood (first poll)'), geom = "area", alpha = opacity, linewidth = linewidth, n = stat_function_n) +
  ylab("Function value") +
  xlab("Satisfaction rate") +
  ggtitle("Customer satisfaction probability model visualization") +
  theme_classic() +
  labs(color = "Function")

# Number of successes, failures, and trials
ns_2 <- sum(second_poll.df_train$satisfaction)
nt_2 <- nrow(second_poll.df_train)
nf_2 <- nt_2 - ns_2

ns_3 <- sum(third_poll.df_train$satisfaction)
nt_3 <- nrow(third_poll.df_train)
nf_3 <- nt_3 - ns_3

library(latex2exp)

ggplot() +
  xlim(0, 1) +
  stat_function(fun = prior, args = c(1, 1), aes(color = 'Prior density (before polling)'), geom = "area", alpha = opacity, linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = prior, args = c(1 + ns_1, 1 + nf_1), aes(color = 'Posterior density (after 1st poll)'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = prior, args = c(1 + ns_1 + ns_2, 1 + nf_1 + nf_2), aes(color = 'Posterior density (after 2nd poll)'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = prior, args = c(1 + ns_1 + ns_2 + ns_3, 1 + nf_1 + nf_2 + nf_3), aes(color = 'Posterior density (after 3rd poll)'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  ylab("Function value") +
  xlab("Satisfaction rate") +
  ggtitle("Customer satisfaction probability model visualization\n- more measurements") +
  labs(color = "Function") +
  annotate(
    "text", label = TeX(sprintf("$\\theta_{pr} = %.2f$", 1/2)),
    x = 0.8, y = 7,
    hjust='left',
    vjust='top'
  ) +
  annotate(
    "text", label = TeX(sprintf("$\\theta_{p1} = %.2f$", (1 + ns_1)/ (1 + nt_1))),
    x = 0.8, y = 6.5,
    hjust='left',
    vjust='top'
  ) +
  annotate(
    "text", label = TeX(sprintf("$\\theta_{p2} = %.2f$", (1 + ns_1 + ns_2)/ (1 + nt_1 + nt_2))),
    x = 0.8, y = 6,
    hjust='left',
    vjust='top'
  ) +
  annotate(
    "text", label = TeX(sprintf("$\\theta_{p2} = %.2f$", (1 + ns_1 + ns_2 + ns_3)/ (1 + nt_1 + nt_2 + nt_3))),
    x = 0.8, y = 5.5,
    hjust='left',
    vjust='top'
  ) +
  annotate(
    "text", label = TeX(sprintf("$\\theta_{all} = %.2f$", mean(df_train$satisfaction))),
    x = 0.8, y = 4.75,
    hjust='left',
    vjust='top'
  ) +
  theme_classic()

# 95% "credible interval" or "highest density interval" or "confidence interval"
# after the first poll

install.packages("HDInterval")
library(HDInterval)

# Bayesian CIs
ci_1 <- qbeta(c(0.025, 0.975), 1 + ns_1, 1 + nf_1)
hdi_1 <- hdi(rbeta(10000, 1 + ns_1, 1 + nf_1))

# Frequentist CIs
# Somebody derived this
ci_1_normal <- prop.test(1 + ns_1, 1 + nt_1, correct = F)$conf  # normal distribution approximation
ci_1_clopper_pearson <- binom.test(1 + ns_1, 1 + nt_1)$conf  # clopper-pearson CI

opacity <- 0.2

ggplot() +
  xlim(0, 1) +
  geom_vline(xintercept = ci_1) +
  geom_vline(xintercept = hdi_1, linetype = "dashed") +
  geom_vline(xintercept = ci_1_normal, color = "red") +
  geom_vline(xintercept = ci_1_clopper_pearson, linetype = "dashed", color = "red") +
  stat_function(fun = prior, args = c(1 + ns_1, 1 + nf_1), color = "blue", geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  ylab("Posterior density (after 1st poll)") +
  xlab("Theta") +
  ggtitle("Customer satisfaction probability model visualization\n- 95% credible interval (quantiles)") +
  labs(color = "Function")

intervals <- data.frame(rbind(
  c("Bayesian CI (quantile)", ci_1),
  c("Bayesian CI (highest density)", hdi_1),
  c("Frequentist CI (Gaussian approximation)", ci_1_normal),
  c("Frequentist CI (Clopper-Pearson)", ci_1_clopper_pearson)
))


# What's the probability of customer satisfaction being 0.5 or greater? (we are really setting the bar low lol)
# Assume we only have data for the first poll

probs <- data.frame(
  rbind(
    c("Bayesian", 1 - pbeta(0.5, 1 + ns_1, 1 + nt_1)),
    c("Frequentist (normal approximation test)", 1 - prop.test(1 + ns_1, 1 + nt_1, correct = F, p = 0.5, alternative = "less")$p.value),
    c("Frequentist (exact binomial test)", 1 - prop.test(1 + ns_1, 1 + nt_1, p = 0.5, alternative = "less")$p.value)
  )
)

# Since we know that the truth is 0.44 satisfaction, the frequentist numbers are terrible, the Bayesian number is amazing - in hindsight, we could also use frequentist estimates and learn the hard way

# Probability comparisons
# Suppose we split our customers into those who travel business class and those who travel economy
# Ask 20 of each for their satisfaction and compare the satisfaction rates
df_business <- df_train[df_train$Class == "Business",][1:20,]
df_economy <- df_train[df_train$Class != "Business",][1:20,]

business.alpha <- 1 + sum(df_business$satisfaction)
business.beta <- 1 + sum(1 - df_business$satisfaction)
business.mean <- business.alpha / (business.alpha + business.beta)

economy.alpha <- 1 + sum(df_economy$satisfaction)
economy.beta <- 1 + sum(1 - df_economy$satisfaction)
economy.mean <- economy.alpha / (economy.alpha + economy.beta)

paste(business.mean, economy.mean)  # insane discrepancy

comparison <- data.frame(
  rbind(
    c("Estimated theta (business)", business.mean),
    c("Estimated theta (economy)", economy.mean),
    c("Risk difference", business.mean - economy.mean),  # equal: 0; eco > business: <0
    c("Risk ratio", business.mean / economy.mean),  # equal: 1; eco > business: <1
    c("Odds ratio", business.mean / (1 - business.mean) / (economy.mean / (1 - economy.mean)))  # equal: 1; eco > business: <1
  )
)

# Note, we could have groups of different sizes and the uncertainty estimates would save us :)

# Frequentist approach to verify that the risk difference is > 0?
wilcox.test(economy.mean, business.mean, alternative="greater")

# Bayesian approach
ggplot() +
  xlim(0, 1) +
  stat_function(fun = prior, args = c(business.alpha, business.beta), aes(color = 'Business'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  stat_function(fun = prior, args = c(economy.alpha, economy.beta), aes(color = 'Economy'), geom = "area", alpha = opacity,  linewidth = linewidth, n = stat_function_n) +
  ylab("Posterior density") +
  xlab("Satisfaction rate") +
  ggtitle("Customer satisfaction probability model visualization\n- business and economy class comparison") +
  labs(color = "Customer class")

# Sample 10000 imaginary customer pairs (one in business class, one in economy class).
# Check if business class customer is strictly more satisfied than the economy class customer.
n_samples <- 10000
business_samples <- rbeta(n_samples, business.alpha, business.beta)
economy_samples <- rbeta(n_samples, economy.alpha, economy.beta)
mean((business_samples - economy_samples) > 0)

# We don't need any derivations here

# Prediction
# Suppose there are 20 customers on a flight. We wish to know how many are satisfied or not.
# Instead of asking and intruding, we predict this number.

# If we have not yet analyzed poll data, we are working with a prior predictive distribution.
# If we already analyzed the poll data, we have a posterior predictive distribution.
# The prior and posterior predictive distribution are beta-binomial distributions.
# Naively applying the estimated posterior satisfaction rate to a binomial distribution does not yield the same results as the posterior predictive.
dbetabinom <- function(k, alpha, beta, n) {
  choose(n, k) * beta(k + alpha, n - k + beta) / beta(alpha, beta)
}

n_customers <- 20

# Plot both distributions
predictive_df <- data.frame(k = c(0:n_customers, 0:n_customers, 0:n_customers))
predictive_df$variable <- c(
  rep("Prior predictive", n_customers + 1),
  rep("Posterior predictive", n_customers + 1),
  rep(sprintf("Binomial(n, %.2f)", (1 + ns_1) / (1 + nf_1 + 1 + ns_1)), n_customers + 1)
  )
predictive_df$value <- c(
  dbetabinom(0:n_customers, 1, 1, n_customers),
  dbetabinom(0:n_customers, 1 + ns_1, 1 + nf_1, n_customers),
  dbinom(0:n_customers, n_customers, (1 + ns_1) / (1 + nf_1 + 1 + ns_1))
) 

ggplot(predictive_df, aes(x = k, y = value)) +
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity") + 
  xlab("Predicted number of satisfied customers") +
  ylab("Density") +
  labs(fill = "")

