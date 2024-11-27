compute_ess <- function(samples) {
  n <- length(samples)
  
  # Compute the mean and variance
  sample_mean <- mean(samples)
  sample_var <- var(samples)
  
  # Center the samples
  centered_samples <- samples - sample_mean
  
  # Compute the autocorrelations
  acf_values <- acf(centered_samples, lag.max = n - 1, plot = FALSE)$acf
  acf_values <- acf_values[2:length(acf_values)]  # Drop lag 0 autocorrelation
  
  # Find the point where the autocorrelation drops below 0
  positive_acf <- acf_values[acf_values > 0]
  
  # Sum the positive autocorrelations
  autocorr_sum <- sum(positive_acf)
  
  # Compute ESS
  ess <- n / (1 + 2 * autocorr_sum)
  
  return(ess)
}

