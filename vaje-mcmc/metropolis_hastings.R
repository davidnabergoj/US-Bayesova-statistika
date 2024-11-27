metropolis_hastings_1d <- function(log_target, init, n_iter, proposal_sd, return_accept_rate = FALSE) {
  # log_target: function to calculate log of the target density
  # init: initial state of the chain
  # n_iter: number of iterations
  # proposal_sd: standard deviation of the Gaussian proposal distribution
  # return_accept_rate: if TRUE, returns the acceptance rate as well
  
  chain <- numeric(n_iter)  # to store the samples
  chain[1] <- init          # initialize the chain
  accept_count <- 0         # to count the number of accepted proposals
  
  
  for (i in 2:n_iter) {
    # Propose a new value
    proposal <- rnorm(1, mean = chain[i - 1], sd = proposal_sd)
    
    # Calculate the log acceptance ratio
    log_accept_ratio <- log_target(proposal) - log_target(chain[i - 1])
    
    # Decide whether to accept the proposal
    if (log(runif(1)) < log_accept_ratio) {
      chain[i] <- proposal  # accept the proposal
      accept_count <- accept_count + 1
    } else {
      chain[i] <- chain[i - 1]  # reject, stay at the current value
    }
  }
  
  acceptance_rate <- accept_count / (n_iter - 1)
  
  if (return_accept_rate) {
    return(list(samples = chain, acceptance_rate = acceptance_rate))
  } else {
    return(chain)
  }
}