connect_vpn <- function (p) {
  did_stablish <- as.logical(rbinom(1, size=1, prob=p))
  return(did_stablish)
}

select_vpn_randomly <- function() {
  if(as.logical(rbinom(1, size=1, prob=0.5)))
    return(1)
  else
    return(2)
}

experiment <- function() {
  num_try = 0
  while(probabilities[1] < 0.9) {
    if(probabilities[1] < 1e-4)
      return(0)
    i <- select_vpn_randomly()
    stablished_vpn <- connect_vpn(success_rates[i])
    if(stablished_vpn) {
      probabilities[i] <<- (success_rates[i] * probabilities[i]) /
        (success_rates[i] * probabilities[i] + success_rates[3-i] * probabilities[3-i])
    }
    else {
      probabilities[i] <<- ((1-success_rates[i]) * probabilities[i]) /
        ((1-success_rates[i]) * probabilities[i] + (1-success_rates[3-i]) * probabilities[3-i])
    }
    probabilities[3-i] <<- 1 - probabilities[i]
    
    num_try <- num_try + 1
  }
  return(num_try)
}

success_rates <- c(0.3, 0.1)

tries = c()
for(j in (1:100)) {
  print(j)
  probabilities <- c(0.5, 0.5)
  num_try = experiment()
  tries <- c(tries, num_try)
}
cat("mean_trials to be 90% certain = ", mean(tries))


