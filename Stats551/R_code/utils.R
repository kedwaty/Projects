# Functions

prob_inf <- function(beta, s, i, x, N){
  rate = period*beta*s*i/N
  return(dpois(x, rate, log = TRUE))
}

prob_rec <- function(gamma, i, x){
  rate <- period*gamma*i
  return(dpois(x, rate, log = TRUE))
}

likelihood_infection <- function(data, beta){
  ll <- 0
  days <- data$day
  for (d in days){
    drow <- data[data$day == d, ]
    s <- drow$S
    i <- drow$I
    x_inf <- drow$Infection
    ll <- ll + (prob_inf(beta, s, i, x_inf, N))
  }
  return(ll)
}

likelihood_rec <- function(data, gamma){
  ll <- 0
  days <- data$day
  for (d in days){
    drow <- data[data$day == d, ]
    i <- drow$I
    x_rec <- drow$Recovery
    ll <- ll + (prob_rec(gamma, i, x_rec))
  }
  return(ll)
}

prior_beta <- function(theta){
  return(dgamma(theta, shape, rate, log = TRUE))
}

prop_pdf <- function(theta, theta_prev){
  return(dlnorm(theta, log(0.2)+1, sigmoid(1/theta_prev), log = T))
}

prop_samplr <- function(theta_prev){
  return(rlnorm(1, log(0.2)+1, sigmoid(1/theta_prev)))
}

posterior_beta <- function(data, beta){
  likelihood <- likelihood_infection(data, beta)
  prior <- prior_beta(beta)
  return(likelihood+prior)
}











