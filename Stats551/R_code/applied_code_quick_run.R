################################################################################

# data and code

# libraries
library(sigmoid)
library(tidyverse)
library(lubridate)

# relevant parameters
period <- 1 # days per period
reps <- 1000 # repetitions for MCMC algorithm
N<-332915074 # population of US


# get real case data
source("data/get_case_data.R")

data = cases
rcountries = c("United States")
data <- data %>%
  filter(location %in% rcountries) %>%
  filter(!is.na(new_cases)) %>%
  group_by(location) %>%
  mutate(recoveries = generate_recovery(new_cases)) %>%
  mutate(currently_infected = cumsum(new_cases) - cumsum(recoveries)) %>% 
  select(location, currently_infected, susceptible_estimate, new_cases, recoveries, date) %>% 
  rename(I=currently_infected, S=susceptible_estimate, Infection=new_cases, Recovery=recoveries, day=date)  %>%
  ungroup()

# date at which vaccination rate exceeds 50%
head(cases_orig %>% filter(location == "United States") %>% 
       filter(people_fully_vaccinated_per_hundred > 50))

# separate data by relevant dates
data_1 = data %>% filter(day<"2020-10-19")
data_2 = data %>% filter(day>"2020-10-18") %>% filter(day<"2021-05-23")
data_3 = data %>% filter(day>"2021-05-22") %>% filter(day<"2021-07-13")
data_4 = data %>% filter(day>"2021-07-12") %>% filter(day<"2021-11-01")
data_5 = data %>% filter(day>"2021-10-31") %>% filter(day<"2022-04-10")

# beta estimation parameters
variance <- 0.7
mod <- 0.2
shape <- 1/(variance-mod)
rate <- (shape-1)/mod

# beta estimation functions
source("R_code/utils.R")

# Implement MH for data_1
set.seed(0193)
reps <- 1000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
beta_list_1 <- c(0.2)
prop_1 <- c(0.2)
a_list_1 <- c(0)
for (i in 2:reps){
  theta_star <- prop_samplr(last(beta_list_1))
  a <- posterior_beta(data_1, theta_star) + prop_pdf(last(beta_list_1), theta_star) - 
    posterior_beta(data_1, last(beta_list_1)) - prop_pdf(theta_star, last(beta_list_1))
  a_list_1 <- c(a_list_1, a)
  a <- exp(a)
  if (a > 1) {
    beta_list_1 <- c(beta_list_1, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      beta_list_1 <- c(beta_list_1, theta_star)
    }
    else{
      prop_1 <- c(prop_1, theta_star)
    }
  }
  setTxtProgressBar(pb,i)
}
save(beta_list_1, file = "save_sims/beta_list_1.rds")


# Implement MH for data_2
set.seed(3846)
reps <- 1000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
beta_list_2 <- c(0.2)
prop_2 <- c(0.2)
a_list_2 <- c(0)
for (i in 2:reps){
  theta_star <- prop_samplr(last(beta_list_2))
  a <- posterior_beta(data_2, theta_star) + prop_pdf(last(beta_list_2), theta_star) - 
    posterior_beta(data_2, last(beta_list_2)) - prop_pdf(theta_star, last(beta_list_2))
  a_list_2 <- c(a_list_2, a)
  a <- exp(a)
  if (a > 1) {
    beta_list_2 <- c(beta_list_2, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      beta_list_2 <- c(beta_list_2, theta_star)
    }
    else{
      prop_2 <- c(prop_2, theta_star)
    }
  }
  setTxtProgressBar(pb,i)
}
save(beta_list_2, file = "save_sims/beta_list_2.rds")


# Implement MH for data_3
set.seed(3994)
reps <- 1000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
beta_list_3 <- c(0.2)
prop_3 <- c(0.2)
a_list_3 <- c(0)
for (i in 2:reps){
  theta_star <- prop_samplr(last(beta_list_3))
  a <- posterior_beta(data_3, theta_star) + prop_pdf(last(beta_list_3), theta_star) - 
    posterior_beta(data_3, last(beta_list_3)) - prop_pdf(theta_star, last(beta_list_3))
  a_list_3 <- c(a_list_3, a)
  a <- exp(a)
  if (a > 1) {
    beta_list_3 <- c(beta_list_3, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      beta_list_3 <- c(beta_list_3, theta_star)
    }
    else{
      prop_3 <- c(prop_3, theta_star)
    }
  }
  setTxtProgressBar(pb,i)
}
save(beta_list_3, file = "save_sims/beta_list_3.rds")


# Implement MH for data_4
set.seed(4535)
reps <- 1000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
beta_list_4 <- c(0.2)
prop_4 <- c(0.2)
a_list_4 <- c(0)
for (i in 2:reps){
  theta_star <- prop_samplr(last(beta_list_4))
  a <- posterior_beta(data_4, theta_star) + prop_pdf(last(beta_list_4), theta_star) - 
    posterior_beta(data_4, last(beta_list_4)) - prop_pdf(theta_star, last(beta_list_4))
  a_list_4 <- c(a_list_4, a)
  a <- exp(a)
  if (a > 1) {
    beta_list_4 <- c(beta_list_4, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      beta_list_4 <- c(beta_list_4, theta_star)
    }
    else{
      prop_4 <- c(prop_4, theta_star)
    }
  }
  setTxtProgressBar(pb,i)
}
save(beta_list_4, file = "save_sims/beta_list_4.rds")

# Implement MH for data 5

set.seed(3562)
reps <- 1000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
beta_list_5 <- c(0.2)
prop_5 <- c(0.2)
a_list_5 <- c(0)
for (i in 2:reps){
  theta_star <- prop_samplr(last(beta_list_5))
  a <- posterior_beta(data_5, theta_star) + prop_pdf(last(beta_list_5), theta_star) - 
    posterior_beta(data_5, last(beta_list_5)) - prop_pdf(theta_star, last(beta_list_5))
  a_list_5 <- c(a_list_5, a)
  a <- exp(a)
  if (a > 1) {
    beta_list_5 <- c(beta_list_5, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      beta_list_5 <- c(beta_list_5, theta_star)
    }
    else{
      prop_5 <- c(prop_5, theta_star)
    }
  }
  setTxtProgressBar(pb,i)
}
save(beta_list_5, file = "save_sims/beta_list_5.rds")


###############

# Recovery rate estimation

data = data %>% filter(day<"2022-04-10")

prob_rec <- function(gamma, i, x){
  rate <- period*gamma*i
  return(dpois(x, rate, log = TRUE))
}
likelihood_rec <- function(data, gamma){
  ll <- 0
  days <- data$day
  for (d in days){
    
    drow <- data[data$day == d, ]
    i <- drow$I
    if(i!=0){
      x_rec <- drow$Recovery
      ll <- ll + (prob_rec(gamma, i, x_rec))
    }
  }
  return(ll)
}

variance <- 0.25
mod <- 0.2
shape <- 1/(variance-mod)
rate <- (shape-1)/mod

prior_gamma <- function(theta){
  return(dgamma(theta, shape, rate, log = TRUE))
}

prop_pdf <- function(theta, theta_prev){
  return(dlnorm(theta, log(0.22)+1, 1+sigmoid(1/theta_prev), log = T))
}

prop_samplr <- function(theta_prev){
  return(rlnorm(1, log(0.22)+1, 1+sigmoid(1/theta_prev)))
}

posterior_gamma <- function(data, gamma){
  likelihood <- likelihood_rec(data, gamma)
  prior <- prior_gamma(gamma)
  return(likelihood+prior)
}

set.seed(8402)
reps <- 10000
pb = txtProgressBar(min = 1, max = reps, initial = 1)
gamma_list <- c(0.2)
prop <- c(0.2)
a_list <- c(0)
for (j in 2:reps){
  theta_star <- prop_samplr(last(gamma_list))
  a <- posterior_gamma(data, theta_star) + prop_pdf(last(gamma_list), theta_star) - 
    posterior_gamma(data, last(gamma_list)) - prop_pdf(theta_star, last(gamma_list))
  a_list <- c(a_list, a)
  a <- exp(a)
  if (a > 1) {
    gamma_list <- c(gamma_list, theta_star)
  }
  else {
    coin <- rbinom(1,1,a)
    if (coin == 1){
      gamma_list <- c(gamma_list, theta_star)
    }
    else{
      prop <- c(prop, theta_star)
    }
  }
  setTxtProgressBar(pb,j)
}
save(gamma_list, file = "save_sims/gamma_list.rds")


## Generate report output

params <- list(beta_list_1, beta_list_2, beta_list_3, beta_list_4, beta_list_5, gamma_list)
# save(params, file = "save_sims/all_params.rds")
means <- lapply(params, FUN = function(x) mean(x[-1])) %>% unlist()
data.frame(Parameter = c(sprintf("$\\beta_%1.0f$", 1:5), "$\\gamma$"),
           `Posterior Mean` = means) %>%
  stargazer::stargazer(type = "latex", summary = FALSE)


dates = ymd(c("2020-10-19", "2021-05-23", "2021-07-13", "2021-11-01", "2022-04-10"))
data %>%
  ggplot(aes(x = day, y = Infection)) +
  geom_line(color = "red") +
  geom_vline(aes(xintercept = dates[1]), color = "blue") +
  geom_vline(aes(xintercept = dates[2]), color = "blue") +
  geom_vline(aes(xintercept = dates[3]), color = "blue") +
  geom_vline(aes(xintercept = dates[4]), color = "blue") +
  geom_vline(aes(xintercept = dates[5]), color = "blue") +
  scale_x_date(date_breaks = "2 months", date_labels = "%B %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "New infections", x = "Date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))





