library(tidyverse)
library(lubridate)


## Download and clean case data
cases_orig <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
regions <- c("Africa", "Asia", "Europe", "High income",
             "International", "Low income", "Lower middle income",
             "North America", "Oceania", "South America", "Upper middle income",
             "World", "Northern Cyprus")
cases <- cases_orig %>% filter(! location %in% regions) %>%
  mutate(data = ymd(date)) %>%
  select(iso_code, location, date, total_cases, new_cases,
         total_deaths, new_deaths, reproduction_rate,
         positive_rate, population, people_vaccinated_per_hundred,
         stringency_index) %>%
  mutate(susceptible_estimate = population - total_cases) %>%
  group_by(location) %>%
  arrange(date) %>%
  mutate(difference = c(NA, diff(stringency_index))) %>%
  ungroup()

# reproduction_rate comes from https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0244474

ggplot() +
  stat_function(fun = dlnorm, args = list(1.5, .5)) +
  xlim(-.1, 30) +
  theme_bw() +
  labs(y = "density", title = "Estimated recovery time")

generate_recovery <- function(x){
  # Takes numeric vector x
  # Returns vector of recoveries estimated
  ndays <- length(x)
  recoveries <- numeric(ndays)
  for(i in 1:(ndays-1)){
    if(x[i] == 0) next
    times <- rlnorm(x[i], 1.5, .5) %>% round()
    tab <- table(times)
    idx <- as.numeric(names(tab))
    tab1 <- tab[idx + i <= ndays]
    idx1 <- as.numeric(names(tab1))
    if(sum(is.na(idx1)) > 0) next
    # print(idx1)
    recoveries[i:ndays][idx1] <- recoveries[i:ndays][idx1] + tab1
    if(length(idx) > length(idx1)){
      recoveries[ndays] <- NA
    }
  }
  return(recoveries)
}




