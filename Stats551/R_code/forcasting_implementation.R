library(sigmoid)


forecast <- function(N, s, i, beta, gamma, ndays){
  data <-  data.frame()
  for (d in 1:ndays){
    inf <- as.integer(period*beta*s*i/N)
    rec <- as.integer(period*gamma*i)
    data <- rbind(data, data.frame(list(S=s, I=i, Infection=inf, Recovery=rec, day=d)))
    s <- s-inf
    i <- i+inf-rec
  }
  return(data)
}


N<-332915074
last_day <- data_5 %>% filter(day == max(day))
set.seed(2039)
april_forecast <- forecast(N = N, s = last_day$S, i = last_day$I, 
                          beta = mean(beta_list_5[-1]), gamma = mean(gamma_list[-1]), ndays = 21)

april_forecast
# save(april_forecast, file= "../save_sims/forecasted_data.rds")
# april_forecast <- readRDS("../save_sims/forecasted_data.rds")
actual_april <- cases %>% filter(location == "United States", date > ymd("2022-04-09"))

april_forecast$type <- "forecast"
actual_april$type <- "observed"
april_forecast$Infection[1:9] %>% sum()
actual_april$new_cases %>% sum()

actual_april <- actual_april %>%
  rename(Infection = new_cases, N = population) %>%
  rowid_to_column("day")
april <- actual_april %>% select(day, Infection, type) %>%
  rbind(select(april_forecast, day, Infection, type))

key = data.frame(day = 1:21,
                 date = seq(ymd('2022-04-10'),ymd('2022-04-30'),by='days'))

april = full_join(april, key, by = "day")

ggplot(april, aes(x = date.y, y = Infection, color = type)) +
  geom_line() +
  ylim(c(0, 60000)) +
  scale_x_date(date_breaks = "days", date_labels = "%B %d %Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

april %>% filter(day <= 9) %>%
  group_by(type) %>%
  summarise(total_infection = sum(Infection)) %>%
  stargazer::stargazer(type = "latex", summary = FALSE)


ggplot(april, aes(x = date.y, y = Infection, color = type)) +
  geom_line() +
  ylim(c(22000, 35000)) +
  scale_x_date(date_breaks = "days", date_labels = "%B %d %Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


