library(tidyverse)
library(lubridate)
library(ggthemes)
library(magrittr)


###############
# Load data
###############

cases_au <- '~/covid-19-plots/data/cases-aus.csv' %>%
  read_csv(col_names = c('date', 'cum_cases'),
                     col_types = cols(
                       date = col_date(format = '%d/%m/%Y'),
                       cum_cases = col_integer())) %>%
  mutate(day = (date - min(date)) %>% as.integer()) %>%
  mutate(country = 'AU') %>%
  mutate(diff_cases = cum_cases - lag(cum_cases))

cases_it <- '~/covid-19-plots/data/cases-itl.csv' %>%
  read_csv(col_names = c('date', 'cum_cases'),
           col_types = cols(
             date = col_date(format = '%Y-%m-%d'),
             cum_cases = col_integer())) %>%
  mutate(day = (date - min(date)) %>% as.integer()) %>%
  mutate(country = 'IT')


###############
# Plots
###############

cases_au %>%
  ggplot(aes(x = date, y = cum_cases)) +
  geom_point() +
  geom_line() +
  theme_fivethirtyeight()

cases_au %>%
  filter(day >= 35) %>%
  ggplot(aes(x = day, y = cum_cases)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  theme_fivethirtyeight()

cases_au %>%
  filter(!is.na(diff_cases)) %>%
  ggplot(aes(x = date, y = diff_cases)) +
  geom_col() +
  theme_fivethirtyeight()


cases <- cases_au %>%
  mutate(day = day - 32) %>%
  bind_rows(cases_it %>% mutate(day = day - 4))


cases %>%
  filter(cum_cases >= 100) %>%
  ggplot(aes(x = day, y = cum_cases, colour = country)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_colour_fivethirtyeight() +
  theme_fivethirtyeight()

cases_au %<>%
  mutate(day = day + 1) %>%
  mutate(log_cases = log(cum_cases))

cases_au %>% ggplot(aes(x = day, y = log_cases)) + geom_point() + geom_line()

cases_au_fit <- cases_au %>% filter(day > 40)



cases_au_fit %>%
  ggplot(aes(x = day, y = log_cases)) +
  geom_smooth(method = 'lm', formula = y ~ x) +
  geom_point() +
  theme_fivethirtyeight()



fit <- lm(log_cases ~ day, data = cases_au_fit)

date_range <- seq(from = min(cases_au_fit$date), to = ymd('2020-04-01'), by = 1)

day_range <- seq(from = min(cases_au_fit$day),
                length.out = length(date_range))

cases_au_extrap <- tibble(date = date_range,
                          day = day_range,
                          log_pred = coef(fit)[1] + coef(fit)[2] * day,
                          pred = exp(log_pred))


cases_au_fit %>%
  mutate(pred_cum_cases = coef(fit)[1] + coef(fit)[2] * day) %>%
  ggplot(aes(x = date, y = exp(log_cases))) +
  geom_line(data = cases_au_extrap, aes(x = date, y = pred), colour = 'blue') +
  geom_point() +
  theme_fivethirtyeight()


cases_au_fit %>%
  mutate(pred_cum_cases = coef(fit)[1] + coef(fit)[2] * day) %>%
  ggplot(aes(x = date, y = exp(log_cases))) +
  geom_line(data = cases_au_extrap, aes(x = date, y = pred), colour = 'blue') +
  geom_point() +
  scale_y_log10() +
  theme_fivethirtyeight()
