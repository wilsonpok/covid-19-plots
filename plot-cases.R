library(tidyverse)
library(lubridate)
library(ggthemes)


'~/covid-19-plots/data/cases-aus.csv' %>%
read_csv(col_names = c('date', 'cum_cases'),
                     col_types = cols(
                       date = col_date(format = '%d/%m/%Y'),
                       cum_cases = col_integer())) %>%
  mutate(day = (date - min(date)) %>% as.integer()) %>%
  mutate(country = 'AU') ->
  cases_au


'~/covid-19-plots/data/cases-itl.csv' %>%
  read_csv(col_names = c('date', 'cum_cases'),
           col_types = cols(
             date = col_date(format = '%Y-%m-%d'),
             cum_cases = col_integer())) %>%
  mutate(day = (date - min(date)) %>% as.integer()) %>%
  mutate(country = 'IT') ->
  cases_it




cases_au %>%
  ggplot(aes(x = day, y = cum_cases)) +
  geom_point() +
  geom_line() +
  theme_fivethirtyeight()


cases_au %>%
  filter(day >= 30 ) %>%
  ggplot(aes(x = day, y = cum_cases)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  theme_fivethirtyeight()



cases <- cases_au %>%
  mutate(day = day - 25) %>%
  bind_rows(cases_it)



cases %>%
  ggplot(aes(x = day, y = cum_cases, colour = country)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  scale_colour_fivethirtyeight() +
  theme_fivethirtyeight()





cases_au_fit <- cases_au %>%
  mutate(day = day + 1) %>%
  mutate(x = log(day)) %>%
  mutate(y = log(cum_cases)) %>%
  filter(x > 3.5)






lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

cases_au_fit %>%
  mutate(pred = preds) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  geom_text(x = 3.65, y = 5, label = lm_eqn(cases_au_fit), parse = TRUE)



fit <- lm(y ~ x, data = cases_au_fit)
preds <- predict(fit)
coef(fit)[1]
coef(fit)[2]





date_range <- seq(from = min(cases_au_fit$date),
                  to = ymd('2020-04-01'),
                  by = 1)

day_range <- seq(from = min(cases_au_fit$day),
                length.out = length(date_range))

cases_au_extrap <- tibble(date = date_range,
                          day = day_range,
                          x = log(day),
                          pred_cum_cases = exp(coef(fit)[1])*exp(coef(fit)[2]*x))

cases_au_fit %>%
  ggplot(aes(x = date, y = cum_cases)) +
  geom_point() +
  geom_line(data = cases_au_extrap,
            aes(x = date, y = pred_cum_cases), colour = 'blue')


