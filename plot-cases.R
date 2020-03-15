library(tidyverse)
library(lubridate)
library(ggthemes)


'~/covid-19-plots/data/cases-aus.csv' %>%
read_csv(col_names = c('date', 'cum_cases'),
                     col_types = cols(
                       date = col_date(format = '%d/%m/%Y'),
                       cum_cases = col_integer())) %>%
  mutate(day = (date - min(date)) %>% as.integer()) ->
  cases_au




cases_au %>%
  filter(day >= 30 ) %>%
  ggplot(aes(x = day, y = cum_cases)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  theme_fivethirtyeight()
