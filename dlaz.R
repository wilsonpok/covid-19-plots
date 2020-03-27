# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(httr)
library(janitor)
library(ggrepel)
library(ggthemes)

# URL where the dataset is stored with automatic updates every day
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time() - 24*3600, "%Y-%m-%d"), ".xlsx", sep = "")
# download the dataset from the website to a local temporary file
GET(url, authenticate(":", ":", type = "ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
# read the Dataset sheet into R
coronavirus_df <- read_excel(tf)
# fix names
coronavirus_df <- clean_names(coronavirus_df, case = 'snake')
# get trajectories
clean_coronavirus_df <- coronavirus_df %>%
  mutate(date = as.Date(date_rep)) %>%
  arrange(countries_and_territories, date) %>%
  group_by(countries_and_territories) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(
    # for countries since 100th case
    cum_cases >= 100,
    # and total number of cases > 1000
    max(cum_cases) > 1000
  ) %>%
  mutate(days_since_100_cases = as.numeric(date - min(date)))


clean_coronavirus_df %>% distinct(geo_id)

clean_coronavirus_df$geo_id %>% unique()
clean_coronavirus_df$countries_and_territories %>% unique()


select_coronavirus_df <- clean_coronavirus_df %>%
  filter(countries_and_territories %in% c('Australia',
                                          'Brazil', 'South_Korea',
                                          'Turkey', 'France', 'Japan',
                                          'United_Kingdom'))

# lovely plot
select_coronavirus_df %>%
  ggplot(
    aes(
      x = days_since_100_cases,
      y = cum_cases,
      colour = countries_and_territories
    )
  ) +
  geom_line(size = 1.5) +
  geom_label_repel(
    data = select_coronavirus_df %>%
      group_by(countries_and_territories) %>%
      filter(days_since_100_cases == max(days_since_100_cases)) %>%
      ungroup(),
    aes(label = countries_and_territories)
  ) +
  ylab("Number of total cases") +
  xlab("Days since 100th case") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = 'none')

ggsave('~/Desktop/covid-19.png', width = 8, height = 6)
