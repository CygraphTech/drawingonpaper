library(dplyr)
library(ggplot2)
library(tidyr)

data_fao <- read.csv("data/FAOSTAT_data_12-22-2021.csv")

data_fao_by_year <- data_fao %>% 
  filter(Item == "Forest land") %>%
  group_by(Year, Item) %>% 
  summarise(surface = sum(Value)) %>%
  ungroup() %>%
  mutate(rate = -(surface - lag(surface, n=1))/1000) %>%
  dplyr::rename(year = Year) %>%
  drop_na() %>%
  select(year, rate) 

data_williams <- read.csv("data/data_williams.csv")

data_williams_by_year <- data_williams[rep(row.names(data_williams), data_williams$n_years), 1:6]
data_williams_by_year <- data_williams_by_year %>% 
  mutate(year = strtoi(substr(date, 1, 4))) %>%
  group_by(date) %>%
  mutate(year = year + row_number() - 1) %>%
  ungroup() %>%
  select(year, rate) %>%
  filter(year <= 1990)

data <- rbind(data_williams_by_year, data_fao_by_year) %>%
  mutate(decade = year - year %% 10) %>%
  group_by(decade) %>%
  summarize(loss = sum(rate))

write_csv(data, "data/data_by_decade.csv")

