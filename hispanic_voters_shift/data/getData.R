library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(reshape)

results_by_county <- read.csv("./data/election_results.csv", stringsAsFactors = FALSE) %>%
  drop_na()
results_by_county <- results_by_county %>%
  mutate(county = str_pad(county, 5, pad = "0")) %>%
  mutate(dem = ifelse(dem_candidate == "Donald J. Trump", rep_candidate, dem_candidate)) %>%
  mutate(dem_v = ifelse(dem_candidate == "Donald J. Trump", rep_vote, dem_vote)) %>%
  mutate(dem_s = ifelse(dem_candidate == "Donald J. Trump", rep_score, dem_score)) %>%
  mutate(rep = ifelse(dem_candidate == "Donald J. Trump", dem_candidate, rep_candidate)) %>%
  mutate(rep_v = ifelse(dem_candidate == "Donald J. Trump", dem_vote, rep_vote)) %>%
  mutate(rep_s = ifelse(dem_candidate == "Donald J. Trump", dem_score, rep_score)) %>%
  select(year, county, dem, rep, dem_v, dem_s, rep_v, rep_s)

write.csv(results_by_county, "data/election_results_by_county_2016_2020.csv", row.names = FALSE)

head(results_by_county)

election_results_by_county_2016_2020 <- read.csv("./data/election_results_by_county_2016_2020.csv", 
                              colClasses = c("character"),
                              stringsAsFactors = FALSE)
election_results_by_county_2016_2020 <- election_results_by_county_2016_2020 %>%
  mutate(dem_v = readr::parse_number(dem_v),
         dem_s = readr::parse_number(dem_s),
         rep_v = readr::parse_number(rep_v),
         rep_s = readr::parse_number(rep_s))

results_m <- election_results_by_county_2016_2020
  melt(id.vars=c("county"), measure.vars=c("rep_s", "year"))

results_2016 <- results_m %>%
  filter(year == "2016") %>%
  mutate(dem_v_16 = dem_v, dem_s_16 = dem_s, rep_v_16 = rep_v, rep_s_16 = rep_s) %>%
  select(c(county, dem_v_16, dem_s_16, rep_v_16, rep_s_16))

results_2020 <- results_m %>%
  filter(year == "2020") %>%
  mutate(dem_v_20 = dem_v, dem_s_20 = dem_s, rep_v_20 = rep_v, rep_s_20 = rep_s) %>%
  select(c(county, dem_v_20, dem_s_20, rep_v_20, rep_s_20))

election_results_by_county_2016_2020 <- results_2016 %>%
  left_join(results_2020, by = "county") %>%
  mutate(margin = rep_s_20 - rep_s_16)

election_results_by_county_2016_2020 %>%
  arrange(desc(margin)) %>%
  head()

#####################
#    POPULATION     #
#####################

population <- read.csv("data/population_by_county.csv", colClasses = c("county"="character"))

population_and_results <- population %>%
  dplyr::rename(pop=P2_001N, pop_hispanic=P2_002N) %>%
  mutate(density= round(pop_hispanic/pop, digits=2)) %>%
  left_join(election_results_by_county_2016_2020, by="county") %>%
  arrange(desc(margin))

population_and_results <- population_and_results %>%
  %>%
  mutate(state=tail(str_split(NAME, ", ")[[1]], n=1))
  select(c(GEO_ID, county, NAME, state, density, margin, pop_hispanic, pop, rep_s_16, rep_s_20, dem_s_16, dem_s_20))

population_and_results %>%
  drop_na() %>%
  arrange(desc(density)) %>%
  head(10) %>%
  View()

population_and_results <- population_and_results 
write.csv(population_and_results, "data/population_and_results_2016_2020.csv", row.names=FALSE)

#######################
# ANES Parallel Study #
#######################
#
# Shift by categories:
# - White non-college men
# - White non-college women
# - White college men
# - White college women
# - Asian or others
# - Hispanic men
# - Hispanic women
# - Black men
# - Black women
#

# The data for 2016:
poll <- read.csv("data/ANES_Parallel_Study_2016.tab", sep="\t")

poll_data <- poll %>% 
  dplyr::rename(pol = POSTVOTE_PRESVTWHO) %>%
  filter(pol %in% c(1,2)) %>%
  select(pol, gender, race, educ) %>%
  filter(educ %in% c(1,2,3,4,5,6)) %>%
  filter(gender %in% c(1,2)) %>%
  mutate(pol = case_when(pol == 1~"Republican", pol == 2~"Democrat")) %>%
  mutate(gender = case_when(gender == 1~"Male", gender == 2~"Female")) %>%
  mutate(educ = case_when(educ %in% c(3,4,5,6)~"College", educ %in% c(1,2)~"Non-college")) %>%
  mutate(race = case_when(race == 1~"White", race %in% c(4,5,6,7,8)~"Asian or other", race == 3~"Hispanic", race == 2~"Black"))

count_by_race <- poll_data %>% 
  group_by(race) %>%
  summarize(total = n())

count_by_race_gender <- poll_data %>% 
  group_by(gender, race) %>%
  summarize(total = n())

count_by_gender_race_educ <- poll_data %>% 
  group_by(gender, race, educ) %>%
  summarize(total = n())

poll_group_race <- poll_data %>%
  group_by(pol, race) %>%
  summarize(count = n()) %>%
  left_join(count_by_race, by = c('race')) %>%
  mutate(support = count / total)

poll_group_gender_race_educ <- poll_data %>%
  group_by(pol, gender, race, educ) %>%
  summarize(count = n()) %>%
  left_join(count_by_gender_race_educ, by = c('gender', 'race', 'educ')) %>%
  mutate(support = count / total)

poll_group_race_gender <- poll_data %>%
  group_by(pol, gender, race) %>%
  summarize(count = n()) %>%
  left_join(count_by_race_gender, by = c('gender', 'race')) %>%
  mutate(support = count / total)

write.csv(poll_group_race, "data/2016_by_race.csv", row.names=FALSE)
write.csv(poll_group_race_gender, "data/2016_by_race_gender.csv", row.names=FALSE)
write.csv(poll_group_gender_race_educ, "data/2016_by_gender_race_educ.csv", row.names=FALSE)


####################
detailed_tables <- read.csv("data/2016_2020_validated_voter_detailed_tables.csv")
race <- detailed_tables[5:10,]
race_and_educ <- detailed_tables[120:126,]
gender_and_educ <- detailed_tables[147:151,]
gender_and_race <- detailed_tables[12:18,]

results_by_category <- read.csv("data/results_by_category.csv") %>%
  mutate(margin = rep_s_20 - rep_s_16) %>%
  select(c(category, margin, rep_s_20, rep_s_16)) %>%
  arrange(desc(margin))
