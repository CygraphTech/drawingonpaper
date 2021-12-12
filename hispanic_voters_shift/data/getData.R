library(tidyr)
library(dplyr)
library(ggplot2)

results_by_county <- read.csv("./data/election_results.csv", stringsAsFactors = FALSE) %>%
  drop_na()
results_by_county2 <- results_by_county %>%
  mutate(dem = ifelse(dem_candidate == "Donald J. Trump", rep_candidate, dem_candidate)) %>%
  mutate(dem_v = ifelse(dem_candidate == "Donald J. Trump", rep_vote, dem_vote)) %>%
  mutate(dem_s = ifelse(dem_candidate == "Donald J. Trump", rep_score, dem_score)) %>%
  mutate(rep = ifelse(dem_candidate == "Donald J. Trump", dem_candidate, rep_candidate)) %>%
  mutate(rep_v = ifelse(dem_candidate == "Donald J. Trump", dem_vote, rep_vote)) %>%
  mutate(rep_s = ifelse(dem_candidate == "Donald J. Trump", dem_score, rep_score)) %>%
  select(year, county, dem, rep, dem_v, dem_s, rep_v, rep_s)

results_by_county2.csv()
write.csv(results_by_county2, "data/election_results2.csv")

# ANES Parallel Study
poll <- read.csv("data/ANES_Parallel_Study_2016.tab", sep="\t")

poll_data <- poll %>% 
  dplyr::rename(pol = pid3) %>%
  select(pol, gender, race, educ) %>%
  filter(educ %in% c(1,2,3,4,5,6)) %>%
  mutate(pol = case_when(pol == 1~"Democrat", pol == 2~"Republican", pol > 2~"Other")) %>%
  mutate(gender = case_when(gender == 1~"Male", gender == 2~"Female")) %>%
  mutate(educ = case_when(educ %in% c(3,4,5,6)~"College", educ %in% c(1,2)~"Non-college")) %>%
  mutate(race = case_when(race == 1~"White", race %in% c(4,5,6,7,8)~"Asian or other", race == 3~"Hispanic", race == 2~"Black"))

count_by_race_gender <- poll_data %>% 
  group_by(gender, race) %>%
  summarize(total = n())

count_by_gender_race_educ <- poll_data %>% 
  group_by(gender, race, educ) %>%
  summarize(total = n())

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
 
  

write.csv(poll_group, "data/2016_support_by_group.csv", row.names=FALSE, quote=FALSE)

####################
detailed_tables <- read.csv("data/2016_2020_validated_voter_detailed_tables.csv")
race_and_educ <- detailed_tables[120:126,]
gender_and_educ <- detailed_tables[147:151,]
gender_and_race <- detailed_tables[12:18,]

category,year,support_dem, support_rep
White non-college men,2016,48,52
White non-college men,2020,
White non-college women,2016,47,53
White non-college women,2020,
White college men,2016,52,48
White college men,2020,
White college women,2016,53,47
White college women,2016,
Asian or other, 2016,
Asian or other, 2020,
Hispanic men,2016,83,17
Hispanic men,2020,
Hispanic women,2016,80,20
Hispanic women,2020,
Black men,2016,81,19
Black men,2020,
Black women,2016,96,4
Black women,2020,
