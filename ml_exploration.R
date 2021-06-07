# Brian Choi
# Project: R Shiny Exploration
# File: Machine Learning Exploration with MLB Data
# File Created: 6.6.2021
# Last Modified: 6.6.2021

#Prediction 1: Predict whether a team will qualify for the postseason based on team performance

head(teams_2019)

#use the teams_2019postbat 'df' to get the teams that qualified for postseason, assign 1. Assign 0 to others

teams_2019_bats <-
  teams_2019 %>%
  mutate(Postseason = case_when(teamID %in% teams_2019postbat$teamID ~ 1,
                                TRUE ~ 0))
head(teams_2019_bats)

#get teams pitching stats for 2019
teams_2019_pitch <-
  df_pitch %>%
  filter(yearID==2019) %>%
  select(4, 6:7,11:18,22:25,27:30) %>% #'G' column: meaningless here since it's by team.
  group_by(teamID) %>%
  summarise_each(list(sum))
head(teams_2019_pitch)

#GIDP in 'teams_2019_pitch' means DP Induced
#Similarly, H,ER,BB,SH/SF, etc... in pitching = allowed by opponents
#Need to sort through these columns...