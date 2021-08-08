# Brian Choi
# Project: R Shiny Exploration
# File: MLB Data Exploration
# File Created: 5.27.2021
# Last Modified: 8.8.2021

#Data Source: http://www.seanlahman.com/baseball-archive/statistics/
#For more details on data dictionary: check 'readme2014.txt'

## Import packages ##
library(dplyr)
library(sqldf)
library(ggplot2)
library(ggrepel)

#See sample data
#read.csv("./Data/AllstarFull.csv") #ALL STARs
#read.csv("./Data/Batting.csv") #Batting stats
#read.csv("./Data/BattingPost.csv") #Postseason Batting (has rounds as well)
#read.csv("./Data/Fielding.csv") #Fielding stats (assists, errors, DP, etc.)
#read.csv("./Data/Pitching.csv") #Pitching stats
#read.csv("./Data/PitchingPost.csv") #Postseason Pitching
#read.csv("./Data/Salaries.csv") #Salaries by year


##############################
######## Import Data #########
##############################

df_as <- read.csv("./Data/AllstarFull.csv") #All Stars
df_bat <- read.csv("./Data/Batting.csv") #batting (reg season)
df_postbat <- read.csv("./Data/BattingPost.csv") #batting (postseason)
df_field <- read.csv("./Data/Fielding.csv") #fielding
df_pitch <- read.csv("./Data/Pitching.csv") #pitching
df_postpitch <- read.csv("./Data/PitchingPost.csv") #Postseason Pitching
df_sal <- read.csv("./Data/Salaries.csv") #salaries
df_people <- read.csv("./Data/People.csv") #people

#Take a look at some columns of interest
names(df_as) #playerID, yearID, startingPos
names(df_bat) #playerID, yearID, G, AB, R, H, 2B, 3B, HR, RBI, SB, BB, SO, etc..
names(df_postbat)#playerID, yearID, G, AB, R, H, 2B, 3B, HR, RBI, SB, BB, SO, etc..
names(df_field) #playerID, yearID, A, E, DP
names(df_pitch) #playerID, yearID, W, L, G, ER, HR, BB, SO, ERA 
names(df_sal) #playerID, yearID, salary
names(df_people) #playerID, first & last names

######################################
######## Subset/Explore Data #########
######################################

## All Star Dataset ## -> probably won't use this data too much yet.
head(df_as)
str(df_as)
summary(df_as$yearID) #Year of All Stars from 1933 to 2019.

#Let's only look at the last decade. 
#Store this dataframe for use later
df_as_decade <- df_as %>% 
  filter(yearID >= 2010) %>%
  select(playerID, yearID, teamID, lgID, startingPos)


## Batting Dataset ##
head(df_bat)
str(df_bat)
summary(df_bat$yearID) #min 1871, max 2020. 2020...?
df_bat %>% filter(yearID == 2020) #Guess there are some 2020 data. Omit them

df_bat_decade <- df_bat %>%
  filter(yearID >= 2010 & yearID < 2020)

## BattingPost Dataset ##
head(df_postbat) #postseason batting stats are separated by each round
summary(df_postbat$yearID) 
df_postbat_decade <- df_postbat %>%
  filter(yearID >= 2010 & yearID < 2020)

## Fielding Dataset ##
head(df_field)
summary(df_field$yearID) #1871 to 2020
df_field_decade <- df_field %>%
  filter(yearID >= 2010 & yearID < 2020) %>%
  select(playerID, yearID, teamID, lgID, POS, G, GS, InnOuts, PO, A, E, DP)


## Pitching Dataset ##
head(df_pitch)
summary(df_pitch$yearID) #1871 to 2020
df_pitch_decade <- df_pitch %>%
  filter(yearID >= 2010 & yearID < 2020) %>%
  select(playerID, yearID, teamID, lgID, W, L, G, GS, CG, SHO, SV, IPouts, 
         H, ER, HR, BB, SO, ERA, HBP, BK, BFP, R, SH, SF, GIDP)
head(df_pitch_decade)


## Salary Dataset ##
head(df_sal)
summary(df_sal$yearID) #only from 1985 to 2016 (not sure why)
#keep all columns


#################################
######## Basic Analysis #########
#################################

# Best Batting Avg Holders from 2010 to 2019
head(df_bat_decade)
#Since there is no 'AVG' column, need to divide H by AB
df_bat_decade$AVG = round(df_bat_decade$H/df_bat_decade$AB, 3)

#check out the null values
df_bat_decade %>% filter(is.na(AVG)) #no hit data.. can remove these rows.
df_bat_decade <- df_bat_decade %>% filter(!is.na(AVG))
head(df_bat_decade)

summary(df_bat_decade$AB) #Mean AB is only 162
summary(df_bat_decade$G) #Mean number of games: 58

#With standard season of 162 games, players need 502 plate appearances (PA) to qualify for the batting title
#For this dataset, let's have PA = AB + BB + IBB + HBP + SH + SF 
#Missing data: defensive interference, reaching base by error, etc. We'll work with what we have.
df_bat_decade2 <- 
  df_bat_decade %>%
  mutate(PA = rowSums(df_bat_decade[, c(7, 16, 18, 19, 20, 21)]))

summary(df_bat_decade2$PA) #mean plate appearance: 182, 3rd quartile 313, max 754
df_bat_decade2 %>% filter(PA>=502) #only 1360 players with 502+ PAs over 10 years?

#Get the highest batting AVG holders for each season

BattingChamps <- 
  df_bat_decade2 %>% 
  filter(PA>=502) %>%
  group_by(yearID) %>%
  top_n(1, AVG)

BattingChamps
#Most recent Batting Champs: 
#Tim Anderson (2019)
#Mookie Betts (2018)
#Jose Altuve (2017 major cheating season ^^)


#Back to using 'df_bat_decade2'

## RBIs vs. Hits ##

df_bat_decade2 %>%
  mutate(Batting=case_when(H >= 200 ~ "200+ Hits",
                       H >= 100 & H < 200 ~ "Between 100 and 200 Hits",
                       H < 100 ~ "Less than 100 Hits")) %>%
  ggplot(aes(x=H, y=RBI, color = Batting)) +
  geom_point() +
  theme_minimal() +
  ggtitle('RBIs vs. Hits')

#seems difficult to obtain those 200+ hits, as there are only a few hitters in that category
# df_bat_decade2 %>%
#   filter(H>=200)

#In general, players with more hits tend to have more RBIs
#Take a look at a few players with 175+hits and < 50 RBIs
#Most, if not all, of these players were lead off hitters, hence the lack of runs batted in.
df_bat_decade2 %>%
  filter(H>=175 & RBI<50)
#They all have a high number of plate appearances.


## SO vs. HRs ##
#to see if those with a lot of HRs also have a high strike out rate (swings aggressively) ##
#For simplicity, let's only look at 2019 records
df_bat_decade2 %>%
  filter(yearID == 2019) %>%
  ggplot(aes(x=HR, y=SO)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method=lm) + #add regression line
  ggtitle('Strikeouts vs Homeruns')
#In general, seems to be true that players with more HRs tend to have more SO
#Pete Alonso hit 53 HRs, and struck out 183 times (3rd most)
df_bat_decade2 %>%
  filter(yearID == 2019) %>%
  mutate(Top5_Most_SO = ifelse(SO >= sort(SO, decreasing = TRUE)[5], T, F)) %>%
  ggplot(aes(x=HR, y=SO, label = playerID)) +
  geom_point(aes(color = Top5_Most_SO)) +
  scale_color_manual(values = c('#595959', 'red')) +
  geom_text_repel(aes(label = ifelse(SO >= sort(SO, decreasing = TRUE)[5], playerID,'')),
                  box.padding = 0.35,
                  point.padding = 0.5,
                  segment.color = 'grey50') +
  theme_minimal() +
  geom_smooth(method=lm) + #add regression line
  ggtitle('Strikeouts vs Homeruns')

#According to sabermetrics, an excellent strikeout rate is (K)%: 10%, and BB%: 15% where
#K% = K / PA
#BB% = BB / PA
#Source: https://library.fangraphs.com/offense/rate-stats/

#Need to calculate the two new variables
head(df_bat_decade2)

df_bat_decade3 <-
  df_bat_decade2 %>%
  mutate(K_Rate = round(SO/PA,2),
         BB_Rate = round(BB/PA,2))
head(df_bat_decade3)

## Strikeout rate summary ##
summary(df_bat_decade3$K_Rate) #median 24%, mean 29%
summary(df_bat_decade3[df_bat_decade3$yearID==2019,]$K_Rate) #Look at single year (2019)
#median: 27%, mean: 35%

## Plot K_Rate vs. H (in 2019 only) ##
df_bat_decade3 %>%
  filter(yearID==2019) %>% #only 2019  
  ggplot(aes(x=K_Rate, y=H)) +
  geom_point() +
  geom_vline(aes(xintercept=0.27), color='red') +
  geom_vline(aes(xintercept=0.35), color = 'blue') +
  annotate(x=0.27,y=+Inf,label="Median K Rate",vjust=2,geom="label") +
  annotate(x=0.35,y=220,label="Average K Rate",vjust=2,geom="label") +
  theme_minimal() +
  ggtitle('K_Rate vs. RBIs')

#Most good hitters (e.g. 150+ H in a season) tend to have a LESS strikeout rate than the median/average K rate.
#Use what's considered "excellent" K rate = 10%
df_bat_decade3 %>%
  filter(yearID==2019) %>% #only 2019  
  ggplot(aes(x=K_Rate, y=H)) +
  geom_point() +
  geom_vline(aes(xintercept=0.10), color='red') +
  annotate(x=0.10,y=+Inf,label="Excellent K Rate",vjust=1,geom="label") +
  theme_minimal() +
  ggtitle('K_Rate vs. RBIs')
#There's very few hitters with K rate < 10%, and only a few of these players have 150+ hits.


## Base on balls (BB) rate summary ##
summary(df_bat_decade3[df_bat_decade3$yearID==2019,]$BB_Rate) #Look at single year (2019)
#median: 6%, mean: 6%

## Plot H vs. BB_Rate
df_bat_decade3 %>%
  filter(yearID==2019) %>% #only 2019  
  ggplot(aes(x=BB_Rate, y=H)) +
  geom_point() +
  geom_vline(aes(xintercept=0.06), color='red') +
  annotate(x=0.06,y=+Inf,label="Average BB Rate",hjust=-0.2, vjust=1, geom="label") +
  theme_minimal() +
  ggtitle('Hits vs. BB_Rate')

#Use what's considered "excellent" BB rate = 15%
df_bat_decade3 %>%
  filter(yearID==2019) %>% #only 2019  
  ggplot(aes(x=BB_Rate, y=H)) +
  geom_point() +
  geom_vline(aes(xintercept=0.15), color='red') +
  annotate(x=0.15,y=+Inf,label="Excellent BB Rate",hjust=-0.2, vjust=1, geom="label") +
  theme_minimal() +
  ggtitle('Hits vs. BB_Rate')
#Players to the right of this line have really good eye (doesn't reach for many bad balls/balls off the plate)


## SB vs. CS ##
#see if running aggressive base running (SB) led to more caught stealing (CS)
df_bat_decade3 %>%
  filter(yearID==2019 & SB>0 & CS>0 & PA>=200) %>% #look at 2019, and PA>=200
  ggplot(aes(x=SB,CS)) +
  geom_point() +
  theme_minimal() +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) +
  ggtitle('Stolen Bases vs. Caught Stealing')
#Players with more SBs tend to have slightly more CS (weak linear pattern)


## Look at team performance (2019)
#use the df_bat dataset
teams_2019 <-
  df_bat %>%
  filter(yearID==2019) %>%
  select(4, 7:22) %>% #'G' column: meaningless here since it's by team.
  group_by(teamID) %>%
  summarise_each(list(sum))
head(teams_2019)

#Teams with most hits
teams_2019 %>%
  arrange(desc(H)) #Boston Red Sox

#Most HR
teams_2019 %>%
  arrange(desc(HR)) %>% #Minnesota Twins
  print(n=15) 

#Most RBI
teams_2019 %>%
  arrange(desc(RBI)) #Minnesota Twins

#Least number of Strikeouts
teams_2019 %>%
  arrange(SO) #Houston Astros

#2019 World Series Champion
teams_2019 %>%
  filter(teamID=='WAS') #WAS won the 2019 World Series without having the best offense 
#They were 7th in H and RBI, 13th in HR, but 4th in number of strikeouts (not bad)

#Look at WAS's postbatting stats
teams_2019postbat <-
  df_postbat %>%
  filter(yearID==2019) %>%
  select(4, 7:22) %>%
  group_by(teamID) %>%
  summarise_each(list(sum))

#Look at team batting avg
head(teams_2019postbat)
teams_2019postbat %>%
  mutate(TeamBatting_Avg = H/AB) %>%
  arrange(desc(TeamBatting_Avg)) #WAS (along with OAK) did have the best batting avg as a team in the postseason.

#Check out team SO rate
teams_2019postbat %>%
  mutate(TeamSO_Rate = SO/AB) %>%
  arrange(TeamSO_Rate) #WAS had a great SO rate as well (2nd after HOU)

#Check out postseason pitching
head(df_postpitch)

teams_2019postpitch <-
  df_postpitch %>%
  filter(yearID==2019) %>%
  select(4,13:19)
head(teams_2019postpitch)

#Calculate number of innings pitched/ER to calculate team ERA
teams_2019postpitch %>%
  select(1:7) %>%
  group_by(teamID) %>%
  summarise_each(list(sum)) %>%
  mutate(IP=IPouts/3, #IP = IPouts/3
         Team_ERA = (9*ER)/IP, #ERA = 9*ER / IP
         Team_WHIP = (H+BB)/IP) %>% #WHIP = (H+BB)/IP
  arrange(Team_ERA) #WAS had a solid team ERA as well
