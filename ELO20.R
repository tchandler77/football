library(dplyr)
library(readr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)
library(nflfastR)
#set up initial game data 
#raw_data20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
season <- 2020
#raw_data20 <- nflfastR::load_pbp(season)
data20 <- raw_data20 %>%
  filter(game_seconds_remaining == "0") %>%
  group_by(week) %>%
  summarize(
    game_id = game_id, week = week, away_team = away_team, away_score = away_score,
    home_team = home_team, home_score = home_score,
    home_spread = spread_line, home_score_differential = result,
    division_game = div_game,
    pre_total = total_line, total = total
  )
data20 <- unique(data20)

data20$season <- 2020


#insert home_result column, win = 1 lose =0
data20$home_result <- 0
data20$away_result <- 0

data20 <- data20 %>% mutate(home_result = ifelse(home_score_differential > 0, 1, home_result))
data20 <- data20 %>% mutate(home_result = ifelse(home_score_differential == 0, 0.5, home_result))
data20 <- data20 %>% mutate(away_result = abs(1-home_result))

data20$week <- as.numeric(data20$week)
#initialize dvoa data
data20$home_dvoa_rank <- 0
data20$away_dvoa_rank <- 0
data20$home_dvoa <- 0
data20$away_dvoa <- 0

data20$dvoa_rank_diff <- 0
data20$dvoa_diff <- 0

data20$home_epa <- 0
data20$away_epa <- 0
data20$epa_diff<- 0
data20$MOV <- 1

#initialize elo data
data20 <- data20 %>% mutate(
  home_ELO = 1500,
  away_ELO = 1500,
  ELOhome_win_pct = 0
)
#glimpse of data
glimpse(data20)

#set up database of teams and their ELO
teams20 <- data20 %>% 
  group_by(away_team) %>% 
  summarize(
    team = away_team, ELO = away_ELO
  )
teams20 <- teams20[c(2:3)]
teams20 <- unique(teams20)
teams20$ELO <- 1500 
#s is the percent weight given to every team's elo from the previous season
s=20
teams20$ELO <- ((s/100)*teams19$ELO)+(1500*(1-(s/100)))


# library(readxl)
# X2020_Team_DVOA_Ratings_Overall <- read_excel("C:/Users/tommychandler9/Desktop/FOOTBALL/2020 Team DVOA Ratings Overall.xlsx")
# View(X2020_Team_DVOA_Ratings_Overall)

# teams20$DVOA_rank <- X2020_Team_DVOA_Ratings_Overall$`Total DVOA Rank`
# teams20$total_DVOA <- X2020_Team_DVOA_Ratings_Overall$`Total DVOA`
# teams20 <- merge(teams20, dvoa20_all_data)

#epa

# teams20$epa <- total_epa$'2020epa'

# initialize score diff
teams20$score_diff <- 0
data20$total_score_diff_home <- 0 
data20$total_score_diff_away <- 0
data20$diff_score_diff <- 0
data20$diff_score_diff_norm <- 0

library(readxl)
dvoa20_all_data <- read_excel("C:/Users/tommychandler9/Desktop/FOOTBALL/data/dvoa20_all_data.xlsx")
epa20 <- read_excel("C:/Users/tommychandler9/Desktop/FOOTBALL/data/epa20.xlsx")

library(readxl)
dvoa20_offense <- read_excel("C:/Users/tommychandler9/Desktop/FOOTBALL/data/dvoa20_offense.xlsx")
data20$home_O_dvoa <- 0
data20$away_O_dvoa <- 0

for(i in 17:32){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "1"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "1"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "1"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "1"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "1"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "1"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 33:48){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "2"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "2"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "2"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "2"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "2"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "2"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 49:63){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "3"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "3"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "3"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "3"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "3"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "3"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 64:77){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "4"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "4"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "4"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "4"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "4"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "4"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 78:91){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "5"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "5"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "5"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "5"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "5"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "5"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 92:105){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "6"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "6"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "6"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "6"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "6"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "6"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 106:119){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "7"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "7"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "7"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "7"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "7"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "7"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 120:133){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "8"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "8"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "8"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "8"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "8"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "8"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 134:147){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "9"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "9"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "9"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "9"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "9"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "9"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 148:161){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "10"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "10"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "10"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "10"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "10"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "10"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}

for(i in 162:177){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "11"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "11"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "11"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "11"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "11"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "11"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 178:192){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "12"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "12"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "12"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "12"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "12"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "12"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 193:208){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "13"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "13"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "13"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "13"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "13"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "13"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 209:224){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "14"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "14"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "14"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "14"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "14"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "14"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 225:240){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "15"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "15"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "15"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "15"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "15"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "15"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 241:256){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "16"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "16"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "16"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "16"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "16"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "16"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
for(i in 257:269){
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  DVOA_H <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_H, "17"])
  DVOA_A <- as.numeric(dvoa20_all_data[dvoa20_all_data$team == team_A, "17"])
  data20$home_dvoa[i] <- DVOA_H
  data20$away_dvoa[i] <- DVOA_A
  EPA_H <- as.numeric(epa20[epa20$team == team_H, "17"])
  EPA_A <- as.numeric(epa20[epa20$team == team_A, "17"])
  data20$home_epa[i] <- EPA_H
  data20$away_epa[i] <- EPA_A
  DVOA_O_H <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_H, "17"])
  DVOA_O_A <- as.numeric(dvoa20_offense[dvoa20_offense$team == team_A, "17"])
  data20$home_O_dvoa[i] <- DVOA_O_H
  data20$away_O_dvoa[i] <- DVOA_O_A 
}
# for(i in 0:16){
#   team_H <- data20$home_team[i]
#   team_A <- data20$away_team[i]
#   DVOA_H <- as.numeric(teams19[teams19$team == team_H, "total_DVOA"])
#   DVOA_A <- as.numeric(teams19[teams19$team == team_A, "total_DVOA"])
#   data20$home_dvoa[i] <- DVOA_H
#   data20$away_dvoa[i] <- DVOA_A
# }
  #ELO CALCULATIONS
#start updating loop
for(i in 1:nrow(data20)){
  print(i)
  
  team_H <- data20$home_team[i]
  team_A <- data20$away_team[i]
  
  data20$total_score_diff_home[i] <- as.numeric(teams20[teams20$team == team_H, "score_diff"])
  data20$total_score_diff_away[i] <- as.numeric(teams20[teams20$team == team_A, "score_diff"])
  diff_score_diff <- as.numeric(data20$total_score_diff_home[i] - data20$total_score_diff_away[i])
  diff_score_diff_norm <- diff_score_diff/data20$week[i] * 17
 
  data20$diff_score_diff[i] <- diff_score_diff
  data20$diff_score_diff_norm[i] <- diff_score_diff_norm
  
  score_diff <- data20$home_score_differential[i]
  score_diff_home <- score_diff 
  score_diff_away <- -1 * score_diff 
  
  total_score_diff_home <-   teams20[teams20$team == team_H, "score_diff"] + score_diff_home
  total_score_diff_away <-   teams20[teams20$team == team_A, "score_diff"] + score_diff_away
  
  
  teams20[teams20$team == team_H, "score_diff"] <- total_score_diff_home
  teams20[teams20$team == team_A, "score_diff"] <- total_score_diff_away
  
  data20$dvoa_diff[i] <- data20$home_dvoa[i] - data20$away_dvoa[i]
  data20$epa_diff[i] <- data20$home_epa[i] - data20$away_epa[i]
  
  Result_H <- data20$home_result[i]
  Result_A <- data20$away_result[i]
  
  #get current elo
  ELO_H <- as.numeric(teams20[teams20$team == team_H, "ELO"])
  ELO_A <- as.numeric(teams20[teams20$team == team_A, "ELO"])
  
  #load current elo into dataset
  data20$home_ELO[i] <- ELO_H + 10
  data20$away_ELO[i] <- ELO_A
  
  #UPDATE ELO 
  R_home <- 10^(data20$home_ELO/400)
  R_away <- 10^(data20$away_ELO/400)
  
  E_home <- R_home/(R_home + R_away)
  E_away <- R_away/(R_home + R_away)
  
  home_spread <- data20$home_spread
  pregame_elo_diff <- abs(ELO_H - ELO_A)
  spread_score_diff <- abs(score_diff - home_spread)
  data20$MOV[i] <- 1 + abs(score_diff)/28
 
  #current win percent
  elo_diff <- ELO_H - ELO_A
  data20$ELOhome_win_pct[i] <- 1/(1+10^(-1*(elo_diff/400)))
  
  #Update ELO
  Elo_Updated_home <- ELO_H + (20*MOV) * (Result_H - E_home)
  Elo_Updated_away <- ELO_A + (20*MOV) * (Result_A - E_away)
  
  teams20[teams20$team == team_H, "ELO"] <- Elo_Updated_home[i]
  teams20[teams20$team == team_A, "ELO"] <- Elo_Updated_away[i]
  
}


#check how well model predicts winners and spread
data20 <- data20 %>% mutate(
  home_ELO = as.numeric(home_ELO),
  away_ELO = as.numeric(away_ELO),
  Elo_Difference = home_ELO - away_ELO,
  Elo_Forecast_Pred = ifelse(home_ELO > away_ELO, 1, 0),
  Elo_Forecast_Result = ifelse(Elo_Forecast_Pred == home_result, 1, 0),
)

model_pct <- sum(data20$Elo_Forecast_Result)/nrow(data20) 
model_pct <- model_pct*100



data20$Elo_Difference_adjusted <- ifelse(input20$Elo_Difference < 0, -1 * sqrt(abs(input20$Elo_Difference)), sqrt(abs(input20$Elo_Difference)))

#check how well model predicts spread
data20 <- data20 %>% mutate(
  predicted_spread = Elo_Difference/25,
  bet_favorite_spread = ifelse(home_spread>0, ifelse(predicted_spread>home_spread, 1,0), ifelse(predicted_spread<home_spread, 1,0)),
  favorite_covers_spread = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread = ifelse(bet_favorite_spread == favorite_covers_spread, 1, 0),
  
)

spread_win_pct <- sum(data20$win_spread)/nrow(data20)
spread_win_pct <- spread_win_pct * 100


#$$$$$$$$$$$$$$$$$$$%^&*&^%$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#NEW MODEL, NEED TO TEST
data20 <- data20 %>% mutate(
  #regression_spread = 0.838477 + home_spread * 0.220940 + dvoa_diff * 12.607141 + epa_diff * 26.228250 + diff_score_diff * -0.029321,
  #regression_spread = 0.356051  + home_spread * 0.411509 + dvoa_diff * 9.536038 + epa_diff * 23.917253   + diff_score_diff_norm * -0.020750,
  #regression_spread = -1.29811 + home_spread * 0.82718 + dvoa_diff * -4.23597 + epa_diff * 16.01364 + diff_score_diff_norm * -0.01118, 
  #regression_spread = 0.731491 + home_spread * 0.264119 + dvoa_diff * 12.041525 + epa_diff * 27.955343 + diff_score_diff_norm * -0.020714,
  regression_spread = home_ELO * 0.04410  + away_ELO * -0.04426 + Elo_Difference *  -0.36847 + dvoa_diff * 12.44874 + home_O_dvoa * 2.29484 + away_O_dvoa * -4.45970,
  bet_favorite_spread1 = ifelse(home_spread>0, ifelse(regression_spread>home_spread, 1,0), ifelse(regression_spread<home_spread, 1,0)),
  favorite_covers_spread1 = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread1 = ifelse(bet_favorite_spread1 == favorite_covers_spread1, 1, 0),
)
spread_win_pct1 <- sum(data20$win_spread1)/nrow(data20)
spread_win_pct1 <- spread_win_pct1 * 100
data20 <- data20 %>% mutate(
  regression_predict_win = ifelse(regression_spread>0, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0))
)
# data20$regression_spread <- format(data20$regression_spread, scientific=F)



regression_win_pct <- sum(data20$regression_predict_win)/nrow(data20)
regression_win_pct <- regression_win_pct * 100

dec_regression_spread_pct <- spread_win_pct1 / 100
dec_elo_spread_pct <- spread_win_pct/100
# units <- (256 * 1 * dec_regression_spread_pct) - (256 * 1.1 * (1 - dec_regression_spread_pct))
units <- (256 * 1 * dec_elo_spread_pct) - (256 * 1.1 * (1 - dec_elo_spread_pct))

print(units)
#print
sprintf("elo model is correct %f percent of the time", model_pct)
sprintf("elo model predicts spread %f percent of the time", spread_win_pct)
# sprintf("regression model predicts winner %f percent of the time", regression_win_pct)
# sprintf("regression model wins spread bet %f percent of the time", spread_win_pct1)


