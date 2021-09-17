library(dplyr)
library(readr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
#set up initial game data 
#raw_data18 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
season <- 2018
#raw_data18 <- nflfastR::load_pbp(season)
data18 <- raw_data18 %>%
  filter(game_seconds_remaining == "0") %>%
  group_by(week) %>%
  summarize(
    game_id = game_id, week = week, away_team = away_team, away_score = away_score,
    home_team = home_team, home_score = home_score,
    home_spread = spread_line, home_score_differential = result,
    division_game = div_game,
    pre_total = total_line, total = total
  )
data18 <- unique(data18)

data18$season <- 2018

#insert home_result column, win = 1 lose =0
data18$home_result <- 0
data18$away_result <- 0

data18 <- data18 %>% mutate(home_result = ifelse(home_score_differential > 0, 1, home_result))
data18 <- data18 %>% mutate(home_result = ifelse(home_score_differential == 0, 0.5, home_result))
data18 <- data18 %>% mutate(away_result = abs(1-home_result))

#initialize dvoa data
data18$home_dvoa_rank <- 0
data18$away_dvoa_rank <- 0
data18$home_dvoa <- 0
data18$away_dvoa <- 0

data18$dvoa_rank_diff <- 0
data18$dvoa_diff <- 0

data18$home_epa <- 0
data18$away_epa <- 0
data18$epa_diff<- 0

#initialize elo data
data18 <- data18 %>% mutate(
  home_ELO = 1500,
  away_ELO = 1500,
  ELOhome_win_pct = 0
)
#glimpse of data
glimpse(data18)

#set up database of teams and their ELO
teams18 <- data18 %>% 
  group_by(away_team) %>% 
  summarize(
    team = away_team, ELO = away_ELO
  )
teams18 <- teams18[c(2:3)]
teams18 <- unique(teams18)
teams18$ELO <- 1500 
#s is the percent weight given to every team's elo from the previous season
s=20
teams18$ELO <- ((s/100)*teams17$ELO)+(1500*(1-(s/100)))


library(readxl)
X2018_Team_DVOA_Ratings_Overall <- read_excel("C:/Users/tommychandler9/Desktop/FOOTBALL/2018 Team DVOA Ratings Overall.xlsx")
# View(X2018_Team_DVOA_Ratings_Overall)

teams18$DVOA_rank <- X2018_Team_DVOA_Ratings_Overall$`Total DVOA Rank`
teams18$total_DVOA <- X2018_Team_DVOA_Ratings_Overall$`Total DVOA`

#epa
teams18$epa <- total_epa$'2018epa'

# initialize score diff
teams18$score_diff <- 0
data18$total_score_diff_home <- 0 
data18$total_score_diff_away <- 0

data18$diff_score_diff <- 0
#ELO CALCULATIONS
#start updating loop
for(i in 1:nrow(data18)){
  print(i)
  
  team_H <- data18$home_team[i]
  team_A <- data18$away_team[i]
  
  data18$total_score_diff_home[i] <- as.numeric(teams18[teams18$team == team_H, "score_diff"])
  data18$total_score_diff_away[i] <- as.numeric(teams18[teams18$team == team_A, "score_diff"])
  diff_score_diff <- as.numeric(data18$total_score_diff_home[i] - data18$total_score_diff_away[i])
  data18$diff_score_diff[i] <- diff_score_diff
  
  score_diff <- data18$home_score_differential[i]
  score_diff_home <- score_diff 
  score_diff_away <- -1 * score_diff 
  
  total_score_diff_home <-   teams18[teams18$team == team_H, "score_diff"] + score_diff_home
  total_score_diff_away <-   teams18[teams18$team == team_A, "score_diff"] + score_diff_away
  
 
  
  teams18[teams18$team == team_H, "score_diff"] <- total_score_diff_home
  teams18[teams18$team == team_A, "score_diff"] <- total_score_diff_away
  
  
  Result_H <- data18$home_result[i]
  Result_A <- data18$away_result[i]
  
  #get dvoa (these values are end of season totals)
  DVOA_rank_H <- as.numeric(teams18[teams18$team == team_H,"DVOA_rank"])
  DVOA_rank_A <- as.numeric(teams18[teams18$team == team_A,"DVOA_rank"])
  DVOA_H <- as.numeric(teams18[teams18$team == team_H,"total_DVOA"])
  DVOA_A <- as.numeric(teams18[teams18$team == team_A,"total_DVOA"])
  DVOA_rank_diff <- DVOA_rank_H - DVOA_rank_A
  DVOA_diff <- DVOA_H - DVOA_A
  
  #load dvoa into dataset
  data18$home_dvoa_rank[i] <- DVOA_rank_H
  data18$away_dvoa_rank[i] <- DVOA_rank_A
  data18$home_dvoa[i] <- DVOA_H
  data18$away_dvoa[i] <- DVOA_A
  
  data18$dvoa_rank_diff[i] <- DVOA_rank_diff
  data18$dvoa_diff[i] <- DVOA_diff
  
  #load epa
  epa_h <- as.numeric(teams18[teams18$team == team_H,"epa"])
  epa_a <- as.numeric(teams18[teams18$team == team_A,"epa"])
  epa_diff <-epa_h - epa_a
  
  data18$home_epa[i] <- epa_h
  data18$away_epa[i] <- epa_a
  data18$epa_diff[i] <- epa_diff
  
  #get current elo
  ELO_H <- as.numeric(teams18[teams18$team == team_H, "ELO"])
  ELO_A <- as.numeric(teams18[teams18$team == team_A, "ELO"])
  
  #load current elo into dataset
  data18$home_ELO[i] <- ELO_H + 10
  data18$away_ELO[i] <- ELO_A
  
  #UPDATE ELO 
  R_home <- 10^(data18$home_ELO/400)
  R_away <- 10^(data18$away_ELO/400)
  
  E_home <- R_home/(R_home + R_away)
  E_away <- R_away/(R_home + R_away)
  
  # Elo_Updated_home <- ELO_H + (20+(5*score_diff)) * (Result_H - E_home)
  # Elo_Updated_away <- ELO_A + (20+(5*score_diff)) * (Result_A - E_away)
  
  # Elo_Updated_home <- ELO_H + (20+(5*score_diff)+(-10*data20$division_game)) * (Result_H - E_home)
  # Elo_Updated_away <- ELO_A + (20+(5*score_diff)+(-10*data20$division_game)) * (Result_A - E_away)
  
  #margin of victory mulitplier
  # pregame_elo_diff <- abs(ELO_H - ELO_A)
  # positive_score_diff <- abs(score_diff)
  # MOV <- log(positive_score_diff + 1)
  # denom1 <- pregame_elo_diff * .001 + 2.2
  # MOV <- MOV * (2.2 / denom1)
  home_spread <- data18$home_spread
  pregame_elo_diff <- abs(ELO_H - ELO_A)
  spread_score_diff <- abs(score_diff - home_spread)
  data18$MOV[i] <- 1 + abs((score_diff/28))
  # MOV <- log(spread_score_diff + 1)
  # denom1 <- pregame_elo_diff * .001 + 2.2
  # MOV <- MOV * (2.2 / denom1)
  
  #current win percent
  elo_diff <- ELO_H - ELO_A
  data18$ELOhome_win_pct[i] <- 1/(1+10^(-1*(elo_diff/400)))
  
  #forecast delta
  # forecast_delta_H <- abs(data20$home_result - data20$ELOhome_win_pct)
  # forecast_delta_A <- abs(data20$away_result - (1 - data20$ELOhome_win_pct))
  
  Elo_Updated_home <- ELO_H + (20*MOV) * (Result_H - E_home)
  Elo_Updated_away <- ELO_A + (20*MOV) * (Result_A - E_away)
  
  #NCAAF_L1_Teams[NCAAF_L1_Teams$Team == Team_B, "ELO"] <- Elo_Updated_B
  teams18[teams18$team == team_H, "ELO"] <- Elo_Updated_home[i]
  teams18[teams18$team == team_A, "ELO"] <- Elo_Updated_away[i]
  
}


#check how well model predicts winners and spread
data18 <- data18 %>% mutate(
  home_ELO = as.numeric(home_ELO),
  away_ELO = as.numeric(away_ELO),
  Elo_Difference = home_ELO - away_ELO,
  Elo_Forecast_Pred = ifelse(home_ELO > away_ELO, 1, 0),
  Elo_Forecast_Result = ifelse(Elo_Forecast_Pred == home_result, 1, 0),
)

model_pct <- sum(data18$Elo_Forecast_Result)/nrow(data18) 
model_pct <- model_pct*100




#check how well model predicts spread
data18 <- data18 %>% mutate(
  predicted_spread = Elo_Difference/25,
  bet_favorite_spread = ifelse(home_spread>0, ifelse(predicted_spread>home_spread, 1,0), ifelse(predicted_spread<home_spread, 1,0)),
  favorite_covers_spread = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread = ifelse(bet_favorite_spread == favorite_covers_spread, 1, 0),
)

spread_win_pct <- sum(data18$win_spread)/nrow(data18)
spread_win_pct <- spread_win_pct * 100


#$$$$$$$$$$$$$$$$$$$%^&*&^%$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#NEW MODEL, NEED TO TEST
data18 <- data18 %>% mutate(
  #regression_spread = (0.17496 * home_spread) + (32.58169 * dvoa_diff) + (-0.03922 * elo_diff),
  regression_spread = 0.731491 + home_spread * 0.264119 + dvoa_diff * 12.041525 + epa_diff * 27.955343 + diff_score_diff_norm * -0.020714,
  bet_favorite_spread1 = ifelse(home_spread>0, ifelse(regression_spread>home_spread, 1,0), ifelse(regression_spread<home_spread, 1,0)),
  favorite_covers_spread1 = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread1 = ifelse(bet_favorite_spread1 == favorite_covers_spread1, 1, 0),
)
spread_win_pct1 <- sum(data18$win_spread1)/nrow(data18)
spread_win_pct1 <- spread_win_pct1 * 100
data18 <- data18 %>% mutate(
  regression_predict_win = ifelse(regression_spread>0, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0)),
)

regression_win_pct <- sum(data18$regression_predict_win)/nrow(data18)
regression_win_pct <- regression_win_pct * 100
#print
sprintf("elo model is correct %f percent of the time", model_pct)
sprintf("elo model predicts spread %f percent of the time", spread_win_pct)
sprintf("regression model predicts winner %f percent of the time", regression_win_pct)
sprintf("regression model wins spread bet %f percent of the time", spread_win_pct1)