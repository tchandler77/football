library(dplyr)
library(nflreadr)
#games <- load_schedules()
games21 <- load_schedules(2021)
data21 <- games21 %>% 
  filter(week <= 2) %>% 
  summarize(
    week, away_team, away_score, home_team, home_score, home_score_differential = result, away_moneyline, home_moneyline, spread_line,
  )

data21$home_ELO <- 0
data21$away_ELO <- 0
data21$ELOhome_win_pct <- 0

#replace all NA with 0
data21[is.na(data21)] <- 0

data21$home_off_epa <- 0
data21$away_off_epa <- 0
data21$home_def_epa <- 0
data21$away_def_epa <- 0
data21$epa_diff<- 0
data21$epa_diff_adj <- 0

data21$MOV <- 0

data21$total_score_diff_home <- 0
data21$total_score_diff_away <- 0

data21$diff_score_diff <- 0
data21$diff_score_diff_norm <- 0

data21 <- data21 %>% mutate(
    ml_win_pct = ifelse(home_moneyline < 0, (-1 * home_moneyline) / (-1 * home_moneyline + 100), (100 / (home_moneyline + 100)))
)

data21$home_result <- 0
data21$away_result <- 0

data21 <- data21 %>% mutate(home_result = ifelse(home_score_differential > 0, 1, home_result))
data21 <- data21 %>% mutate(home_result = ifelse(home_score_differential == 0, 0.5, home_result))
data21 <- data21 %>% mutate(away_result = abs(1-home_result))
# ggplot(data=games, aes(x=season, y=total)) + geom_point(aes(size=5, alpha=0.7))+
#   stat_summary(fun.data=mean_cl_normal) +
#   geom_smooth(method='lm', formula= y~x)

teams21 <- teams20 
teams21 <- unique(teams21)
teams21$score_diff <- 0

#s is the percent weight given to every team's elo from the previous season
s=20
teams21$ELO <- ((s/100)*teams20$ELO)+(1500*(1-(s/100)))

#epa
# off_epa_data20 <- off_epa_data %>% 
#   filter(week == 17) %>%
#   filter(season == 2020) 
# def_epa_data20 <- def_epa_data %>% 
#   filter(week == 17) %>%
#   filter(season == 2020) 

teams21$off_epa <- 0 
teams21$def_epa <- 0 

epa_data21 <- raw_data21 %>%
  dplyr::filter(
    !is.na(epa), !is.na(ep), !is.na(posteam),
    play_type == "pass" | play_type == "run" | penalty == 1, qb_kneel != 1
  )

off_epa_data21 <- epa_data21 %>% 
  dplyr::group_by(posteam, season, week) %>% 
  dplyr::summarize(off_epa = sum(epa), n_offense = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(posteam, season) %>% 
  dplyr::mutate(cumulative_off_epa = cumsum(off_epa)) %>% 
  dplyr::mutate(cumulative_n_offense = cumsum(n_offense)) %>% 
  dplyr::mutate(cumulative_epa_per_play = cumulative_off_epa / cumulative_n_offense)
off_epa_data21$season <- 2021
off_epa_data21$team_code <- paste(off_epa_data21$season,off_epa_data21$posteam,off_epa_data21$week + 1)
#view(off_epa_data21)

def_epa_data21 <- epa_data21 %>% 
  dplyr::group_by(defteam, season, week) %>% 
  dplyr::summarize(def_epa = sum(epa), n_defense = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(defteam, season) %>% 
  dplyr::mutate(cumulative_def_epa = cumsum(def_epa)) %>% 
  dplyr::mutate(cumulative_n_defense = cumsum(n_defense)) %>% 
  dplyr::mutate(cumulative_epa_per_play = cumulative_def_epa / cumulative_n_defense)

def_epa_data21$team_code <- paste(def_epa_data21$season,def_epa_data21$defteam,def_epa_data21$week + 1)
#view(def_epa_data21)

data21$home_epa <- data21$home_off_epa - data21$home_def_epa
data21$away_epa <- data21$away_off_epa - data21$away_def_epa
data21$epa_diff <- data21$home_epa - data21$away_epa
data21$epa_diff_adjusted <- ifelse(data21$epa_diff < 0, -1 * abs(data21$epa_diff^2), abs(data21$epa_diff^2))
#this is using week 17 2020 data for epa
for(i in 1:nrow(teams21)){
  print(i)
  
  team <- teams21$team[i]
  
  off_epa <- as.numeric(off_epa_data21[off_epa_data21$posteam == team, "cumulative_epa_per_play"])
  teams21$off_epa[i] <- off_epa
  
  def_epa <- as.numeric(def_epa_data21[def_epa_data21$defteam == team, "cumulative_epa_per_play"])
  teams21$def_epa[i] <- def_epa

}

teams21 <- teams21 %>% 
  mutate(
  total_epa = off_epa - def_epa,
  )



for(i in 1:nrow(data21)){
  print(i)
  
  team_H <- data21$home_team[i]
  team_A <- data21$away_team[i]
  
  data21$total_score_diff_home[i] <- as.numeric(teams21[teams21$team == team_H, "score_diff"])
  data21$total_score_diff_away[i] <- as.numeric(teams21[teams21$team == team_A, "score_diff"])

  diff_score_diff <- as.numeric(data21$total_score_diff_home[i] - data21$total_score_diff_away[i])
  diff_score_diff_norm <- diff_score_diff/data21$week[i] * 17
  diff_score_diff_norm <- ifelse(diff_score_diff_norm <= 0, -1 * sqrt(abs(diff_score_diff_norm)), sqrt(abs(diff_score_diff_norm)))
  
  data21$diff_score_diff[i] <- diff_score_diff
  data21$diff_score_diff_norm[i] <- diff_score_diff_norm

  score_diff <- data21$home_score_differential[i]
  score_diff_home <- score_diff 
  score_diff_away <- -1 * score_diff 
  
  total_score_diff_home <- teams21[teams21$team == team_H, "score_diff"] + score_diff_home
  total_score_diff_away <- teams21[teams21$team == team_A, "score_diff"] + score_diff_away

  teams21[teams21$team == team_H, "score_diff"] <- total_score_diff_home
  teams21[teams21$team == team_A, "score_diff"] <- total_score_diff_away


  data21$epa_diff[i] <- data21$home_epa[i] - data21$away_epa[i]
  
  Result_H <- data21$home_result[i]
  Result_A <- data21$away_result[i]
  
  #get current elo
  ELO_H <- as.numeric(teams21[teams21$team == team_H, "ELO"])
  ELO_A <- as.numeric(teams21[teams21$team == team_A, "ELO"])
  
  #load current elo into dataset
  data21$home_ELO[i] <- ELO_H + 10
  data21$away_ELO[i] <- ELO_A
  
  #UPDATE ELO 
  R_home <- 10^(data21$home_ELO/400)
  R_away <- 10^(data21$away_ELO/400)
  
  E_home <- R_home/(R_home + R_away)
  E_away <- R_away/(R_home + R_away)
  
  data21$MOV[i] <- 1 + abs(score_diff)/28
  
  #current win percent
  elo_diff <- ELO_H - ELO_A
  data21$ELOhome_win_pct[i] <- 1/(1+10^(-1*(elo_diff/400)))
  
  #Update ELO
  Elo_Updated_home <- ELO_H + (20*MOV) * (Result_H - E_home)
  Elo_Updated_away <- ELO_A + (20*MOV) * (Result_A - E_away)
  
  teams21[teams21$team == team_H, "ELO"] <- Elo_Updated_home[i]
  teams21[teams21$team == team_A, "ELO"] <- Elo_Updated_away[i]
  
}

for (i in 1:nrow(data21)){
  print(i)
  
  team_H <- data21$home_team[i]
  team_A <- data21$away_team[i]
  
  data21$home_off_epa[i] <- as.numeric(teams21[teams21$team == team_H, "off_epa"])
  data21$away_off_epa[i] <- as.numeric(teams21[teams21$team == team_A, "off_epa"])
  
  data21$home_def_epa[i] <- as.numeric(teams21[teams21$team == team_H, "def_epa"])
  data21$away_def_epa[i] <- as.numeric(teams21[teams21$team == team_A, "def_epa"])
  
  data21$home_epa[i] <- as.numeric(teams21[teams21$team == team_H, "total_epa"])
  data21$away_epa[i] <- as.numeric(teams21[teams21$team == team_A, "total_epa"])
  
  # data21$home_ELO[i] <- as.numeric(teams21[teams21$team == team_H, "ELO"])
  # data21$away_ELO[i] <- as.numeric(teams21[teams21$team == team_A, "ELO"])
  
  data21$total_score_diff_home[i] <- as.numeric(teams21[teams21$team == team_H, "score_diff"])
  data21$total_score_diff_home[i] <- as.numeric(teams21[teams21$team == team_A, "score_diff"])
}

coefs <- (coefficients(model))
data21$regression_spread <- 0

data21 <- data21 %>% 
  mutate(
    epa_diff = home_off_epa - away_off_epa,
    epa_diff_adj = ifelse(epa_diff < 0, -1 * abs(epa_diff^2), abs(epa_diff^2)),
    
    elo_diff = home_ELO - away_ELO,
    elo_diff_adj = ifelse(epa_diff < 0, -1 * sqrt(abs(elo_diff)), sqrt(abs(elo_diff))),
    ELOhome_win_pct = 1/(1+10^(-1*(elo_diff/400))),
    
    diff_score_diff = total_score_diff_home - total_score_diff_home,
    #NEED TO ADJUST
    # diff_score_diff_norm = ifelse(diff_score_diff < 0, -1 * sqrt(abs(diff_score_diff)), sqrt(abs(diff_score_diff))),
    elo_spread = elo_diff / 25,
    
    regression_spread =  home_ELO * coefs[1] + away_ELO * coefs[2] + elo_diff_adj * coefs[3] 
    + epa_diff_adj * coefs[4] + diff_score_diff_norm * coefs[5] + home_off_epa * coefs[6] 
    + away_off_epa * coefs[7] + home_def_epa * coefs[8] + away_def_epa * coefs[9],
  )

spreads21 <- data21 %>% 
  filter(week == 2) %>% 
  summarize(
      week, away_team, home_team, home_score_differential, spread_line, regression_spread, ml_win_pct, ELOhome_win_pct, ELO_predicted_spread = elo_diff/25, 
    )
spreads21 <- spreads21 %>%  mutate(
    bet_favorite_regression = ifelse(spread_line>0, ifelse(regression_spread>spread_line, 1,0), ifelse(regression_spread<spread_line, 1,0)),
    bet_favorite_ELO = ifelse(spread_line>0, ifelse(ELO_predicted_spread>spread_line, 1,0), ifelse(ELO_predicted_spread<spread_line, 1,0)),
    bet_favorite_ELO_ml = ifelse(spread_line>0, ifelse(ELOhome_win_pct>ml_win_pct, 1,0), ifelse(ELOhome_win_pct<ml_win_pct, 1,0)),
  )
view(spreads21)
  
ggplot(data=spreads21, aes(x=regression_spread, y=spread_line)) + geom_point(aes(size=5, alpha=0.7))+
    stat_summary(fun.data=mean_cl_normal) +
    geom_smooth(method='lm', formula= y~x)

#                      Estimate Std. Error t value Pr(>|t|)   
# home_ELO               0.06328    0.02111   2.998  0.00281 **
#   away_ELO              -0.06250    0.02109  -2.964  0.00314 **
#   elo_diff_adjusted     -0.37950    0.21318  -1.780  0.07546 . 
# epa_diff_adjusted    -13.10639   13.82874  -0.948  0.34356   
# diff_score_diff_norm   0.47731    0.31087   1.535  0.12512   
# home_off_epa          13.36174    9.06272   1.474  0.14081   
# away_off_epa         -19.48074    8.76359  -2.223  0.02653 * 
#   home_def_epa          -9.08572    9.02678  -1.007  0.31449   
# away_def_epa          -0.33573    8.30434  -0.040  0.96776 