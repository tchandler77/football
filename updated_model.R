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
#library(rstan)
library(lme4)
library(tidyverse)
library(DT)
library(tidybayes)
library(ggrepel)
library(magick)
library(resample)


set.seed(1995)

epa_data <- rbind(raw_data20, raw_data19, raw_data18, raw_data17, raw_data16, raw_data15) %>%
  dplyr::filter(
    !is.na(epa), !is.na(ep), !is.na(posteam),
    play_type == "pass" | play_type == "run" | penalty == 1, qb_kneel != 1
  )

off_epa_data <- epa_data %>% 
  dplyr::group_by(posteam, season, week) %>% 
  dplyr::summarize(off_epa = sum(epa), n_offense = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(posteam, season) %>% 
  dplyr::mutate(cumulative_off_epa = cumsum(off_epa)) %>% 
  dplyr::mutate(cumulative_n_offense = cumsum(n_offense)) %>% 
  dplyr::mutate(cumulative_epa_per_play = cumulative_off_epa / cumulative_n_offense)

off_epa_data$team_code <- paste(off_epa_data$season,off_epa_data$posteam,off_epa_data$week + 1)
view(off_epa_data)

def_epa_data <- epa_data %>% 
  dplyr::group_by(defteam, season, week) %>% 
  dplyr::summarize(def_epa = sum(epa), n_defense = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(defteam, season) %>% 
  dplyr::mutate(cumulative_def_epa = cumsum(def_epa)) %>% 
  dplyr::mutate(cumulative_n_defense = cumsum(n_defense)) %>% 
  dplyr::mutate(cumulative_epa_per_play = cumulative_def_epa / cumulative_n_defense)

def_epa_data$team_code <- paste(def_epa_data$season,def_epa_data$defteam,def_epa_data$week + 1)
view(def_epa_data)

data15$season <- 2015
data16$season <- 2016
data17$season <- 2017
data18$season <- 2018
data19$season <- 2019
data20$season <- 2020
input <- rbind(data16, data17, data18, data19, data20)
input <- input %>%
  summarize(
    season = season,
    week = week,
    game_id = game_id,
    home_team = home_team,
    home_score = home_score,
    away_team = away_team,
    away_score = away_score,
    home_score_differential = home_score_differential,
    home_spread = home_spread,
    home_ELO = home_ELO,
    away_ELO = away_ELO,
    elo_diff = home_ELO - away_ELO,
    division_game = division_game,
    ELOhome_win_pct = ELOhome_win_pct,
    diff_score_diff = diff_score_diff,
    diff_score_diff_norm = diff_score_diff/week * 17,
    diff_score_diff_norm = ifelse(diff_score_diff_norm < 0, -1 * sqrt(abs(diff_score_diff_norm)), sqrt(abs(diff_score_diff_norm)))
  )
input$elo_diff_adjusted <- ifelse(input$elo_diff < 0, -1 * sqrt(abs(input$elo_diff)), sqrt(abs(input$elo_diff)))
input$diff_score_diff_norm <- ifelse(input$diff_score_diff_norm < 0, -1 * sqrt(abs(input$diff_score_diff_norm)), sqrt(abs(input$diff_score_diff_norm)))

any(is.na(input))

input$home_off_epa <- 0
input$away_off_epa <- 0
input$home_def_epa <- 0
input$away_def_epa <- 0
input$home_epa <- 0
input$away_epa <- 0
input$epa_diff <- 0
input$home_code <- paste(input$season,input$home_team,input$week)
input$away_code <- paste(input$season,input$away_team,input$week)

for(i in 1:nrow(input)){
  print(i)

  team_H <- input$home_code[i]
  team_A <- input$away_code[i]

  home_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_H, "cumulative_epa_per_play"])
  input$home_off_epa[i] <- home_off_epa

  away_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_A, "cumulative_epa_per_play"])
  input$away_off_epa[i] <- away_off_epa

  #account for na values due to bye week
  ifelse(is.na(input$home_off_epa[i])==TRUE, input$home_code[i]<-paste(input$season[i],input$home_team[i],input$week[i]-1), 20)
  ifelse(is.na(input$away_off_epa[i])==TRUE, input$away_code[i]<-paste(input$season[i],input$away_team[i],input$week[i]-1), 20)

  team_H <- input$home_code[i]
  team_A <- input$away_code[i]

  ifelse(is.na(input$home_off_epa[i])==TRUE,home_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_H, "cumulative_epa_per_play"]),20)
  input$home_off_epa[i] <- home_off_epa

  ifelse(is.na(input$home_off_epa[i])==TRUE,home_off_epa <- 0, 20)
  input$home_off_epa[i] <- home_off_epa

  ifelse(is.na(input$away_off_epa[i])==TRUE,away_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_A, "cumulative_epa_per_play"]),20)
  input$away_off_epa[i] <- away_off_epa

  ifelse(is.na(input$away_off_epa[i])==TRUE,away_off_epa <- 0, 20)
  input$away_off_epa[i] <- away_off_epa
}
for(i in 1:nrow(input)){
  print(i)

  team_H <- input$home_code[i]
  team_A <- input$away_code[i]

  home_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_H, "cumulative_epa_per_play"])
  input$home_def_epa[i] <- home_def_epa

  away_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_A, "cumulative_epa_per_play"])
  input$away_def_epa[i] <- away_def_epa

  #account for na values due to bye week
  ifelse(is.na(input$home_def_epa[i])==TRUE, input$home_code[i]<-paste(input$season[i],input$home_team[i],input$week[i]-1), 20)
  ifelse(is.na(input$away_def_epa[i])==TRUE, input$away_code[i]<-paste(input$season[i],input$away_team[i],input$week[i]-1), 20)

  team_H <- input$home_code[i]
  team_A <- input$away_code[i]

  ifelse(is.na(input$home_def_epa[i])==TRUE,home_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_H, "cumulative_epa_per_play"]),20)
  input$home_def_epa[i] <- home_def_epa

  ifelse(is.na(input$home_def_epa[i])==TRUE,home_def_epa <- 0, 20)
  input$home_def_epa[i] <- home_def_epa

  ifelse(is.na(input$away_def_epa[i])==TRUE,away_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_A, "cumulative_epa_per_play"]),20)
  input$away_def_epa[i] <- away_def_epa

  ifelse(is.na(input$away_def_epa[i])==TRUE,away_def_epa <- 0, 20)
  input$away_def_epa[i] <- away_def_epa
}

input$home_epa <- input$home_off_epa - input$home_def_epa
input$away_epa <- input$away_off_epa - input$away_def_epa
input$epa_diff <- input$home_epa - input$away_epa
input$epa_diff_adjusted <- ifelse(input$epa_diff < 0, -1 * abs(input$epa_diff^2), abs(input$epa_diff^2))


# ggplot(data=input, aes(x=home_score_differential, y=dvoa_diff)) + geom_point(aes(size=5, alpha=0.7))+
#   stat_summary(fun.data=mean_cl_normal) +
#   geom_smooth(method='lm', formula= y~x)
# corrgram(input, lower.panel=panel.shade, upper.panel=panel.cor)


input_reg <- filter(input, home_score_differential >= -28, home_score_differential <= 28 , week >= 2, week <= 16)  


set.seed(1995)
  
sampleSplit <- sample.split(Y=input_reg$home_score_differential, SplitRatio=0.7)
trainSet <- subset(x=input_reg, sampleSplit==TRUE)
testSet <- subset(x=input_reg, sampleSplit==FALSE)
 # model <- lm(home_score_differential ~   home_ELO + away_ELO + elo_diff_adjusted + diff_score_diff_norm
 #             + epa_diff_adjusted + 0, data=trainSet)
 model <- lm(home_score_differential ~ home_ELO + away_ELO + elo_diff_adjusted + 
             epa_diff_adjusted + diff_score_diff_norm + home_off_epa + away_off_epa 
             + home_def_epa + away_def_epa + 0, data=trainSet)

summary(model)

inputwk2 <- input %>% 
  filter(week==1)

view(inputwk2)

wk2fav <- sum(inputwk2$favorite_covers_spread)
print(wk2fav)
print(nrow(inputwk2))
wk2fav_pct <- wk2fav/nrow(inputwk2)
print(wk2fav_pct)

#graph of residuals
modelResiduals <- as.data.frame(residuals(model))
ggplot(modelResiduals, aes(residuals(model))) +
  geom_histogram(fill='deepskyblue', color='black')

#ads can find average of different columns
# ads <- sum(input$elo_diff)/nrow(input)
# print(ads)

#make predictions
preds <- predict(model, testSet)

#evaluate using test set
modelEval <- cbind(testSet$home_score_differential, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
preds <- predict(model, testSet)

mse <- mean((modelEval$Actual - modelEval$Predicted)^2)
rmse <- sqrt(mse)

sprintf("model is wrong by %f points on average", rmse)

coefs <- (coefficients(model))
input <- input %>% mutate(
  regression_spread =  home_ELO * coefs[1] + away_ELO * coefs[2] + elo_diff_adjusted * coefs[3] 
  + epa_diff_adjusted * coefs[4] + diff_score_diff_norm * coefs[5] + home_off_epa * coefs[6] 
  + away_off_epa * coefs[7], 
  # regression_spread =  home_ELO * coefs[1] + away_ELO * coefs[2] + elo_diff_adjusted * coefs[3]
  # + home_epa * coefs[4] + away_epa * coefs[5] + epa_diff_adjusted * coefs[6] + diff_score_diff_norm * coefs[7],
  # + home_off_epa * coefs[8] + away_off_epa * coefs[9],# + home_spread * coefs[8],
  #regression_spread = home_ELO * 0.04410  + away_ELO * -0.04426 + elo_diff_adjusted  *  -0.36847 + dvoa_diff * 12.44874 + home_O_dvoa * 2.29484 + away_O_dvoa * -4.45970,
  #57.249071 percent
  bet_favorite_spread1 = ifelse(home_spread>0, ifelse(regression_spread>home_spread, 1,0), ifelse(regression_spread<home_spread, 1,0)),
  favorite_covers_spread1 = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread1 = ifelse(bet_favorite_spread1 == favorite_covers_spread1, 1, 0),
  regression_predict_win = ifelse(regression_spread>0, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0)),
  elo_predict_win = ifelse(ELOhome_win_pct>0.50, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0)),
)
#predict spread wins
spread_win_pct1 <- sum(input$win_spread1)/nrow(input)
spread_win_pct1 <- spread_win_pct1 * 100

#predict game wins
regression_win_pct <- sum(input$regression_predict_win)/nrow(input)
regression_win_pct <- regression_win_pct * 100

dec_regression_spread_pct <- spread_win_pct1 / 100



#printspread_win_pct)
input <- input %>% mutate(
  predicted_spread = elo_diff / 25,
  bet_favorite_spread = ifelse(home_spread>0, ifelse(predicted_spread>home_spread, 1,0), ifelse(predicted_spread<home_spread, 1,0)),
  favorite_covers_spread = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread = ifelse(bet_favorite_spread == favorite_covers_spread, 1, 0),
  
)


model_pct <- sum(data20$Elo_Forecast_Result)/nrow(data20) 
model_pct <- model_pct*100
elospread_win_pct <- sum(input$win_spread)/nrow(input)
elospread_win_pct <- elospread_win_pct * 100

sprintf("regression model predicts winner %f percent of the time", regression_win_pct)
sprintf("regression model wins spread bet %f percent of the time", spread_win_pct1)
sprintf("elo model is correct %f percent of the time", model_pct)
sprintf("elo model predicts spread %f percent of the time", elospread_win_pct)

input <- input %>% 
  mutate(
    both_models_agree = ifelse(bet_favorite_spread==bet_favorite_spread1, 1, 0),
    both_models_win = ifelse(win_spread==win_spread1, ifelse(win_spread == 1, 1, 0), 0)
  )

input_filtered <- input %>% 
  mutate(score_spread = home_score_differential-home_spread,
         score_regress = home_score_differential-regression_spread,
         regress_spread = regression_spread-home_spread,
  ) %>% 
  filter(season >= 2020, week>=1, week <= 2, bet_favorite_spread1==0) 

games1 <- nrow(input_filtered)
games <- sum(input_filtered$both_models_agree)
regression_model_spread_pct <- sum(input_filtered$win_spread1) / nrow(input_filtered)
print(regression_model_spread_pct)
both_models_win <- sum(input_filtered$both_models_win) / sum(input_filtered$both_models_agree)
print(both_models_win)
print(games)
print(games1)
total_units <- (games * 1 * both_models_win) - (games * 1.1 * (1 - both_models_win))
print(total_units)
reg_units <- (games1 * 1 * regression_model_spread_pct) - (games1 * 1.1 * (1 - regression_model_spread_pct))
print(reg_units)

