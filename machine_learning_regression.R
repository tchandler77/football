library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)

#INITIALIZE DATA
#2020
regdata20 <- data20 %>%
  group_by(game_id) %>%
  summarize(
  game_id = game_id, home_spread = home_spread,
  home_score_differential = home_score_differential,
  dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
  epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
  home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
  diff_score_diff = diff_score_diff , week = week, division_game = division_game,
  win_elospread = win_spread,
)

#2019
regdata19 <- data19 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
    epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
    home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
    diff_score_diff = diff_score_diff , week = week, division_game = division_game,
    win_elospread = win_spread,
  )
#2018
regdata18 <- data18 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
    epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
    home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
    diff_score_diff = diff_score_diff , week = week, division_game = division_game,
    win_elospread = win_spread,
  )
#2017
regdata17 <- data17 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
    epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
    home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
    diff_score_diff = diff_score_diff , week = week, division_game = division_game,
    win_elospread = win_spread,
  )
#2016
regdata16 <- data16 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
    epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
    home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
    diff_score_diff = diff_score_diff , week = week, division_game = division_game,
    win_elospread = win_spread,
  )
#2015
regdata15 <- data15 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    dvoa_diff = dvoa_diff, elo_diff = Elo_Difference,
    epa_diff = epa_diff, bet_fav_elo = bet_favorite_spread, bet_fav_reg = bet_favorite_spread1,
    home_dvoa = home_dvoa, away_dvoa = away_dvoa, home_epa = home_epa, away_epa = away_epa,
    diff_score_diff = diff_score_diff , week = week, division_game = division_game,
    win_elospread = win_spread,
  )  

#MERGE

# df1 <- rbind(regdata20, regdata19, regdata18, regdata17, regdata16, regdata15)
df1 <- regdata20
# df1 <- filter(df1, home_score_differential >= -28, home_score_differential <= 28 )
# df1 <- filter(df1, week >= 6, week <= 17)
any(is.na(df1))

elospread_win_pct <- sum(df1$win_elospread)/nrow(df1)
print(elospread_win_pct)
 #initialize columns

df1$predicted <- 0
df1$diff_score_diff_norm <- df1$diff_score_diff/df1$week * 17
bet_favorite_spread <- 0
favorite_covers_spread <- 0
win_spread <- 0


#graph and correlation

  #ggplot(data=df1, aes(x=diff_score_diff, y=home_score_differential)) + geom_point(aes(size=5, alpha=0.7))
  ggplot(data=df1, aes(x=week, y=diff_score_diff)) + geom_point(aes(size=5, alpha=0.7))+
    stat_summary(fun.data=mean_cl_normal) + 
    geom_smooth(method='lm', formula= y~x)
 # corrgram(df1, lower.panel=panel.shade, upper.panel=panel.cor)

#REGRESSION
set.seed(77)
sampleSplit <- sample.split(Y=df1$home_score_differential, SplitRatio=0.7)
trainSet <- subset(x=df1, sampleSplit==TRUE)
testSet <- subset(x=df1, sampleSplit==FALSE)

model <- lm(home_score_differential ~ home_spread + dvoa_diff + epa_diff + diff_score_diff_norm, data=trainSet)
summary(model)

#graph of residuals
 modelResiduals <- as.data.frame(residuals(model))
 ggplot(modelResiduals, aes(residuals(model))) +
geom_histogram(fill='deepskyblue', color='black')


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

# ggplot(data=modelEval, aes(x=Actual, y=Predicted)) + geom_point(aes(size=Actual/Predicted, alpha=0.7))
# df1$predicted = -0.190818 + df1$home_spread * 0.653488 + df1$dvoa_diff * 6.258039 + df1$elo_diff * 0.004624
#-0.190818
df1 <- df1 %>% mutate(
    #Pred. Just using elo
    #predicted = elo_diff /25,
  
    #prediction for all score results
    #predicted = 1.076800 + home_spread * 0.221526 + dvoa_diff * 23.065532 + epa_diff * 26.836209 + diff_score_diff_norm * -0.027124, 
    
    #prediction for all results 28 and below
    predicted = 0.731491 + home_spread * 0.264119 + dvoa_diff * 12.041525 + epa_diff * 27.955343 + diff_score_diff_norm * -0.020714,
    #predicted = 0.356051  + home_spread * 0.411509 + dvoa_diff * 9.536038 + epa_diff * 23.917253   + diff_score_diff_norm * -0.020750,
    bet_favorite_spread = ifelse(home_spread>0, ifelse(predicted>home_spread, 1,0), ifelse(predicted<home_spread, 1,0)),
    favorite_covers_spread = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
    win_spread = ifelse(bet_favorite_spread == favorite_covers_spread, 1, 0),
    
    regression_predict_win = ifelse(predicted>0, ifelse(home_score_differential>0, 1, 0), 
                                    ifelse(home_score_differential<0, 1, 0)),
  )
spread_win_pct <- sum(df1$win_spread)/nrow(df1)
spread_win_pct <- spread_win_pct * 100
regression_win_pct <- sum(df1$regression_predict_win)/nrow(df1)
regression_win_pct <- regression_win_pct * 100
sprintf("regression model leads to winning spread bet %f percent of the time", spread_win_pct)
sprintf("regression model predicts winner of game %f percent of the time", regression_win_pct)
# "regression model leads to winning spread bet 64.623584 percent of the time, Multiple R-squared: 0.2838, 9.900306 points"
# predicted = 0.731491 + home_spread * 0.264119 + dvoa_diff * 12.041525 + epa_diff * 27.955343 + diff_score_diff_norm * -0.020714,
ggplot(data=df1, aes(x=predicted, y=home_score_differential)) + geom_point(aes(size=5, alpha=0.7))+
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm', formula= y~x)