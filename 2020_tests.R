
input20 <- data20 %>% 
  summarize(
    game_id = game_id,
    home_team = home_team,
    away_team = away_team,
    week = week,
    home_spread = home_spread,
    home_ELO = home_ELO,
    away_ELO = away_ELO,
    dvoa_diff = dvoa_diff,
    home_O_dvoa = home_O_dvoa,
    away_O_dvoa = away_O_dvoa,
    # total_O_dvoa = home_O_dvoa + away_O_dvoa,
    home_score_differential = home_score_differential,
    division_game = division_game,
    ELOhome_win_pct = ELOhome_win_pct,
    Elo_Difference = Elo_Difference,
    diff_score_diff = diff_score_diff,
    diff_score_diff_norm = diff_score_diff_norm,
    predicted_spread = predicted_spread,
  ) 
input20$Elo_Difference_adjusted <- ifelse(input20$Elo_Difference < 0, -1 * sqrt(abs(input20$Elo_Difference)), sqrt(abs(input20$Elo_Difference)))
input20$diff_score_diff_norm <- ifelse(input20$diff_score_diff_norm < 0, -1 * sqrt(abs(input20$diff_score_diff_norm)), sqrt(abs(input20$diff_score_diff_norm)))

input20$home_off_epa <- 0
input20$away_off_epa <- 0
input20$home_def_epa <- 0
input20$away_def_epa <- 0
input20$home_epa <- 0
input20$away_epa <- 0
input20$epa_diff <- 0
input20$home_code <- paste(input20$home_team,input20$week)
input20$away_code <- paste(input20$away_team,input20$week)

for(i in 17:nrow(input20)){
  print(i)
  
  team_H <- input20$home_code[i]
  team_A <- input20$away_code[i]
  
  home_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_H, "cumulative_epa_per_play"])
  input20$home_off_epa[i] <- home_off_epa
  
  away_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_A, "cumulative_epa_per_play"])
  input20$away_off_epa[i] <- away_off_epa
  
  #account for na values due to bye week
  ifelse(is.na(input20$home_off_epa[i])==TRUE, input20$home_code[i]<-paste(input20$home_team[i],input20$week[i]-1), 20)
  ifelse(is.na(input20$away_off_epa[i])==TRUE, input20$away_code[i]<-paste(input20$away_team[i],input20$week[i]-1), 20)
  
  team_H <- input20$home_code[i]
  team_A <- input20$away_code[i]
  
  ifelse(is.na(input20$home_off_epa[i])==TRUE,home_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_H, "cumulative_epa_per_play"]),20)
  input20$home_off_epa[i] <- home_off_epa
  
  ifelse(is.na(input20$away_off_epa[i])==TRUE,away_off_epa <- as.numeric(off_epa_data[off_epa_data$team_code == team_A, "cumulative_epa_per_play"]),20)
  input20$away_off_epa[i] <- away_off_epa 
}
for(i in 17:nrow(input20)){
  print(i)
  
  team_H <- input20$home_code[i]
  team_A <- input20$away_code[i]
  
  home_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_H, "cumulative_epa_per_play"])
  input20$home_def_epa[i] <- home_def_epa
  
  away_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_A, "cumulative_epa_per_play"])
  input20$away_def_epa[i] <- away_def_epa
  
  #account for na values due to bye week
  ifelse(is.na(input20$home_def_epa[i])==TRUE, input20$home_code[i]<-paste(input20$home_team[i],input20$week[i]-1), 20)
  ifelse(is.na(input20$away_def_epa[i])==TRUE, input20$away_code[i]<-paste(input20$away_team[i],input20$week[i]-1), 20)
  
  team_H <- input20$home_code[i]
  team_A <- input20$away_code[i]
  
  ifelse(is.na(input20$home_def_epa[i])==TRUE,home_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_H, "cumulative_epa_per_play"]),20)
  input20$home_def_epa[i] <- home_def_epa
  
  ifelse(is.na(input20$away_def_epa[i])==TRUE,away_def_epa <- as.numeric(def_epa_data[def_epa_data$team_code == team_A, "cumulative_epa_per_play"]),20)
  input20$away_def_epa[i] <- away_def_epa 
}
input20$home_epa <- input20$home_off_epa - input20$home_def_epa
input20$away_epa <- input20$away_off_epa - input20$away_def_epa
input20$epa_diff <- input20$home_epa - input20$away_epa
input20$epa_diff_adjusted <- ifelse(input20$epa_diff < 0, -1 * (input20$epa_diff^2), (input20$epa_diff^2))


# ggplot(data=input20, aes(x=home_score_differential, y=dvoa_diff)) + geom_point(aes(size=5, alpha=0.7))+
#   stat_summary(fun.data=mean_cl_normal) +
#   geom_smooth(method='lm', formula= y~x)
# corrgram(input20, lower.panel=panel.shade, upper.panel=panel.cor)

input20_reg <- filter(input20, home_score_differential >= -28, home_score_differential <= 28 , week >= 4, week <= 17)  

set.seed(77)
sampleSplit <- sample.split(Y=input20_reg$home_score_differential, SplitRatio=0.7)
trainSet <- subset(x=input20_reg, sampleSplit==TRUE)
testSet <- subset(x=input20_reg, sampleSplit==FALSE)
# model <- lm(home_score_differential ~  home_spread + epa_diff + dvoa_diff + home_O_dvoa + away_O_dvoa +
#               diff_score_diff + ELOhome_win_pct +  diff_score_diff_norm + 0, data=trainSet)
model <- lm(home_score_differential ~  home_spread + home_ELO + away_ELO + Elo_Difference_adjusted  
            + home_epa + away_epa + epa_diff_adjusted + diff_score_diff_norm + 0, data=trainSet)

#graph of residuals
# modelResiduals <- as.data.frame(residuals(model))
# ggplot(modelResiduals, aes(residuals(model))) +
#   geom_histogram(fill='deepskyblue', color='black')
# ads <- sum(input20$Elo_Difference)/nrow(input20)
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
input20 <- input20 %>% mutate(
  # regression_spread = home_spread * 0.64716 + dvoa_diff * 0.34976 + epa_diff * 2.37329 + home_O_dvoa * 3.50717 + away_O_dvoa * -2.47914 + diff_score_diff * 0.01891 + ELOhome_win_pct * -7.26737 + diff_score_diff_norm * -0.05070,
  regression_spread = home_spread * coefs[1] + home_ELO * coefs[2] + away_ELO * coefs[3] + Elo_Difference_adjusted * coefs[4]
  + home_epa * coefs[5] + away_epa * coefs[6] + epa_diff_adjusted * coefs[7] + diff_score_diff_norm * coefs[8],
  #regression_spread = home_ELO * 0.04410  + away_ELO * -0.04426 + Elo_Difference_adjusted  *  -0.36847 + dvoa_diff * 12.44874 + home_O_dvoa * 2.29484 + away_O_dvoa * -4.45970,
  #57.249071 percent
  bet_favorite_spread1 = ifelse(home_spread>0, ifelse(regression_spread>home_spread, 1,0), ifelse(regression_spread<home_spread, 1,0)),
  favorite_covers_spread1 = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread1 = ifelse(bet_favorite_spread1 == favorite_covers_spread1, 1, 0),
  regression_predict_win = ifelse(regression_spread>0, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0)),
  elo_predict_win = ifelse(ELOhome_win_pct>0.50, ifelse(home_score_differential>0, 1, 0), ifelse(home_score_differential<0, 1, 0)),
  )
#predict spread wins
spread_win_pct1 <- sum(input20$win_spread1)/nrow(input20)
spread_win_pct1 <- spread_win_pct1 * 100

#predict game wins
regression_win_pct <- sum(input20$regression_predict_win)/nrow(input20)
regression_win_pct <- regression_win_pct * 100

dec_regression_spread_pct <- spread_win_pct1 / 100
reg_units <- (256 * 1 * dec_regression_spread_pct) - (256 * 1.1 * (1 - dec_regression_spread_pct))
print(reg_units)


#printspread_win_pct)
input20 <- input20 %>% mutate(
  predicted_spread = Elo_Difference / 25,
  bet_favorite_spread = ifelse(home_spread>0, ifelse(predicted_spread>home_spread, 1,0), ifelse(predicted_spread<home_spread, 1,0)),
  favorite_covers_spread = ifelse(home_spread>0, ifelse(home_score_differential>home_spread, 1,0), ifelse(home_score_differential<home_spread, 1,0)),
  win_spread = ifelse(bet_favorite_spread == favorite_covers_spread, 1, 0),
  
)


model_pct <- sum(data20$Elo_Forecast_Result)/nrow(data20) 
model_pct <- model_pct*100
elospread_win_pct <- sum(input20$win_spread)/nrow(input20)
elospread_win_pct <- elospread_win_pct * 100

sprintf("regression model predicts winner %f percent of the time", regression_win_pct)
sprintf("regression model wins spread bet %f percent of the time", spread_win_pct1)
sprintf("elo model is correct %f percent of the time", model_pct)
sprintf("elo model predicts spread %f percent of the time", elospread_win_pct)

input20 <- input20 %>% 
  mutate(
    both_models_agree = ifelse(bet_favorite_spread==bet_favorite_spread1, 1, 0),
    both_models_win = ifelse(win_spread==win_spread1, ifelse(win_spread == 1, 1, 0), 0)
  )

input20_filtered <- input20 %>% 
  filter(week>=2, week <=17, abs(regression_spread)<7) %>% 
  mutate(score_spread = home_score_differential-home_spread,
         score_regress = home_score_differential-regression_spread,
         regress_spread = regression_spread-home_spread,
         ) 

# ggplot(data=input20, aes(x=regression_spread, y=home_score_differential)) + geom_point(aes(size=5, alpha=0.7)) +
#   stat_summary(fun.data=mean_cl_normal) +
#   geom_smooth(method='lm', formula= y~x)
# R2 <-cor(input20_filtered$home_score_differential, input20_filtered$regression_spread)^2
# print(R2)

regression_model_spread_pct <- sum(input20_filtered$win_spread1) / nrow(input20_filtered)
print(regression_model_spread_pct)
both_models_win <- sum(input20_filtered$both_models_win) / sum(input20_filtered$both_models_agree)
print(both_models_win)
games <- sum(input20_filtered$both_models_agree)
print(games)
total_units <- (games * 1 * both_models_win) - (games * 1.1 * (1 - both_models_win))
print(total_units)
summary(model)