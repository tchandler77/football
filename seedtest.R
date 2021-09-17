seedtest <- setNames(data.frame(matrix(ncol = 6, nrow = 10000)), 
                     c("seed", "regression_spread_pct", "reg_units", "combo_spread_test", "total_combo_units", "games"))
input_reg <- filter(input, home_score_differential >= -28, home_score_differential <= 28 , week >= 2, week <= 16)  

seedtest$home_elo_coef <- 0
seedtest$away_elo_coef <- 0
seedtest$elo_diff_adj_coef <- 0
seedtest$epa_diff_adj_coef <- 0
seedtest$diff_score_coef <- 0
seedtest$home_off_epa_coef <- 0
seedtest$away_off_epa_coef <- 0
input_reg$diff_score_diff_norm <- ifelse(diff_score_diff_norm < 0, -1 * sqrt(abs(diff_score_diff_norm)), sqrt(abs(diff_score_diff_norm)))
for(i in 1:10000) {
  set.seed(i)
  print(i)
  sampleSplit <- sample.split(Y=input_reg$home_score_differential, SplitRatio=0.7)
  trainSet <- subset(x=input_reg, sampleSplit==TRUE)
  testSet <- subset(x=input_reg, sampleSplit==FALSE)
  # model <- lm(home_score_differential ~   home_ELO + away_ELO + elo_diff_adjusted + diff_score_diff_norm
  #             + epa_diff_adjusted + 0, data=trainSet)
  model <- lm(home_score_differential ~ home_ELO + away_ELO + elo_diff_adjusted + 
                epa_diff_adjusted + diff_score_diff_norm + home_off_epa + away_off_epa 
              + home_def_epa + away_def_epa + 0, data=trainSet)
  
  summary(model)
  
  #graph of residuals
  # modelResiduals <- as.data.frame(residuals(model))
  # ggplot(modelResiduals, aes(residuals(model))) +
  #   geom_histogram(fill='deepskyblue', color='black')
  
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
  
  #sprintf("model is wrong by %f points on average", rmse)
  
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
  
  # sprintf("regression model predicts winner %f percent of the time", regression_win_pct)
  # sprintf("regression model wins spread bet %f percent of the time", spread_win_pct1)
  # sprintf("elo model is correct %f percent of the time", model_pct)
  # sprintf("elo model predicts spread %f percent of the time", elospread_win_pct)
  
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
    filter(season >= 2016, week>=2, week <=17, abs(regress_spread)<10) 
  
  # ggplot(data=input, aes(x=regression_spread, y=home_score_differential)) + geom_point(aes(size=5, alpha=0.7)) +
  #   stat_summary(fun.data=mean_cl_normal) +
  #   geom_smooth(method='lm', formula= y~x)
  # R2 <-cor(input_filtered$home_score_differential, input_filtered$regression_spread)^2
  # print(R2)
  summary(model)
  games1 <- nrow(input_filtered)
  games <- sum(input_filtered$both_models_agree)
  regression_model_spread_pct <- sum(input_filtered$win_spread1) / nrow(input_filtered)
  # print(regression_model_spread_pct)
  both_models_win <- sum(input_filtered$both_models_win) / sum(input_filtered$both_models_agree)
  # print(both_models_win)
  # print(games)
  # print(games1)
  total_units <- (games * 1 * both_models_win) - (games * 1.1 * (1 - both_models_win))
  # print(total_units)
  reg_units <- (games1 * 1 * regression_model_spread_pct) - (games1 * 1.1 * (1 - regression_model_spread_pct))
  # print(reg_units)
  
  seedtest$seed[i] <- i
  seedtest$regression_spread_pct[i] <- regression_model_spread_pct
  seedtest$combo_spread_test[i] <- both_models_win
  seedtest$total_combo_units[i] <- total_units
  seedtest$reg_units[i] <- reg_units
  seedtest$games[i] <- games1
  
  seedtest$home_elo_coef[i] <- coefs[1]
  seedtest$away_elo_coef[i] <- coefs[2]
  seedtest$elo_diff_adj_coef[i] <- coefs[3]
  seedtest$epa_diff_adj_coef[i] <- coefs[4]
  seedtest$diff_score_coef[i] <- coefs[5]
  seedtest$home_off_epa_coef[i] <- coefs[6]
  seedtest$away_off_epa_coef[i] <- coefs[7]
  seedtest$home_def_epa_coef[i] <- coefs[8]
  seedtest$away_def_epa_coef[i] <- coefs[9]
}


ggplot(seedtest, aes(total_combo_units)) +
  geom_histogram(fill='deepskyblue', color='black')
view(seedtest)
