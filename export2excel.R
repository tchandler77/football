install.packages("Rtools")

install.packages("writexl")
library(writexl)


testdataexcel <- write_xlsx(tempfile)

print(tempfile)
install.packages("Rtools")
install.packages("xlsx")

library(xlsx)
write.xlsx(data20, file="myworkbook.xlsx")

scoredifferential <- df1 %>%
  group_by(game_id) %>%
  summarize(
    game_id = game_id, home_spread = home_spread,
    home_score_differential = home_score_differential,
    diff_score_diff = diff_score_diff , week = week, predicted = predicted,
  )
weekly_wins <- df1 %>% 
  group_by(week) %>% 
  summarize(
    week, win_spread, regression_predict_win,
  )
#convert r data to an excel csv file
#csv has to already exist
tempfile<- write.csv(weekly_wins, "C:/Users/tommychandler9/Desktop/FOOTBALL/data/wins_vs_weeks.csv")
print(tempfile)

write.csv(weekly_wins, "C:/Users/tommychandler9/Desktop/FOOTBALL/data/wins_vs_weeks.csv")
"C:\Users\tommychandler9\Desktop\FOOTBALL\data\score_differential.csv"

"C:\Users\tommychandler9\Desktop\FOOTBALL\abcde.csv"


#THIS ONE WORKS
#copies data to clipboard, then I have to paste in excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(data20)