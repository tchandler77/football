library(rvest)
library(stringr)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

schedule_url <- read_html("https://www.pro-football-reference.com/years/2020/games.htm")
nfl_2020_results <- schedule_url %>% html_node("table") %>% html_table(fill=TRUE)
duplicated(nfl_2020_results)

nfl_2020_results <-  nfl_2020_results[ -c(6,8) ] 
nfl_2020_results <- nfl_2020_results %>% distinct()
nfl_2020_results = nfl_2020_results[-17,]
nfl_2020_results = nfl_2020_results[-257,]

id <- nfl_2020_results[ c("Week","Winner/tie")]
id <- unite(id, "game_id", c("Week","Winner/tie"),sep = "_", remove = TRUE) 
nfl_2020_results$game_id <- id$game_id

test_url <- read_html("https://www.espn.com/nfl/schedule/_/year/2020")
test2020 <- test_url %>%  html_node("table") %>% html_table(fill=TRUE)

nfl20 <- nfl_2020_results

teams <- nfl20[c("Winner/tie","Loser/tie")]


  
