#make sure to library the following:
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
#recieve data with the following for 2019/2020 szn
raw_data20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
dim(data20)
raw_data19 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
dim(raw_data19)
#> [1] 48034   340 #this means there are 48034 plays in this data, with 340 columns of data for each play

# Dyplr
data20_1 <- data20 %>% filter(game_seconds_remaining == "0")
data20_1_spread <- data20_1 %>% filter(spread_line <= "3" & spread_line >= "-3")
#viewing data
names(data) #shows all column names
view(data20) #display data table
#show data columns in one table
data20 %>% 
  select(home_team, away_team, posteam, desc) %>%
  View()
#head() function prints data in console
data20 %>% 
  select(posteam, defteam, desc, rush, pass) %>% 
  head()
#example of a filter, this one only shows special teams plays
data20 %>% 
  filter(special == 1) %>%
  select(down, ydstogo, desc) %>% 
  head()
#filter so just run and pass plays, with epa not missing
pbp_rp <- data20 %>%
  filter(rush == 1 | pass == 1, !is.na(epa))
#example from tutorial, filtering out patriot's rushers
pbp_rp %>%
  filter(posteam == "NE", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)
#going to be doing a lot of patriots work, so making data set for pats
pats_o_stats <- pbp_rp %>%
  filter(posteam == "NE")
pats_D <- pbp_rp %>%
  filter(defteam == "NE")
