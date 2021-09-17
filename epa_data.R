library(rstan)
library(lme4)
library(tidyverse)
library(DT)
library(tidybayes)
library(ggrepel)
library(magick)
library(resample)

set.seed(1234)

# seasons <- 2018:2020
# dat <- purrr::map_df(seasons, function(x) {
#   readRDS(
#     url(
#       glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
#     )
#   )
# })
# 
# post16 <- filter(dat,
#                  season_type == 'REG' & 
#                    !is.na(epa) &
#                    play_type %in% c('pass','run') &
#                    down %in% c(1,2,3,4)
# ) %>%
#   dplyr::select(season, week, posteam, defteam, epa, play_id, qtr, quarter_seconds_remaining) %>%
#   mutate(def_id = as.numeric(factor(str_c(defteam, '-', season))),
#          off_id = as.numeric(factor(str_c(posteam, '-', season))))
# 
# epa_off_actual <- post16 %>%
#   group_by(posteam, week, season, off_id) %>%
#   summarise(offensive_epa = mean(epa),
#             n_offense = n())
# 
# epa_def_actual <- post16 %>%
#   group_by(defteam, week,  season, def_id) %>%
#   summarise(defensive_epa = mean(epa))
# 
# epa_actual <- epa_off_actual %>%
#   inner_join(epa_def_actual, by = c('posteam' = 'defteam', 'week', 'season','off_id' = 'def_id'))

##################################################################################

# seasons <- 2020
raw_data17 <- nflfastR::load_pbp(2017)
raw_data16 <- nflfastR::load_pbp(2016)
raw_data15 <- nflfastR::load_pbp(2015)
raw_data18 <- nflfastR::load_pbp(2018)
raw_data19 <- nflfastR::load_pbp(2019)
raw_data20 <- nflfastR::load_pbp(2020)
raw_data21 <- nflfastR::load_pbp(2021)
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