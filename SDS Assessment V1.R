# Charlotte Athletics SDS Assessment: Football

library(tidymodels)
library(tidyverse)
library(dplyr)
library(cfbfastR)
library(gt)

# ********** API Key from www.collegefootballdata.com required**********
# Previously loaded into R environment

# Loading data for season 2014-2020 
tictoc::tic()
pbp <- data.frame()
seasons <- 2014:2020
progressr::with_progress({
  future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})
tictoc::toc()

# Filtering for 2020 play-by-play data (only need that year)
pbp2020.01 <- pbp %>%
  filter(year == 2020)

# Removing 2014-2020 pbp dataframe 
rm(pbp)

# Looking up postseason game info for Ohio State to identify game_id against Clemson
osu <- cfbd_game_info(2020, season_type = "postseason", team = "Ohio State")  
osu

# Filtering for only play-by-play data for Ohio State vs. Clemson 
pbp_osu_clem <- pbp2020 %>%
  filter(game_id == 401240173) 

# Making sure total points scored in play-by-play data matches cfbd_game_info & assessment detail
pbp_osu_clem %>% 
  group_by(pos_team) %>%
  summarise(pts = sum(pts_scored))

# Total passing stats by team
pass_total <- pbp_osu_clem %>%
  filter(pass_attempt == 1) %>%
  group_by(pos_team) %>%
  summarise(Completions = sum(completion), Attempts = sum(pass_attempt), Passing_Yards = sum(yds_receiving, na.rm = TRUE), 
            Passing_TDs = sum(pass_td), Interceptions = sum(int), Yards_Per_Attempt = Passing_Yards/Attempts, Mean_EPA = mean(EPA)) 

# Checking stats by play types
pbp_osu_clem %>%
  filter(pass_attempt == 1) %>%
  group_by(pos_team, play_type) %>%
  summarise(Completions = sum(completion), Attempts = sum(pass_attempt), Passing_Yards = sum(yds_receiving, na.rm = TRUE), 
            Passing_TDs = sum(pass_td), Interceptions = sum(int), Mean_EPA = mean(EPA)) 

### 1. What was OSU QB Justin Fields' passing numbers (completions, attempts, yards, TD's, INT's) when he was in the red zone? 
pbp_fields_rz <- pbp_osu_clem %>%
  mutate(pbp_osu_clem,
         red_zone =  ifelse(rz_play == 1, "Red Zone", "Outside Red Zone")) %>%
  filter(passer_player_name == "Justin Fields") %>%
  group_by(red_zone) %>%
  summarize(Completions = sum(completion), Attempts = sum(pass_attempt), Passing_Yards = sum(yds_receiving, na.rm = TRUE), 
            Passing_TDs = sum(pass_td), Interceptions = sum(int), Yards_Per_Attempt = Passing_Yards/Attempts, 
            Mean_EPA = mean(EPA)) %>%
  rename("Field Position" = red_zone) %>%
  slice(2,1)

# Format into Readable Table
pbp_fields_rz %>% 
  gt() %>% 
  tab_header(title = "2020 Sugar Bowl: Justin Fields Red Zone Efficiency") %>%
  cols_label(Completions = "C", 
             Attempts = "Att",
             Passing_Yards = "Yds",
             Passing_TDs = "TDs",
             Interceptions = "INTs",
             Yards_Per_Attempt = "YPA",
             Mean_EPA = "Avg EPA") %>%
  fmt_number(
    columns = vars(Yards_Per_Attempt, Mean_EPA),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "yellow"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1:8,
      rows = Passing_TDs >= 3)
  ) %>%
  gt_theme_538(table.width = px(550)) 

# Alternate Method using Yards to Goal to determine Red Zone threshold
pbp_fields_rz2 <- pbp_osu_clem %>%
  mutate(pbp_osu_clem,
         red_zone2 =  ifelse(yards_to_goal <= 20, "Red Zone", "Outside Red Zone")) %>%
  filter(passer_player_name == "Justin Fields") %>%
  group_by(red_zone2) %>%
  summarize(Completions = sum(completion), Attempts = sum(pass_attempt), Passing_Yards = sum(yds_receiving, na.rm = TRUE), 
            Passing_TDs = sum(pass_td), Interceptions = sum(int), Yards_Per_Attempt = Passing_Yards/Attempts, 
            Mean_EPA = mean(EPA)) %>%
  rename("Field Position" = red_zone2)

pbp_fields_rz2 %>%
  slice(2,1)
  
### 2- What was Clemson QB Trevor Lawrence's passing numbers (completions, attempts, yards, TD's INT's) on 1st down?  
pbp_lawrence_downs <- pbp_osu_clem %>%
  mutate(pbp_osu_clem,
         down_fix = case_when(down == 1 ~ "First Down",
                              down == 2 ~ "Second Down",
                              down == 3 ~ "Third Down",
                              down == 4 ~ "Fourth Down")) %>%
  filter(passer_player_name == "Trevor Lawrence") %>%
  group_by(down_fix) %>%
  summarize(Completions = sum(completion), Attempts = sum(pass_attempt), Passing_Yards = sum(yds_receiving, na.rm = TRUE), 
            Passing_TDs = sum(pass_td), Interceptions = sum(int), Yards_Per_Attempt = Passing_Yards/Attempts, 
            Mean_EPA = mean(EPA)) %>%
  rename(`Down Number` = down_fix) %>%
  slice(1,3,4,2)

pbp_lawrence_downs %>% 
  gt() %>% 
  tab_header(title = "2020 Sugar Bowl: Trevor Lawrence on First Down") %>%
  cols_label(Completions = "C", 
             Attempts = "Att",
             Passing_Yards = "Yds",
             Passing_TDs = "TDs",
             Interceptions = "INTs",
             Yards_Per_Attempt = "YPA",
             Mean_EPA = "Avg EPA") %>%
  fmt_number(
    columns = vars(Yards_Per_Attempt, Mean_EPA),
    decimals = 2
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "yellow"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = 1:8,
      rows = Attempts >= 20)
  ) %>%
  gt_theme_538(table.width = px(550))  
