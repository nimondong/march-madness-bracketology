### Nimon Dong
### Final Project: Data Cleaning Script
### STAT 301-2
### 3/2/2020

# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readr)
library(janitor)
library(skimr)
library(modelr)
library(broom)
library(dplyr)

select <- dplyr::select

# DATA CLEANING -----------------------------------------------------------

#### General Set-up 
# Tournament matchups
# Team 1 | Team 2 | Team 1 regular season stats ... | Team 2 regular season  stats ... | Team1-Team2 stats ...  | Win?
# TeamA | TeamB  | TeamA regular season  stats  ... | TeamB regular season  stats ... | TeamA - TeamB stats ...|1
# TeamB | TeamA |  TeamB regular season  stats  ... | TeamA regular season  stats ... | TeamB - TeamA stats ...|0

# Reading in Dataset 
reg_season_stats <- read_csv("data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv") %>%
  clean_names()

team_names <- read_csv("data/MDataFiles_Stage1/MTeams.csv") %>%
  clean_names()

# Merging in team names -> easier identifications
team_names_w <- team_names %>%
  rename(w_team_id = team_id) %>%
  select(w_team_id, team_name)

team_names_l <- team_names %>%
  rename(l_team_id = team_id) %>%
  select(l_team_id, team_name)

# winning team column
reg_season_stats_w <- reg_season_stats %>%
  left_join(team_names_w, by = "w_team_id") %>%
  select(-l_team_id) %>%
  mutate(wins = 1,
         losses = 0) %>%
  rename(team_id = w_team_id, 
         score = w_score,  
         fgm = wfgm, 
         fga = wfga, 
         fgm3 = wfgm3,
         fga3 = wfga3, 
         ftm = wftm, 
         fta = wfta, 
         or = wor, 
         dr = wdr, 
         ast = w_ast, 
         to = wto, 
         stl = w_stl, 
         blk = w_blk, 
         pf = wpf,
         opp_score = l_score,  
         opp_fgm = lfgm, 
         opp_fga = lfga, 
         opp_fgm3 = lfgm3, 
         opp_fga3 = lfga3,
         opp_ftm = lftm, 
         opp_fta = lfta, 
         opp_or = lor, 
         opp_dr = ldr, 
         opp_ast = l_ast, 
         opp_to = lto, 
         opp_stl = l_stl, 
         opp_blk = l_blk, 
         opp_pf = lpf)

# losing teams column
reg_season_stats_l <- reg_season_stats %>%
  left_join(team_names_l, by = "l_team_id") %>%
  select(-w_team_id) %>%
  mutate(wins = 0,
         losses = 1) %>%
  rename(team_id = l_team_id, 
         score = l_score,  
         fgm = lfgm, 
         fga = lfga, 
         fgm3 = lfgm3,
         fga3 = lfga3, 
         ftm = lftm, 
         fta = lfta, 
         or = lor, 
         dr = ldr, 
         ast = l_ast, 
         to = lto, 
         stl = l_stl, 
         blk = l_blk, 
         pf = lpf,
         opp_score = w_score,  
         opp_fgm = wfgm, 
         opp_fga = wfga, 
         opp_fgm3 = wfgm3, 
         opp_fga3 = wfga3, 
         opp_ftm = wftm, 
         opp_fta = wfta, 
         opp_or = wor, 
         opp_dr = wdr, 
         opp_ast = w_ast, 
         opp_to = wto, 
         opp_stl = w_stl, 
         opp_blk = w_blk, 
         opp_pf = wpf)

# binding win and losing team data sets together
reg_season_stats_wl <- rbind(reg_season_stats_w, reg_season_stats_l)

# game stats by season
season_stats <- reg_season_stats_wl %>%
  mutate(total_games = 1) %>%
  group_by(team_name, team_id, season) %>%
  summarise(games_played = sum(total_games),
            wins = sum(wins),
            losses = sum(losses),
            score = sum(score),  
            num_ot = sum(num_ot),
            fgm = sum(fgm), 
            fga = sum(fga), 
            fgm3 = sum(fgm3), 
            fga3 = sum(fga3), 
            ftm = sum(ftm), 
            fta = sum(fta), 
            or = sum(or), 
            dr = sum(dr), 
            ast = sum(ast), 
            to = sum(to), 
            stl = sum(stl), 
            blk = sum(blk), 
            pf = sum(pf),
            # Opponent Stats
            opp_score = sum(opp_score),  
            opp_fgm = sum(opp_fgm), 
            opp_fga = sum(opp_fga), 
            opp_fgm3 = sum(opp_fgm3), 
            opp_fga3 = sum(opp_fga3), 
            opp_ftm = sum(opp_ftm), 
            opp_fta = sum(opp_fta), 
            opp_or = sum(opp_or), 
            opp_dr = sum(opp_dr), 
            opp_ast = sum(opp_ast), 
            opp_to = sum(opp_to), 
            opp_stl = sum(opp_stl), 
            opp_blk = sum(opp_blk), 
            opp_pf = sum(opp_pf))

season_stats 

# Calculating advanced stats

### Advanced Stats Glossary ###
# win_pct = Win Percentage
# pos = Possessions
# opp_pos = Opponent Possessions
# pace = Average Possessions Per Game (Slightly difference interpretation)
# efg_pct = Effective Field Goal Percentage 
# ts_percent = True Shooting Percent
# r3P = 3-point Attempt Rate
# or_pct = Offensive Rebounding Percentage 
# dr_pct = Defensive Rebounding Percentage 
# trb_pct = Total Rebound Percentage
# ast_pct = Assist Percentage
# stl_pct = Steal Percentage
# to_pct = Turnover Percentage 
# blk_pct = Block Percentage
# ftr = Free Throw Rate 
# ORtg = Offensive Rating / Efficiency
# DRtg = Defensive Rating / Efficiency

advanced_stats <- season_stats %>%
  mutate(win_pct = wins / games_played,
         pos = 0.96*((fga)+(to)+0.44*(fta)-(or)),
         opp_pos = 0.96*((opp_fga)+(opp_to)+0.44*(opp_fta)-(opp_or)),
         pace = pos / games_played,
         efg_pct = (fgm + 0.5*fgm3)/fga,
         ts_pct = score / (2*fga + 0.44*fta),
         r3P = fga3 / fga,
         or_pct = or / (or + opp_dr),
         dr_pct = dr / (dr + opp_or),
         trb_pct = (or + dr) / (or + dr + opp_or + opp_dr),
         ast_pct = ast / fgm,
         stl_pct = stl / opp_pos,
         to_pct = to / pos,
         blk_pct = blk / opp_fga,
         ftr = fta / fga,
         ORtg = (100*score) / pos,
         DRtg = (100*opp_score) / pos
         ) %>%
  select(team_name, team_id, season, win_pct, pace, efg_pct, ts_pct, r3P, or_pct, dr_pct, trb_pct, ast_pct, stl_pct, to_pct, blk_pct, ftr, ORtg, DRtg)

advanced_stats_team_1 <- advanced_stats %>%
  rename(team_1 = team_id,
         team_1_name = team_name,
         team_1_win_pct = win_pct, 
         team_1_pace = pace, 
         team_1_efg_pct = efg_pct, 
         team_1_ts_pct = ts_pct, 
         team_1_r3P = r3P, 
         team_1_or_pct = or_pct, 
         team_1_dr_pct = dr_pct, 
         team_1_trb_pct = trb_pct, 
         team_1_ast_pct = ast_pct, 
         team_1_stl_pct = stl_pct, 
         team_1_to_pct = to_pct, 
         team_1_blk_pct = blk_pct, 
         team_1_ftr = ftr, 
         team_1_ORtg = ORtg, 
         team_1_DRtg = DRtg,
         )


advanced_stats_team_2 <- advanced_stats %>%
  rename(team_2 = team_id,
         team_2_name = team_name,
         team_2_win_pct = win_pct, 
         team_2_pace = pace, 
         team_2_efg_pct = efg_pct, 
         team_2_ts_pct = ts_pct, 
         team_2_r3P = r3P, 
         team_2_or_pct = or_pct, 
         team_2_dr_pct = dr_pct, 
         team_2_trb_pct = trb_pct, 
         team_2_ast_pct = ast_pct, 
         team_2_to_pct = to_pct, 
         team_2_stl_pct = stl_pct, 
         team_2_blk_pct = blk_pct, 
         team_2_ftr = ftr, 
         team_2_ORtg = ORtg, 
         team_2_DRtg = DRtg,
         )

# Read in Datasets

tourney_data <- read_csv("data/MDataFiles_Stage1/MNCAATourneyCompactResults.csv") %>%
  clean_names()

# win = if team 1 wins
tourney_matchups_w <- tourney_data %>%
  filter(season >= 2003) %>%
  select(season, w_team_id, l_team_id) %>%
  rename(team_1 = w_team_id,
         team_2 = l_team_id) %>%
  mutate(win = 1)

tourney_matchups_l <- tourney_data %>%
  filter(season >= 2003) %>%
  select(season, w_team_id, l_team_id) %>%
  rename(team_1 = l_team_id,
         team_2 = w_team_id) %>%
  mutate(win = 0)

tourney_matchups <- rbind(tourney_matchups_w, tourney_matchups_l)

tourney_matchups

tourney_matchups <- tourney_matchups %>%
  left_join(advanced_stats_team_1, by = c("team_1", "season")) %>%
  left_join(advanced_stats_team_2, by = c("team_2", "season"))

# Seeds 

seed_data <- read_csv("data/MDataFiles_Stage1/MNCAATourneySeeds.csv") %>%
  clean_names()

# team one seed
seed_data_1 <- seed_data %>%
  filter(season >= 2003) %>%
  mutate(seed = as.double(str_sub(seed, 2, 3))) %>%
  rename(team_1 = team_id,
         team_1_seed = seed)

# team two seed
seed_data_2 <- seed_data %>%
  filter(season >= 2003) %>%
  mutate(seed = as.double(str_sub(seed, 2, 3))) %>%
  rename(team_2 = team_id,
         team_2_seed = seed)

# merge with tourney matchups

tourney_matchups <- tourney_matchups %>%
  left_join(seed_data_1, by = c("team_1", "season")) %>%
  left_join(seed_data_2, by = c("team_2", "season"))


# add in stat differentials
tourney_matchups <- tourney_matchups %>%
  select(season, win, team_1, team_2, team_1_name, team_2_name, team_1_seed, team_2_seed, everything()) %>%
  mutate(diff_seed = team_1_seed - team_2_seed,
         diff_win_pct = team_1_win_pct - team_2_win_pct, 
         diff_pace = team_1_pace - team_2_pace, 
         diff_efg_pct = team_1_efg_pct - team_2_efg_pct, 
         diff_ts_pct = team_1_ts_pct - team_2_ts_pct, 
         diff_r3P = team_1_r3P - team_2_r3P, 
         diff_or_pct = team_1_or_pct - team_2_or_pct, 
         diff_dr_pct = team_1_dr_pct - team_2_dr_pct, 
         diff_trb_pct = team_1_trb_pct - team_2_trb_pct, 
         diff_ast_pct = team_1_ast_pct - team_2_ast_pct, 
         diff_to_pct = team_1_to_pct - team_2_to_pct, 
         diff_stl_pct = team_1_stl_pct - team_2_stl_pct, 
         diff_blk_pct = team_1_blk_pct - team_2_blk_pct, 
         diff_ftr = team_1_ftr - team_2_ftr, 
         diff_ORtg = team_1_ORtg - team_2_ORtg, 
         diff_DRtg = team_1_DRtg - team_2_DRtg)

write_csv(tourney_matchups, "data/cleaned_data.csv")



