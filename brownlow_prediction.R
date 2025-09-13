### getting data from dataset ###
data_path = "c:/Users/Owen/OneDrive/RGit/brownlow_medal_2025/brownlow_data"  # Replace with your dataset path
files <- list.files(data_path, pattern = "*.csv", full.names = TRUE)
data_list <- lapply(files, read.csv)
data <- do.call(rbind, data_list)

### loading libraries ###
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

### data cleaning ###
summary(as.Date(data$match_date))
# not reading date correctly, so need to convert
summary(as.Date(data$match_date, format = "%d/%m/%Y" ))
#thats better

#cleaning data - need to sort out date and a heap of factors (teams, stadiums, coaches, position, etc.)
df <- data %>%
  mutate(match_date = as.Date(match_date, format = "%d/%m/%Y"),
         match_home_team = as.factor(match_home_team),
         match_away_team = as.factor(match_away_team),
         venue_name = as.factor(venue_name),
         match_winner = as.factor(match_winner),
         team_coach = as.factor(team_coach),
         player_team = as.factor(player_team),
         coach_team = as.factor(player_team),
         player_position = as.factor(player_position)
         ) %>%
  filter(!is.na(match_date),
         year(match_date) != 2000) 
#removing 2000 as covid year could be an outlier due to crowds, changes in game times, etc.

#developing a team only data frame
game_df <- df %>%
  mutate(match_margin = (home_final_score - away_final_score),
         #match_round = as.factor(match_round)
         ) %>%
  select(match_id, match_date, match_round, match_home_team, match_away_team, 
         match_winner, match_margin, venue_name, team_coach) %>%
  distinct(match_id, .keep_all = TRUE) 

season_points <- df %>%
  mutate(home_points = case_when(home_final_score > away_final_score ~ 4,
                                 home_final_score == away_final_score ~ 2,
                                 TRUE ~ 0),
         away_points = case_when(away_final_score > home_final_score ~ 4,
                                 home_final_score == away_final_score ~ 2,
                                 TRUE ~ 0)
         ) %>%
  distinct(match_id, .keep_all = TRUE) %>%
  select(match_id, match_date, match_round, match_home_team, match_away_team, 
         match_winner, match_margin, venue_name, team_coach, home_points, away_points) %>%
  pivot_longer(cols = c(match_home_team, match_away_team),
               names_to = "home_away",
               values_to = "team") %>%
  mutate(points = ifelse(home_away == "match_home_team", home_points, away_points)) %>%
  select(-home_away, -home_points, -away_points) %>%
  arrange(match_date) %>%
  group_by(team, year(match_date)) %>%
  mutate(cumulative_points = cumsum(points),
         games_played = row_number(),
         avg_points_per_game = cumulative_points / games_played
         ) %>%
  ungroup() %>%
  dplyr::select(match_id, team, points, cumulative_points, games_played, avg_points_per_game)

# getting team sums per game of player stats
team_stats <- df %>%
  group_by(match_id, player_team) %>%
  summarise(across(c(coaches_votes:spoils), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(team = player_team)

game_stats <- df %>%
  group_by(match_id) %>%
  summarise(across(c(coaches_votes:spoils), sum, na.rm = TRUE)) %>%
  ungroup()

#joining df with summarised stats to get a better view of how 
df1 <- df %>%
  left_join(team_stats, by = c("match_id", "player_team" = "team"), suffix = c("", "_team")) %>%
  left_join(game_stats, by = "match_id", suffix = c("", "_game")) %>%
  left_join(season_points, by = c("match_id", "player_team" = "team")) %>%
  arrange(match_date) %>%
  group_by(player_id) %>%
  mutate(points_scored = 6*goals + 1*behinds,
         points_scored_pctg = ifelse(player_team==match_home_team,
                                     points_scored/home_final_score,
                                     points_scored/away_final_score),
         coaches_votes_pctg_team = coaches_votes/coaches_votes_team,
         coaches_votes_pctg_game = coaches_votes/coaches_votes_game,
         supercoach_pctg_team = supercoach_score/supercoach_score_team,
         supercoach_pctg_game = supercoach_score/supercoach_score_game,
         afl_fantasy_pctg_team = afl_fantasy_score/afl_fantasy_score_team,
         afl_fantasy_pctg_game = afl_fantasy_score/afl_fantasy_score_game,
         rating_points_pctg_team = rating_points/rating_points_team,
         rating_points_pctg_game = rating_points/rating_points_game,
         kicks_pctg_team = kicks/kicks_team,
         kicks_pctg_game = kicks/kicks_game,
         marks_pctg_team = marks/marks_team,
         marks_pctg_game = marks/marks_game,
         handballs_pctg_team = handballs/handballs_team,
         handballs_pctg_game = handballs/handballs_game,
         disposals_pctg_team = disposals/disposals_team,
         disposals_pctg_game = disposals/disposals_game,
         effective_disposals_pctg_team = effective_disposals/effective_disposals_team,
         effective_disposals_pctg_game = effective_disposals/effective_disposals_game,
         goals_pctg_team = goals/goals_team,
         goals_pctg_game = goals/goals_game,
         behinds_pctg_team = behinds/behinds_team,
         behinds_pctg_game = behinds/behinds_game,
         hitouts_pctg_team = hitouts/hitouts_team,
         hitouts_pctg_game = hitouts/hitouts_game,
         tackles_pctg_team = tackles/tackles_team,
         tackles_pctg_game = tackles/tackles_game,
         rebounds_pctg_team = rebounds/rebounds_team,
         rebounds_pctg_game = rebounds/rebounds_game,
         inside50s_pctg_team = inside_fifties/inside_fifties_team,
         inside50s_pctg_game = inside_fifties/inside_fifties_game,
         clearances_pctg_team = clearances/clearances_team,
         clearances_pctg_game = clearances/clearances_game,
         clangers_pctg_team = clangers/clangers_team,
         clangers_pctg_game = clangers/clangers_game,
         free_kicks_for_pctg_team = free_kicks_for/free_kicks_for_team,
         free_kicks_for_pctg_game = free_kicks_for/free_kicks_for_game,
         free_kicks_against_pctg_team = free_kicks_against/free_kicks_against_team,
         free_kicks_against_pctg_game = free_kicks_against/free_kicks_against_game,
         contested_possessions_pctg_team = contested_possessions/contested_possessions_team,
         contested_possessions_pctg_game = contested_possessions/contested_possessions_game,
         uncontested_possessions_pctg_team = uncontested_possessions/uncontested_possessions_team,
         uncontested_possessions_pctg_game = uncontested_possessions/uncontested_possessions_game,
         contested_marks_pctg_team = contested_marks/contested_marks_team,
         contested_marks_pctg_game = contested_marks/contested_marks_game,
         marks_inside50_pctg_team = marks_inside_fifty /marks_inside_fifty_team,
         marks_inside50_pctg_game = marks_inside_fifty/marks_inside_fifty_game,
         one_percenters_pctg_team = one_percenters/one_percenters_team,
         one_percenters_pctg_game = one_percenters/one_percenters_game,
         bounces_pctg_team = bounces/bounces_team,
         bounces_pctg_game = bounces/bounces_game,
         goal_assists_pctg_team = goal_assists/goal_assists_team,
         goal_assists_pctg_game = goal_assists/goal_assists_game,
         centre_clearances_pctg_team = centre_clearances/centre_clearances_team,
         centre_clearances_pctg_game = centre_clearances/centre_clearances_game,
         stoppage_clearances_pctg_team = stoppage_clearances/stoppage_clearances_team,
         stoppage_clearances_pctg_game = stoppage_clearances/stoppage_clearances_game,
         score_involvements_pctg_team = score_involvements/score_involvements_team,
         score_involvements_pctg_game = score_involvements/score_involvements_game,
         metres_gained_pctg_team = metres_gained/metres_gained_team,
         metres_gained_pctg_game = metres_gained/metres_gained_game,
         turnovers_pctg_team = turnovers/turnovers_team,
         turnovers_pctg_game = turnovers/turnovers_game,
         intercepts_pctg_team = intercepts/intercepts_team,
         intercepts_pctg_game = intercepts/intercepts_game,
         tackles_inside_fifty_pctg_team = tackles_inside_fifty/tackles_inside_fifty_team,
         tackles_inside_fifty_pctg_game = tackles_inside_fifty/tackles_inside_fifty_game,
         contest_def_losses_pctg_team = contest_def_losses/contest_def_losses_team,
         contest_def_losses_pctg_game = contest_def_losses/contest_def_losses_game,
         contest_def_one_on_ones_pctg_team = contest_def_one_on_ones/contest_def_one_on_ones_team,
         contest_def_one_on_ones_pctg_game = contest_def_one_on_ones/contest_def_one_on_ones_game,
         contest_off_wins_pctg_team = contest_off_wins/contest_off_wins_team,
         contest_off_wins_pctg_game = contest_off_wins/contest_off_wins_game,
         contest_off_one_on_ones_pctg_team = contest_off_one_on_ones/contest_off_one_on_ones_team,
         contest_off_one_on_ones_pctg_game = contest_off_one_on_ones/contest_off_one_on_ones_game,
         def_half_pressure_acts_pctg_team = def_half_pressure_acts/def_half_pressure_acts_team,
         def_half_pressure_acts_pctg_game = def_half_pressure_acts/def_half_pressure_acts_game,
         effective_kicks_pctg_team = effective_kicks/effective_kicks_team,
         effective_kicks_pctg_game = effective_kicks/effective_kicks_game,
         f50_ground_ball_gets_pctg_team = f50_ground_ball_gets/f50_ground_ball_gets_team,
         f50_ground_ball_gets_pctg_game = f50_ground_ball_gets/f50_ground_ball_gets_game,
         ground_ball_gets_pctg_team = ground_ball_gets/ground_ball_gets_team,
         ground_ball_gets_pctg_game = ground_ball_gets/ground_ball_gets_game,
         hitouts_to_advantage_pctg_team = hitouts_to_advantage/hitouts_to_advantage_team,
         hitouts_to_advantage_pctg_game = hitouts_to_advantage/hitouts_to_advantage_game,
         intercept_marks_pctg_team = intercept_marks/intercept_marks_team,
         intercept_marks_pctg_game = intercept_marks/intercept_marks_game,
         marks_on_lead_pctg_team = marks_on_lead/marks_on_lead_team,
         marks_on_lead_pctg_game = marks_on_lead/marks_on_lead_game,
         pressure_acts_pctg_team = pressure_acts/pressure_acts_team,
         pressure_acts_pctg_game = pressure_acts/pressure_acts_game,
         ruck_contests_pctg_team = ruck_contests/ruck_contests_team,
         ruck_contests_pctg_game = ruck_contests/ruck_contests_game,
         score_launches_pctg_team = score_launches/score_launches_team,
         score_launches_pctg_game = score_launches/score_launches_game,
         shots_at_goal_pctg_team = shots_at_goal/shots_at_goal_team,
         shots_at_goal_pctg_game = shots_at_goal/shots_at_goal_game,
         spoils_pctg_team = spoils/spoils_team,
         spoils_pctg_game = spoils/spoils_game,
         three_votes = ifelse(brownlow_votes==3,1,0),
         two_votes = ifelse(brownlow_votes==2,1,0),
         one_votes = ifelse(brownlow_votes==1,1,0)) %>%
  ungroup()

#interaction_features development




#training_dataset - df1 before 2024
train_df <- df1 %>% filter()

