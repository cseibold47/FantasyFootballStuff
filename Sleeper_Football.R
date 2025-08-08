
#### Loading Packages ####

library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

#### Loading Sources ####

source("~/Desktop/Jva_Misc_Utils.R")

source("~/Desktop/everything_else/Coding_Practice/Football_R/Sleeper_Football_Utils.R")

#### Settings ####

Positions_I_Care_About <- c("QB", "RB", "WR", "TE", "DEF", "K")

#### Scraping Sleeper Projections ####

Sleeper_Proj_Save_Path = "~/Desktop/everything_else/Coding_Practice/Football_R/Sleeper_Projections/"

Scrape_And_Save_Sleeper_Projections(Sleeper_Proj_Save_Path = Sleeper_Proj_Save_Path)

#### Loading Data ####

Sleeper_Projections_Data <- Get_Saved_Sleeper_Projections(
  Sleeper_Proj_Save_Path = Sleeper_Proj_Save_Path,
  Years_To_Get = 2020:2024
  )

Espn_Nfl_Roster_Data <- Get_Saved_Nfl_Rosters_Via_Espn() %>%
  transform(player_id = as.integer(player_id))

Schedule_Data <- nflfastR::fast_scraper_schedules() %>%
  mutate(across(c(away_team, home_team), ~ifelse(. == "LA", "LAR", .)))

Week_Of_Interest <- Schedule_Data %>%
  filter(as.Date(gameday) >= Sys.Date()) %>%
  filter(as.Date(gameday) == min(as.Date(gameday))) %>%
  head(1) %>%
  pull(week)

Master_Table_Df <- "https://api.sleeper.app/v1/players/nfl" %>%
  httr::GET() %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON(flatten = TRUE) %>%
  map_df(~ as_tibble(clean_list(.)))

##### Stealing Sleepers' Stats #####

Weeks_To_Scrape <- 1:18
Years_To_Scrape <- 2020:2024

Player_Week_Stats <- purrr::pmap(
  .l = as.list(expand_grid(week = Weeks_To_Scrape, year = Years_To_Scrape)),
  .progress = TRUE,
  .f = purrr::possibly(
    .f = function(week, year){
      Stats_Link <- paste0(
        "https://api.sleeper.com/stats/nfl/", year, "/", week, 
        "?season_type=regular&position[]=DEF&position[]=K&position[]=QB&",
        "position[]=RB&position[]=TE&position[]=WR&order_by=pts_ppr")
      
      Stats_Link %>%
        httr::GET() %>%
        httr::content(as = "text") %>%
        fromJSON(flatten = TRUE)
    }
  )
) %>%
  Filter(f = function(x) is.data.frame(x) && nrow(x) > 0 && ncol(x) > 0, x = .) %>%
  list_rbind() %>%
  janitor::clean_names()

Locked_Weekly_Scoring <- Player_Week_Stats %>%
  filter(
    as.Date(date) < Sys.Date(),
    week == Week_Of_Interest, 
    season == as.character(lubridate::year(Sys.Date()))
    ) %>%
  select(season, week, player_id, forced_real_score = stats_pts_half_ppr) %>%
  glimpse()

Simple_Past_Half_Ppr_Points <- Player_Week_Stats %>%
  select(date, season, week, player_id, team, pts_half_ppr = stats_pts_half_ppr) %>%
  glimpse()

#### Wrangling Boyyy ####

Combined_Df <- Master_Table_Df %>%
  group_by(player_id) %>%
  mutate(fantasy_positions = paste(sort(fantasy_positions), collapse = ", ")) %>%
  ungroup() %>%
  distinct() %>%
  full_join(Sleeper_Projections_Data, by = "player_id", relationship = "one-to-many") %>%
  filter(week > Week_Of_Interest, season == as.character(lubridate::year(Sys.Date()))) %>%
  janitor::clean_names() %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE")
    )

Projections_Aggregated <- Combined_Df %>%
  mutate(player_name = paste(first_name, last_name)) %>%
  group_by(player_id, player_name, fantasy_positions) %>%
  summarise(
    pts_half_ppr_avg = mean(stats_pts_half_ppr[stats_pts_half_ppr > 0], na.rm = TRUE),
    pts_half_ppr = sum(stats_pts_half_ppr, na.rm = TRUE),
    pts_ppr_avg = mean(stats_pts_ppr[stats_pts_ppr > 0], na.rm = TRUE),
    pts_ppr = sum(stats_pts_ppr, na.rm = TRUE),
    .groups = "drop_last") %>%
  ungroup() %>%
  filter(pts_half_ppr > 0) %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE")
  ) %>%
  glimpse()

#### Projections Retrospectives ####

Pivoted_Venue_Data <- Schedule_Data %>%
  select(season, week, away_team, home_team, roof:wind) %>%
  pivot_longer(ends_with("_team"), names_to = "home_away", values_to = "team") %>%
  glimpse()

Vizi_Temp_Df <- Master_Table_Df %>%
  unnest(fantasy_positions) %>%
  filter(fantasy_positions %in% Positions_I_Care_About) %>%
  group_by(player_id) %>%
  mutate(fantasy_positions = paste(sort(fantasy_positions), collapse = ", ")) %>%
  ungroup() %>%
  distinct() %>%
  select(-team) %>% 
  full_join(Sleeper_Projections_Data, by = "player_id", relationship = "one-to-many") %>%
  janitor::clean_names() %>%
  rename(half_ppr_projection = stats_pts_half_ppr) %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE")
  ) %>%
  inner_join(
    Pivoted_Venue_Data %>%
      mutate(season = as.character(season)),
    by = c("season", "week", "team"),
    relationship = "many-to-one"
  ) %>%
  filter(!(season == "2020" & week == 18)) %>%
  group_by(player_id, season) %>%
  filter(sum(half_ppr_projection, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  group_by(team, season, week, position) %>%
  arrange(desc(half_ppr_projection)) %>%
  mutate(team_week_pos_rank = row_number()) %>%
  ungroup() %>%
  left_join(
    Simple_Past_Half_Ppr_Points %>%
      select(player_id, season, week, pts_half_ppr), 
    by = c("player_id", "season", "week"), 
    relationship = "one-to-one") %>%
  mutate(
    half_ppr_projection = replace_na(half_ppr_projection, 0),
    pts_half_ppr = replace_na(pts_half_ppr, 0),
    residual = pts_half_ppr - half_ppr_projection)

Vizi_Temp_Df %>%
  filter(!is.na(team)) %>%
  glimpse() %>%
  transform(season = as.numeric(season)) %>%
  ggplot(aes(team, week)) +
  facet_wrap(~ season) +
  geom_tile(fill = "red") +
  geom_tile(data = . %>% inner_join(Pivoted_Venue_Data, by = c("season", "week", "team"))) +
  labs(
    x = "Team",
    y = "Week",
    title = "Visualizing Schedules And Byes"
  ) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

Vizi_Temp_Df %>%
  filter(position != "FB") %>%
  group_by(team, season, week, position) %>%
  filter(sum(pts_half_ppr, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  filter(
    team_week_pos_rank == 1,
    !(position == "QB" & half_ppr_projection < 7.5),
    !(position == "K" & half_ppr_projection < 4)
  ) %>%
  # filter(is.na(half_ppr_projection), position == "QB", pts_half_ppr > 0) %>%
  # arrange(desc(pts_half_ppr)) %>%
  # glimpse()
  ggplot(aes(half_ppr_projection, pts_half_ppr)) +
  facet_wrap(~ position, scales = "free") +
  geom_point() +
  geom_abline(color = "red") +
  clean_loess_smooth() +
  clean_lm_smooth(se = TRUE) +
  labs(
    x = "Half PPR Projection",
    y = "Observed Half PPR Points",
    caption = "orange = loess fit, blue = linear fit",
    title = "2020-2024 Sleeper Projections Calibration Plot By Position") +
  theme_clean()

Vizi_Temp_Df %>%
  filter(position != "FB", half_ppr_projection > 0) %>%
  group_by(team, season, week, position) %>%
  filter(sum(pts_half_ppr, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  filter(team_week_pos_rank == 1) %>%
  glimpse() %>%
  ggplot(aes(residual)) +
  facet_wrap(~ paste0(position, 1), scales = "free") +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Half PPR Residual",
    y = "Count",
    title = "Projection Residual Distribution By Position"
  ) +
  theme_clean()

Vizi_Temp_Df %>%
  filter(position != "FB", half_ppr_projection > 0) %>%
  group_by(team, season, week, position) %>%
  filter(sum(pts_half_ppr, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  filter(team_week_pos_rank == 1) %>%
  ggplot(aes(half_ppr_projection, abs(residual))) +
  facet_wrap(~ paste0(position, 1), scales = "free") +
  geom_point() +
  clean_loess_smooth() +
  clean_lm_smooth(se = TRUE) +
  labs(
    x = "Half PPR Projection",
    y = "Absolute Error",
    title = "Projection Residual Distribution By Position"
  ) +
  theme_clean()

Vizi_Temp_Df %>%
  filter(half_ppr_projection > 0, position %in% c("QB", "RB", "WR", "TE", "K", "DEF")) %>%
  ggplot(aes(half_ppr_projection)) +
  facet_wrap(~ position) +
  geom_histogram(aes(fill = as.factor(team_week_pos_rank)), binwidth = 1) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(
    x = "Half PPR Projection", 
    y = "Count", 
    title = "Weekly Projection By Team-Week Position Rank",
    fill = "Team-Week\nPosition Rank") +
  theme_clean() +
  theme(legend.background = element_rect(color = "black"))

Vizi_Temp_Df %>%
  filter(position != "FB", team_week_pos_rank == 1) %>%
  group_by(team, season, week) %>%
  filter(sum(pts_half_ppr[position == "QB"], na.rm = TRUE) > 5) %>%
  ungroup() %>%
  group_by(team, season, week, position) %>%
  filter(sum(pts_half_ppr, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  filter(team_week_pos_rank == 1) %>%
  select(team, season, week, position, residual) %>%
  self_join(by = c("team", "season", "week")) %>%
  filter(position_1 > position_2) %>%
  ggplot(aes(residual_1, residual_2)) +
  facet_grid(paste0(position_2, 1) ~ paste0(position_1, 1), scales = "free") +
  geom_hline(color = "red", yintercept = 0) +
  geom_vline(color = "red", xintercept = 0) +
  geom_point() +
  clean_loess_smooth() +
  clean_lm_smooth() +
  geom_label(
    data = . %>%
      group_by(position_1, position_2) %>%
      summarise(
        x_min = min(residual_1),
        y_max = max(residual_2),
        cor = cor(residual_1, residual_2, method = "pearson"),
        cor_rank = cor(residual_1, residual_2, method = "spearman"),
        .groups = "drop_last") %>%
      ungroup() %>%
      mutate(label = paste0("r: ", round(cor_rank, 3))),
    aes(x = x_min, y = y_max, label = label), 
    color = "blue", hjust = 0, vjust = 1, alpha = 0.75
  ) +
  labs(
    x = "Half PPR Projection Residual",
    y = "Half PPR Projection Residual",
    title = "2020-2024 Same Team Correlation of Projection Residuals") +
  theme_clean()

#### Projection Re-calibration Linear Models ####

Projection_Recalibration_Data <- Master_Table_Df %>%
  unnest(fantasy_positions) %>%
  filter(fantasy_positions %in% Positions_I_Care_About) %>%
  group_by(player_id) %>%
  mutate(fantasy_positions = paste(sort(fantasy_positions), collapse = ", ")) %>%
  ungroup() %>%
  distinct() %>%
  select(-team) %>% 
  full_join(Sleeper_Projections_Data, by = "player_id", relationship = "one-to-many") %>%
  janitor::clean_names() %>%
  rename(half_ppr_projection = stats_pts_half_ppr) %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE")
  ) %>%
  inner_join(
    Pivoted_Venue_Data %>%
      mutate(season = as.character(season)),
    by = c("season", "week", "team"),
    relationship = "many-to-one"
  ) %>%
  filter(!(season == "2020" & week == 18)) %>%
  group_by(player_id, season) %>%
  filter(sum(half_ppr_projection, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  group_by(team, season, week, position) %>%
  arrange(desc(half_ppr_projection)) %>%
  mutate(team_week_pos_rank = row_number()) %>%
  ungroup() %>%
  left_join(
    Simple_Past_Half_Ppr_Points %>%
      select(player_id, season, week, pts_half_ppr), 
    by = c("player_id", "season", "week"), 
    relationship = "one-to-one") %>%
  mutate(
    half_ppr_projection = replace_na(half_ppr_projection, 0),
    pts_half_ppr = replace_na(pts_half_ppr, 0),
    residual = pts_half_ppr - half_ppr_projection) %>%
  filter(position != "FB") %>%
  group_by(team, season, week, position) %>%
  filter(sum(pts_half_ppr, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  filter(
    team_week_pos_rank == 1,
    !(position == "QB" & half_ppr_projection < 7.5),
    !(position == "K" & half_ppr_projection < 4)
  ) %>%
  glimpse()

Projection_Recalibration_Lm <- lm(
  data = Projection_Recalibration_Data,
  formula = pts_half_ppr ~ half_ppr_projection * position
)

summary(Projection_Recalibration_Lm)

Projection_Recalibration_Data <- Projection_Recalibration_Data %>%
  mutate(
    adjusted_projections = predict(Projection_Recalibration_Lm, ., type = "response"),
    resid = pts_half_ppr - adjusted_projections,
    abs_resid = abs(resid)
  ) %>%
  glimpse() 

Projection_Recalibration_Data %>%
  ggplot(aes(adjusted_projections, pts_half_ppr)) +
  facet_wrap(~ position, scales = "free") +
  geom_point() +
  geom_abline(color = "red") +
  clean_loess_smooth() +
  clean_lm_smooth() +
  theme_clean()

Projection_Recalibration_Data %>%
  filter(roof == "outdoors", !is.na(temp)) %>%
  ggplot(aes(temp, pts_half_ppr - adjusted_projections)) +
  facet_wrap(~ position, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_point() +
  clean_loess_smooth() +
  clean_lm_smooth(se = TRUE) +
  theme_clean()

Projection_Recalibration_Data %>%
  ggplot(aes(adjusted_projections, abs_resid)) +
  facet_wrap(~ position, scales = "free") +
  geom_point() +
  clean_loess_smooth() +
  clean_lm_smooth() +
  theme_clean()

Post_Recal_Error_Lm <- lm(
  data = Projection_Recalibration_Data,
  abs_resid ~ adjusted_projections * position
)

summary(Post_Recal_Error_Lm)

#### Sleeper League Scraping / Processing ####

League_Id <- 1134708939104567296

My_Display_Name <- "TaysomsLeftNut"

Users_Data <- paste0("https://api.sleeper.app/v1/league/", League_Id, "/users") %>%
  httr::GET() %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE)

My_User_Id <- Users_Data %>%
  filter(display_name == My_Display_Name) %>%
  pull(user_id)

Schedule_Weeks <- 1:17

Syracuse_Roster_Settings <- c(
  "QB" = 1, "RB" = 2, "WR" = 2, "TE" = 1, "FLEX" = 1, "OP" = 1, "K" = 1, "DEF" = 1)

Matchup_Data <- purrr::map(
  .x = Schedule_Weeks,
  .f = purrr::possibly(
    .f = ~{
      Matchups <- httr::GET(paste0("https://api.sleeper.app/v1/league/", League_Id, "/matchups/", .x))
      
      fromJSON(content(Matchups, "text"), flatten = TRUE) %>%
        mutate(week = .x)
    }
  )
)

Processed_Matchup_Data <- purrr::map(
  .x = 1:length(Matchup_Data),
  .progress = TRUE,
  .f = ~{
    Player_Week_Scoring <- Matchup_Data[[.x]] %>%
      janitor::clean_names() %>%
      pivot_longer(
        cols = starts_with("players_points_"), values_to = "points_scored", names_to = "player_id") %>%
      filter(!is.na(points_scored)) %>%
      mutate(player_id = str_remove(player_id, "players_points_")) %>%
      select(player_id, week, points_scored)
    
    Starting_Player_Ids <- Matchup_Data[[.x]] %>%
      janitor::clean_names() %>%
      unnest(starters) %>%
      pull(starters) %>%
      unique()
    
    Matchup_Data[[.x]] %>%
      janitor::clean_names() %>%
      select(-starts_with("players_points_"), -starts_with("starters")) %>%
      unnest(players) %>%
      rename(player_id = players) %>%
      left_join(
        Player_Week_Scoring, 
        by = c("player_id", "week"), 
        relationship = "one-to-one") %>%
      mutate(started = player_id %in% Starting_Player_Ids)
  }
) %>%
  list_rbind() %>%
  glimpse()

Minimalist_Matchups_Df <- Processed_Matchup_Data %>%
  select(roster_id, matchup_id, week) %>%
  distinct() %>%
  group_by(week, matchup_id) %>%
  mutate(arbitrary_home_away = ifelse(roster_id == min(roster_id), "home", "away")) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(matchup_id, week), 
    names_from = "arbitrary_home_away",
    values_from = "roster_id") %>%
  glimpse()

Syracuse_Data <- paste0("https://api.sleeper.app/v1/league/", League_Id, "/rosters") %>%
  httr::GET() %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE)

Owned_Player_Ids <- Syracuse_Data %>%
  unnest(players) %>%
  pull(players) %>%
  unique()

Starting_Player_Ids <- Syracuse_Data %>%
  unnest(starters) %>%
  pull(starters) %>%
  unique()

Reserve_Player_Ids <- Syracuse_Data %>%
  unnest(reserve) %>%
  pull(reserve) %>%
  unique()

Syracuse_Data %>%
  unnest(players) %>%
  rename(player_id = players) %>%
  # select(owner_id, roster_id, player_id) %>%
  mutate(
    class = case_when(
      player_id %in% Starting_Player_Ids ~ "starting",
      player_id %in% Reserve_Player_Ids ~ "on reserve",
      TRUE ~ "on bench"
      )
    ) %>%
  left_join(
    Master_Table_Df %>%
      select(player_id, first_name, last_name) %>%
      distinct(),
    by = "player_id",
    relationship = "one-to-one") %>%
  mutate(player_name = paste(first_name, last_name)) %>%
  left_join(
    Sleeper_Projections_Data %>%
      filter(week == Week_Of_Interest, season == as.character(lubridate::year(Sys.Date()))) %>%
      select(player_id, projected_pts = stats_pts_half_ppr),
    by = "player_id"
    ) %>%
  group_by(roster_id) %>%
  arrange(desc(projected_pts)) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(player_name = str_replace(player_name, pattern = " ", replacement = "\n")) %>%
  left_join(
    Users_Data %>%
      select(user_id, display_name),
    by = c("owner_id" = "user_id")
  ) %>%
  {print(distinct(select(., display_name, roster_id)));.} %>%
  ggplot(aes(display_name, row)) +
  geom_label(aes(label = player_name, color = class)) +
  scale_color_manual(values = c("starting" = "green4", "on bench" = "blue3", "on reserve" = "red3")) +
  # geom_label(aes(label = player_name, color = is.na(projected_pts))) +
  # scale_x_continuous(breaks = 1:14) +
  scale_y_reverse(breaks = 1:17) +
  labs(
    x = "Sleeper User Name",
    y = paste("Week", Week_Of_Interest, "Projections Rank"),
    title = "Syracuse League Rosters Breakdown"
    ) +
  theme_clean() +
  theme(
    legend.title = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##### My Roster #####

My_Roster <- Syracuse_Data %>%
  filter(owner_id == My_User_Id) %>% 
  unnest(players) %>%
  rename(player_id = players) %>%
  # select(owner_id, roster_id, player_id) %>%
  mutate(
    class = case_when(
      player_id %in% Starting_Player_Ids ~ "starting",
      player_id %in% Reserve_Player_Ids ~ "on reserve",
      TRUE ~ "on bench"),
    season = as.character(lubridate::year(Sys.Date()))
  ) %>%
  left_join(
    Master_Table_Df %>%
      select(player_id, first_name, last_name, position) %>%
      distinct(),
    by = "player_id",
    relationship = "one-to-one") %>%
  mutate(player_name = paste(first_name, last_name)) %>%
  left_join(
    Sleeper_Projections_Data %>%
      select(player_id, week, season, projected_pts = stats_pts_half_ppr),
    by = c("player_id", "season")
  ) %>%
  mutate(projected_pts = replace_na(projected_pts, 0)) %>%
  group_by(roster_id) %>%
  arrange(desc(projected_pts)) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(name_labels = str_replace(player_name, pattern = " ", replacement = "\n")) %>%
  janitor::clean_names() %>%
  `colnames<-`(str_remove(colnames(.), pattern = "^settings_")) %>%
  glimpse()

My_Roster %>%
  filter(projected_pts > 0, position != "DEF") %>%
  ggplot(aes(week, projected_pts, color = player_name, fill = player_name)) +
  geom_vline(xintercept = Week_Of_Interest) +
  geom_line(linewidth = 2) +
  geom_point(aes(shape = position), color = "black", size = 5) +
  scale_shape_manual(values = 21:25) +
  labs(
    x = "Week",
    y = "Projected Points",
    shape = "Position",
    color = "Player Name", fill = "Player Name"
    ) +
  guides(
    color = "none",
    fill = guide_legend(override.aes = list(shape = 21))) +
  theme_clean()

My_Roster %>%
  filter(week == Week_Of_Interest) %>%
  group_by(position, week) %>%
  arrange(desc(projected_pts)) %>%
  mutate(position_rank = row_number()) %>%
  ungroup() %>%
  ggplot(aes(position, position_rank)) +
  # facet_wrap(~ week) +
  geom_label(aes(label = paste0(name_labels, " [", round(projected_pts, 1), "]"))) +
  scale_y_reverse() +
  labs(x = "Position", y = "Position Rank", title = paste("Week", Week_Of_Interest, "Position Ranks")) +
  theme_clean()

#### Simulating My Sleeper Season ####

##### Preparing Materials #####

Top_Free_Agent_Players <- Sleeper_Projections_Data %>%
  filter(
    !player_id %in% Owned_Player_Ids, 
    week >= Week_Of_Interest, 
    season == as.character(lubridate::year(Sys.Date())),
    !is.na(stats_pts_half_ppr)) %>%
  left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
  filter(position %in% Positions_I_Care_About) %>%
  group_by(position, week) %>%
  arrange(desc(stats_pts_half_ppr)) %>%
  mutate(free_agent_position_rank = row_number()) %>%
  ungroup() %>%
  filter(
    (position %in% c("K", "DEF") & free_agent_position_rank == 1) |
      (position == "QB" & free_agent_position_rank <= 2) |
      (position == "TE" & free_agent_position_rank <= 3) |
      (position %in% c("RB", "WR") & free_agent_position_rank <= 4)
    ) %>%
  select(week, player_id)

Empty_Slot_Options <- data.frame(
  position = unname(
    unlist(mapply(rep, names(Syracuse_Roster_Settings), Syracuse_Roster_Settings)))
  ) %>%
  data.frame() %>%
  group_by(position) %>%
  mutate(player_id = paste0("empty_", position, row_number())) %>%
  ungroup() %>%
  expand_grid(
    roster_id = -1, 
    projected_pts = 0, 
    full_name = "empty",
    week = Week_Of_Interest:17
  )

Locked_In_Starters <- Syracuse_Data %>%
  unnest(starters) %>%
  pull(starters) %>%
  .[. %in% Locked_Weekly_Scoring$player_id]

Pivoted_Player_Eligibilities <- Syracuse_Data %>%
  unnest(players) %>%
  rename(player_id = players) %>%
  bind_rows(data.frame(player_id = unique(Top_Free_Agent_Players$player_id))) %>%
  mutate(season = as.character(lubridate::year(Sys.Date()))) %>%
  left_join(
    Master_Table_Df %>%
      select(player_id, first_name, last_name, position) %>%
      distinct(),
    by = "player_id",
    relationship = "one-to-one") %>%
  left_join(
    Sleeper_Projections_Data %>%
      filter(week >= Week_Of_Interest) %>%
      select(player_id, week, season, projected_pts = stats_pts_half_ppr, team),
    by = c("player_id", "season")
  ) %>%
  inner_join(
    Pivoted_Venue_Data %>%
      mutate(season = as.character(season)) %>%
      select(season, week, team),
    by = c("season", "week", "team"),
    relationship = "many-to-one"
    ) %>%
  mutate(projected_pts = replace_na(projected_pts, 0)) %>%
  janitor::clean_names() %>%
  `colnames<-`(str_remove(colnames(.), pattern = "^settings_")) %>%
  select(-c(starters, reserve)) %>%
  left_join(
    Master_Table_Df[,c("player_id", "fantasy_positions")],
    by = "player_id",
    relationship = "many-to-many") %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE"),
    def = str_detect(fantasy_positions, "DEF"),
    k = str_detect(fantasy_positions, "K"),
    flex = rb | wr | te,
    op = qb | rb | wr | te
  ) %>%
  select(season, player_id, week, roster_id, projected_pts, qb:op) %>%
  pivot_longer(qb:op, names_to = "position", values_to = "eligibility") %>%
  filter(eligibility) %>%
  mutate(position = toupper(position)) %>%
  select(season, player_id, week, roster_id, position, projected_pts) %>%
  distinct() %>%
  left_join(
    distinct(Master_Table_Df[,c("player_id", "full_name")]), 
    by = "player_id") %>%
  bind_rows(Empty_Slot_Options) %>%
  rename(half_ppr_projection = projected_pts, slot = position) %>%
  left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
  mutate(adjusted_projections = predict(Projection_Recalibration_Lm, ., type = "response")) %>%
  mutate(
    expected_error = predict(Post_Recal_Error_Lm, ., type = "response"),
    across(
      .cols = c(adjusted_projections, expected_error), 
      .fns = ~ifelse(str_detect(player_id, "^empty"), 0, .x)),
    # forcing bs score to get in lineup, will be overwritten
    adjusted_projections = case_when(
      week == Week_Of_Interest & player_id %in% Locked_In_Starters ~ 100,
      week == Week_Of_Interest & player_id %in% Locked_Weekly_Scoring$player_id ~ 0,
      TRUE ~ adjusted_projections),
    expected_error = ifelse(
      player_id %in% Locked_Weekly_Scoring$player_id, 0, expected_error)
    ) %>%
  select(
    season, player_id, week, roster_id, position = slot, 
    projected_pts = adjusted_projections, expected_error) %>%
  left_join(Locked_Weekly_Scoring, by = c("season", "week", "player_id")) %>%
  glimpse()

Pivoted_Player_Eligibilities %>%
  filter(!is.na(forced_real_score)) %>%
  glimpse() %>%
  pull(week) %>% 
  table()

##### Optimizing Lineups #####

Optimized_Lineup_Expectations <- purrr::map(
  .x = Week_Of_Interest:17,
  .progress = TRUE,
  .f = function(Sim_Week_Of_Interest){
    Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
      filter(week == Sim_Week_Of_Interest) %>%
      pull(player_id)
      
    Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
      filter(week == Sim_Week_Of_Interest)
    
    purrr::map(
      .x = unique(Syracuse_Data$roster_id),
      .f = ~{
        Available_Player_Ids <- Syracuse_Data %>%
          filter(roster_id == .x) %>%
          unnest(players) %>%
          pull(players) %>%
          append(Top_Free_Agent_Player_Ids) %>%
          append(unique(Empty_Slot_Options$player_id))
        
        Optimized_Lineup <- Get_Optimal_Lineup(
          Input_Pivoted_Player_Eligibilities = Pivoted_Player_Eligibilities,
          Input_Roster_Constraints = Syracuse_Roster_Settings,
          Possible_Lineup_Players = Available_Player_Ids
          )
        
        if(Sim_Week_Of_Interest == Week_Of_Interest){
          Team_Mean <- Optimized_Lineup %>%
            mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
            pull(live_projection) %>%
            sum()
        } else {
          Team_Mean <- sum(Optimized_Lineup$projected_pts)
        }
        
        Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
        
        data.frame(
          week = Sim_Week_Of_Interest,
          roster_id = .x,
          team_mean = Team_Mean,
          team_sd = Team_Sd
          )
        }
      ) %>%
      list_rbind()
    
  }
) %>%
  list_rbind() %>%
  left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = "roster_id") %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = c("owner_id" = "user_id"))

Optimized_Lineup_Expectations %>%
  ggplot(aes(week, team_mean, color = as.factor(roster_id))) +
  geom_line() +
  geom_label(aes(label = display_name)) +
  guides(color = "none") +
  labs(
    x = "Week",
    y = "Mean Projection",
    title = "Team Projected Points By Week"
    ) +
  theme_clean()

Ros_Weekly_Win_Probabilities <- Minimalist_Matchups_Df %>%
  filter(week >= Week_Of_Interest) %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("away" = "roster_id", "week"),
    relationship = "one-to-one") %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("home" = "roster_id", "week"),
    relationship = "one-to-one",
    suffix = c("_away", "_home")) %>%
  mutate(
    mean_diff = team_mean_away - team_mean_home,
    std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
    z_score = mean_diff / std_diff,
    away_win_prob = pnorm(z_score)
    ) %>%
  glimpse()

Pivoted_Playoff_Week_Projections <- Optimized_Lineup_Expectations %>%
  filter(week %in% 15:17) %>%
  pivot_wider(
    id_cols = c(roster_id, owner_id, display_name), 
    names_from = "week", 
    values_from = c(team_mean, team_sd)) %>%
  glimpse()

Ros_Weekly_Win_Probabilities %>%
  pivot_longer(starts_with("display_name"), values_to = "display_name") %>%
  select(display_name, week, away_win_prob, name) %>%
  mutate(
    name = str_remove(name, "^display_name_"),
    win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
    ) %>%
  ggplot(aes(week, win_prob, color = display_name)) + 
  geom_line() +
  geom_point() +
  geom_point(
    data = . %>% filter(display_name == "TaysomsLeftNut"), 
    shape = 1, color = "red", size = 5, stroke = 2) +
  geom_text(aes(label = display_name)) +
  geom_vline(xintercept = 14.5) +
  labs(
    x = "Week",
    y = "Projected Win Probability",
    color = "Team",
    title = "Team Projected Win Probability By Week"
  ) +
  theme_clean()

Ros_Weekly_Win_Probabilities %>%
  pivot_longer(starts_with("display_name"), values_to = "display_name") %>%
  select(display_name, week, away_win_prob, name) %>%
  mutate(
    name = str_remove(name, "^display_name_"),
    win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
  ) %>%
  group_by(display_name) %>%
  summarise(x_wins = sum(win_prob), x_losses = sum(1 - win_prob)) %>%
  ungroup() %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = "display_name")  %>%
  left_join(Syracuse_Data, by = c("user_id" = "owner_id")) %>%
  janitor::clean_names() %>%
  mutate(
    x_eos_wins = x_wins + settings_wins,
    x_eos_losses = x_losses + settings_losses
    ) %>%
  glimpse()

Temp_Sims <- purrr::map(
  .x = 1:1000,
  .progress = TRUE,
  .f = function(Sim_Run_Number){
    Regular_Season_Results <- Ros_Weekly_Win_Probabilities %>%
      mutate(
        sim_pts_home = team_mean_home + team_sd_home * rnorm(n = nrow(.)),
        sim_pts_away = team_mean_away + team_sd_away * rnorm(n = nrow(.)),
        across(starts_with("sim_pts_"), ~ .x, .names = "dup_{.col}"),
        win_home = sim_pts_home > sim_pts_away,
        win_away = !win_home
      ) %>%
      pivot_longer(starts_with("owner_id"), names_to = "owner_id_name", values_to = "owner_id") %>%
      pivot_longer(starts_with("display_name"), names_to = "home_away", values_to = "display_name") %>%
      pivot_longer(starts_with("sim_pts_"), names_to = "sim_pts_name", values_to = "sim_pts") %>%
      pivot_longer(starts_with("win_"), names_to = "win_name", values_to = "wins") %>%
      mutate(home_away = str_extract(home_away, ".{4}$")) %>%
      filter(
        str_extract(win_name, ".{4}$") == home_away,
        str_extract(sim_pts_name, ".{4}$") == home_away,
        str_extract(owner_id_name, ".{4}$") == home_away
      ) %>%
      mutate(sim_pts_against = ifelse(home_away == "home", dup_sim_pts_away, dup_sim_pts_home)) %>%
      group_by(display_name, owner_id) %>%
      summarise(across(c(sim_pts, sim_pts_against, wins), sum), .groups = "drop_last") %>%
      ungroup() %>%
      left_join(Syracuse_Data %>% select(owner_id, starts_with("settings")), by = "owner_id") %>%
      janitor::clean_names() %>%
      mutate(
        sim_wins = wins + settings_wins,
        sim_pts = sim_pts + settings_fpts + settings_fpts_decimal / 100,
        sim_pts_against = sim_pts_against + settings_fpts_against + settings_fpts_against_decimal / 100
      ) %>%
      select(display_name, owner_id, starts_with("sim")) %>%
      arrange(desc(sim_wins), desc(sim_pts), sim_pts_against) %>%
      mutate(seed = row_number(), sim_num = Sim_Run_Number) 
    
    Postseason_Result_Probabilities <- Regular_Season_Results %>%
      filter(seed <= 6) %>%
      left_join(
        Pivoted_Playoff_Week_Projections, 
        by = c("display_name", "owner_id"), 
        relationship = "one-to-one") %>%
      mutate(seed = paste0("seed_", seed)) %>%
      pivot_wider(id_cols = sim_num, names_from = seed, values_from = matches("^team_.*_1[567]")) %>%
      mutate(
        # round one match up win probabilities
        round_1_seed_4_win_prob = pnorm(
          (team_mean_15_seed_4 - team_mean_15_seed_5) / sqrt(team_sd_15_seed_4^2 + team_sd_15_seed_5^2)),
        round_1_seed_5_win_prob = 1 - round_1_seed_4_win_prob,
        round_1_seed_3_win_prob = pnorm(
          (team_mean_15_seed_3 - team_mean_15_seed_6) / sqrt(team_sd_15_seed_3^2 + team_sd_15_seed_6^2)),
        round_1_seed_6_win_prob = 1 - round_1_seed_3_win_prob,
        # round two match up win probabilities
        round_2_seed_1_win_prob_vs_seed_4 = pnorm(
          (team_mean_16_seed_1 - team_mean_16_seed_4) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_4^2)),
        round_2_seed_1_win_prob_vs_seed_5 = pnorm(
          (team_mean_16_seed_1 - team_mean_16_seed_5) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_5^2)),
        round_2_seed_2_win_prob_vs_seed_3 = pnorm(
          (team_mean_16_seed_2 - team_mean_16_seed_3) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_3^2)),
        round_2_seed_2_win_prob_vs_seed_6 = pnorm(
          (team_mean_16_seed_2 - team_mean_16_seed_6) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_6^2)),
        # championship match up win probabilities
        round_3_seed_1_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_2^2)),
        round_3_seed_1_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_3^2)),
        round_3_seed_1_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_6^2)),
        round_3_seed_4_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_2^2)),
        round_3_seed_4_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_3^2)),
        round_3_seed_4_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_6^2)),
        round_3_seed_5_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_2^2)),
        round_3_seed_5_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_3^2)),
        round_3_seed_5_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_6^2)),
        # odds of reaching championship
        seed_1_champ_berth_prob = round_1_seed_4_win_prob * round_2_seed_1_win_prob_vs_seed_4 +
          round_1_seed_5_win_prob * round_2_seed_1_win_prob_vs_seed_5,
        seed_4_champ_berth_prob = round_1_seed_4_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_4),
        seed_5_champ_berth_prob = round_1_seed_5_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_5),
        seed_2_champ_berth_prob = round_1_seed_3_win_prob * round_2_seed_2_win_prob_vs_seed_3 +
          round_1_seed_6_win_prob * round_2_seed_2_win_prob_vs_seed_6,
        seed_3_champ_berth_prob = round_1_seed_3_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_3),
        seed_6_champ_berth_prob = round_1_seed_6_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_6),
        # odds of winning the championship
        seed_1_champ_win_prob = seed_1_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_2 +
          seed_3_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_3 +
          seed_6_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_6),
        seed_2_champ_win_prob = seed_2_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_2) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_2) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_2)),
        seed_3_champ_win_prob = seed_3_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_3) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_3) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_3)),
        seed_4_champ_win_prob = seed_4_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_2 +
            seed_3_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_3 +
            seed_6_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_6),
        seed_5_champ_win_prob = seed_5_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_2 +
            seed_3_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_3 +
            seed_6_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_6),
        seed_6_champ_win_prob = seed_6_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_6) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_6) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_6)),
        # sanity checks
        berth_sum_145 = seed_1_champ_berth_prob + seed_4_champ_berth_prob + seed_5_champ_berth_prob,
        berth_sum_236 = seed_2_champ_berth_prob + seed_3_champ_berth_prob + seed_6_champ_berth_prob,
        champ_win_prob_sum = seed_1_champ_win_prob + seed_2_champ_win_prob + seed_3_champ_win_prob +
          seed_4_champ_win_prob + seed_5_champ_win_prob + seed_6_champ_win_prob
        )
    
    Regular_Season_Results %>%
      left_join(
        Postseason_Result_Probabilities %>%
          select(sim_num, ends_with("_champ_win_prob")) %>%
          pivot_longer(-sim_num, names_to = "seed", values_to = "x_championships") %>%
          mutate(seed = as.integer(str_remove(str_remove(seed, "_champ_win_prob$"), "^seed_"))),
        by = c("sim_num", "seed"),
        relationship = "one-to-one") %>%
      mutate(x_championships = tidyr::replace_na(x_championships, replace = 0))
    }
  ) %>%
  list_rbind()

Temp_Sims %>%
  ggplot(aes(seed, fill = display_name)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(
    x = "Simulation Seed Result",
    y = "Count",
    fill = "Team",
    title = "Simulated Seed Distribution"
    ) +
  theme_clean()

Temp_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    .groups = "drop_last"
    ) %>%
  ungroup() %>%
  pivot_longer(ends_with("_pct")) %>%
  group_by(name) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  glimpse() %>%
  ggplot(aes(rank, value, fill = display_name)) +
  facet_wrap(~ str_remove(name, "_pct$")) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(100 * round(value, 3), "%")),
    angle = 90, hjust = 0, nudge_y = 0.01
    ) +
  labs(
    x = "Rank",
    y = "Probability Via Simulation",
    title = "Simulation Playoff Probailities",
    fill = "Team"
  ) +
  theme_clean() +
  clean_enlarge_text()

Sleeper_Projections_Data %>%
  rename(half_ppr_projection = stats_pts_half_ppr) %>%
  left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  mutate(adjusted_projections = predict(Projection_Recalibration_Lm, ., type = "response")) %>%
  mutate(expected_error = predict(Post_Recal_Error_Lm, ., type = "response")) %>%
  filter(week >= Week_Of_Interest, season == as.character(lubridate::year(Sys.Date())), !is.na(adjusted_projections)) %>%
  group_by(season, week, position) %>%
  mutate(
    best_free_agent_adjusted_projection = max(c(adjusted_projections[!player_id %in% Owned_Player_Ids], 0), na.rm = TRUE)
    ) %>%
  ungroup() %>%
  mutate(
    free_agent_surplus = adjusted_projections - best_free_agent_adjusted_projection,
    player_name = paste(player_first_name, player_last_name)
    ) %>%
  filter(player_id %in% Owned_Player_Ids) %>%
  group_by(season, player_id, position, player_name) %>%
  summarise(free_agent_surplus = sum(pmax(free_agent_surplus, 0)), .groups = "drop_last") %>%
  ungroup() %>%
  filter(position != "QB") %>%
  arrange(desc(free_agent_surplus)) %>%
  glimpse()

#### Values Of Subtraction ####

Subtractive_Optimized_Lineup_Expectations <- purrr::map(
  .x = unique(Syracuse_Data$roster_id),
  .progress = TRUE,
  .f = function(Roster_Of_Interest){
    Rostered_Player_Ids <- Syracuse_Data %>%
      filter(roster_id == Roster_Of_Interest) %>%
      unnest(players) %>%
      pull(players)
    
    purrr::map(
      .x = Week_Of_Interest:17,
      .f = function(Week_Of_Interest){
        Temp_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
          filter(week == Week_Of_Interest)
        
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Week_Of_Interest) %>%
          pull(player_id)
        
        purrr::map(
          .x = Rostered_Player_Ids,
          .f = function(Hold_Out_Player_Id){
            Available_Player_Ids <- Rostered_Player_Ids %>%
              .[. != Hold_Out_Player_Id] %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Temp_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Syracuse_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            Team_Mean <- sum(Optimized_Lineup$projected_pts)
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Week_Of_Interest,
              roster_id = Roster_Of_Interest,
              hold_out_player_id = Hold_Out_Player_Id,
              team_mean = Team_Mean,
              team_sd = Team_Sd
              )
            }
          ) %>%
          list_rbind()
      }
    ) %>%
      list_rbind()
  }
) %>%
  list_rbind() %>%
  left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = "roster_id") %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = c("owner_id" = "user_id"))

Subtractive_Player_Values <- Subtractive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id", "owner_id", "display_name"),
    relationship = "many-to-one",
    suffix = c("_subtractive", "_baseline")) %>%
  mutate(pts_delta = team_mean_subtractive - team_mean_baseline) %>%
  group_by(hold_out_player_id, roster_id, display_name, owner_id) %>%
  summarise(pts_delta = sum(pts_delta), .groups = "drop_last") %>%
  ungroup() %>%
  mutate(value_via_subtraction = -pts_delta) %>%
  arrange((value_via_subtraction)) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_id, player_name, position) %>%
      distinct(),
    by = c("hold_out_player_id" = "player_id"),
    relationship = "one-to-one") %>%
  glimpse()

Subtractive_Player_Values %>%
  ggplot(aes(value_via_subtraction, fill = position)) +
  geom_histogram(binwidth = 1) +
  theme_clean()

Subtractive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id", "owner_id", "display_name"),
    relationship = "many-to-one",
    suffix = c("_subtractive", "_baseline")) %>%
  mutate(pts_delta = team_mean_baseline - team_mean_subtractive) %>%
  filter(owner_id == "1000428933445140480", hold_out_player_id == "4950") %>%
  glimpse() %>%
  ggplot(aes(week)) +
  geom_line(aes(y = team_mean_subtractive), color = "red") +
  geom_line(aes(y = team_mean_baseline), color = "black") +
  labs(y = "Team Mean Points") +
  theme_clean()

#### Values of Addition ####

Additive_Player_Ids_To_Investigate <- Subtractive_Player_Values %>%
  filter(value_via_subtraction > 0, !position %in% c("K", "DEF")) %>%
  pull(hold_out_player_id) %>%
  unique()

Additive_Optimized_Lineup_Expectations <- purrr::map(
  .x = unique(Syracuse_Data$roster_id),
  .progress = TRUE,
  .f = function(Roster_Of_Interest){
    Rostered_Player_Ids <- Syracuse_Data %>%
      filter(roster_id == Roster_Of_Interest) %>%
      unnest(players) %>%
      pull(players)
    
    Player_Ids_To_Loop_Through <- Additive_Player_Ids_To_Investigate %>%
      .[!. %in% Rostered_Player_Ids]
    
    purrr::map(
      .x = Week_Of_Interest:17,
      .f = function(Week_Of_Interest){
        Temp_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
          filter(week == Week_Of_Interest)
        
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Week_Of_Interest) %>%
          pull(player_id)
        
        purrr::map(
          .x = Player_Ids_To_Loop_Through,
          .f = function(Adding_Player_Id){
            Available_Player_Ids <- Rostered_Player_Ids %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(Adding_Player_Id) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Temp_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Syracuse_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            Team_Mean <- sum(Optimized_Lineup$projected_pts)
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Week_Of_Interest,
              roster_id = Roster_Of_Interest,
              adding_player_id = Adding_Player_Id,
              team_mean = Team_Mean,
              team_sd = Team_Sd
            )
          }
        ) %>%
          list_rbind()
      }
    ) %>%
      list_rbind()
  }
) %>%
  list_rbind() %>%
  left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = "roster_id") %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = c("owner_id" = "user_id"))

Additive_Player_Values <- Additive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id", "owner_id", "display_name"),
    relationship = "many-to-one",
    suffix = c("_additive", "_baseline")) %>%
  mutate(pts_delta = team_mean_additive - team_mean_baseline) %>%
  group_by(adding_player_id, roster_id, owner_id, display_name) %>%
  summarise(pts_delta = sum(pts_delta), .groups = "drop_last") %>%
  ungroup() %>%
  arrange(desc(pts_delta)) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_id, player_name, position) %>%
      distinct(),
    by = c("adding_player_id" = "player_id"),
    relationship = "many-to-one") %>%
  glimpse()

#### Addition-Subtraction Mismatches ####

Add_Sub_Players_Df <- Subtractive_Player_Values %>%
  inner_join(
    Additive_Player_Values,
    by = c("hold_out_player_id" = "adding_player_id", "position", "player_name"),
    suffix = c("_subtractive", "_additive")
    ) %>%
  rename(player_id = hold_out_player_id) %>%
  mutate(
    value_via_addition = pts_delta_additive,
    across(starts_with("value"), ~ifelse(.x < 0, 0, .x))) 

Add_Sub_Players_Df %>%
  ggplot(aes(value_via_subtraction, value_via_addition)) +
  facet_wrap(~ display_name_additive) +
  geom_point(aes(color = position)) +
  geom_abline() +
  labs(
    x = "Value According To Subtraction",
    y = "Value According To Addition",
    caption = paste("points above the diagonal would be valued more highly on",
                    "the strip titled team than their current"),
    title = "Untapped Surplus Value Plot"
    ) +
  theme_clean() +
  theme(legend.background = element_rect(color = "black"))

# adds that make the most sense for me
Add_Sub_Players_Df %>%
  filter(roster_id_additive == 8) %>%
  arrange(desc(value_via_addition - value_via_subtraction)) %>%
  head() %>%
  select(player_id, contains("display_name"), player_name, position, starts_with("value"))

# subtractions that make the most sense for me
Add_Sub_Players_Df %>%
  filter(roster_id_subtractive == 8) %>%
  arrange(desc(value_via_addition - value_via_subtraction)) %>%
  head() %>%
  select(player_id, contains("display_name"), player_name, position, starts_with("value"))

Untapped_Surplus_One_For_Ones <- Add_Sub_Players_Df %>%
  mutate(untapped_surplus = value_via_addition - value_via_subtraction) %>%
  filter(
    (display_name_additive == My_Display_Name | display_name_subtractive == My_Display_Name),
    untapped_surplus > 0) %>%
  group_by(roster_id_subtractive, roster_id_additive) %>%
  filter(untapped_surplus == max(untapped_surplus)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(player_name, display_name_subtractive, display_name_additive, 
         roster_id_subtractive, roster_id_additive, untapped_surplus) %>%
  self_join(
    by = c("display_name_subtractive" = "display_name_additive", 
           "display_name_additive" = "display_name_subtractive"),
    suffix = c("_subtractive", "_additive")
    ) %>%
  filter(display_name_additive < display_name_subtractive) %>%
  mutate(
    mutual_surplus_unlocked = untapped_surplus_subtractive + untapped_surplus_additive
    ) %>%
  arrange(desc(mutual_surplus_unlocked)) %>%
  glimpse()

#### Counter-factual Scoring Strengths and Regular Season Wins ####

Possible_141_Trades <- Syracuse_Data %>%
  unnest(players) %>%
  select(player_id = players, roster_id) %>%
  mutate(universal_join_key = 1) %>%
  self_join(by = "universal_join_key") %>%
  filter(roster_id_1 < roster_id_2, (roster_id_1 == 8 | roster_id_2 == 8)) %>%
  glimpse()

Prior_Regular_Szn_Win_Expectations <- Ros_Weekly_Win_Probabilities %>%
  pivot_longer(starts_with("display_name"), values_to = "display_name") %>%
  select(display_name, week, away_win_prob, name) %>%
  mutate(
    name = str_remove(name, "^display_name_"),
    win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
  ) %>%
  filter(week < 15) %>%
  group_by(display_name) %>%
  summarise(x_reg_szn_wins = sum(win_prob)) %>%
  ungroup() %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = "display_name") %>%
  left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = c("user_id" = "owner_id")) %>%
  glimpse()

Prior_Rest_Of_Szn_Scoring_Expectations <- Optimized_Lineup_Expectations %>%
  filter(week >= Week_Of_Interest) %>%
  group_by(roster_id) %>%
  summarise(ros_x_pts = sum(team_mean)) %>%
  ungroup() %>%
  glimpse()

Possible_141_Results_Collection <- pmap(
  .l = Possible_141_Trades %>%
    select(player_id_1, roster_id_1, player_id_2, roster_id_2) %>%
    as.list(),
  .progress = TRUE,
  .f = function(player_id_1, roster_id_1, player_id_2, roster_id_2){
    Cf_Roster_Statuses <- data.frame(
      player_id = c(player_id_1, player_id_2),
      roster_id = c(roster_id_2, roster_id_1)
      )
    
    Cf_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
      left_join(Cf_Roster_Statuses, by = "player_id", suffix = c("", "_cf")) %>%
      mutate(roster_id = coalesce(roster_id_cf, roster_id), .keep = "unused")
    
    Cf_Optimized_Lineup_Expectations <- purrr::map(
      .x = Week_Of_Interest:17,
      .progress = TRUE,
      .f = function(Week_Of_Interest){
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Week_Of_Interest) %>%
          pull(player_id)
        
        Cf_Pivoted_Player_Eligibilities <- Cf_Pivoted_Player_Eligibilities %>%
          filter(week == Week_Of_Interest)
        
        purrr::map(
          .x = unique(Syracuse_Data$roster_id),
          .f = ~{
            Available_Player_Ids <- Cf_Pivoted_Player_Eligibilities %>%
              filter(roster_id == .x) %>%
              unnest(player_id) %>%
              pull(player_id) %>%
              unique() %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Cf_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Syracuse_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            Team_Mean <- sum(Optimized_Lineup$projected_pts)
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Week_Of_Interest,
              roster_id = .x,
              team_mean = Team_Mean,
              team_sd = Team_Sd
            )
          }
        ) %>%
          list_rbind()
        
      }
    ) %>%
      list_rbind() %>%
      left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = "roster_id") %>%
      left_join(Users_Data[,c("user_id", "display_name")], by = c("owner_id" = "user_id"))
    
    Rest_Of_Szn_Scoring_Expectations <- Cf_Optimized_Lineup_Expectations %>%
      filter(roster_id == roster_id_1 | roster_id == roster_id_2) %>%
      group_by(roster_id) %>%
      summarise(ros_x_pts = sum(team_mean)) %>%
      ungroup() %>%
      mutate(dummy_roster_id = ifelse(roster_id == roster_id_1, "1", "2")) %>%
      select(dummy_roster_id, ros_x_pts) %>%
      pivot_wider(names_prefix = "ros_x_pts_", names_from = dummy_roster_id, values_from = ros_x_pts)
    
    Regular_Szn_Win_Expectations <- Minimalist_Matchups_Df %>%
      filter(week >= Week_Of_Interest) %>%
      left_join(
        Cf_Optimized_Lineup_Expectations, 
        by = c("away" = "roster_id", "week"),
        relationship = "one-to-one") %>%
      left_join(
        Cf_Optimized_Lineup_Expectations, 
        by = c("home" = "roster_id", "week"),
        relationship = "one-to-one",
        suffix = c("_away", "_home")) %>%
      mutate(
        mean_diff = team_mean_away - team_mean_home,
        std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
        z_score = mean_diff / std_diff,
        away_win_prob = pnorm(z_score)
      ) %>%
      pivot_longer(starts_with("display_name"), values_to = "display_name") %>%
      select(display_name, week, away_win_prob, name) %>%
      mutate(
        name = str_remove(name, "^display_name_"),
        win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
      ) %>%
      filter(week < 15) %>%
      group_by(display_name) %>%
      summarise(x_reg_szn_wins = sum(win_prob)) %>%
      ungroup() %>%
      left_join(Users_Data[,c("user_id", "display_name")], by = "display_name") %>%
      left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = c("user_id" = "owner_id")) %>%
      filter(roster_id == roster_id_1 | roster_id == roster_id_2) %>%
      mutate(dummy_roster_id = ifelse(roster_id == roster_id_1, "1", "2")) %>%
      select(dummy_roster_id, x_reg_szn_wins) %>%
      pivot_wider(
        names_prefix = "x_reg_szn_wins_", 
        names_from = dummy_roster_id, 
        values_from = x_reg_szn_wins)
    
    data.frame(player_id_1, roster_id_1, player_id_2, roster_id_2) %>%
      cbind(Regular_Szn_Win_Expectations) %>%
      cbind(Rest_Of_Szn_Scoring_Expectations)
  }
) %>%
  list_rbind()

Possible_141_Results_Collection %>%
  left_join(
    Prior_Regular_Szn_Win_Expectations %>%
      select(roster_id, prior_x_reg_szn_wins_1 = x_reg_szn_wins),
    by = c("roster_id_1" = "roster_id")
  ) %>%
  left_join(
    Prior_Regular_Szn_Win_Expectations %>%
      select(roster_id, prior_x_reg_szn_wins_2 = x_reg_szn_wins),
    by = c("roster_id_2" = "roster_id")
  ) %>%
  left_join(
    Prior_Rest_Of_Szn_Scoring_Expectations %>%
      rename(prior_ros_x_pts_1 = ros_x_pts),
    by = c("roster_id_1" = "roster_id")
  ) %>%
  left_join(
    Prior_Rest_Of_Szn_Scoring_Expectations %>%
      rename(prior_ros_x_pts_2 = ros_x_pts),
    by = c("roster_id_2" = "roster_id")
  ) %>%
  mutate(
    reg_szn_wins_delta_1 = x_reg_szn_wins_1 - prior_x_reg_szn_wins_1,
    reg_szn_wins_delta_2 = x_reg_szn_wins_2 - prior_x_reg_szn_wins_2,
    ros_pts_delta_1 = ros_x_pts_1 - prior_ros_x_pts_1,
    ros_pts_delta_2 = ros_x_pts_2 - prior_ros_x_pts_2
    ) %>%
  filter(reg_szn_wins_delta_1 > 0, reg_szn_wins_delta_2 > 0) %>%
  # arrange(desc(reg_szn_wins_delta_1 + reg_szn_wins_delta_2)) %>%
  filter(ros_pts_delta_1 > 0, ros_pts_delta_2 > 0) %>%
  arrange(desc(ros_pts_delta_1 + ros_pts_delta_2)) %>%
  glimpse()

Master_Table_Df %>%
  filter(player_id == "6011") %>%
  glimpse() %>%
  pull(full_name)

#### Counter-factual Simulations ####

Master_Table_Df %>%
  filter(
    first_name == "Diontae"#,
    # last_name == "Olave"
    ) %>%
  glimpse() %>%
  pull(player_id)

Cf_Roster_Statuses <- data.frame(
  player_name = c("Diontae", "8144"),
  player_id = c("6011","7525"),
  roster_id = c(8, 13)
  # player_id = c("11566", "6770"),
  # player_name = c("Daniels", "Burrow"),
  # roster_id = c(8, 9)
  )

Cf_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
  left_join(Cf_Roster_Statuses, by = "player_id", suffix = c("", "_cf")) %>%
  mutate(roster_id = coalesce(roster_id_cf, roster_id), .keep = "unused") %>%
  glimpse()

Cf_Pivoted_Player_Eligibilities %>%
  filter(player_id %in% Cf_Roster_Statuses$player_id) %>%
  select(player_id, roster_id, player_name) %>%
  distinct()

Cf_Optimized_Lineup_Expectations <- purrr::map(
  .x = Week_Of_Interest:17,
  .progress = TRUE,
  .f = function(Sim_Week_Of_Interest){
    Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
      filter(week == Sim_Week_Of_Interest) %>%
      pull(player_id)
    
    Cf_Pivoted_Player_Eligibilities <- Cf_Pivoted_Player_Eligibilities %>%
      filter(week == Sim_Week_Of_Interest)
    
    purrr::map(
      .x = unique(Syracuse_Data$roster_id),
      .f = ~{
        Available_Player_Ids <- Cf_Pivoted_Player_Eligibilities %>%
          filter(roster_id == .x) %>%
          unnest(player_id) %>%
          pull(player_id) %>%
          unique() %>%
          append(Top_Free_Agent_Player_Ids) %>%
          append(unique(Empty_Slot_Options$player_id))
        
        Optimized_Lineup <- Get_Optimal_Lineup(
          Input_Pivoted_Player_Eligibilities = Cf_Pivoted_Player_Eligibilities,
          Input_Roster_Constraints = Syracuse_Roster_Settings,
          Possible_Lineup_Players = Available_Player_Ids
          )
        
        if(Sim_Week_Of_Interest == Week_Of_Interest){
          Team_Mean <- Optimized_Lineup %>%
            mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
            pull(live_projection) %>%
            sum()
        } else {
          Team_Mean <- sum(Optimized_Lineup$projected_pts)
        }
        
        Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
        
        data.frame(
          week = Sim_Week_Of_Interest,
          roster_id = .x,
          team_mean = Team_Mean,
          team_sd = Team_Sd
        )
      }
    ) %>%
      list_rbind()
    
  }
) %>%
  list_rbind() %>%
  left_join(Syracuse_Data[,c("roster_id", "owner_id")], by = "roster_id") %>%
  left_join(Users_Data[,c("user_id", "display_name")], by = c("owner_id" = "user_id"))

Cf_Ros_Weekly_Win_Probabilities <- Minimalist_Matchups_Df %>%
  filter(week >= Week_Of_Interest) %>%
  left_join(
    Cf_Optimized_Lineup_Expectations, 
    by = c("away" = "roster_id", "week"),
    relationship = "one-to-one") %>%
  left_join(
    Cf_Optimized_Lineup_Expectations, 
    by = c("home" = "roster_id", "week"),
    relationship = "one-to-one",
    suffix = c("_away", "_home")) %>%
  mutate(
    mean_diff = team_mean_away - team_mean_home,
    std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
    z_score = mean_diff / std_diff,
    away_win_prob = pnorm(z_score)
  ) %>%
  glimpse()

Cf_Pivoted_Playoff_Week_Projections <- Cf_Optimized_Lineup_Expectations %>%
  filter(week %in% 15:17) %>%
  pivot_wider(
    id_cols = c(roster_id, owner_id, display_name), 
    names_from = "week", 
    values_from = c(team_mean, team_sd)
    ) %>%
  glimpse()

Cf_Sims <- purrr::map(
  .x = 1:100,
  .progress = TRUE,
  .f = function(Sim_Run_Number){
    Regular_Season_Results <- Cf_Ros_Weekly_Win_Probabilities %>%
      mutate(
        sim_pts_home = team_mean_home + team_sd_home * rnorm(n = nrow(.)),
        sim_pts_away = team_mean_away + team_sd_away * rnorm(n = nrow(.)),
        across(starts_with("sim_pts_"), ~ .x, .names = "dup_{.col}"),
        win_home = sim_pts_home > sim_pts_away,
        win_away = !win_home
      ) %>%
      pivot_longer(starts_with("owner_id"), names_to = "owner_id_name", values_to = "owner_id") %>%
      pivot_longer(starts_with("display_name"), names_to = "home_away", values_to = "display_name") %>%
      pivot_longer(starts_with("sim_pts_"), names_to = "sim_pts_name", values_to = "sim_pts") %>%
      pivot_longer(starts_with("win_"), names_to = "win_name", values_to = "wins") %>%
      mutate(home_away = str_extract(home_away, ".{4}$")) %>%
      filter(
        str_extract(win_name, ".{4}$") == home_away,
        str_extract(sim_pts_name, ".{4}$") == home_away,
        str_extract(owner_id_name, ".{4}$") == home_away
      ) %>%
      mutate(sim_pts_against = ifelse(home_away == "home", dup_sim_pts_away, dup_sim_pts_home)) %>%
      group_by(display_name, owner_id) %>%
      summarise(across(c(sim_pts, sim_pts_against, wins), sum), .groups = "drop_last") %>%
      ungroup() %>%
      left_join(Syracuse_Data %>% select(owner_id, starts_with("settings")), by = "owner_id") %>%
      janitor::clean_names() %>%
      mutate(
        sim_wins = wins + settings_wins,
        sim_pts = sim_pts + settings_fpts + settings_fpts_decimal / 100,
        sim_pts_against = sim_pts_against + settings_fpts_against + settings_fpts_against_decimal / 100
      ) %>%
      select(display_name, owner_id, starts_with("sim")) %>%
      arrange(desc(sim_wins), desc(sim_pts), sim_pts_against) %>%
      mutate(seed = row_number(), sim_num = Sim_Run_Number) 
    
    Postseason_Result_Probabilities <- Regular_Season_Results %>%
      filter(seed <= 6) %>%
      left_join(
        Cf_Pivoted_Playoff_Week_Projections, 
        by = c("display_name", "owner_id"), 
        relationship = "one-to-one") %>%
      mutate(seed = paste0("seed_", seed)) %>%
      pivot_wider(id_cols = sim_num, names_from = seed, values_from = matches("^team_.*_1[567]")) %>%
      mutate(
        # round one match up win probabilities
        round_1_seed_4_win_prob = pnorm(
          (team_mean_15_seed_4 - team_mean_15_seed_5) / sqrt(team_sd_15_seed_4^2 + team_sd_15_seed_5^2)),
        round_1_seed_5_win_prob = 1 - round_1_seed_4_win_prob,
        round_1_seed_3_win_prob = pnorm(
          (team_mean_15_seed_3 - team_mean_15_seed_6) / sqrt(team_sd_15_seed_3^2 + team_sd_15_seed_6^2)),
        round_1_seed_6_win_prob = 1 - round_1_seed_3_win_prob,
        # round two match up win probabilities
        round_2_seed_1_win_prob_vs_seed_4 = pnorm(
          (team_mean_16_seed_1 - team_mean_16_seed_4) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_4^2)),
        round_2_seed_1_win_prob_vs_seed_5 = pnorm(
          (team_mean_16_seed_1 - team_mean_16_seed_5) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_5^2)),
        round_2_seed_2_win_prob_vs_seed_3 = pnorm(
          (team_mean_16_seed_2 - team_mean_16_seed_3) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_3^2)),
        round_2_seed_2_win_prob_vs_seed_6 = pnorm(
          (team_mean_16_seed_2 - team_mean_16_seed_6) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_6^2)),
        # championship match up win probabilities
        round_3_seed_1_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_2^2)),
        round_3_seed_1_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_3^2)),
        round_3_seed_1_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_1 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_6^2)),
        round_3_seed_4_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_2^2)),
        round_3_seed_4_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_3^2)),
        round_3_seed_4_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_4 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_6^2)),
        round_3_seed_5_win_prob_vs_seed_2 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_2^2)),
        round_3_seed_5_win_prob_vs_seed_3 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_3^2)),
        round_3_seed_5_win_prob_vs_seed_6 = pnorm(
          (team_mean_17_seed_5 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_6^2)),
        # odds of reaching championship
        seed_1_champ_berth_prob = round_1_seed_4_win_prob * round_2_seed_1_win_prob_vs_seed_4 +
          round_1_seed_5_win_prob * round_2_seed_1_win_prob_vs_seed_5,
        seed_4_champ_berth_prob = round_1_seed_4_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_4),
        seed_5_champ_berth_prob = round_1_seed_5_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_5),
        seed_2_champ_berth_prob = round_1_seed_3_win_prob * round_2_seed_2_win_prob_vs_seed_3 +
          round_1_seed_6_win_prob * round_2_seed_2_win_prob_vs_seed_6,
        seed_3_champ_berth_prob = round_1_seed_3_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_3),
        seed_6_champ_berth_prob = round_1_seed_6_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_6),
        # odds of winning the championship
        seed_1_champ_win_prob = seed_1_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_2 +
            seed_3_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_3 +
            seed_6_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_6),
        seed_2_champ_win_prob = seed_2_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_2) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_2) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_2)),
        seed_3_champ_win_prob = seed_3_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_3) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_3) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_3)),
        seed_4_champ_win_prob = seed_4_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_2 +
            seed_3_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_3 +
            seed_6_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_6),
        seed_5_champ_win_prob = seed_5_champ_berth_prob * (
          seed_2_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_2 +
            seed_3_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_3 +
            seed_6_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_6),
        seed_6_champ_win_prob = seed_6_champ_berth_prob * (
          seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_6) +
            seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_6) +
            seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_6)),
        # sanity checks
        berth_sum_145 = seed_1_champ_berth_prob + seed_4_champ_berth_prob + seed_5_champ_berth_prob,
        berth_sum_236 = seed_2_champ_berth_prob + seed_3_champ_berth_prob + seed_6_champ_berth_prob,
        champ_win_prob_sum = seed_1_champ_win_prob + seed_2_champ_win_prob + seed_3_champ_win_prob +
          seed_4_champ_win_prob + seed_5_champ_win_prob + seed_6_champ_win_prob
      )
    
    Regular_Season_Results %>%
      left_join(
        Postseason_Result_Probabilities %>%
          select(sim_num, ends_with("_champ_win_prob")) %>%
          pivot_longer(-sim_num, names_to = "seed", values_to = "x_championships") %>%
          mutate(seed = as.integer(str_remove(str_remove(seed, "_champ_win_prob$"), "^seed_"))),
        by = c("sim_num", "seed"),
        relationship = "one-to-one") %>%
      mutate(x_championships = tidyr::replace_na(x_championships, replace = 0))
  }
) %>%
  list_rbind()

##### results analysis #####

Pre_Trade_Results <- Temp_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    n_sims = n(),
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  glimpse()

Post_Trade_Results <- Cf_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    n_sims = n(),
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  glimpse()

Pre_Trade_Results %>%
  left_join(
    Post_Trade_Results, 
    by = c("owner_id", "display_name"), 
    suffix = c("_pre", "_post")) %>%
  filter(display_name %in% c("TaysomsLeftNut", "cseibold47"))

Pre_Trade_Results %>%
  filter(display_name %in% c("TaysomsLeftNut", "cseibold47")) %>%
  pivot_longer(ends_with("_pct")) %>%
  inner_join(
    Post_Trade_Results %>%
      pivot_longer(ends_with("_pct")), 
    by = c("owner_id", "display_name", "name"), 
    suffix = c("_pre", "_post")) %>%
  mutate(
    successes_pre = round(value_pre * n_sims_pre),
    successes_post = round(value_post * n_sims_post),
    p_value = pmap_dbl(
      list(successes_pre, successes_post, n_sims_pre, n_sims_post),
      ~ prop.test(x = c(..1, ..2), n = c(..3, ..4))$p.value
    )
  )

#### ESPN League Stuff ####

# Asel_Espn_League_Id <- 1084973 # Asel
Asel_Espn_League_Id <- 851807286 # Baltimore

Espn_Cookies <- c(
  `swid` = "{8D0E5A4E-2F6A-43C4-8E5A-4E2F6A73C4D5}",
  `espn_s2` =  "AEB%2F3B23inWpXsjpW2JL9UADc3%2BzIDYAw75PIHGrO%2FQnowdxsOdzGiKEIiiAEulxON1VdAAHH3WXsXNFj2rxv7CnYcSRzx73ecy2kk89%2BRWNN3BZCqa%2FgRHXW5x7FQw2vRIB9%2Fg0Dunb3VRtEIVrZdM2O5vVRa4XClI%2Bpe1Kvkq4SugFrLp8b%2Bq1uwj9fV7Eu9SLR1Du7V2am4ieOnxWaMMFJ0OQAi2D%2BjfVLs1CvwryXsZmnhKWM%2B%2FO3x%2Bm0PUWeJUQaudcbP8Lh0cdxJJDjw8f2a5Rg7dNt40oL26keVtmjg%3D%3D"
  )

Espn_Get_Config <- Espn_Cookies %>%
  paste(names(.), ., sep = "=", collapse = ";") %>%
  httr::config(cookie = .)

Season_Of_Interest <- lubridate::year(Sys.Date())

Asel_League_Info <- Asel_Espn_League_Id %>%
  paste0(
    "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", Season_Of_Interest, 
    "/segments/0/leagues/", ., "?view=mSettings&view=mRoster&view=mTeam&view=modular&view=mNav") %>%
  httr::GET(config = Espn_Get_Config) %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE)

Asel_Reg_Szn_Matchups_Count <- Asel_League_Info$settings$scheduleSettings$matchupPeriodCount

Asel_Reg_Szn_Matchup_Length <- Asel_League_Info$settings$scheduleSettings$matchupPeriodLength

Asel_Reg_Szn_Weeks <- Asel_Reg_Szn_Matchups_Count * Asel_Reg_Szn_Matchup_Length

Asel_Playoff_Matchup_Length <- Asel_League_Info$settings$scheduleSettings$playoffMatchupPeriodLength

Asel_Playoff_Seeding_Rule <- Asel_League_Info$settings$scheduleSettings$playoffSeedingRule

Asel_Playoff_Reseeding <- Asel_League_Info$settings$scheduleSettings$playoffReseed

Asel_Playoff_Team_Count <- Asel_League_Info$settings$scheduleSettings$playoffTeamCount

Asel_First_Week <- Asel_League_Info$status$firstScoringPeriod

Asel_Last_Week <- Asel_League_Info$status$finalScoringPeriod

Asel_Playoff_Rounds <- case_when(
  Asel_Playoff_Team_Count == 2 ~ 1,
  Asel_Playoff_Team_Count <= 4 ~ 2,
  Asel_Playoff_Team_Count <= 8 ~ 3
)

Asel_Playoff_Weeks <- seq(
  from = Asel_Reg_Szn_Weeks + Asel_First_Week, 
  to = Asel_Reg_Szn_Weeks + Asel_Playoff_Rounds * Asel_Playoff_Matchup_Length, 
  by = 1)

# Asel_League_Info$settings$scheduleSettings$playoffSeedingRuleBy
  
Asel_Matchup_Info <- paste0(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", Season_Of_Interest, 
  "/segments/0/leagues/", Asel_Espn_League_Id, "?view=mMatchupScore&view=mStatus&view=mSettings") %>%
  httr::GET(config = Espn_Get_Config) %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  .[["schedule"]] %>%
  janitor::clean_names() %>%
  select(matchup_id = id, week = matchup_period_id, winner, away_team_id, home_team_id) %>%
  glimpse()

Asel_Members <- Asel_League_Info$members %>%
  janitor::clean_names() %>%
  select(-notification_settings)

Asel_Teams <- Asel_League_Info$teams %>%
  janitor::clean_names() %>%
  select(
    team_abbrev = abbrev, team_id = id, team_name = name, 
    primary_owner, starts_with("record_overall"))

Asel_Team_Rosters <- Asel_League_Info$teams %>%
  janitor::clean_names() %>%
  select(team_id = id, roster_entries) %>%
  unnest(roster_entries) %>%
  janitor::clean_names() %>%
  select(-matches("player_rankings|rank|outlook|ownership")) %>%
  `colnames<-`(str_remove(colnames(.), "^player_pool_entry_")) %>%
  janitor::clean_names() %>%
  select(-c(player_stats, player_eligible_slots)) %>%
  glimpse()

Espn_Pro_Team_Id_Abbrev_Map <- Asel_Team_Rosters %>%
  filter(player_pro_team_id == 0) %>%
  select(player_id, player_pro_team_id) %>%
  inner_join(Espn_Nfl_Roster_Data, by = "player_id") %>%
  select(pro_team_id = player_pro_team_id, team_abbrev) %>%
  distinct() %>%
  arrange(pro_team_id)

Sleeper_To_Espn_Defense_Map <- Master_Table_Df %>%
  filter(position == "DEF") %>%
  select(player_id, first_name, last_name, position, espn_id) %>%
  mutate(
    espn_abbrev = tolower(player_id) %>%
      ifelse(. == "was", "wsh", .)
    ) %>%
  left_join(
    Espn_Pro_Team_Id_Abbrev_Map, 
    by = c("espn_abbrev" = "team_abbrev"),
    relationship = "one-to-one"
    ) %>%
  mutate(espn_id = -16000 -pro_team_id) #%>%
  # select(sleeper_id = player_id, espn_id)

Asel_League_Roster_Slots <- Asel_League_Info$settings$rosterSettings$lineupSlotCounts %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "slot_type_id", values_to = "slots") %>%
  mutate(slot_type_id = as.integer(str_remove(slot_type_id, pattern = "^X"))) %>%
  glimpse()

Asel_Roster_Entries <- paste0(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", 
  Season_Of_Interest, "/segments/0/leagues/", Asel_Espn_League_Id, "?view=mMatchup&view=mMatchupScore&view=mRoster&view=mScoreboard&view=mSettings&view=mStatus&view=mTeam&view=modular&view=mNav") %>%
  httr::GET(config = Espn_Get_Config) %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  .[["teams"]] %>%
  .[["roster.entries"]] %>%
  list_rbind() %>%
  janitor::clean_names() %>%
  select(
    player_id, lineup_slot_id, roster_id = player_pool_entry_on_team_id, 
    player_locked = player_pool_entry_roster_locked, injured = player_pool_entry_player_injured) %>%
  glimpse()

json_to_get_me_untruncated_list <- list(
  players = list(
    limit = 1500,  # Set a high limit to retrieve all players
    sortDraftRanks = list(
      sortPriority = 100,
      sortAsc = TRUE,
      value = "STANDARD"
    )
  )
) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

Asel_Espn_All_Players_Df <- paste0(
  "https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/", Season_Of_Interest, 
  "/segments/0/leagues/", Asel_Espn_League_Id, "?scoringPeriodId=11&view=kona_player_info") %>%
  httr::GET(
    add_headers(`x-fantasy-filter` = json_to_get_me_untruncated_list),
    config = Espn_Get_Config
    ) %>%
  httr::content(as = "text") %>%
  fromJSON(flatten = TRUE) %>%
  .[["players"]] %>%
  janitor::clean_names() %>% 
  select(-matches("player_rankings|rank|outlook|ownership"))

Sleeper_Espn_Position_Id_Map <- Master_Table_Df %>%
  filter(position %in% Positions_I_Care_About, active, !is.na(team)) %>%
  select(player_id, espn_id, position, team) %>%
  inner_join(
    Asel_Espn_All_Players_Df %>%
      select(player_id, player_default_position_id), 
    by = c("espn_id" = "player_id")) %>%
  group_by(position, player_default_position_id) %>% 
  summarise(n = n(), .groups = "drop_last") %>%
  group_by(player_default_position_id) %>%
  filter(n == max(n)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-n)

Espn_Sleeper_Player_Id_Map <- Asel_Espn_All_Players_Df %>%
  filter(!player_id %in% unique(na.omit(Master_Table_Df$espn_id))) %>%
  left_join(Espn_Nfl_Roster_Data, by = "player_id", relationship = "one-to-one") %>%
  left_join(Sleeper_Espn_Position_Id_Map, by = "player_default_position_id") %>%
  mutate(position = ifelse(player_id < 0 & player_last_name == "D/ST", "DEF", position)) %>%
  left_join(
    Sleeper_To_Espn_Defense_Map %>%
      select(espn_abbrev, sleeper_team_abbrev = player_id), 
    by = c("team_abbrev" = "espn_abbrev"),
    relationship = "many-to-one") %>%
  filter(player_active, !is.na(player_pro_team_id)) %>%
  rename(espn_id = player_id, espn_full_name = player_full_name) %>%
  inner_join(
    Master_Table_Df %>%
      filter(!is.na(team), active, is.na(espn_id)) %>%
      select(position, team, player_id, sleeper_full_name = full_name), 
    by = c("sleeper_team_abbrev" = "team", "position"),
    relationship = "many-to-many") %>%
  select(espn_id, sleeper_id = player_id, sleeper_full_name, espn_full_name) %>%
  mutate(
    across(
      .cols = ends_with("full_name"),
      .fns = ~{
        tolower(.x) %>%
          str_replace_all(pattern = "[-]", replacement = " ") %>%
          str_remove_all(pattern = "[^a-z ]") %>%
          str_remove(pattern = " jr$") %>%
          str_remove(pattern = " i{1,}$") %>%
          str_remove(pattern = " sr$") %>%
          stringr::str_squish()
      }
    ),
    full_name_string_dist = stringdist::stringdist(sleeper_full_name, espn_full_name)
  ) %>%
  group_by(sleeper_id) %>%
  mutate(sleeper_min_dist = min(full_name_string_dist)) %>%
  group_by(espn_id) %>%
  mutate(espn_min_dist = min(full_name_string_dist)) %>%
  ungroup() %>%
  filter(full_name_string_dist == sleeper_min_dist, full_name_string_dist == espn_min_dist) %>%
  select(espn_id, sleeper_id) %>%
  bind_rows(
    Master_Table_Df %>%
      select(sleeper_id = player_id, espn_id) %>%
      distinct() %>%
      na.omit()
    )

###### looking at slot usage by team ######

Asel_Roster_Entries %>%
  group_by(roster_id, lineup_slot_id) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(id_cols = lineup_slot_id, names_from = roster_id, values_from = n)

Asel_Espn_All_Players_Df %>%
  filter(on_team_id > 0) %>%
  group_by(on_team_id) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  glimpse() %>%
  ggplot(aes(on_team_id, row, label = player_full_name)) +
  geom_label() +
  theme_clean()

#### Simulating ESPN League(s) ####

Owned_Player_Ids <- Asel_Espn_All_Players_Df %>%
  filter(on_team_id != 0) %>%
  left_join(Espn_Sleeper_Player_Id_Map, by = c("player_id" = "espn_id")) %>%
  pull(sleeper_id)

Top_Free_Agent_Players <- Sleeper_Projections_Data %>%
  left_join(
    Espn_Sleeper_Player_Id_Map, 
    by = c("player_id" = "sleeper_id"),
    relationship = "many-to-one",
    suffix = c("_duplicate", "")) %>%
  filter(
    !player_id %in% Owned_Player_Ids, 
    week >= Week_Of_Interest, 
    season == as.character(lubridate::year(Sys.Date())),
    !is.na(stats_pts_half_ppr)) %>%
  left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
  filter(position %in% Positions_I_Care_About) %>%
  group_by(position, week) %>%
  arrange(desc(stats_pts_half_ppr)) %>%
  mutate(free_agent_position_rank = row_number()) %>%
  ungroup() %>%
  filter(
    (position %in% c("K", "DEF") & free_agent_position_rank == 1) |
      (position == "QB" & free_agent_position_rank <= 2) |
      (position == "TE" & free_agent_position_rank <= 3) |
      (position %in% c("RB", "WR") & free_agent_position_rank <= 4)
  ) %>%
  select(week, player_id) %>%
  distinct()

# Asel_Roster_Settings <- c(
#   "QB" = 2, "RB" = 2, "WR" = 2, "TE" = 1, "FLEX" = 2, "K" = 1, "DEF" = 1)
Asel_Roster_Settings <- c(
  "QB" = 1, "RB" = 2, "WR" = 3, "TE" = 1, "FLEX" = 1, "OP" = 1, "K" = 1, "DEF" = 1)

Empty_Slot_Options <- data.frame(
  position = unname(
    unlist(mapply(rep, names(Asel_Roster_Settings), Asel_Roster_Settings)))
) %>%
  data.frame() %>%
  group_by(position) %>%
  mutate(player_id = paste0("empty_", position, row_number())) %>%
  ungroup() %>%
  expand_grid(
    roster_id = -1, 
    projected_pts = 0, 
    full_name = "empty",
    week = Week_Of_Interest:Asel_Last_Week
  )

Locked_In_Starters <- Asel_Roster_Entries %>%
  filter(!lineup_slot_id %in% c(20, 21), player_locked) %>%
  pull(player_id)

Pivoted_Player_Eligibilities <- Asel_Espn_All_Players_Df %>%
  select(espn_id = player_id, roster_id = on_team_id) %>%
  left_join(Espn_Sleeper_Player_Id_Map, by = "espn_id") %>%
  rename(player_id = sleeper_id) %>%
  filter(player_id %in% c(Top_Free_Agent_Players$player_id, Owned_Player_Ids)) %>%
  # Syracuse_Data %>%
  # unnest(players) %>%
  # rename(player_id = players) %>%
  # bind_rows(data.frame(player_id = unique(Top_Free_Agent_Players$player_id))) %>%
  mutate(season = as.character(lubridate::year(Sys.Date()))) %>%
  left_join(
    Master_Table_Df %>%
      select(player_id, first_name, last_name, position) %>%
      distinct(),
    by = "player_id",
    relationship = "one-to-one") %>%
  left_join(
    Sleeper_Projections_Data %>%
      filter(week >= Week_Of_Interest) %>%
      select(player_id, week, season, projected_pts = stats_pts_half_ppr, team),
    by = c("player_id", "season")
  ) %>%
  inner_join(
    Pivoted_Venue_Data %>%
      mutate(season = as.character(season)) %>%
      select(season, week, team),
    by = c("season", "week", "team"),
    relationship = "many-to-one"
  ) %>%
  mutate(projected_pts = replace_na(projected_pts, 0)) %>%
  janitor::clean_names() %>%
  # `colnames<-`(str_remove(colnames(.), pattern = "^settings_")) %>%
  # select(-c(starters, reserve)) %>%
  left_join(
    Master_Table_Df[,c("player_id", "fantasy_positions")],
    by = "player_id",
    relationship = "many-to-many") %>%
  mutate(
    qb = str_detect(fantasy_positions, "QB"),
    rb = str_detect(fantasy_positions, "RB"),
    wr = str_detect(fantasy_positions, "WR"),
    te = str_detect(fantasy_positions, "TE"),
    def = str_detect(fantasy_positions, "DEF"),
    k = str_detect(fantasy_positions, "K"),
    flex = rb | wr | te,
    op = qb | rb | wr | te
  ) %>%
  select(season, player_id, week, roster_id, projected_pts, qb:op) %>%
  pivot_longer(qb:op, names_to = "position", values_to = "eligibility") %>%
  filter(eligibility) %>%
  mutate(position = toupper(position)) %>%
  select(season, player_id, week, roster_id, position, projected_pts) %>%
  distinct() %>%
  left_join(
    distinct(Master_Table_Df[,c("player_id", "full_name")]), 
    by = "player_id") %>%
  bind_rows(Empty_Slot_Options) %>%
  rename(half_ppr_projection = projected_pts, slot = position) %>%
  left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
  mutate(adjusted_projections = predict(Projection_Recalibration_Lm, ., type = "response")) %>%
  mutate(
    expected_error = predict(Post_Recal_Error_Lm, ., type = "response"),
    across(
      .cols = c(adjusted_projections, expected_error), 
      .fns = ~ifelse(str_detect(player_id, "^empty"), 0, .x)),
    # forcing bs score to get in lineup, will be overwritten
    adjusted_projections = case_when(
      week == Week_Of_Interest & player_id %in% Locked_In_Starters ~ 100,
      week == Week_Of_Interest & player_id %in% Locked_Weekly_Scoring$player_id ~ 0,
      TRUE ~ adjusted_projections),
    expected_error = ifelse(
      player_id %in% Locked_Weekly_Scoring$player_id, 0, expected_error)
  ) %>%
  select(
    season, player_id, week, roster_id, position = slot, 
    projected_pts = adjusted_projections, expected_error) %>%
  left_join(Locked_Weekly_Scoring, by = c("season", "week", "player_id")) %>%
  filter(position %in% names(Asel_Roster_Settings)) %>%
  glimpse()

##### Optimizing Lineups #####

Optimized_Lineup_Expectations <- purrr::map(
  .x = Week_Of_Interest:Asel_Last_Week,
  .progress = TRUE,
  .f = function(Sim_Week_Of_Interest){
    Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
      filter(week == Sim_Week_Of_Interest) %>%
      pull(player_id)
    
    Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
      filter(week == Sim_Week_Of_Interest)
    
    purrr::map(
      .x = unique(Asel_Teams$team_id),
      .f = ~{
        Available_Player_Ids <- Pivoted_Player_Eligibilities %>%
          filter(roster_id == .x) %>%
          pull(player_id) %>%
          append(Top_Free_Agent_Player_Ids) %>%
          append(unique(Empty_Slot_Options$player_id))
        
        Optimized_Lineup <- Get_Optimal_Lineup(
          Input_Pivoted_Player_Eligibilities = Pivoted_Player_Eligibilities,
          Input_Roster_Constraints = Asel_Roster_Settings,
          Possible_Lineup_Players = Available_Player_Ids
          )
        
        if(Sim_Week_Of_Interest == Week_Of_Interest){
          Team_Mean <- Optimized_Lineup %>%
            mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
            pull(live_projection) %>%
            sum()
          } else {
          Team_Mean <- sum(Optimized_Lineup$projected_pts)
          }
        
        Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
        
        data.frame(
          week = Sim_Week_Of_Interest,
          roster_id = .x,
          team_mean = Team_Mean,
          team_sd = Team_Sd
        )
      }
    ) %>%
      list_rbind()
    
  }
) %>%
  list_rbind() %>%
  left_join(Asel_Teams, by = c("roster_id" = "team_id"))

Optimized_Lineup_Expectations %>%
  ggplot(aes(week, team_mean, color = as.factor(roster_id))) +
  geom_line() +
  geom_label(aes(label = team_abbrev)) +
  guides(color = "none") +
  labs(
    x = "Week",
    y = "Mean Projection",
    title = "Team Projected Points By Week"
  ) +
  theme_clean()

Minimalist_Matchups_Df <- Asel_Matchup_Info %>%
  select(matchup_id, week, away = away_team_id, home = home_team_id)

Ros_Weekly_Win_Probabilities <- Minimalist_Matchups_Df %>%
  filter(week >= Week_Of_Interest) %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("away" = "roster_id", "week"),
    relationship = "one-to-one") %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("home" = "roster_id", "week"),
    relationship = "one-to-one",
    suffix = c("_away", "_home")) %>%
  mutate(
    mean_diff = team_mean_away - team_mean_home,
    std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
    z_score = mean_diff / std_diff,
    away_win_prob = pnorm(z_score)
  ) %>%
  glimpse()

Pivoted_Playoff_Week_Projections <- Optimized_Lineup_Expectations %>%
  filter(week %in% Asel_Playoff_Weeks) %>%
  pivot_wider(
    id_cols = c(roster_id, team_abbrev, primary_owner), 
    names_from = "week", 
    values_from = c(team_mean, team_sd)) %>%
  glimpse()

Ros_Weekly_Win_Probabilities %>%
  pivot_longer(starts_with("team_abbrev"), values_to = "team_abbrev") %>%
  select(team_abbrev, week, away_win_prob, name) %>%
  mutate(
    name = str_remove(name, "^team_abbrev_"),
    win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
  ) %>%
  ggplot(aes(week, win_prob, color = team_abbrev)) + 
  geom_line() +
  geom_point() +
  geom_text(aes(label = team_abbrev)) +
  geom_vline(xintercept = (Asel_Reg_Szn_Weeks + 0.5)) +
  labs(
    x = "Week",
    y = "Projected Win Probability",
    color = "Team",
    title = "Team Projected Win Probability By Week"
  ) +
  theme_clean()

Past_Head_To_Head_Results <- Asel_Matchup_Info %>%
  filter(winner != "UNDECIDED") %>%
  expand_grid(perspective = c("home", "away")) %>%
  mutate(
    team = ifelse(perspective == "away", away_team_id, home_team_id),
    opponent = ifelse(perspective == "away", home_team_id, away_team_id),
    win_points = case_when(
      winner == "TIE" ~ 0.5,
      tolower(winner) == perspective ~ 1,
      TRUE ~ 0
      )
    ) %>%
  group_by(team, opponent) %>%
  summarise(win_points = sum(win_points, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup() %>%
  glimpse()

##### running the simulations #####

Temp_Sims <- purrr::map(
  .x = 1:2000,
  .progress = TRUE,
  .f = function(Sim_Run_Number){
    Weekly_Regular_Season_Results <- Ros_Weekly_Win_Probabilities %>%
      mutate(
        sim_pts_home = team_mean_home + team_sd_home * rnorm(n = nrow(.)),
        sim_pts_away = team_mean_away + team_sd_away * rnorm(n = nrow(.)),
        across(starts_with("sim_pts_"), ~ .x, .names = "dup_{.col}"),
        win_home = sim_pts_home > sim_pts_away,
        win_away = !win_home
      ) %>%
      pivot_longer(starts_with("primary_owner"), names_to = "owner_id_name", values_to = "owner_id") %>%
      pivot_longer(starts_with("team_abbrev"), names_to = "home_away", values_to = "display_name") %>%
      pivot_longer(starts_with("sim_pts_"), names_to = "sim_pts_name", values_to = "sim_pts") %>%
      pivot_longer(starts_with("win_"), names_to = "win_name", values_to = "wins") %>%
      mutate(home_away = str_extract(home_away, ".{4}$")) %>%
      filter(
        str_extract(win_name, ".{4}$") == home_away,
        str_extract(sim_pts_name, ".{4}$") == home_away,
        str_extract(owner_id_name, ".{4}$") == home_away
      ) %>%
      mutate(sim_pts_against = ifelse(home_away == "home", dup_sim_pts_away, dup_sim_pts_home))
    
    # Weekly_Regular_Season_Results %>%
    #   mutate(
    #     team = ifelse(home_away == "home", home, away),
    #     opponent = ifelse(home_away == "away", home, away)
    #     ) %>%
    #   glimpse()
    
    Regular_Season_Results <- Weekly_Regular_Season_Results %>%
      group_by(display_name, owner_id) %>%
      summarise(across(c(sim_pts, sim_pts_against, wins), sum), .groups = "drop_last") %>%
      ungroup() %>%
      left_join(
        Asel_Teams %>% 
          select(team_abbrev, starts_with("record_overall_")), 
        by = c("display_name" = "team_abbrev")) %>%
      janitor::clean_names() %>%
      mutate(
        sim_wins = wins + record_overall_wins,
        sim_pts = sim_pts + record_overall_points_for,
        sim_pts_against = sim_pts_against + record_overall_points_against
      ) %>%
      select(display_name, owner_id, starts_with("sim")) 
    
    if(Asel_Playoff_Seeding_Rule == "TOTAL_POINTS_SCORED"){
      Regular_Season_Results <- Regular_Season_Results %>%
        arrange(desc(sim_wins), desc(sim_pts), sim_pts_against) %>%
        mutate(seed = row_number(), sim_num = Sim_Run_Number)
    } else {
        stop("unrecognized playoff seeding rule")
      }
    
    Postseason_Result_Probabilities <- Regular_Season_Results %>%
      filter(seed <= Asel_Playoff_Team_Count) %>%
      left_join(
        Pivoted_Playoff_Week_Projections, 
        by = c("display_name" = "team_abbrev", "owner_id" = "primary_owner"), 
        relationship = "one-to-one") %>%
      mutate(seed = paste0("seed_", seed)) %>%
      pivot_wider(id_cols = sim_num, names_from = seed, values_from = matches("^team_.*_1[4567]")) 
    
    if(Asel_Playoff_Team_Count == 6 & Asel_Playoff_Matchup_Length == 1){
      Postseason_Result_Probabilities <- Postseason_Result_Probabilities %>%
        mutate(
          # round one match up win probabilities
          round_1_seed_4_win_prob = pnorm(
            (team_mean_15_seed_4 - team_mean_15_seed_5) / sqrt(team_sd_15_seed_4^2 + team_sd_15_seed_5^2)),
          round_1_seed_5_win_prob = 1 - round_1_seed_4_win_prob,
          round_1_seed_3_win_prob = pnorm(
            (team_mean_15_seed_3 - team_mean_15_seed_6) / sqrt(team_sd_15_seed_3^2 + team_sd_15_seed_6^2)),
          round_1_seed_6_win_prob = 1 - round_1_seed_3_win_prob,
          # round two match up win probabilities
          round_2_seed_1_win_prob_vs_seed_4 = pnorm(
            (team_mean_16_seed_1 - team_mean_16_seed_4) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_4^2)),
          round_2_seed_1_win_prob_vs_seed_5 = pnorm(
            (team_mean_16_seed_1 - team_mean_16_seed_5) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_5^2)),
          round_2_seed_2_win_prob_vs_seed_3 = pnorm(
            (team_mean_16_seed_2 - team_mean_16_seed_3) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_3^2)),
          round_2_seed_2_win_prob_vs_seed_6 = pnorm(
            (team_mean_16_seed_2 - team_mean_16_seed_6) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_6^2)),
          # championship match up win probabilities
          round_3_seed_1_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_2^2)),
          round_3_seed_1_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_3^2)),
          round_3_seed_1_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_6^2)),
          round_3_seed_4_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_2^2)),
          round_3_seed_4_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_3^2)),
          round_3_seed_4_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_6^2)),
          round_3_seed_5_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_2^2)),
          round_3_seed_5_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_3^2)),
          round_3_seed_5_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_6^2)),
          # odds of reaching championship
          seed_1_champ_berth_prob = round_1_seed_4_win_prob * round_2_seed_1_win_prob_vs_seed_4 +
            round_1_seed_5_win_prob * round_2_seed_1_win_prob_vs_seed_5,
          seed_4_champ_berth_prob = round_1_seed_4_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_4),
          seed_5_champ_berth_prob = round_1_seed_5_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_5),
          seed_2_champ_berth_prob = round_1_seed_3_win_prob * round_2_seed_2_win_prob_vs_seed_3 +
            round_1_seed_6_win_prob * round_2_seed_2_win_prob_vs_seed_6,
          seed_3_champ_berth_prob = round_1_seed_3_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_3),
          seed_6_champ_berth_prob = round_1_seed_6_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_6),
          # odds of winning the championship
          seed_1_champ_win_prob = seed_1_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_6),
          seed_2_champ_win_prob = seed_2_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_2) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_2) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_2)),
          seed_3_champ_win_prob = seed_3_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_3) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_3) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_3)),
          seed_4_champ_win_prob = seed_4_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_6),
          seed_5_champ_win_prob = seed_5_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_6),
          seed_6_champ_win_prob = seed_6_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_6) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_6) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_6)),
          # sanity checks
          berth_sum_145 = seed_1_champ_berth_prob + seed_4_champ_berth_prob + seed_5_champ_berth_prob,
          berth_sum_236 = seed_2_champ_berth_prob + seed_3_champ_berth_prob + seed_6_champ_berth_prob,
          champ_win_prob_sum = seed_1_champ_win_prob + seed_2_champ_win_prob + seed_3_champ_win_prob +
            seed_4_champ_win_prob + seed_5_champ_win_prob + seed_6_champ_win_prob
        )
    } else if (Asel_Playoff_Team_Count == 4 & Asel_Playoff_Matchup_Length == 2){
      Postseason_Result_Probabilities <- Postseason_Result_Probabilities %>%
        mutate(
          # round one match up win probabilities
          round_1_seed_1_win_prob = pnorm(
            ((team_mean_14_seed_1 + team_mean_15_seed_1) - (team_mean_14_seed_4 + team_mean_15_seed_4)) / 
              sqrt(team_sd_14_seed_1^2 + team_sd_14_seed_4^2 + team_sd_15_seed_1^2 + team_sd_15_seed_4^2)),
          round_1_seed_4_win_prob = 1 - round_1_seed_1_win_prob,
          round_1_seed_2_win_prob = pnorm(
            ((team_mean_14_seed_2 + team_mean_15_seed_2) - (team_mean_14_seed_3 + team_mean_15_seed_3)) / 
              sqrt(team_sd_14_seed_2^2 + team_sd_14_seed_3^2 + team_sd_15_seed_2^2 + team_sd_15_seed_3^2)),
          round_1_seed_3_win_prob = 1 - round_1_seed_2_win_prob,
          # championship match up win probabilities
          round_2_seed_1_win_prob_vs_seed_2 = pnorm(
            ((team_mean_16_seed_1 + team_mean_17_seed_1) - (team_mean_16_seed_2 + team_mean_17_seed_2)) / 
              sqrt(team_sd_16_seed_1^2 + team_sd_17_seed_1^2 + team_sd_16_seed_2^2 + team_sd_17_seed_2^2)),
          round_2_seed_1_win_prob_vs_seed_3 = pnorm(
            ((team_mean_16_seed_1 + team_mean_17_seed_1) - (team_mean_16_seed_3 + team_mean_17_seed_3)) / 
              sqrt(team_sd_16_seed_1^2 + team_sd_17_seed_1^2 + team_sd_16_seed_3^2 + team_sd_17_seed_3^2)),
          round_2_seed_4_win_prob_vs_seed_2 = pnorm(
            ((team_mean_16_seed_4 + team_mean_17_seed_4) - (team_mean_16_seed_2 + team_mean_17_seed_2)) / 
              sqrt(team_sd_16_seed_4^2 + team_sd_17_seed_4^2 + team_sd_16_seed_2^2 + team_sd_17_seed_2^2)),
          round_2_seed_4_win_prob_vs_seed_3 = pnorm(
            ((team_mean_16_seed_4 + team_mean_17_seed_4) - (team_mean_16_seed_3 + team_mean_17_seed_3)) / 
              sqrt(team_sd_16_seed_4^2 + team_sd_17_seed_4^2 + team_sd_16_seed_3^2 + team_sd_17_seed_3^2)),
          # championship win probabilities
          seed_1_champ_win_prob = round_1_seed_1_win_prob * 
            (round_1_seed_2_win_prob * round_2_seed_1_win_prob_vs_seed_2 +
               round_1_seed_3_win_prob * round_2_seed_1_win_prob_vs_seed_3),
          seed_4_champ_win_prob = round_1_seed_4_win_prob * 
            (round_1_seed_2_win_prob * round_2_seed_4_win_prob_vs_seed_2 +
               round_1_seed_3_win_prob * round_2_seed_4_win_prob_vs_seed_3),
          seed_2_champ_win_prob = round_1_seed_2_win_prob * 
            (round_1_seed_1_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_2) +
               round_1_seed_4_win_prob * (1 - round_2_seed_4_win_prob_vs_seed_2)),
          seed_3_champ_win_prob = round_1_seed_3_win_prob * 
            (round_1_seed_1_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_3) +
               round_1_seed_4_win_prob * (1 - round_2_seed_4_win_prob_vs_seed_3))
        )
    }
    
    Regular_Season_Results %>%
      left_join(
        Postseason_Result_Probabilities %>%
          select(sim_num, ends_with("_champ_win_prob")) %>%
          pivot_longer(-sim_num, names_to = "seed", values_to = "x_championships") %>%
          mutate(seed = as.integer(str_remove(str_remove(seed, "_champ_win_prob$"), "^seed_"))),
        by = c("sim_num", "seed"),
        relationship = "one-to-one") %>%
      mutate(x_championships = tidyr::replace_na(x_championships, replace = 0))
  }
) %>%
  list_rbind()

Temp_Sims %>%
  ggplot(aes(seed, fill = display_name)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(
    x = "Simulation Seed Result",
    y = "Count",
    fill = "Team",
    title = "Simulated Seed Distribution"
  ) +
  theme_clean()

Temp_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  pivot_longer(ends_with("_pct")) %>%
  group_by(name) %>%
  arrange(desc(value)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  glimpse() %>%
  ggplot(aes(rank, value, fill = display_name)) +
  facet_wrap(~ str_remove(name, "_pct$")) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(100 * round(value, 3), "%")),
    angle = 90, hjust = 0, nudge_y = 0.01
  ) +
  labs(
    x = "Rank",
    y = "Probability Via Simulation",
    title = "Simulation Playoff Probailities",
    fill = "Team"
  ) +
  theme_clean() +
  clean_enlarge_text()

#### Subtractive Values ####

Subtractive_Optimized_Lineup_Expectations <- purrr::map(
  .x = unique(Asel_Teams$team_id),
  .progress = TRUE,
  .f = function(Roster_Of_Interest){
    Rostered_Player_Ids <- Pivoted_Player_Eligibilities %>%
      filter(roster_id == Roster_Of_Interest) %>%
      pull(player_id) %>%
      unique()
    
    purrr::map(
      .x = Week_Of_Interest:Asel_Last_Week,
      .f = function(Sim_Week_Of_Interest){
        Temp_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
          filter(week == Sim_Week_Of_Interest)
        
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Sim_Week_Of_Interest) %>%
          pull(player_id)
        
        purrr::map(
          .x = Rostered_Player_Ids,
          .f = function(Hold_Out_Player_Id){
            Available_Player_Ids <- Rostered_Player_Ids %>%
              .[. != Hold_Out_Player_Id] %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Temp_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Asel_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            if(Sim_Week_Of_Interest == Week_Of_Interest){
              Team_Mean <- Optimized_Lineup %>%
                mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
                pull(live_projection) %>%
                sum()
            } else {
              Team_Mean <- sum(Optimized_Lineup$projected_pts)
            }
            
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Sim_Week_Of_Interest,
              roster_id = Roster_Of_Interest,
              hold_out_player_id = Hold_Out_Player_Id,
              team_mean = Team_Mean,
              team_sd = Team_Sd
            )
          }
        ) %>%
          list_rbind()
      }
    ) %>%
      list_rbind()
  }
) %>%
  list_rbind() %>%
  glimpse()

xSubtractive_Player_Values <- Subtractive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id"),
    relationship = "many-to-one",
    suffix = c("_subtractive", "_baseline")) %>%
  mutate(pts_delta = team_mean_subtractive - team_mean_baseline) %>%
  group_by(hold_out_player_id, roster_id) %>%
  summarise(pts_delta = sum(pts_delta), .groups = "drop_last") %>%
  ungroup() %>%
  mutate(value_via_subtraction = -pts_delta) %>%
  arrange(desc(value_via_subtraction)) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_id, player_name, position) %>%
      distinct(),
    by = c("hold_out_player_id" = "player_id"),
    relationship = "one-to-one") %>%
  left_join(
    Asel_Teams %>%
      select(team_id, team_abbrev, team_name), 
    by = c("roster_id" = "team_id")) %>%
  glimpse()

Subtractive_Player_Values %>%
  ggplot(aes(value_via_subtraction, fill = position)) +
  geom_histogram(binwidth = 1) +
  theme_clean()

Subtractive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id"),
    relationship = "many-to-one",
    suffix = c("_subtractive", "_baseline")) %>%
  mutate(pts_delta = team_mean_baseline - team_mean_subtractive) %>%
  filter(roster_id == 1, hold_out_player_id == "10222") %>%
  glimpse() %>%
  ggplot(aes(week)) +
  geom_line(aes(y = team_mean_subtractive), color = "red") +
  geom_line(aes(y = team_mean_baseline), color = "black") +
  labs(y = "Team Mean Points") +
  theme_clean()

#### Values of Addition ####

Additive_Player_Ids_To_Investigate <- Subtractive_Player_Values %>%
  filter(value_via_subtraction > 0, !position %in% c("K", "DEF")) %>%
  pull(hold_out_player_id) %>%
  unique()

Additive_Optimized_Lineup_Expectations <- purrr::map(
  .x = unique(Asel_Teams$team_id),
  .progress = TRUE,
  .f = function(Roster_Of_Interest){
    Rostered_Player_Ids <- Pivoted_Player_Eligibilities %>%
      filter(roster_id == Roster_Of_Interest) %>%
      pull(player_id) %>%
      unique()
    
    Player_Ids_To_Loop_Through <- Additive_Player_Ids_To_Investigate %>%
      .[!. %in% Rostered_Player_Ids]
    
    purrr::map(
      .x = Week_Of_Interest:Asel_Last_Week,
      .f = function(Sim_Week_Of_Interest){
        Temp_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
          filter(week == Sim_Week_Of_Interest)
        
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Sim_Week_Of_Interest) %>%
          pull(player_id)
        
        purrr::map(
          .x = Player_Ids_To_Loop_Through,
          .f = function(Adding_Player_Id){
            Available_Player_Ids <- Rostered_Player_Ids %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(Adding_Player_Id) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Temp_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Asel_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            if(Sim_Week_Of_Interest == Week_Of_Interest){
              Team_Mean <- Optimized_Lineup %>%
                mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
                pull(live_projection) %>%
                sum()
            } else {
              Team_Mean <- sum(Optimized_Lineup$projected_pts)
            }
            
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Sim_Week_Of_Interest,
              roster_id = Roster_Of_Interest,
              adding_player_id = Adding_Player_Id,
              team_mean = Team_Mean,
              team_sd = Team_Sd
            )
          }
        ) %>%
          list_rbind()
      }
    ) %>%
      list_rbind()
  }
) %>%
  list_rbind()

Additive_Player_Values <- Additive_Optimized_Lineup_Expectations %>%
  left_join(
    Optimized_Lineup_Expectations, 
    by = c("week", "roster_id"),
    relationship = "many-to-one",
    suffix = c("_additive", "_baseline")) %>%
  mutate(pts_delta = team_mean_additive - team_mean_baseline) %>%
  group_by(adding_player_id, roster_id) %>%
  summarise(pts_delta = sum(pts_delta), .groups = "drop_last") %>%
  ungroup() %>%
  arrange(desc(pts_delta)) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_id, player_name, position) %>%
      distinct(),
    by = c("adding_player_id" = "player_id"),
    relationship = "many-to-one") %>%
  left_join(
    Asel_Teams %>%
      select(team_id, team_abbrev, team_name), 
    by = c("roster_id" = "team_id")) %>%
  glimpse()

#### Addition-Subtraction Mismatches ####

Add_Sub_Players_Df <- Subtractive_Player_Values %>%
  inner_join(
    Additive_Player_Values,
    by = c("hold_out_player_id" = "adding_player_id", "position", "player_name"),
    suffix = c("_subtractive", "_additive")
  ) %>%
  rename(player_id = hold_out_player_id) %>%
  mutate(
    value_via_addition = pts_delta_additive,
    across(starts_with("value"), ~ifelse(.x < 0, 0, .x))) 

Add_Sub_Players_Df %>%
  ggplot(aes(value_via_subtraction, value_via_addition)) +
  facet_wrap(~ team_name_additive) +
  geom_point(aes(color = position)) +
  geom_abline() +
  labs(
    x = "Value According To Subtraction",
    y = "Value According To Addition",
    caption = paste("points above the diagonal would be valued more highly on",
                    "the strip titled team than their current"),
    title = "Untapped Surplus Value Plot"
  ) +
  theme_clean() +
  theme(legend.background = element_rect(color = "black"))

# adds that make the most sense for me
Add_Sub_Players_Df %>%
  filter(roster_id_additive == 2) %>%
  arrange(desc(value_via_addition - value_via_subtraction)) %>%
  head() %>%
  select(player_id, contains("team_name"), player_name, position, starts_with("value"))

# subtractions that make the most sense for me
Add_Sub_Players_Df %>%
  filter(roster_id_subtractive == 2) %>%
  arrange(desc(value_via_addition - value_via_subtraction)) %>%
  head() %>%
  select(player_id, contains("team_name"), player_name, position, starts_with("value"))

Untapped_Surplus_One_For_Ones <- Add_Sub_Players_Df %>%
  mutate(untapped_surplus = value_via_addition - value_via_subtraction) %>%
  filter(
    (team_name_additive == "Johnny's Scary Team" | team_name_subtractive == "Johnny's Scary Team"),
    untapped_surplus > 0) %>%
  group_by(roster_id_subtractive, roster_id_additive) %>%
  filter(untapped_surplus == max(untapped_surplus)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(player_name, team_name_subtractive, team_name_additive, 
         roster_id_subtractive, roster_id_additive, untapped_surplus) %>%
  self_join(
    by = c("team_name_subtractive" = "team_name_additive", 
           "team_name_additive" = "team_name_subtractive"),
    suffix = c("_subtractive", "_additive")
  ) %>%
  filter(team_name_additive < team_name_subtractive) %>%
  mutate(
    mutual_surplus_unlocked = untapped_surplus_subtractive + untapped_surplus_additive
  ) %>%
  arrange(desc(mutual_surplus_unlocked)) %>%
  glimpse()

#### Counter-factual Simulations ####

Add_Sub_Players_Df %>%
  filter(roster_id_subtractive == 11, roster_id_additive == 2) %>%
  arrange(desc(pts_delta_additive)) %>%
  glimpse()

Master_Table_Df %>%
  filter(position %in% Positions_I_Care_About, active, !is.na(team)) %>%
  filter(player_id == "10229") %>%
  pull(full_name)

Sleeper_Projections_Data %>%
  # filter(player_id %in% c("7588", "8228", "6813", "11628"), season == "2024") %>%
  # filter(player_id %in% c("9758", "96", "4892", "4943"), season == "2024") %>%
  filter(player_id %in% c(
    "1479", "2216", "9502", "8167", "8146", "11628", "7090"
    ), season == "2024") %>%
  # filter(player_id %in% c("4018", "9509", "3198"), season == "2024") %>%
  ggplot(aes(week, stats_pts_half_ppr, color = paste(player_last_name, player_id))) +
  geom_line() +
  labs(color = "player") +
  theme_clean()

Master_Table_Df %>%
  filter(position %in% Positions_I_Care_About, active, !is.na(team)) %>%
  filter(
    # first_name == "Keenan"
    # first_name == "Josh",
    last_name == "Mooney"
  ) %>%
  glimpse() %>%
  pull(player_id)

Cf_Roster_Statuses <- data.frame(
  # player_name = c("Stroud", "A-Rod", "Warren"),
  # player_id = c("9758", "96", "8228"),
  # roster_id = c(2, 7, 7)
  
  # player_name = c("Daniels", "M Evans", "Warren", "Javonte", "A-Rod", "Levis"),
  # player_id = c("11566", "2216", "8228", "7588", "96", "9999"),
  # player_name = c("Daniels", "Wilson", "Warren", "Javonte", "A-Rod", "Levis"),
  # player_id = c("11566", "8146", "8228", "7588", "96", "9999"),
  # roster_id = c(2, 5, 5, 2, 5, 2)
  
  player_name = c("Daniels", "Wilson", "Warren", "Javonte", "A-Rod", "Levis", "Mooney"),
  player_id = c("11566", "8146", "8228", "7588", "96", "9999", "7090"),
  roster_id = c(2, 5, 5, 2, 5, 2, 5)
  
  # player_name = c("J Taylor", "M Harrison", "Warren", "Javonte"),
  # player_id = c("6813", "11628", "8228", "7588"),
  # roster_id = c(2, 5, 5, 2)
  
  # player_id = c("4943", "2216"),
  # player_name = c("Darnold", "Evans"),
  # roster_id = c(2, 1)
  )

Cf_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
  left_join(Cf_Roster_Statuses, by = "player_id", suffix = c("", "_cf")) %>%
  mutate(roster_id = coalesce(roster_id_cf, roster_id), .keep = "unused") %>%
  glimpse()

Cf_Optimized_Lineup_Expectations <- purrr::map(
  .x = Week_Of_Interest:Asel_Last_Week,
  .progress = TRUE,
  .f = function(Sim_Week_Of_Interest){
    Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
      filter(week == Sim_Week_Of_Interest) %>%
      pull(player_id)
    
    Cf_Pivoted_Player_Eligibilities <- Cf_Pivoted_Player_Eligibilities %>%
      filter(week == Sim_Week_Of_Interest)
    
    purrr::map(
      .x = unique(Asel_Teams$team_id),
      .f = ~{
        Available_Player_Ids <- Cf_Pivoted_Player_Eligibilities %>%
          filter(roster_id == .x) %>%
          pull(player_id) %>%
          unique() %>%
          append(Top_Free_Agent_Player_Ids) %>%
          append(unique(Empty_Slot_Options$player_id))
        
        Optimized_Lineup <- Get_Optimal_Lineup(
          Input_Pivoted_Player_Eligibilities = Cf_Pivoted_Player_Eligibilities,
          Input_Roster_Constraints = Asel_Roster_Settings,
          Possible_Lineup_Players = Available_Player_Ids
          )
        
        if(Sim_Week_Of_Interest == Week_Of_Interest){
          Team_Mean <- Optimized_Lineup %>%
            mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
            pull(live_projection) %>%
            sum()
        } else {
          Team_Mean <- sum(Optimized_Lineup$projected_pts)
        }
        
        Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
        
        data.frame(
          week = Sim_Week_Of_Interest,
          roster_id = .x,
          team_mean = Team_Mean,
          team_sd = Team_Sd
        )
      }
    ) %>%
      list_rbind()
    
  }
) %>%
  list_rbind() %>%
  left_join(
    Asel_Teams %>%
      select(team_id, team_abbrev, team_name, primary_owner), 
    by = c("roster_id" = "team_id")) %>%
  glimpse()

Cf_Ros_Weekly_Win_Probabilities <- Minimalist_Matchups_Df %>%
  filter(week >= Week_Of_Interest) %>%
  left_join(
    Cf_Optimized_Lineup_Expectations, 
    by = c("away" = "roster_id", "week"),
    relationship = "one-to-one") %>%
  left_join(
    Cf_Optimized_Lineup_Expectations, 
    by = c("home" = "roster_id", "week"),
    relationship = "one-to-one",
    suffix = c("_away", "_home")) %>%
  mutate(
    mean_diff = team_mean_away - team_mean_home,
    std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
    z_score = mean_diff / std_diff,
    away_win_prob = pnorm(z_score)
  ) %>%
  glimpse()

Cf_Pivoted_Playoff_Week_Projections <- Cf_Optimized_Lineup_Expectations %>%
  filter(week %in% min(Asel_Playoff_Weeks):Asel_Last_Week) %>%
  pivot_wider(
    id_cols = c(roster_id, team_abbrev, primary_owner), 
    names_from = "week", 
    values_from = c(team_mean, team_sd)) %>%
  glimpse()

Cf_Sims <- purrr::map(
  .x = 1:2000,
  .progress = TRUE,
  .f = function(Sim_Run_Number){
    Weekly_Regular_Season_Results <- Cf_Ros_Weekly_Win_Probabilities %>%
      mutate(
        sim_pts_home = team_mean_home + team_sd_home * rnorm(n = nrow(.)),
        sim_pts_away = team_mean_away + team_sd_away * rnorm(n = nrow(.)),
        across(starts_with("sim_pts_"), ~ .x, .names = "dup_{.col}"),
        win_home = sim_pts_home > sim_pts_away,
        win_away = !win_home
      ) %>%
      pivot_longer(starts_with("primary_owner"), names_to = "owner_id_name", values_to = "owner_id") %>%
      pivot_longer(starts_with("team_abbrev"), names_to = "home_away", values_to = "display_name") %>%
      pivot_longer(starts_with("sim_pts_"), names_to = "sim_pts_name", values_to = "sim_pts") %>%
      pivot_longer(starts_with("win_"), names_to = "win_name", values_to = "wins") %>%
      mutate(home_away = str_extract(home_away, ".{4}$")) %>%
      filter(
        str_extract(win_name, ".{4}$") == home_away,
        str_extract(sim_pts_name, ".{4}$") == home_away,
        str_extract(owner_id_name, ".{4}$") == home_away
      ) %>%
      mutate(sim_pts_against = ifelse(home_away == "home", dup_sim_pts_away, dup_sim_pts_home))
    
    # Weekly_Regular_Season_Results %>%
    #   mutate(
    #     team = ifelse(home_away == "home", home, away),
    #     opponent = ifelse(home_away == "away", home, away)
    #     ) %>%
    #   glimpse()
    
    Regular_Season_Results <- Weekly_Regular_Season_Results %>%
      group_by(display_name, owner_id) %>%
      summarise(across(c(sim_pts, sim_pts_against, wins), sum), .groups = "drop_last") %>%
      ungroup() %>%
      left_join(
        Asel_Teams %>% 
          select(team_abbrev, starts_with("record_overall_")), 
        by = c("display_name" = "team_abbrev")) %>%
      janitor::clean_names() %>%
      mutate(
        sim_wins = wins + record_overall_wins,
        sim_pts = sim_pts + record_overall_points_for,
        sim_pts_against = sim_pts_against + record_overall_points_against
      ) %>%
      select(display_name, owner_id, starts_with("sim")) 
    
    if(Asel_Playoff_Seeding_Rule == "TOTAL_POINTS_SCORED"){
      Regular_Season_Results <- Regular_Season_Results %>%
        arrange(desc(sim_wins), desc(sim_pts), sim_pts_against) %>%
        mutate(seed = row_number(), sim_num = Sim_Run_Number)
    } else {
      stop("unrecognized playoff seeding rule")
    }
    
    Postseason_Result_Probabilities <- Regular_Season_Results %>%
      filter(seed <= Asel_Playoff_Team_Count) %>%
      left_join(
        Cf_Pivoted_Playoff_Week_Projections, 
        by = c("display_name" = "team_abbrev", "owner_id" = "primary_owner"), 
        relationship = "one-to-one") %>%
      mutate(seed = paste0("seed_", seed)) %>%
      pivot_wider(id_cols = sim_num, names_from = seed, values_from = matches("^team_.*_1[4567]")) 
    
    if(Asel_Playoff_Team_Count == 6 & Asel_Playoff_Matchup_Length == 1){
      Postseason_Result_Probabilities <- Postseason_Result_Probabilities %>%
        mutate(
          # round one match up win probabilities
          round_1_seed_4_win_prob = pnorm(
            (team_mean_15_seed_4 - team_mean_15_seed_5) / sqrt(team_sd_15_seed_4^2 + team_sd_15_seed_5^2)),
          round_1_seed_5_win_prob = 1 - round_1_seed_4_win_prob,
          round_1_seed_3_win_prob = pnorm(
            (team_mean_15_seed_3 - team_mean_15_seed_6) / sqrt(team_sd_15_seed_3^2 + team_sd_15_seed_6^2)),
          round_1_seed_6_win_prob = 1 - round_1_seed_3_win_prob,
          # round two match up win probabilities
          round_2_seed_1_win_prob_vs_seed_4 = pnorm(
            (team_mean_16_seed_1 - team_mean_16_seed_4) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_4^2)),
          round_2_seed_1_win_prob_vs_seed_5 = pnorm(
            (team_mean_16_seed_1 - team_mean_16_seed_5) / sqrt(team_sd_16_seed_1^2 + team_sd_16_seed_5^2)),
          round_2_seed_2_win_prob_vs_seed_3 = pnorm(
            (team_mean_16_seed_2 - team_mean_16_seed_3) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_3^2)),
          round_2_seed_2_win_prob_vs_seed_6 = pnorm(
            (team_mean_16_seed_2 - team_mean_16_seed_6) / sqrt(team_sd_16_seed_2^2 + team_sd_16_seed_6^2)),
          # championship match up win probabilities
          round_3_seed_1_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_2^2)),
          round_3_seed_1_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_3^2)),
          round_3_seed_1_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_1 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_1^2 + team_sd_17_seed_6^2)),
          round_3_seed_4_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_2^2)),
          round_3_seed_4_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_3^2)),
          round_3_seed_4_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_4 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_4^2 + team_sd_17_seed_6^2)),
          round_3_seed_5_win_prob_vs_seed_2 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_2) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_2^2)),
          round_3_seed_5_win_prob_vs_seed_3 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_3) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_3^2)),
          round_3_seed_5_win_prob_vs_seed_6 = pnorm(
            (team_mean_17_seed_5 - team_mean_17_seed_6) / sqrt(team_sd_17_seed_5^2 + team_sd_17_seed_6^2)),
          # odds of reaching championship
          seed_1_champ_berth_prob = round_1_seed_4_win_prob * round_2_seed_1_win_prob_vs_seed_4 +
            round_1_seed_5_win_prob * round_2_seed_1_win_prob_vs_seed_5,
          seed_4_champ_berth_prob = round_1_seed_4_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_4),
          seed_5_champ_berth_prob = round_1_seed_5_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_5),
          seed_2_champ_berth_prob = round_1_seed_3_win_prob * round_2_seed_2_win_prob_vs_seed_3 +
            round_1_seed_6_win_prob * round_2_seed_2_win_prob_vs_seed_6,
          seed_3_champ_berth_prob = round_1_seed_3_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_3),
          seed_6_champ_berth_prob = round_1_seed_6_win_prob * (1 - round_2_seed_2_win_prob_vs_seed_6),
          # odds of winning the championship
          seed_1_champ_win_prob = seed_1_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_1_win_prob_vs_seed_6),
          seed_2_champ_win_prob = seed_2_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_2) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_2) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_2)),
          seed_3_champ_win_prob = seed_3_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_3) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_3) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_3)),
          seed_4_champ_win_prob = seed_4_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_4_win_prob_vs_seed_6),
          seed_5_champ_win_prob = seed_5_champ_berth_prob * (
            seed_2_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_2 +
              seed_3_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_3 +
              seed_6_champ_berth_prob * round_3_seed_5_win_prob_vs_seed_6),
          seed_6_champ_win_prob = seed_6_champ_berth_prob * (
            seed_1_champ_berth_prob * (1 - round_3_seed_1_win_prob_vs_seed_6) +
              seed_4_champ_berth_prob * (1 - round_3_seed_4_win_prob_vs_seed_6) +
              seed_5_champ_berth_prob * (1 - round_3_seed_5_win_prob_vs_seed_6)),
          # sanity checks
          berth_sum_145 = seed_1_champ_berth_prob + seed_4_champ_berth_prob + seed_5_champ_berth_prob,
          berth_sum_236 = seed_2_champ_berth_prob + seed_3_champ_berth_prob + seed_6_champ_berth_prob,
          champ_win_prob_sum = seed_1_champ_win_prob + seed_2_champ_win_prob + seed_3_champ_win_prob +
            seed_4_champ_win_prob + seed_5_champ_win_prob + seed_6_champ_win_prob
        )
    } else if (Asel_Playoff_Team_Count == 4 & Asel_Playoff_Matchup_Length == 2){
      Postseason_Result_Probabilities <- Postseason_Result_Probabilities %>%
        mutate(
          # round one match up win probabilities
          round_1_seed_1_win_prob = pnorm(
            ((team_mean_14_seed_1 + team_mean_15_seed_1) - (team_mean_14_seed_4 + team_mean_15_seed_4)) / 
              sqrt(team_sd_14_seed_1^2 + team_sd_14_seed_4^2 + team_sd_15_seed_1^2 + team_sd_15_seed_4^2)),
          round_1_seed_4_win_prob = 1 - round_1_seed_1_win_prob,
          round_1_seed_2_win_prob = pnorm(
            ((team_mean_14_seed_2 + team_mean_15_seed_2) - (team_mean_14_seed_3 + team_mean_15_seed_3)) / 
              sqrt(team_sd_14_seed_2^2 + team_sd_14_seed_3^2 + team_sd_15_seed_2^2 + team_sd_15_seed_3^2)),
          round_1_seed_3_win_prob = 1 - round_1_seed_2_win_prob,
          # championship match up win probabilities
          round_2_seed_1_win_prob_vs_seed_2 = pnorm(
            ((team_mean_16_seed_1 + team_mean_17_seed_1) - (team_mean_16_seed_2 + team_mean_17_seed_2)) / 
              sqrt(team_sd_16_seed_1^2 + team_sd_17_seed_1^2 + team_sd_16_seed_2^2 + team_sd_17_seed_2^2)),
          round_2_seed_1_win_prob_vs_seed_3 = pnorm(
            ((team_mean_16_seed_1 + team_mean_17_seed_1) - (team_mean_16_seed_3 + team_mean_17_seed_3)) / 
              sqrt(team_sd_16_seed_1^2 + team_sd_17_seed_1^2 + team_sd_16_seed_3^2 + team_sd_17_seed_3^2)),
          round_2_seed_4_win_prob_vs_seed_2 = pnorm(
            ((team_mean_16_seed_4 + team_mean_17_seed_4) - (team_mean_16_seed_2 + team_mean_17_seed_2)) / 
              sqrt(team_sd_16_seed_4^2 + team_sd_17_seed_4^2 + team_sd_16_seed_2^2 + team_sd_17_seed_2^2)),
          round_2_seed_4_win_prob_vs_seed_3 = pnorm(
            ((team_mean_16_seed_4 + team_mean_17_seed_4) - (team_mean_16_seed_3 + team_mean_17_seed_3)) / 
              sqrt(team_sd_16_seed_4^2 + team_sd_17_seed_4^2 + team_sd_16_seed_3^2 + team_sd_17_seed_3^2)),
          # championship win probabilities
          seed_1_champ_win_prob = round_1_seed_1_win_prob * 
            (round_1_seed_2_win_prob * round_2_seed_1_win_prob_vs_seed_2 +
               round_1_seed_3_win_prob * round_2_seed_1_win_prob_vs_seed_3),
          seed_4_champ_win_prob = round_1_seed_4_win_prob * 
            (round_1_seed_2_win_prob * round_2_seed_4_win_prob_vs_seed_2 +
               round_1_seed_3_win_prob * round_2_seed_4_win_prob_vs_seed_3),
          seed_2_champ_win_prob = round_1_seed_2_win_prob * 
            (round_1_seed_1_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_2) +
               round_1_seed_4_win_prob * (1 - round_2_seed_4_win_prob_vs_seed_2)),
          seed_3_champ_win_prob = round_1_seed_3_win_prob * 
            (round_1_seed_1_win_prob * (1 - round_2_seed_1_win_prob_vs_seed_3) +
               round_1_seed_4_win_prob * (1 - round_2_seed_4_win_prob_vs_seed_3))
        )
    }
    
    Regular_Season_Results %>%
      left_join(
        Postseason_Result_Probabilities %>%
          select(sim_num, ends_with("_champ_win_prob")) %>%
          pivot_longer(-sim_num, names_to = "seed", values_to = "x_championships") %>%
          mutate(seed = as.integer(str_remove(str_remove(seed, "_champ_win_prob$"), "^seed_"))),
        by = c("sim_num", "seed"),
        relationship = "one-to-one") %>%
      mutate(x_championships = tidyr::replace_na(x_championships, replace = 0))
  }
) %>%
  list_rbind()

# Cf_Sims <- Cf_Sims %>%
#   bind_rows(Cf_Sims_2)
# 
# Cf_Sims_2 <- Cf_Sims

##### results analysis #####

bind_rows(
  Optimized_Lineup_Expectations %>%
    mutate(state = "pre-trade"),
  Cf_Optimized_Lineup_Expectations %>%
    mutate(state = "post-trade")
) %>%
  filter(roster_id %in% Cf_Roster_Statuses$roster_id) %>%
  ggplot(aes(week, team_mean, color = state)) +
  facet_wrap(~ team_name) +
  geom_point() +
  geom_line() +
  labs(
    x = "Week",
    y = "Scoring Expectation",
    color = "State",
    title = "Pre/Post Trade Scoring Expectations"
    ) +
  theme_clean()

Pre_Trade_Results <- Temp_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    n_sims = n(),
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    # champ_per_playoff_pct = champ_pct / playoffs_pct,
    x_wins = mean(sim_wins),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  glimpse()

Post_Trade_Results <- Cf_Sims %>%
  group_by(display_name, owner_id) %>%
  summarise(
    n_sims = n(),
    playoffs_pct = mean(seed <= 6),
    bye_pct = mean(seed <= 2),
    champ_pct = mean(x_championships),
    # champ_per_playoff_pct = champ_pct / playoffs_pct,
    x_wins = mean(sim_wins),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  glimpse()

Pre_Trade_Results %>%
  left_join(
    Post_Trade_Results, 
    by = c("owner_id", "display_name"), 
    suffix = c("_pre", "_post")) %>%
  filter(display_name %in% with(Asel_Teams, team_abbrev[team_id %in% Cf_Roster_Statuses$roster_id])) %>%
  glimpse()

Pre_Trade_Results %>%
  filter(display_name %in% with(Asel_Teams, team_abbrev[team_id %in% Cf_Roster_Statuses$roster_id])) %>%
  pivot_longer(ends_with("_pct")) %>%
  inner_join(
    Post_Trade_Results %>%
      pivot_longer(ends_with("_pct")), 
    by = c("owner_id", "display_name", "name"), 
    suffix = c("_pre", "_post")) %>%
  mutate(
    successes_pre = round(value_pre * n_sims_pre),
    successes_post = round(value_post * n_sims_post),
    p_value = pmap_dbl(
      list(successes_pre, successes_post, n_sims_pre, n_sims_post),
      ~ prop.test(x = c(..1, ..2), n = c(..3, ..4))$p.value
    )
  )

#### Counter-factual Scoring Strengths and Regular Season Wins ####

Possible_141_Trades <- Pivoted_Player_Eligibilities %>%
  filter(position %in% c("QB", "RB", "WR", "TE")) %>%
  select(player_id, roster_id) %>%
  distinct() %>%
  mutate(universal_join_key = 1) %>%
  self_join(by = "universal_join_key") %>%
  filter(roster_id_1 == 2 & roster_id_2 != 2) %>%
  # filter(roster_id_1 < roster_id_2, (roster_id_1 == 2 | roster_id_2 == 2)) %>%
  # filter(roster_id_1 == 2, roster_id_2 == 3) %>%
  anti_join(
    Additive_Player_Values %>%
      filter(pts_delta == 0),
    by = c("player_id_1" = "adding_player_id", "roster_id_2" = "roster_id")
    ) %>%
  anti_join(
    Additive_Player_Values %>%
      filter(pts_delta == 0),
    by = c("player_id_2" = "adding_player_id", "roster_id_1" = "roster_id")
  ) %>%
  glimpse()

Prior_Regular_Szn_Win_Expectations <- Ros_Weekly_Win_Probabilities %>%
  pivot_longer(starts_with("team_name"), values_to = "team_name") %>%
  select(team_name, week, away_win_prob, name) %>%
  mutate(
    name = str_remove(name, "^team_name_"),
    win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
  ) %>%
  filter(week < min(Asel_Playoff_Weeks)) %>%
  group_by(team_name) %>%
  summarise(x_reg_szn_wins = sum(win_prob)) %>%
  ungroup() %>%
  left_join(
    Asel_Teams %>%
      select(team_name, team_id, primary_owner),
    by = "team_name"
  ) %>% 
  rename(display_name = team_name, roster_id = team_id, owner_id = primary_owner) %>%
  glimpse()

Prior_Rest_Of_Szn_Scoring_Expectations <- Optimized_Lineup_Expectations %>%
  filter(week >= Week_Of_Interest) %>%
  group_by(roster_id) %>%
  summarise(ros_x_pts = sum(team_mean)) %>%
  ungroup() %>%
  glimpse()

Prior_Postseason_Strengths <- Optimized_Lineup_Expectations %>%
  filter(week >= min(Asel_Playoff_Weeks)) %>%
  group_by(roster_id) %>%
  summarise(postseason_strength = mean(team_mean)) %>%
  ungroup() %>%
  glimpse()

Possible_141_Results_Collection <- pmap(
  .l = Possible_141_Trades %>%
    select(player_id_1, roster_id_1, player_id_2, roster_id_2) %>%
    as.list(),
  .progress = TRUE,
  .f = function(player_id_1, roster_id_1, player_id_2, roster_id_2){
    Cf_Roster_Statuses <- data.frame(
      player_id = c(player_id_1, player_id_2),
      roster_id = c(roster_id_2, roster_id_1)
    )
    
    Cf_Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
      left_join(Cf_Roster_Statuses, by = "player_id", suffix = c("", "_cf")) %>%
      mutate(roster_id = coalesce(roster_id_cf, roster_id), .keep = "unused")
    
    Cf_Optimized_Lineup_Expectations <- purrr::map(
      .x = Week_Of_Interest:Asel_Last_Week,
      .progress = TRUE,
      .f = function(Sim_Week_Of_Interest){
        Top_Free_Agent_Player_Ids <- Top_Free_Agent_Players %>%
          filter(week == Sim_Week_Of_Interest) %>%
          pull(player_id)
        
        Cf_Pivoted_Player_Eligibilities <- Cf_Pivoted_Player_Eligibilities %>%
          filter(week == Sim_Week_Of_Interest)
        
        purrr::map(
          .x = unique(Asel_Teams$team_id),
          .f = ~{
            Available_Player_Ids <- Cf_Pivoted_Player_Eligibilities %>%
              filter(roster_id == .x) %>%
              pull(player_id) %>%
              unique() %>%
              append(Top_Free_Agent_Player_Ids) %>%
              append(unique(Empty_Slot_Options$player_id))
            
            Optimized_Lineup <- Get_Optimal_Lineup(
              Input_Pivoted_Player_Eligibilities = Cf_Pivoted_Player_Eligibilities,
              Input_Roster_Constraints = Asel_Roster_Settings,
              Possible_Lineup_Players = Available_Player_Ids
            )
            
            if(Sim_Week_Of_Interest == Week_Of_Interest){
              Team_Mean <- Optimized_Lineup %>%
                mutate(live_projection = coalesce(forced_real_score, projected_pts)) %>%
                pull(live_projection) %>%
                sum()
            } else {
              Team_Mean <- sum(Optimized_Lineup$projected_pts)
            }
            
            Team_Sd <- sqrt(sum(Optimized_Lineup$expected_error^2))
            
            data.frame(
              week = Sim_Week_Of_Interest,
              roster_id = .x,
              team_mean = Team_Mean,
              team_sd = Team_Sd
            )
          }
        ) %>%
          list_rbind()
        
      }
    ) %>%
      list_rbind() %>%
      left_join(
        Asel_Teams %>%
          select(team_id, display_name = team_name), 
        by = c("roster_id" = "team_id"),
        relationship = "many-to-one")
    
    Rest_Of_Szn_Scoring_Expectations <- Cf_Optimized_Lineup_Expectations %>%
      filter(roster_id == roster_id_1 | roster_id == roster_id_2) %>%
      group_by(roster_id) %>%
      summarise(ros_x_pts = sum(team_mean)) %>%
      ungroup() %>%
      mutate(dummy_roster_id = ifelse(roster_id == roster_id_1, "1", "2")) %>%
      select(dummy_roster_id, ros_x_pts) %>%
      pivot_wider(names_prefix = "ros_x_pts_", names_from = dummy_roster_id, values_from = ros_x_pts)
    
    Postseason_Scoring_Expectations <- Cf_Optimized_Lineup_Expectations %>%
      filter((roster_id == roster_id_1 | roster_id == roster_id_2), week >= min(Asel_Playoff_Weeks)) %>%
      group_by(roster_id) %>%
      summarise(postseason_strength = mean(team_mean)) %>%
      ungroup() %>%
      mutate(dummy_roster_id = ifelse(roster_id == roster_id_1, "1", "2")) %>%
      select(dummy_roster_id, postseason_strength) %>%
      pivot_wider(names_prefix = "postseason_strength_", names_from = dummy_roster_id, values_from = postseason_strength)
    
    Regular_Szn_Win_Expectations <- Minimalist_Matchups_Df %>%
      filter(week >= Week_Of_Interest) %>%
      left_join(
        Cf_Optimized_Lineup_Expectations, 
        by = c("away" = "roster_id", "week"),
        relationship = "one-to-one") %>%
      left_join(
        Cf_Optimized_Lineup_Expectations, 
        by = c("home" = "roster_id", "week"),
        relationship = "one-to-one",
        suffix = c("_away", "_home")) %>%
      mutate(
        mean_diff = team_mean_away - team_mean_home,
        std_diff = sqrt(team_sd_away^2 + team_sd_home^2),
        z_score = mean_diff / std_diff,
        away_win_prob = pnorm(z_score)
      ) %>%
      pivot_longer(starts_with("display_name"), values_to = "display_name") %>%
      select(display_name, week, away_win_prob, name) %>%
      mutate(
        name = str_remove(name, "^display_name_"),
        win_prob = ifelse(name == "away", away_win_prob, 1 - away_win_prob)
      ) %>%
      filter(week < min(Asel_Playoff_Weeks)) %>%
      group_by(display_name) %>%
      summarise(x_reg_szn_wins = sum(win_prob)) %>%
      ungroup() %>%
      left_join(
        Asel_Teams %>%
          select(roster_id = team_id, display_name = team_name),
        by = "display_name",
        relationship = "many-to-one") %>%
      filter(roster_id == roster_id_1 | roster_id == roster_id_2) %>%
      mutate(dummy_roster_id = ifelse(roster_id == roster_id_1, "1", "2")) %>%
      select(dummy_roster_id, x_reg_szn_wins) %>%
      pivot_wider(
        names_prefix = "x_reg_szn_wins_", 
        names_from = dummy_roster_id, 
        values_from = x_reg_szn_wins)
    
    data.frame(player_id_1, roster_id_1, player_id_2, roster_id_2) %>%
      cbind(Regular_Szn_Win_Expectations) %>%
      cbind(Rest_Of_Szn_Scoring_Expectations) %>%
      cbind(Postseason_Scoring_Expectations)
  }
) %>%
  list_rbind()

Temp <- Possible_141_Results_Collection %>%
  left_join(
    Prior_Regular_Szn_Win_Expectations %>%
      select(roster_id, prior_x_reg_szn_wins_1 = x_reg_szn_wins),
    by = c("roster_id_1" = "roster_id")
  ) %>%
  left_join(
    Prior_Regular_Szn_Win_Expectations %>%
      select(roster_id, prior_x_reg_szn_wins_2 = x_reg_szn_wins),
    by = c("roster_id_2" = "roster_id")
  ) %>%
  left_join(
    Prior_Rest_Of_Szn_Scoring_Expectations %>%
      rename(prior_ros_x_pts_1 = ros_x_pts),
    by = c("roster_id_1" = "roster_id")
  ) %>%
  left_join(
    Prior_Rest_Of_Szn_Scoring_Expectations %>%
      rename(prior_ros_x_pts_2 = ros_x_pts),
    by = c("roster_id_2" = "roster_id")
  ) %>%
  left_join(
    Prior_Postseason_Strengths %>%
      rename(prior_postseason_strength_1 = postseason_strength),
    by = c("roster_id_1" = "roster_id")
  ) %>%
  left_join(
    Prior_Postseason_Strengths %>%
      rename(prior_postseason_strength_2 = postseason_strength),
    by = c("roster_id_2" = "roster_id")
  ) %>%
  mutate(
    reg_szn_wins_delta_1 = x_reg_szn_wins_1 - prior_x_reg_szn_wins_1,
    reg_szn_wins_delta_2 = x_reg_szn_wins_2 - prior_x_reg_szn_wins_2,
    ros_pts_delta_1 = ros_x_pts_1 - prior_ros_x_pts_1,
    ros_pts_delta_2 = ros_x_pts_2 - prior_ros_x_pts_2
    ) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_name, player_id), 
    by = c("player_id_1" = "player_id")) %>%
  left_join(
    Master_Table_Df %>%
      mutate(player_name = paste(first_name, last_name)) %>%
      select(player_name, player_id), 
    by = c("player_id_2" = "player_id"),
    suffix = c("_1", "_2")) %>%
  glimpse()

g <- Temp %>%
  ggplot(aes(reg_szn_wins_delta_1, reg_szn_wins_delta_2, idk = paste(player_name_1, player_name_2))) +
  geom_point(aes(color = postseason_strength_1 - prior_postseason_strength_1)) +
  scale_color_gradient2(limits = c(-1, 1), oob = scales::squish) +
  labs(color = "delta\npost-szn\nstrength") +
  theme_clean()

plotly::ggplotly(g)

g <- Temp %>%
  ggplot(aes(postseason_strength_1, reg_szn_wins_delta_2, idk = paste(player_name_1, player_name_2))) +
  geom_point() +
  theme_clean()
plotly::ggplotly(g)

Temp %>%
  filter(reg_szn_wins_delta_1 > 0, reg_szn_wins_delta_2 > 0) %>%
  # arrange(desc(reg_szn_wins_delta_1 + reg_szn_wins_delta_2)) %>%
  filter(ros_pts_delta_1 > 0, ros_pts_delta_2 > 0) %>%
  arrange(desc(ros_pts_delta_1 + ros_pts_delta_2)) %>%
  glimpse()

Temp %>%
  filter(reg_szn_wins_delta_1 > 0, reg_szn_wins_delta_2 > 0, roster_id_2 == 3) %>%
  # arrange(desc(reg_szn_wins_delta_1 + reg_szn_wins_delta_2 - abs(reg_szn_wins_delta_1 - reg_szn_wins_delta_2))) %>%
  head()

#### Leverage Idea ####

Sleeper_Projections_Data %>%
  filter(
    player_id %in% with(Pivoted_Player_Eligibilities, player_id[roster_id > 0]), 
    season == "2024",
    week >= Week_Of_Interest,
    week <= Asel_Last_Week
    ) %>%
  mutate(playoffs = ifelse(week >= min(Asel_Playoff_Weeks), "post", "reg")) %>%
  group_by(player_id, playoffs) %>%
  summarise(
    proj_games = sum(stats_pts_half_ppr > 0, na.rm = TRUE), 
    avg = mean(stats_pts_half_ppr, na.rm = TRUE), 
    .groups = "drop_last") %>%
  ungroup() %>%
  pivot_wider(id_cols = player_id, names_from = playoffs, values_from = c(proj_games, avg)) %>%
  filter(proj_games_post == 3, avg_post > 10) %>%
  mutate(playoff_boost_ratio = avg_post / avg_reg) %>%
  arrange(desc(playoff_boost_ratio)) %>%
  glimpse()

#### ESPN League(s) Shenanigans ####

library(ffanalytics)

my_scrape <- scrape_data(src = c("CBS", "ESPN", "Yahoo"), 
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = 2018, week = 0)

my_scrape[[1]]
