
#### GPT Data Operations ####

clean_list <- function(x) {
  # Replace NULL with NA
  x[sapply(x, is.null)] <- NA
  
  # Convert any nested lists to character strings to avoid size mismatches
  x <- lapply(x, function(y) {
    if (is.list(y)) {
      return(paste(unlist(y), collapse = ", "))
    } else {
      return(y)
    }
  })
  
  return(x)
}

#### Optimal Lineup Stuff ####

Get_Optimal_Lineup <- function(
    Input_Pivoted_Player_Eligibilities,
    Input_Roster_Constraints = c(QB = 1, RB = 2, WR = 2, TE = 1, DEF = 1, K = 1, OP = 1, FLEX = 1),
    Possible_Lineup_Players
){
  Input_Pivoted_Player_Eligibilities <- Input_Pivoted_Player_Eligibilities %>%
    filter(player_id %in% Possible_Lineup_Players)
  
  Objective <- Input_Pivoted_Player_Eligibilities$projected_pts
  
  position_constraints <- do.call(rbind, lapply(names(Input_Roster_Constraints), function(pos) {
    as.numeric(Input_Pivoted_Player_Eligibilities$position == pos)
  })) %>%
    as.matrix()
  
  constraint_directions <- rep("==", length(Input_Roster_Constraints))
  
  constraint_rhs <- Input_Roster_Constraints
  
  unique_player_ids <- unique(Input_Pivoted_Player_Eligibilities$player_id)
  
  unique_constraints <- sapply(unique_player_ids, function(player_id) {
    as.numeric(Input_Pivoted_Player_Eligibilities$player_id == player_id)
  }) %>%
    t()
  
  constraint_matrix <- rbind(position_constraints, unique_constraints)
  
  constraint_directions <- c(constraint_directions, rep("<=", length(unique_player_ids)))
  
  constraint_rhs <- c(constraint_rhs, rep(1, length(unique_player_ids)))
  
  result <- lpSolve::lp(
    direction = "max",
    objective.in = Objective,
    const.mat = constraint_matrix,
    const.dir = constraint_directions,
    const.rhs = constraint_rhs,
    all.bin = TRUE
  )
  
  Input_Pivoted_Player_Eligibilities[result$solution == 1,]
}

#### Scraping & Saving Functions ####

Scrape_Birthday_From_Espn_Id <- function(Espn_Id_Of_Interest){
  Player_Birthday <- Espn_Id_Of_Interest %>%
    paste0("https://www.espn.com/nfl/player/stats/_/id/", ., "/") %>%
    rvest::read_html() %>%
    rvest::html_nodes(".fw-medium.clr-black") %>%
    rvest::html_text(trim = TRUE) %>%
    .[stringr::str_detect(., "/[12][90][901][0-9] \\(\\d{2}\\)$")] %>%
    dplyr::first() %>%
    stringr::str_extract("^\\d{1,2}/\\d{1,2}/\\d{4}")
  
  data.frame(espn_id = Espn_Id_Of_Interest, birth_date = Player_Birthday)
}



Scrape_And_Save_Sleeper_Projections <- function(
  Weeks_To_Scrape = 1:18,
  Years_To_Scrape = lubridate::year(Sys.Date()),
  Sleeper_Proj_Save_Path = "~/Desktop/everything_else/Coding_Practice/Football_R/Sleeper_Projections/"
  ){
  Sleeper_Projections_Data <- purrr::pmap(
    .l = as.list(tidyr::expand_grid(year = Years_To_Scrape, week = Weeks_To_Scrape)),
    .progress = TRUE,
    .f = function(year, week){
      Projections_Link <- paste0(
        "https://api.sleeper.com/projections/nfl/", year, "/", week, 
        "?season_type=regular&position[]=DEF&position[]=K&position[]=QB&position[]",
        "=RB&position[]=TE&position[]=WR&order_by=pts_2qb")
      
      jsonlite::fromJSON(content(httr::GET(Projections_Link), "text"), flatten = TRUE) %>%
        mutate(scrape_time = Sys.time())
    }
  ) %>%
    purrr::list_rbind() %>%
    janitor::clean_names()
  
  Scrape_Time_Label <- Sleeper_Projections_Data %>%
    pull(scrape_time) %>%
    min(na.rm = TRUE) %>%
    format("%Y_%m_%d_%H_%M")
  
  purrr::map(
    .x = Years_To_Scrape,
    .progress = TRUE,
    .f = ~{
      Season_Save_Path <- paste0(Sleeper_Proj_Save_Path, .x, "/")
      
      if(!dir.exists(Season_Save_Path)){
        system(paste("mkdir", Season_Save_Path))
      }
      
      Sleeper_Projections_Data %>%
        filter(season == as.character(.x)) %>%
        saveRDS(file = paste0(Sleeper_Proj_Save_Path, .x, "/Sleeper_Projections_", Scrape_Time_Label, ".rds")) 
    }
  )
}

#### Loading Data Functions ####

Get_Saved_Sleeper_Projections <- function(
    Sleeper_Proj_Save_Path = "~/Desktop/everything_else/Coding_Practice/Football_R/Sleeper_Projections/",
    Years_To_Get = lubridate::year(Sys.Date())
){
  Unpackable_Years <- Sleeper_Proj_Save_Path %>%
    list.files() %>%
    .[str_detect(., pattern = "^[0-9]{4}$")] %>%
    as.integer()
  
  Years_Of_Interest <- Unpackable_Years %>%
    .[. %in% Years_To_Get]
  
  purrr::map(
    .x = Years_Of_Interest,
    .progress = TRUE,
    .f = function(Year_To_Unpack){
      Unpackable_Rds_Files <- Sleeper_Proj_Save_Path %>%
        paste0(Year_To_Unpack, "/") %>%
        list.files() %>%
        .[stringr::str_detect(., "\\.rds$")]
      
      Scrape_Date_Times <- Unpackable_Rds_Files %>%
        stringr::str_remove(pattern = "^Sleeper_Projections_") %>%
        stringr::str_remove(pattern = "\\.rds$") %>%
        strptime(format = "%Y_%m_%d_%H_%M")
      
      Most_Recent_Index <- Scrape_Date_Times == max(Scrape_Date_Times)
      
      Unpackable_Rds_Files[Most_Recent_Index] %>%
        paste0(Sleeper_Proj_Save_Path, Year_To_Unpack, "/", .) %>%
        readRDS()
    }
  ) %>%
    purrr::list_rbind()
}



Get_Saved_Nfl_Rosters_Via_Espn <- function(
    Data_Path = "~/Desktop/everything_else/Coding_Practice/Football_R/Nfl_Rosters_Via_Espn/"
){
  Unpackable_Rds_Files <- Data_Path %>%
    list.files() %>%
    .[stringr::str_detect(., "\\.rds$")]
  
  Scrape_Date_Times <- Unpackable_Rds_Files %>%
    stringr::str_remove(pattern = "^espn_nfl_rosters_") %>%
    stringr::str_remove(pattern = "\\.rds$") %>%
    strptime(format = "%Y_%m_%d_%H_%M")
  
  Most_Recent_Index <- Scrape_Date_Times == max(Scrape_Date_Times)
  
  Unpackable_Rds_Files[Most_Recent_Index] %>%
    paste0(Data_Path, .) %>%
    readRDS() %>%
    return()
}

#### Simulation Function(s) ####

Run_Simulations <- function(
  Number_Of_Sims_To_Run = 1000,
  Verbose = TRUE
  ){
  Optimized_Lineup_Expectations <- purrr::map(
    .x = Week_Of_Interest:17,
    .progress = TRUE,
    .f = function(Week_Of_Interest){
      Top_Free_Agent_Player_Ids <- Top_Free_Agent_Player_Ids %>%
        filter(week == Week_Of_Interest) %>%
        pull(player_id)
      
      Pivoted_Player_Eligibilities <- Pivoted_Player_Eligibilities %>%
        filter(week == Week_Of_Interest)
      
      purrr::map(
        .x = unique(Syracuse_Data$roster_id),
        .f = ~{
          Available_Player_Ids <- Syracuse_Data %>%
            filter(roster_id == .x) %>%
            unnest(players) %>%
            pull(players) %>%
            append(Top_Free_Agent_Player_Ids)
          
          Optimized_Lineup <- Get_Optimal_Lineup(
            Input_Pivoted_Player_Eligibilities = Pivoted_Player_Eligibilities,
            Input_Roster_Constraints = Syracuse_Roster_Settings,
            Possible_Lineup_Players = Available_Player_Ids
          ) %>%
            arrange(desc(projected_pts)) %>%
            rename(half_ppr_projection = projected_pts, slot = position) %>%
            left_join(distinct(Master_Table_Df[,c("player_id", "position")]), by = "player_id") %>%
            mutate(adjusted_projections = predict(Projection_Recalibration_Lm, ., type = "response")) %>%
            mutate(expected_error = predict(Post_Recal_Error_Lm, ., type = "response"))
          
          Team_Mean <- sum(Optimized_Lineup$adjusted_projections)
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
  
  print("prep done, starting sims")
  
  purrr::map(
    .x = 1:Number_Of_Sims_To_Run,
    .progress = Verbose,
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
        left_join(Pivoted_Playoff_Week_Projections, by = c("display_name", "owner_id"), relationship = "one-to-one") %>%
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
}
