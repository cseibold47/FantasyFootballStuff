
#### loading packages #####

library(tidyverse)
library(rvest)

#### web crawling ####

base_url <- "https://www.espn.com/nfl/teams"

webpage <- read_html(base_url)

roster_links <- webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  .[str_detect(., "roster")] %>%
  na.omit()

collection_of_rosters <- map(
  .x = roster_links,
  .progress = TRUE,
  .f = purrr::possibly(
    .f = function(roster_url){
      player_links <- roster_url %>%
        paste0("https://www.espn.com", .) %>%
        read_html() %>%
        html_nodes("div.page-container.cf") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        .[str_detect(., "^https://www.espn.com/nfl/player/_/id/")] %>%
        unique()
      
      collection_of_players <- map(
        .x = player_links,
        .f = purrr::possibly(
          .f = function(player_url){
            player_bio_text <- player_url %>%
              read_html() %>%
              html_nodes("div.PlayerHeader__Bio.pv5") %>%
              html_text() %>%
              .[str_detect(., "\\d{1,2}/\\d{1,2}/\\d{4}")] %>%
              first()
            
            data.frame(
              player_link = player_url,
              bio_text = player_bio_text
            )
          }
        )
      ) %>%
        list_rbind() %>%
        mutate(roster_link = roster_url)
    }
  )
) %>%
  list_rbind()

#### post-scraping data wrangling ####

scraped_roster_data <- collection_of_rosters %>%
  mutate(
    player_id = str_extract(player_link, "(?<=/)[0-9]{1,}(?=/[^/]*$)"),
    birth_date = str_extract(bio_text, "(?<=Birthdate)\\d{1,2}/\\d{1,2}/\\d{4}"),
    link_player_name = str_extract(player_link, "(?<=/)[a-z-]+$"),
    height = str_extract(bio_text, "(?<=HT/WT)[0-9]{1,2}'\\s?[0-9]{1,2}\""),
    height_in = as.integer(
      12 * as.numeric(str_extract(height, "(?<=^)[0-9]+")) +
        as.numeric(str_extract(height, "(?<=')\\s?[0-9]+"))),
    weight = as.integer(str_extract(bio_text, "(?<=\\s)[0-9]{3}(?=\\slbs)")),
    college = str_extract(bio_text, "(?<=College)[A-Za-z ]+(?=(Draft|Status))"),
    draft_year = as.integer(str_extract(bio_text, "(?<=Draft Info)[0-9]{4}(?=: Rd)")),
    draft_round = as.integer(str_extract(bio_text, "(?<=Rd )[0-9]+")),
    draft_pick = as.integer(str_extract(bio_text, "(?<=Pk )[0-9]+")),
    team_abbrev = str_extract(roster_link, "(?<=/)[a-z]{2,3}(?=/[^/]*$)"),
    link_team_name = str_extract(roster_link, "(?<=/)[a-z-]+$"),
    time_of_scrape_save = Sys.time()
    )

scrape_time_label <- scraped_roster_data %>%
  pull(time_of_scrape_save) %>%
  min(na.rm = TRUE) %>%
  format("%Y_%m_%d_%H_%M")

"~/Desktop/everything_else/Coding_Practice/Football_R/Nfl_Rosters_Via_Espn/espn_nfl_rosters_" %>%
  paste0(scrape_time_label, ".rds") %>%
  saveRDS(file = ., object = scraped_roster_data)

