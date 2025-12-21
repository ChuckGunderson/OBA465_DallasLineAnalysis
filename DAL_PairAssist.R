setwd("/Users/hobo/Desktop/OBA 465/GroupProject")

library(fastRhockey)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(scales)

# Got skaters from enviornment from different code

# 1️⃣ Filter for Dallas goal events
# Assumption: event_team_type 'away' means Dallas is the scoring team for Dallas (DAL_loc)
dal_goals <- skaters %>%
  filter(event_type == "GOAL", DAL_loc == "away", event_team_type == "away")

# 2️⃣ Extract assists from description
# Example description: "Roope Hintz (33) Wrist Shot, assists: Alexander Radulov (18), Joe Pavelski (48)"
dal_goals <- dal_goals %>%
  mutate(
    assists_text = str_extract(description, "assists:.*"),
    assists_text = str_replace(assists_text, "assists: ", ""),      # Remove label
    assists_text = str_remove(assists_text, "\\)$"),                # Remove trailing parens if needed
    assists_list = str_split(assists_text, ",\\s*")                 # Split into list
  )

# Clean up assists for each goal
assists_df <- dal_goals %>%
  filter(!is.na(assists_text)) %>%
  mutate(
    assist_names = map(assists_text, ~ str_trim(str_extract_all(.x, "[A-Za-z\\.\\s]+")[[1]]))
  ) %>%
  unnest_longer(assist_names) %>%
  mutate(
    assist_name = str_squish(assist_names) %>%
      str_replace_all("\\.", " ") %>%  # Replace dots with spaces
      str_squish()                     # Remove extra spaces
  ) %>%
  select(game_id, assist_name)

# Count total assists for each player
player_assist_counts <- assists_df %>%
  count(assist_name, name = "total_assists")

# Fix skater names in dal_goals
fix_names <- function(x) {
  x %>%
    str_replace_all("\\.", " ") %>%
    str_squish()
}

dal_goals_fixed <- dal_goals %>%
  mutate(across(starts_with("skater"), fix_names))

# All skaters
all_skaters <- unique(c(dal_goals_fixed$skater1, dal_goals_fixed$skater2, dal_goals_fixed$skater3,
                        dal_goals_fixed$skater4, dal_goals_fixed$skater5, dal_goals_fixed$skater6)) %>%
  na.omit()

# All possible pairs
all_pairs <- t(combn(all_skaters, 2))
all_pairs_df <- as.data.frame(all_pairs, stringsAsFactors = FALSE) %>%
  rename(skater1 = V1, skater2 = V2)

# Summarize assists by either skater
full_assist_summary <- all_pairs_df %>%
  left_join(player_assist_counts, by = c("skater1" = "assist_name")) %>%
  rename(skater1_assists = total_assists) %>%
  left_join(player_assist_counts, by = c("skater2" = "assist_name")) %>%
  rename(skater2_assists = total_assists) %>%
  mutate(
    skater1_assists = replace_na(skater1_assists, 0),
    skater2_assists = replace_na(skater2_assists, 0),
    total_assists_either = skater1_assists + skater2_assists
  ) %>%
  arrange(desc(total_assists_either))
