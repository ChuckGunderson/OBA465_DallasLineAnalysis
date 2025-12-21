setwd("/Users/hobo/Desktop/OBA 465/GroupProject")

library(fastRhockey)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(scales)

# Load the data
pbp <- load_nhl_pbp(seasons = 2022)
DAL <- pbp %>% filter(home_abbreviation == "DAL" | away_abbreviation == "DAL")

# Identify Skaters
skaters <- DAL %>%
  mutate(DAL_loc = ifelse(home_abbreviation == "DAL", "home", "away")) %>%
  rowwise() %>%
  mutate(skater_list = list(if (DAL_loc == "home") c_across(starts_with("home_on_")) else c_across(starts_with("away_on_")))) %>%
  ungroup() %>%
  mutate(skater_list = map(skater_list, ~ .x[!is.na(.x)])) %>%
  unnest_wider(skater_list, names_sep = "") %>%
  rename_with(~ paste0("skater", seq_along(.)), starts_with("skater_list")) %>%
  select(game_id, period, game_seconds, event_type, DAL_loc, event_team_type, description, starts_with("skater"))

# Normalize skater names: "First.Last" -> "First Last"
name_cols <- paste0("skater", 1:6)
skaters[name_cols] <- lapply(skaters[name_cols], function(col) {
  gsub("\\.", " ", col)
})
