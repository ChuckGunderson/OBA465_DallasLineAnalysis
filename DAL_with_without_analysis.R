setwd("/Users/hobo/Desktop/OBA 465/GroupProject")

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(corrplot)
library(reshape2)

# Load Data from DAL_Correlation where we calculated season summary pair data
pair_summary <- read_csv("/Users/hobo/Desktop/OBA 465/GroupProject/pair_summary.csv")

# Calculate with-vs-without performance
pair_deltas <- pair_summary %>%
  group_by(PlayerA) %>%
  mutate(
    total_goals_for = sum(goals_for),
    total_ice_time = sum(ice_time),
    total_shots_for = sum(shots_for),
    total_goals_against = sum(goals_against),
    
    goals_for_without = (total_goals_for - goals_for) / (total_ice_time - ice_time) * 60,
    shots_for_without = (total_shots_for - shots_for) / (total_ice_time - ice_time) * 60,
    goals_against_without = (total_goals_against - goals_against) / (total_ice_time - ice_time) * 60,
    
    delta_goals_for_per_60 = goals_for_per_60 - goals_for_without,
    delta_shots_for_per_60 = shots_for_per_60 - shots_for_without,
    delta_goals_against_per_60 = goals_against_per_60 - goals_against_without
  ) %>%
  mutate(
    normalized_pair = sapply(strsplit(pair, " & "), function(x) {
      paste(sort(x), collapse = " & ")
    })
  ) %>%
  mutate(ice_time_cat = case_when(
    ice_time < 3600 ~ "< 3600",
    ice_time >= 3600 & ice_time <= 10800 ~ "3600–10800",
    ice_time > 10800 ~ "> 10800"
  )) %>%
  ungroup()

# BAr chart for top pairs
top_gf <- pair_deltas %>%
  filter(ice_time > 1250) %>%
  arrange(desc(delta_goals_for_per_60)) %>%
  slice_head(n = 10)

ggplot(top_gf, aes(x = reorder(normalized_pair, delta_goals_for_per_60),
                   y = delta_goals_for_per_60,
                   fill = ice_time_cat)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("< 3600" = "red", "3600–10800" = "gold", "> 10800" = "seagreen")) +
  labs(title = "Top 10 Offensive Impact Pairs",
       x = "Player Pair", y = "Δ Goals For/60",
       fill = "Ice Time") +
  theme_minimal()

top_sf <- pair_deltas %>%
  filter(ice_time > 1250) %>%
  arrange(desc(delta_shots_for_per_60)) %>%
  slice_head(n = 10)

ggplot(top_sf, aes(x = reorder(normalized_pair, delta_shots_for_per_60),
                   y = delta_shots_for_per_60,
                   fill = ice_time_cat)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("< 3600" = "red", "3600–10800" = "gold", "> 10800" = "seagreen")) +
  labs(title = "Top 10 Offensive Impact Pairs",
       x = "Player Pair", y = "Δ Shots For/60",
       fill = "Ice Time") +
  theme_minimal()

# Offensive impact: average goal boost
offensive_boosters <- pair_deltas %>%
  group_by(PlayerB) %>%
  summarize(
    avg_offensive_boost = mean(delta_goals_for_per_60, na.rm = TRUE),
    avg_shot_boost = mean(delta_shots_for_per_60, na.rm = TRUE),
    n_pairs = n()
  )

top_offboost <- offensive_boosters %>%
  arrange(desc(avg_offensive_boost)) %>%
  slice_head(n = 10)

ggplot(top_offboost, aes(x = reorder(PlayerB, avg_offensive_boost), y = avg_offensive_boost)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 Offensive Boosts by Player",
    x = "Player",
    y = "Avg. Increase in Goals For per 60 Minutes (Teammates with Player)"
  ) +
  theme_minimal()

# Defensive impact: average defensive suppression
defensive_boosters <- pair_deltas %>%
  group_by(PlayerB) %>%
  summarize(
    avg_defensive_boost = -1 * mean(delta_goals_against_per_60, na.rm = TRUE),  # Negative means better defense
    n_pairs = n()
  )

top_defboost <- defensive_boosters %>%
  arrange(desc(avg_defensive_boost)) %>%
  slice_min(avg_defensive_boost, n = 10)

ggplot(top_defboost, aes(x = reorder(PlayerB, avg_defensive_boost), y = avg_defensive_boost)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 Players by Defensive Boost to Teammates",
    x = "Player",
    y = "Avg. Change in Goals Against per 60 Minutes (Teammates with Player)"
  ) +
  theme_minimal()
