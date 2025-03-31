# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)
# Also uses {here}, {tidyr}, {ggdark}

# Read data into R -------------------------------------------------------------

metadata <- readRDS(here::here("_Gists/t20wc_match_metadata.rds"))

# Tidy the data ----------------------------------------------------------------

scores <- metadata %>%
  select(
    match_id, venue_name, contains("competitors_id"), contains("abbreviation"),
    contains("qualifier"), toss_won_by, toss_decision, winner_id,
    match_result_type, match_result, contains("period_scores_home"),
    contains("period_scores_away")) %>%
  mutate_at(
    .vars = vars(contains("period_scores")),
    .funs = as.numeric) %>%
  mutate(
    # venue_name = stringr::str_wrap(venue_name, width = 20),
    toss_decision = case_when(
      is.na(toss_decision) ~ "bat",
      TRUE                 ~ toss_decision),
    period_scores_home_wickets1 = case_when(
      is.na(period_scores_home_wickets1) ~ 0,
      TRUE                               ~ period_scores_home_wickets1),
    period_scores_home_wickets2 = case_when(
      is.na(period_scores_home_wickets2) ~ 0,
      TRUE                               ~ period_scores_home_wickets2),
    period_scores_away_wickets1 = case_when(
      is.na(period_scores_away_wickets1) ~ 0,
      TRUE                               ~ period_scores_away_wickets1),
    period_scores_away_wickets2 = case_when(
      is.na(period_scores_away_wickets2) ~ 0,
      TRUE                               ~ period_scores_away_wickets2),
    home_team_score = period_scores_home_score1 + period_scores_home_score2,
    home_team_wickets_lost = period_scores_home_wickets1 + period_scores_home_wickets2,
    away_team_score = period_scores_away_score1 + period_scores_away_score2,
    away_team_wickets_lost = period_scores_away_wickets1 + period_scores_away_wickets2) %>%
  select(-contains("period_scores")) %>%
  tidyr::pivot_longer(
    cols = c(competitors_id1, competitors_id2),
    names_to = "key",
    values_to = "competitors_id") %>%
  mutate(
    competitors_abbreviation = case_when(
      key == "competitors_id1" ~ competitors_abbreviation1,
      key == "competitors_id2" ~ competitors_abbreviation2),
    competitors_qualifier = case_when(
      key == "competitors_id1" ~ competitors_qualifier1,
      key == "competitors_id2" ~ competitors_qualifier2),
    toss_won_flag = case_when(
      toss_won_by == competitors_id ~ 1,
      TRUE                          ~ 0),
    toss_outcome_batting = case_when(
      toss_won_flag == 1 & toss_decision == "bat"  ~ "bat 1st",
      toss_won_flag == 0 & toss_decision == "bat"  ~ "bat 2nd",
      toss_won_flag == 1 & toss_decision == "bowl" ~ "bat 2nd",
      toss_won_flag == 0 & toss_decision == "bowl" ~ "bat 1st"),
    toss_decision = case_when(
      toss_won_flag == 1 ~ toss_decision,
      TRUE               ~ NA_character_),
    home_team_score = case_when(
      competitors_qualifier == "home" ~ home_team_score,
      TRUE                            ~ 0),
    home_team_wickets_lost = case_when(
      competitors_qualifier == "home" ~ home_team_wickets_lost,
      TRUE                            ~ 0),
    away_team_score = case_when(
      competitors_qualifier == "away" ~ away_team_score,
      TRUE                            ~ 0),
    away_team_wickets_lost = case_when(
      competitors_qualifier == "away" ~ away_team_wickets_lost,
      TRUE                            ~ 0),
    team_score = home_team_score + away_team_score,
    team_wickets_lost = home_team_wickets_lost + away_team_wickets_lost,
    team_result = case_when(
      winner_id == competitors_id ~ "won",
      TRUE                        ~ "lost"),
    match_result_type = case_when(
      winner_id == competitors_id ~ match_result_type,
      TRUE                        ~ NA_character_)) %>%
  select(
    -c("competitors_abbreviation1", "competitors_abbreviation2",
       "competitors_qualifier1", "competitors_qualifier2"),
    -contains("home"), -contains("away"), -toss_won_by, -key) %>%
  select(
    match_id, venue_name, contains("competitors"), contains("toss"),
    winner_id, team_result, contains("match_result"), everything())

# Create variables for opposing team score and wickets lost
scores_opp_team <- scores %>%
  select(match_id, competitors_id, team_score, team_wickets_lost) %>%
  group_by(match_id) %>%
  mutate(
    team_label = rep(c("team1", "team2"), times = length(match_id) / 2)) %>%
  ungroup() %>%
  mutate(
    competitors_id_team1 = case_when(
      team_label == "team1" ~ competitors_id,
      TRUE                  ~ NA_character_),
    competitors_id_team2 = case_when(
      team_label == "team2" ~ competitors_id,
      TRUE                  ~ NA_character_)) %>%
  tidyr::fill(competitors_id_team1, .direction = "down") %>%
  tidyr::fill(competitors_id_team2, .direction = "up") %>%
  mutate(
    opp_competitors_id = case_when(
      team_label == "team1" ~ competitors_id_team2,
      team_label == "team2" ~ competitors_id_team1)) %>%
  select(-contains("competitors_id_team")) %>%
  mutate(
    team_score_team1 = case_when(
      team_label == "team1" ~ team_score,
      TRUE                  ~ NA_real_),
    team_score_team2 = case_when(
      team_label == "team2" ~ team_score,
      TRUE                  ~ NA_real_)) %>%
  tidyr::fill(team_score_team1, .direction = "down") %>%
  tidyr::fill(team_score_team2, .direction = "up") %>%
  mutate(
    opp_team_score = case_when(
      team_label == "team1" ~ team_score_team2,
      team_label == "team2" ~ team_score_team1)) %>%
  select(-contains("team_score_team")) %>%
  mutate(
    team_wickets_lost_team1 = case_when(
      team_label == "team1" ~ team_wickets_lost,
      TRUE                  ~ NA_real_),
    team_wickets_lost_team2 = case_when(
      team_label == "team2" ~ team_wickets_lost,
      TRUE                  ~ NA_real_)) %>%
  tidyr::fill(team_wickets_lost_team1, .direction = "down") %>%
  tidyr::fill(team_wickets_lost_team2, .direction = "up") %>%
  mutate(
    opp_team_wickets_lost = case_when(
      team_label == "team1" ~ team_wickets_lost_team2,
      team_label == "team2" ~ team_wickets_lost_team1)) %>%
  select(-contains("team_wickets_lost_team"),
         -c("team_label", "team_score", "team_wickets_lost"))

# Merge back in with tidied venues data frame
scores_merged <- left_join(
  scores, scores_opp_team, by = c("match_id", "competitors_id")) %>%
  select(
    match_id, venue_name, competitors_id, competitors_abbreviation,
    competitors_qualifier, opp_competitors_id, everything()) %>%
  mutate(
    team_success_direction = case_when(
      team_score > opp_team_score ~ "successful",
      team_score < opp_team_score ~ "not_successful"))
  
# Summarise the data
scores_summary <- scores_merged %>%
  group_by(toss_outcome_batting, team_success_direction) %>%
  summarise(
    n_matches = length(unique(match_id)),
    score_median = round(median(team_score), 0),
    score_iqr_lower = round(quantile(team_score, 0.25), 0),
    score_iqr_upper = round(quantile(team_score, 0.75), 0),
    .groups = "keep") %>%
  ungroup() %>%
  mutate(
    score_median_label = paste(score_median, "runs", sep = " "),
    team_success_direction = factor(
      team_success_direction,
      levels = c("successful", "not_successful")))

# Prep to plot -----------------------------------------------------------------

# Import font from Google
font_add_google("Nanum Gothic Coding", "nanum")
showtext_auto()

# Set custom parameters
strip_labels <- c(
  "bat 1st" = "Setting (batting 1st)",
  "bat 2nd" = "Chasing (batting 2nd)")

x_axis_labels <- c(
  "successful" = "Successful",
  "not_successful" = "Not successful")

team_success_colours <- c(
  "successful" = "#17BEBB",
  "not_successful" = "#EF3E36")

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_point(
    data = scores_summary,
    aes(x = team_success_direction, y = score_median,
        colour = team_success_direction),
    size = 10) +
  geom_segment(
    data = scores_summary,
    aes(x = team_success_direction, xend = team_success_direction,
        y = score_iqr_lower, yend = score_iqr_upper,
        colour = team_success_direction),
    linewidth = 3, lineend = "round") +
  geom_text(
    data = scores_summary,
    aes(x = team_success_direction, y = score_median,
        label = score_median_label),
    nudge_x = +0.3, size = 15, family = "nanum") +
  facet_wrap(~toss_outcome_batting, nrow = 1,
             labeller = labeller(
               toss_outcome_batting = strip_labels)) +
  labs(
    title = "T20 World Cup 2023: 160 is the yardstick",
    subtitle = "Runs scored when setting vs. chasing targets; point = median, line = IQR",
    x = NULL, y = NULL,
    caption = "Plot by @jacquietran@mastodon.social | Data via Sportradar") +
  scale_x_discrete(
    labels = x_axis_labels) +
  scale_y_continuous(
    limits = c(0, 180),
    breaks = seq(0, 180, by = 30)) +
  scale_colour_manual(
    values = team_success_colours) +
  ggdark::dark_theme_minimal() +
  theme(
    text = element_text(family = "nanum"),
    plot.title = element_text(size = rel(8), face = "bold", margin = margin(b = 10, unit = "pt")),
    plot.subtitle = element_text(size = rel(4.5), margin = margin(b = 30, unit = "pt")),
    strip.text = element_text(size = rel(5), face = "bold"),
    axis.text = element_text(size = rel(5)),
    plot.caption = element_text(size = rel(4), margin = margin(t = 40, unit = "pt")),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#434C43"),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(1,1,1,1, unit = "cm"),
    plot.background = element_rect(fill = "#131313", colour = "#131313"))

# Save to file
ggsave(
  here::here("_Gists/t20wc_targets_20230227.png"), last_plot(),
  width = 10, height = 10, units = "in", dpi = 300)

# showtext_auto(FALSE)