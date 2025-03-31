# Load libraries ---------------------------------------------------------------

library(dplyr)
library(showtext)
library(ggplot2)
# Also uses {here}, {tidyr}, {stringr}, {scales}, {tibble}, {ggdark}

# Read data into R -------------------------------------------------------------

bbb <- readRDS(here::here("t20wc_match_bbb.rds"))

# Create dimensions ------------------------------------------------------------

# Create data about pitch dimensions
pitch_dim <- tibble::tibble(
  pitch_width = 305,
  width_between_return_creases = 264,
  popping_crease_to_behind_stumps = 2012,
  pitch_length = popping_crease_to_behind_stumps + 122,
  stumps_at_bowlers_end = 0 - 122,
  pitch_xmin = 0,
  pitch_xmax = pitch_width,
  pitch_ymin = stumps_at_bowlers_end - 160,
  pitch_ymax = pitch_length)

# Create data about stump dimensions
stump_dim <- tibble::tibble(
  end = rep(c("batter", "bowler"), each = 2),
  x = rep(c(141.05, 163.95), times = 2),
  y = c(
    rep(1890, times = 2),
    rep(pitch_dim$stumps_at_bowlers_end, times = 2)))

# Create data about wicket channel dimensions
wicket_channel_dim <- stump_dim %>%
  mutate(
    xmin = min(x),
    xmax = max(x),
    ymin = min(y),
    ymax = max(y)) %>%
  select(xmin, xmax, ymin, ymax) %>%
  distinct(xmin, .keep_all = TRUE)

# Create data about pitching zone labels
pitch_zone_labels <- tibble::tribble(
  ~label, ~y,
  
  "FULL TOSS", 1829,
  "YORKER", 1729,
  "FULL", 1490,
  "GOOD", 1190,
  "SHORT", 990) %>%
  mutate(
    x = (0.95*pitch_dim$pitch_width))

# Draw popping crease line at striker's end
geom_hline(
  yintercept = pitch_dim$popping_crease_to_behind_stumps - 244,
  colour = "#FFFFFF") +
  # Draw 2 m from stumps - full to yorker
  geom_hline(yintercept = 1690, linetype = "dashed") +
  # Draw 6 m from stumps - good to full
  geom_hline(yintercept = 1290, linetype = "dashed") +
  # Draw 8 m from stumps - back of a length to good length
  geom_hline(yintercept = 1090, linetype = "dashed") +
  # Draw 10 m from stumps - halfway to back of a length
  geom_hline(yintercept = 890, linetype = "dashed") +

# Tidy the retrieved data ------------------------------------------------------

# Subset to focus on largely bowling-related variables
bowling <- bbb %>%
  # Exclude timeline records that are not balls bowled
  filter(!timeline_type %in% c("period_start", "match_started", "match_ended")) %>%
  select(
    match_id, timeline_id, timeline_type, time, period_name, inning,
    over_number, ball_number, display_score, display_overs,
    contains("batting_params"), contains("bowling_params"),
    contains("dismissal_params"), drs) %>%
  tidyr::unnest(bowling_params_bowler, names_sep = "_") %>%
  # Revalue pitch_y coordinates so that higher Y value is closer to striker's stumps
  # Reference: https://developer.sportradar.com/docs/read/cricket/Cricket_v2#frequently-asked-questions
  mutate(
    bowling_params_pitch_y_rev = 52 - bowling_params_pitch_y) %>%
  # Rescale pitch_x and pitch_y coordinates to match metric measurements
  # Reference: https://en.wikipedia.org/wiki/Cricket_pitch#/media/File:Cricket_pitch.svg
  # First, add a temporary row with zero values for X and Y so that rescaling
  # is correctly performed
  add_row(
    timeline_type = "temp",
    bowling_params_pitch_x = 0,
    bowling_params_pitch_y_rev = 0) %>%
  mutate(
    bowling_params_pitch_x_cm = scales::rescale(
      bowling_params_pitch_x, to = c(0, pitch_dim$width_between_return_creases)),
    bowling_params_pitch_y_cm = scales::rescale(
      bowling_params_pitch_y_rev, to = c(0, pitch_dim$popping_crease_to_behind_stumps))) %>%
  filter(timeline_type != "temp") %>% # Remove temporary row
  # Identify ball types including dot balls and run-scoring balls
  mutate(
    ball_type = case_when(
      bowling_params_extra_runs_type == "WD"    ~ "wide",
      bowling_params_extra_runs_type == "NB"    ~ "no_ball",
      bowling_params_extra_runs_type == "NB+LB" ~ "no_ball",
      is.na(bowling_params_extra_runs_type) &
        batting_params_runs_scored == 0 &
        timeline_type != "wicket"               ~ "dot_ball",
      TRUE                                      ~ timeline_type),
    ball_type_simple = case_when(
      ball_type %in% c(
        "dot_ball", "wicket") ~ ball_type,
      TRUE                    ~ "other")) %>%
  # Fix instances where bowler's country code is NA
  mutate(
    bowling_params_bowler_country_code = case_when(
      stringr::str_detect(
        bowling_params_bowler_name, "Vastrakar") ~ "IND",
      stringr::str_detect(
        bowling_params_bowler_name, "Iqbal")     ~ "PAK",
      stringr::str_detect(
        bowling_params_bowler_name, "Tucker")    ~ "ZAF",
      stringr::str_detect(
        bowling_params_bowler_name, "Ramharack") ~ "TTO", # Trinidad and Tobago
      stringr::str_detect(
        bowling_params_bowler_name, "Gajnabi")   ~ "GUY", # Guyana
      stringr::str_detect(
        bowling_params_bowler_name, "Penfold")   ~ "NZL",
      TRUE                                       ~ bowling_params_bowler_country_code)) %>%
  # Tidy up bowling team designation - primarily grouping countries within West Indies
  mutate(
    bowling_team = case_when(
      bowling_params_bowler_country_code == "ZAF" ~ "South Africa",
      bowling_params_bowler_country_code == "LKA" ~ "Sri Lanka",
      bowling_params_bowler_country_code == "ENG" ~ "England",
      bowling_params_bowler_country_code == "NZL" ~ "New Zealand",
      bowling_params_bowler_country_code == "AUS" ~ "Australia",
      bowling_params_bowler_country_code == "IND" ~ "India",
      bowling_params_bowler_country_code == "PAK" ~ "Pakistan",
      bowling_params_bowler_country_code == "BGD" ~ "Bangladesh",
      bowling_params_bowler_country_code == "IRL" ~ "Ireland",
      bowling_params_bowler_country_code %in% c(
        "JAM", "BRB", "GRD", "LCA", "TTO", "GUY") ~ "West Indies")) %>%
  # Fix misspelled bowler names %>%
  mutate(
    bowling_params_bowler_name = case_when(
      bowling_params_bowler_name == "Darice Brown" ~ "Brown, Darcie",
      TRUE                                         ~ bowling_params_bowler_name)) %>%
  # Fix instances where striker's country code is NA
  mutate(
    batting_params_striker_country_code = case_when(
      stringr::str_detect(
        batting_params_striker_name, "Gajnabi")   ~ "GUY", # Guyana
      stringr::str_detect(
        batting_params_striker_name, "Tucker")    ~ "ZAF",
      stringr::str_detect(
        batting_params_striker_name, "Rashada")   ~ "JAM", # Jamaican
      stringr::str_detect(
        batting_params_striker_name, "Vastrakar") ~ "IND",
      TRUE                                        ~ batting_params_striker_country_code)) %>%
  # Tidy up batting team designation - primarily grouping countries within West Indies
  mutate(
    batting_team = case_when(
      batting_params_striker_country_code == "ZAF" ~ "South Africa",
      batting_params_striker_country_code == "LKA" ~ "Sri Lanka",
      batting_params_striker_country_code == "ENG" ~ "England",
      batting_params_striker_country_code == "NZL" ~ "New Zealand",
      batting_params_striker_country_code == "AUS" ~ "Australia",
      batting_params_striker_country_code == "IND" ~ "India",
      batting_params_striker_country_code == "PAK" ~ "Pakistan",
      batting_params_striker_country_code == "BGD" ~ "Bangladesh",
      batting_params_striker_country_code == "IRL" ~ "Ireland",
      batting_params_striker_country_code %in% c(
        "JAM", "BRB", "GRD", "LCA", "TTO", "GUY") ~ "West Indies"))
  
# Focus on Darcie's pitch maps
kapunda <- bowling %>%
  filter(bowling_params_bowler_name == "Brown, Darcie") %>%
  # Create labels that distinguish between group matches and finals
  mutate(
    match_label = case_when(
      stringr::str_detect(match_id, "37494639") ~ "m1_nzl",
      stringr::str_detect(match_id, "37494679") ~ "m2_bgd",
      stringr::str_detect(match_id, "37494685") ~ "m3_lka",
      stringr::str_detect(match_id, "37494739") ~ "m4_zaf",
      stringr::str_detect(match_id, "37520665") ~ "sf_ind",
      stringr::str_detect(match_id, "37520673") ~ "f_zaf")) %>%
  # Set factor order on batting_team to match order of games played
  mutate(
    match_label = factor(
      match_label,
      levels = c("m1_nzl",
                 "m2_bgd",
                 "m3_lka",
                 "m4_zaf",
                 "sf_ind",
                 "f_zaf"))) %>%
  # Set factor order on ball_type_simple
  mutate(
    ball_type_simple = factor(
      ball_type_simple,
      levels = c("other", "dot_ball", "wicket")))

# Create data frames to draw pitch areas, draw wicket channels, and
# mark pitch zone labels in appropriate match facets
matches_darcie <- kapunda %>%
  distinct(match_label)
pitch_dim_darcie <- bind_cols(
  matches_darcie, pitch_dim)
wicket_channel_dim_darcie <- bind_cols(
  matches_darcie, wicket_channel_dim)
pitch_zone_labels_darcie <- pitch_zone_labels %>%
  mutate(match_label = "m1_nzl") %>%
  add_row(match_label = "m2_bgd") %>%
  add_row(match_label = "m3_lka") %>%
  add_row(match_label = "m4_zaf") %>%
  add_row(match_label = "sf_ind") %>%
  add_row(match_label = "f_zaf") %>%
  mutate(
    match_label = factor(
      match_label,
      levels = c("m1_nzl",
                 "m2_bgd",
                 "m3_lka",
                 "m4_zaf",
                 "sf_ind",
                 "f_zaf")))

# Prep to plot -----------------------------------------------------------------

# Import font from Google
font_add_google("Nanum Gothic Coding", "nanum")
showtext_auto()

# Set custom parameters
match_label_display <- c(
  "m1_nzl" = "Match 1 vs. New Zealand",
  "m2_bgd" = "Match 2 vs. Bangladesh",
  "m3_lka" = "Match 3 vs. Sri Lanka",
  "m4_zaf" = "Match 4 vs. South Africa",
  "sf_ind" = "Semi vs. India",
  "f_zaf" = "Final vs. South Africa")

ball_type_colours <- c(
  "other" = "#000000",
  "dot_ball" = "#FFFFFF",
  "wicket" = "red")

# Build plot -------------------------------------------------------------------

ggplot() +
  # Draw pitch area
  geom_rect(
    data = pitch_dim_darcie,
    aes(
      xmin = pitch_xmin, xmax = pitch_xmax,
      ymin = pitch_ymin, ymax = pitch_ymax),
    fill = "#E3DCAC") +
  # Draw wicket channel
  geom_rect(
    data = wicket_channel_dim_darcie,
    aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#F9F7F1") +
  # Draw stump line at striker's end
  # geom_hline(yintercept = 1890, linewidth = 2, colour = "#FFFFFF") +
  # Mark stumps at striker's end
  geom_line(
    data = stump_dim %>% filter(end == "batter"),
    aes(x = x, y = y),
    linewidth = 2) +
  # Draw popping crease line at striker's end
  geom_hline(
    yintercept = pitch_dim$popping_crease_to_behind_stumps - 244,
    colour = "#FFFFFF") +
  # Draw 2 m from stumps - full to yorker
  geom_hline(yintercept = 1690, linetype = "dashed") +
  # Draw 6 m from stumps - good to full
  geom_hline(yintercept = 1290, linetype = "dashed") +
  # Draw 8 m from stumps - back of a length to good length
  geom_hline(yintercept = 1090, linetype = "dashed") +
  # Draw 10 m from stumps - halfway to back of a length
  geom_hline(yintercept = 890, linetype = "dashed") +
  # Draw popping crease line at bowler's end
  geom_hline(yintercept = 0, colour = "#FFFFFF") +
  # Draw stump line at bowler's end
  # geom_hline(yintercept = pitch_dim$stumps_at_bowlers_end, linewidth = 2, colour = "#FFFFFF") +
  # Mark stumps at bowler's end
  geom_line(
    data = stump_dim %>% filter(end == "bowler"),
    aes(x = x, y = y),
    linewidth = 2) +
  # Apply pitch zone labels
  geom_text(
    data = pitch_zone_labels_darcie,
    aes(x = x, y = y, label = label),
    size = 15, hjust = 1, family = "nanum", fontface = "bold") +
  # Draw pitch map - other balls
  geom_point(
    data = kapunda %>% filter(ball_type_simple == "other"),
    aes(x = bowling_params_pitch_x_cm, y = bowling_params_pitch_y_cm,
        colour = ball_type_simple, group = ball_type_simple),
    size = 3, shape = 16) +
  # Draw pitch map - dot balls
  geom_point(
    data = kapunda %>% filter(ball_type_simple == "dot_ball"),
    aes(x = bowling_params_pitch_x_cm, y = bowling_params_pitch_y_cm,
        colour = ball_type_simple, group = ball_type_simple),
    size = 3, shape = 16) +
  # Draw pitch map - wicket balls
  geom_point(
    data = kapunda %>% filter(ball_type_simple == "wicket"),
    aes(x = bowling_params_pitch_x_cm, y = bowling_params_pitch_y_cm,
        colour = ball_type_simple, group = ball_type_simple),
    size = 4.5, shape = 16) +
  facet_wrap(
    ~match_label, nrow = 1,
    labeller = labeller(match_label = match_label_display)) +
  labs(
    title = "T20 World Cup 2023: Pitch map - Darcie Brown (AUS)",
    caption = "Plot by @jacquietran@mastodon.social | Data via Sportradar") +
  scale_colour_manual(
    values = ball_type_colours,
    breaks = c("wicket", "dot_ball", "other"),
    labels = c("Wicket", "Dot ball", "Other")) +
  scale_y_continuous(
    limits = c(pitch_dim$stumps_at_bowlers_end - 160, pitch_dim$pitch_length)) +
  scale_x_continuous(
    limits = c(-20.5, pitch_dim$pitch_width + 20.5)) +
  theme_void() +
  theme(
    text = element_text(family = "nanum"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#D2D9DA", colour = "#D2D9DA"),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1.5, 'cm'),
    legend.box.margin = margin(t = 15, unit = "pt"),
    legend.text = element_text(size = rel(5), margin = margin(l = -10, r = 20, unit = "pt")),
    legend.spacing.x = unit(0.2, 'cm'),
    plot.title = element_text(size = rel(8), face = "bold", margin = margin(b = 30, unit = "pt")),
    strip.text = element_text(size = rel(5), face = "bold", margin = margin(b = 10, unit = "pt")),
    plot.caption = element_text(size = rel(4), margin = margin(t = 40, unit = "pt")),
    panel.spacing = unit(3, "lines"),
    panel.background = element_rect(fill = "darkgreen", colour = "darkgreen"),
    plot.margin = margin(1,1,1,1, unit = "cm"),
    plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 7)))

# Save to file
ggsave(
  here::here("t20wc_bowling_darcie_pitch_map_20230304.png"), last_plot(),
  width = 24, height = 12, units = "in", dpi = 300)

# showtext_auto(FALSE)