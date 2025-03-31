# Load libraries ---------------------------------------------------------------

# credentials::set_github_pat()
# remotes::install_github("jacquietran/sportradar")
library(sportradar)
library(dplyr)
library(showtext)
library(ggplot2)
# Also uses {rstudioapi}, {here}, {ggpubr}, {stringr}

# Retrieve data from Sportradar ------------------------------------------------

# Interactively specify API key
key <- rstudioapi::askForSecret(
  name = "Enter Sportradar API key below",
  title = "Sportradar API key")

bbb_data <- fetch_sportradar_cricket(
  api_key = key,
  endpoint = "match_timeline",
  match_id = "39425841", # 2023-03-05 UP Warriorz vs. Gujarat Giants
  timeline_output = "bbb")

# Tidy the data ----------------------------------------------------------------

garris <- bbb_data %>%
  filter(batting_params_striker_name == "Harris, Grace") %>%
  mutate(
    ball_number_for_garris = row_number(),
    batting_params_striker_runs_scored_so_far = batting_params_striker_runs_scored_so_far,
    extras_label = case_when(
      !is.na(bowling_params_extra_runs_type) ~ bowling_params_extra_runs_type,
      TRUE                                   ~ NA_character_))

# Prep to plot -----------------------------------------------------------------

# Import font from Google
font_add_google("Nanum Gothic Coding", "nanum")
showtext_auto()

# Set custom colours
ball_type_colours <- c(
  "ball" = "#FFFFFF",
  "boundary" = "#8B3EA7",
  "six" = "#FFC857")

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_line(
    data = garris,
    aes(
      x = ball_number_for_garris,
      y = batting_params_striker_runs_scored_so_far),
    linewidth = 1.5) +
  geom_point(
    data = garris,
    aes(
      x = ball_number_for_garris,
      y = batting_params_striker_runs_scored_so_far,
      group = batting_params_striker_name, colour = timeline_type),
    size = 6) +
  geom_text(
    data = garris,
    aes(
      x = ball_number_for_garris,
      y = batting_params_striker_runs_scored_so_far,
      label = extras_label),
    size = 12, colour = "#FFFFFF", family = "nanum", fontface = "bold",
    nudge_y = 2.2) +
  ggpubr::geom_bracket(
    xmin = 16, xmax = 28, y.position = 17, label = "",
    tip.length = c(-0.06, -0.65), colour = "#FFFFFF", size = 0.8) +
  geom_label(
    aes(x = 22, y = 16.5),
    label = stringr::str_wrap(
      "From ball 16 onwards, Harris struck 5 fours and 3 sixes, powering the UP Warriorz to a successful run chase in their first ever Women's Premier League match.",
      width = 45),
    size = 12, lineheight = 0.35, family = "nanum",
    label.padding = unit(0.7, "lines"), label.r = unit(0.6, "lines"),
    label.size = 0.8) +
  scale_x_continuous(
    limits = c(0, 28),
    breaks = seq(0, 25, by = 5)) +
  scale_colour_manual(
    values = ball_type_colours) +
  labs(
    title = "Women's Premier League: Grace Harris flicks the switch",
    subtitle = "UP Warriorz vs. Gujarat Giants, 5 Mar 2023",
    x = "Balls faced", y = "Cumulative runs scored",
    caption = "Plot by @jacquietran@mastodon.social | Data via Sportradar") +
  ggdark::dark_theme_minimal() +
  theme(
    text = element_text(family = "nanum"),
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1.5, 'cm'),
    legend.text = element_text(size = rel(5), margin = margin(l = -10, r = 10, unit = "pt")),
    plot.title = element_text(size = rel(7), face = "bold", margin = margin(b = 20, unit = "pt")),
    plot.subtitle = element_text(size = rel(4.5), margin = margin(b = -27, unit = "pt")),
    axis.title.x = element_text(size = rel(5), margin = margin(t = 20, unit = "pt")),
    axis.title.y = element_text(size = rel(5), margin = margin(r = 20, unit = "pt")),
    axis.text = element_text(size = rel(4)),
    plot.caption = element_text(size = rel(4), margin = margin(t = 30, unit = "pt")),
    panel.grid.major = element_line(colour = "#434C43"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#131313", colour = "#131313"),
    plot.margin = margin(1,1.5,1,1, unit = "cm"))
  
# Save to file
ggsave(
  here::here("20230306_wpl_grace_harris.png"), last_plot(),
  width = 12, height = 9, units = "in", dpi = 300)