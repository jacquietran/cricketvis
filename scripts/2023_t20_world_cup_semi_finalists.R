# Load libraries ---------------------------------------------------------------

library(cricketdata)
library(dplyr)
library(stringr)
library(showtext)
library(ggplot2)
# Also uses {tibble}, {ggdark}, {here}

# Read data into R -------------------------------------------------------------

# Retrieve ball-by-ball data from Cricsheet
df <- fetch_cricsheet(
  type = "bbb",
  gender = "female",
  competition = "t20is")

# Tidy the data ----------------------------------------------------------------

# Subset to focus on group matches from the T20 World Cup 2023
t20wc <- df %>%
  filter(start_date >= "2023-02-11" & start_date <= "2023-02-22") %>%
  filter(venue %in% c(
    "Boland Park, Paarl",
    "Newlands, Cape Town",
    "St George's Park, Gqeberha"))

# Average runs per over by semi-finalist teams
t20wc_batting_semi_finalists <- t20wc %>%
  filter(batting_team %in% c("India", "Australia", "England", "South Africa")) %>%
  mutate(
    runs_scored = runs_off_bat + extras) %>%
  group_by(match_id, batting_team, over) %>%
  summarise(
    runs_per_over = sum(runs_scored),
    .groups = "keep") %>%
  ungroup() %>%
  group_by(batting_team, over) %>%
  summarise(
    runs_per_over_avg = mean(runs_per_over),
    counts_of_over_number = length(over),
    .groups = "keep") %>%
  ungroup() %>%
  mutate(
    semi_final_label = case_when(
      batting_team %in% c("India", "Australia") ~ "Teams in Semi-Final 1: AUS vs. IND",
      TRUE                                      ~ "Teams in Semi-Final 2: ENG vs. SA"),
    semi_final_code = case_when(
      str_detect(semi_final_label, "1") ~ "SF1",
      TRUE                              ~ "SF2"))

# Define over categories
batting_phases <- tibble::tribble(
  ~over_category, ~start, ~end,
  
  "Powerplay", 1, 6,
  "Middle", 6, 16,
  "Death", 16, 20)

# Prep to plot -----------------------------------------------------------------

# Import font from Google
font_add_google("Nanum Gothic Coding", "nanum")
showtext_auto()

# Define team colours
colour_aus <- "#FFD20A"
colour_ind <- "#1E90B3"
colour_eng <- "#DC0618"
colour_sa <- "#0CA341"

# Define common plot options
opt <- list(
  # Batting phases
  geom_rect(
    data = batting_phases,
    aes(xmin = start, xmax = end, fill = over_category),
    ymin = -Inf, ymax = Inf, alpha = 0.2),
  scale_fill_manual(
    values = c(
      "Powerplay" = "#FFFFFF",
      "Middle"    = "#9B9A9B",
      "Death"     = "#363536")),
  geom_text(
    aes(x = 3.5, y = 17, label = "POWERPLAY"),
    family = "nanum", fontface = "bold", size = 8,
    colour = "#E6E6E6"),
  geom_text(
    aes(x = 11, y = 17, label = "MIDDLE"),
    family = "nanum", fontface = "bold", size = 8,
    colour = "#E6E6E6"),
  geom_text(
    aes(x = 18, y = 17, label = "DEATH"),
    family = "nanum", fontface = "bold", size = 8,
    colour = "#E6E6E6"),
  # Styling
  scale_y_continuous(
    limits = c(0,17),
    breaks = seq(0,16, by = 4)),
  scale_x_continuous(
    breaks = seq(0,20, by = 1)),
  ggdark::dark_theme_minimal(),
  theme(
    text = element_text(family = "nanum"),
    plot.title = element_text(size = rel(4), face = "bold", margin = margin(b = 5, unit = "pt")),
    plot.subtitle = element_text(size = rel(2.5), margin = margin(b = 15, unit = "pt")),
    plot.caption = element_text(size = rel(1.2), margin = margin(t = 15, unit = "pt")),
    axis.text = element_text(size = rel(1.8)),
    axis.title = element_text(face = "bold"),
    axis.title.x = element_text(size = rel(2), margin = margin(t = 5, unit = "pt")),
    axis.title.y = element_text(size = rel(2), margin = margin(r = 5, unit = "pt")),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#333333", linewidth = 0.4),
    plot.background = element_rect(fill = "#131313", colour = "#131313"),
    plot.margin = margin(10,10,10,10, unit = "pt"),
    legend.position = "none"))

# Build plot: AUS vs. IND ------------------------------------------------------

# Build plot
aus_vs_ind <- ggplot() +
  opt +
  # Run rates
  geom_line(
    data = t20wc_batting_semi_finalists %>% filter(semi_final_code == "SF1"),
    aes(x = over, y = runs_per_over_avg, group = batting_team,
        colour = batting_team),
    linewidth = 1.5) +
  # Annotations for India
  annotate(
    geom = "curve",
    x = 1.8, xend = 1.35, y = 11.5, yend = 8,
    curvature = 0.4, arrow = arrow(length = unit(2, "mm")), colour = colour_ind) +
  geom_label(
    aes(x = 2.4, y = 11.2, label = "India"),
    family = "nanum", fontface = "bold", size = 8,
    colour = colour_ind, fill = "#292929", label.padding = unit(0.35, "lines"),
    label.size = 0.8) +
  # Annotations for Australia
  annotate(
    geom = "curve",
    x = 2, xend = 1.35, y = 2.5, yend = 4.4,
    curvature = -0.4, arrow = arrow(length = unit(2, "mm")), colour = colour_aus) +
  geom_label(
    aes(x = 3.1, y = 2.5, label = "Australia"),
    family = "nanum", fontface = "bold", size = 8,
    colour = colour_aus, fill = "#292929", label.padding = unit(0.35, "lines"),
    label.size = 0.8) +
  scale_colour_manual(
    values = c(
      "Australia" = colour_aus,
      "India" = colour_ind)) +
  labs(
    title = "Who will win: The fast starters or fast finishers?",
    subtitle = "T20 World Cup 2023: Average runs per over from group stage matches",
    x = "Over number", y = "Average runs scored",
    caption = "Plot by @jacquietran@mastodon.social | Data from Cricsheet.org via {cricketdata}")

# Save to file
ggsave(
  here::here("t20wc_aus_vs_ind_20230223.png"), aus_vs_ind,
  width = 6, height = 4, units = "in", dpi = 300)

# Build plot: ENG vs. SA -------------------------------------------------------

# Build plot
eng_vs_sa <- ggplot() +
  opt +
  # Run rates
  geom_line(
    data = t20wc_batting_semi_finalists %>% filter(semi_final_code == "SF2"),
    aes(x = over, y = runs_per_over_avg, group = batting_team,
        colour = batting_team),
    linewidth = 1.5) +
  # Annotations for England
  annotate(
    geom = "curve",
    x = 2, xend = 1.5, y = 12.2, yend = 9.8,
    curvature = 0.5, arrow = arrow(length = unit(2, "mm")), colour = colour_eng) +
  geom_label(
    aes(x = 2.9, y = 12.4, label = "England"),
    family = "nanum", fontface = "bold", size = 8,
    colour = colour_eng, fill = "#292929", label.padding = unit(0.35, "lines"),
    label.size = 0.8) +
  # Annotations for South Africa
  annotate(
    geom = "curve",
    x = 2, xend = 1.5, y = 1.2, yend = 3.5,
    curvature = -0.5, arrow = arrow(length = unit(2, "mm")), colour = colour_sa) +
  geom_label(
    aes(x = 3.4, y = 1.2, label = "South Africa"),
    family = "nanum", fontface = "bold", size = 8,
    colour = colour_sa, fill = "#292929", label.padding = unit(0.35, "lines"),
    label.size = 0.8) +
  scale_colour_manual(
    values = c(
      "England" = colour_eng,
      "South Africa" = colour_sa)) +
  labs(
    title = "Can the hosts break England's heady scoring rate?",
    subtitle = "T20 World Cup 2023: Average runs per over from group stage matches",
    x = "Over number", y = "Average runs scored",
    caption = "Plot by @jacquietran@mastodon.social | Data from Cricsheet.org via {cricketdata}")

# Save to file
ggsave(
  here::here("t20wc_eng_vs_sa_20230223.png"), eng_vs_sa,
  width = 6, height = 4, units = "in", dpi = 300)

showtext_auto(FALSE)
