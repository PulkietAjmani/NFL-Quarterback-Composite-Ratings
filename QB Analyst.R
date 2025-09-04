# === Load Libraries ===
library(nflfastR)
library(dplyr)
library(lme4)
library(gt)
library(gtExtras)
library(readr)
library(scales)

# === 1. Load & Filter QB Play-by-Play Data ===
qb_pbp <- load_pbp(seasons = 2024) %>%
  filter(season_type == "REG", qb_dropback == 1, !is.na(passer_player_id)) %>%
  mutate(passer_player_name = paste0(passer_player_id, "_", posteam)) %>%
  select(epa, yardline_100, down, ydstogo, shotgun, no_huddle, 
         quarter_seconds_remaining, half_seconds_remaining, qtr, wp, 
         passer_player_name, posteam, defteam)

# === 2. Mixed Effects Model for Situational Performance ===
mixed_model <- lmer(
  epa ~ yardline_100 + quarter_seconds_remaining + half_seconds_remaining +
    qtr + down + ydstogo + shotgun + no_huddle + wp + 
    (1 | passer_player_name) + (1 | posteam) + (1 | defteam), 
  data = qb_pbp
)

# === 3. Extract Random Effects ===
qb_effects <- as.data.frame(ranef(mixed_model)$passer_player_name)
qb_effects$passer_player_name <- rownames(qb_effects)
colnames(qb_effects)[1] <- "situational_coef"
qb_effects$posteam <- sub(".*_", "", qb_effects$passer_player_name)
qb_effects <- qb_effects %>% mutate(passer_player_id = sub("_.*", "", passer_player_name))

# === 4. Join with Roster Info ===
rosters <- fast_scraper_roster(2024) %>%
  select(passer_player_id = gsis_id, name = full_name, headshot = headshot_url)

qb_effects <- qb_effects %>% left_join(rosters, by = "passer_player_id")

# === 5. Add Team Logos ===
teams <- teams_colors_logos %>%
  select(team = team_abbr, logo = team_logo_espn)

qb_effects <- qb_effects %>% left_join(teams, by = c("posteam" = "team"))

# === 6. Identify Starters ===
starter_qbs <- qb_pbp %>%
  group_by(passer_player_name, posteam) %>%
  summarize(dropbacks = n(), .groups = "drop") %>%
  arrange(posteam, desc(dropbacks)) %>%
  group_by(posteam) %>%
  filter(row_number() == 1) %>%
  ungroup()

qb_effects <- qb_effects %>%
  inner_join(starter_qbs, by = c("passer_player_name", "posteam"))

# === 7. Load Full Extended Inputs ===
context_data <- read_csv("final_qb_rankings.csv")

# === 8. Final Composite Score Calculation ===
qb_df <- qb_effects %>%
  select(logo, team = posteam, headshot, name, dropbacks, situational_coef) %>%
  left_join(context_data, by = "name") %>%
  mutate(
    situational_scaled = rescale(situational_coef, to = c(0, 50)),
    final_score = situational_scaled +
                  qb_stats_score +                  # 0–25
                  cold_weather_bonus +              # 0–10
                  opponent_def_strength +           # 0–10
                  consensus_rank_score -            # 0–10
                  (pro_bowlers * 1.5) +             # penalty
                  games_played_penalty +            # -10 if < 9 games
                  comeback_bonus +                  # +2 each
                  winning_situation_count / 2 +     # scaled bonus
                  rushing_score +                   # mobility factor
                  clutchness_score +                # late-game performance
                  leadership_score,                 # intangible wins
    final_score = pmin(pmax(final_score, 0), 100),
    final_score = round(final_score),
    rank = rank(-final_score)
  ) %>%
  arrange(rank)

# === 9. Generate GT Table ===
caption <- htmltools::HTML('<span style="float: left;">Data: nflverse + Manual Inputs</span><span style="float: right;">Model by YOU</span>')

gt_qbs <- qb_df %>%
  select(rank, logo, team, headshot, name, dropbacks, final_score) %>%
  gt(id = "qb_rating_table") %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_img_rows(columns = headshot, height = 50) %>%
  cols_label(
    rank = md("**Rank**"),
    logo = "",
    team = md("**Team**"),
    headshot = "",
    name = md("**QB**"),
    dropbacks = md("**Dropbacks**"),
    final_score = md("**Final Rating**")
  ) %>%
  gt_theme_538(quiet = TRUE) %>%
  cols_align(align = "center", columns = everything()) %>%
  gt_hulk_col_numeric(columns = final_score, domain = c(0, 100)) %>%
  tab_header(
    title = md("**2024 NFL Quarterback Composite Ratings**"),
    subtitle = md("*Performance, Context, Leadership, & Intangibles*")
  ) %>%
  tab_source_note(source_note = caption)

# === 10. Save to PNG ===
gtsave(gt_qbs, "2024_qb_composite_full_ratings.png", vwidth = 4000, vheight = 3000)
