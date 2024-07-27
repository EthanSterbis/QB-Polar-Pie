library(tidyverse)
library(nflreadr)
library(ggplot2)
library(cowplot)
library(magick)
library(grid)
library(nflfastR)
library(geomtextpath)

dictPart <- nflreadr::dictionary_participation
dictFTN <- nflreadr::dictionary_ftn_charting
dictPBP <- nflreadr::dictionary_pbp

# load rosters
rosters <- nflreadr::load_rosters(seasons = 2023) %>% 
  select(season, team, depth_chart_position, full_name,
         first_name, football_name, last_name, gsis_id,
         headshot_url) %>% 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# load depth chart position
depthChartPos <- rosters %>% select(gsis_id, depth_chart_position)

# loading all necessary metrics
ultStats2023 <- nflreadr::load_participation(2023, include_pbp = T) %>% 
  left_join(nflreadr::load_ftn_charting(2023), by = c("nflverse_game_id" = "nflverse_game_id",
                                                      "play_id" = "nflverse_play_id",
                                                      "week" = "week", "season" = "season"))

# step A
qbAPol <- ultStats2023 %>%
  left_join(depthChartPos, by = c("passer_id" = "gsis_id")) %>% 
  filter(depth_chart_position == "QB") %>% 
  filter(!is.na(passer_id), (rush == 1 | pass == 1), !is.na(down), qb_kneel == 0, qb_spike == 0) %>%
  group_by(passer_id) %>%
  mutate(tot_plays = n()) %>% 
  filter(tot_plays >= 50) %>%
  mutate(
    tot_plays = n(),
    tot_dropbacks = sum(qb_dropback, na.rm = TRUE),
    tot_scrambles = sum(qb_scramble, na.rm = TRUE),
    tot_rushes = sum(rush[passer_id == rusher_id], na.rm = TRUE),
    dropback_epa = mean(qb_epa[qb_dropback == 1], na.rm = TRUE),
    scramble_epa = mean(qb_epa[qb_scramble == 1], na.rm = TRUE),
    avg_air_yds = mean(air_yards, na.rm = TRUE),
    qb_fault_sack_rate = sum(ifelse(is_qb_fault_sack == "TRUE", 1, 0)) / tot_dropbacks,
    int_worthy_rate = sum(ifelse(is_interception_worthy == "TRUE", 1, 0)) / sum(pass_attempt, na.rm = TRUE),
    catchable_ball_rate = sum(ifelse(is_catchable_ball == "TRUE", 1, 0)) / sum(pass_attempt, na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  group_by(passer_id) %>%
  mutate(
    short_cpoe = mean(cpoe[air_yards <= 10], na.rm = TRUE),
    medium_cpoe = mean(cpoe[air_yards > 10 & air_yards <= 20], na.rm = TRUE),
    deep_cpoe = mean(cpoe[air_yards > 20], na.rm = TRUE)
  ) %>%
  summarise(
    season = first(season), 
    posteam = first(posteam), 
    passer_id = first(passer_id), 
    passer = first(passer),
    tot_plays = first(tot_plays), 
    tot_dropbacks = first(tot_dropbacks), 
    tot_scrambles = first(tot_scrambles), 
    tot_rushes = first(tot_rushes),
    dropback_epa = first(dropback_epa), 
    scramble_epa = first(scramble_epa), 
    avg_air_yds = first(avg_air_yds),
    qb_fault_sack_rate = first(qb_fault_sack_rate), 
    int_worthy_rate = first(int_worthy_rate), 
    catchable_ball_rate = first(catchable_ball_rate),
    cpoe = mean(cpoe, na.rm = TRUE), 
    short_cpoe = first(short_cpoe), 
    medium_cpoe = first(medium_cpoe), 
    deep_cpoe = first(deep_cpoe)
  ) %>% 
  unique() %>% 
  left_join(rosters, by  = c("season" = "season", "posteam" = "team", "passer_id" = "gsis_id"))

# calculate designed QB rush EPA
qbRushEpa <- ultStats2023 %>% 
  filter(rush == 1, qb_scramble == 0) %>% 
  left_join(depthChartPos, by = c("rusher_id" = "gsis_id")) %>% 
  filter(depth_chart_position == "QB") %>% 
  ungroup() %>% 
  group_by(rusher_id) %>% 
  mutate(rush_epa = mean(epa[rush == 1], na.rm = T)) %>% 
  summarise(rusher_id, rush_epa) %>% 
  unique()

# step B
qbBPol <- qbAPol %>% 
  left_join(qbRushEpa, by = c("passer_id" = "rusher_id")) %>% 
  ungroup() %>% 
  group_by(passer_id) %>% 
  mutate(posteam = max(posteam)) %>% 
  ungroup() %>% 
  unique() %>% 
  filter(!is.na(full_name))

# step C (add percentiles)
qbCPol <- qbBPol %>% 
  ungroup() %>% 
  mutate(
    pctile_tot_plays = percent_rank(tot_plays),
    pctile_tot_dropbacks = percent_rank(tot_dropbacks),
    pctile_tot_scrambles = percent_rank(tot_scrambles),
    pctile_tot_rushes = percent_rank(tot_rushes),
    pctile_dropback_epa = percent_rank(dropback_epa),
    pctile_scramble_epa = percent_rank(scramble_epa),
    pctile_avg_air_yards = percent_rank(avg_air_yds),
    pctile_qb_fault_sack_rate = 1 - ifelse(tot_dropbacks >= 100, percent_rank(qb_fault_sack_rate), NA_real_),
    pctile_int_worthy_rate = 1 - percent_rank(int_worthy_rate),
    pctile_catchable_ball_rate = percent_rank(catchable_ball_rate),
    pctile_cpoe = percent_rank(cpoe),
    pctile_short_cpoe = percent_rank(short_cpoe),
    pctile_medium_cpoe = percent_rank(medium_cpoe),
    pctile_deep_cpoe = percent_rank(deep_cpoe),
    pctile_rush_epa = percent_rank(rush_epa)
         )

# input options
focusQBList <- qbAPol %>% 
  summarise(passer)# %>% 
  #pull(passer)

# select focusQB
focusQB <- "A.O'Connell"

# get data for focusQB
focusQBPctiles <- qbCPol %>% 
  filter(passer == focusQB) %>% 
  select(season, posteam, passer_id, passer,
         pctile_dropback_epa, pctile_scramble_epa, pctile_rush_epa,
         pctile_avg_air_yards, pctile_qb_fault_sack_rate,
         pctile_int_worthy_rate, pctile_catchable_ball_rate,
         pctile_cpoe, pctile_short_cpoe, pctile_medium_cpoe, pctile_deep_cpoe,
         first_name, football_name, last_name, full_name,
         headshot_url, team_name, team_nick, team_color, team_color2, team_color3, team_color4,
         team_logo_wikipedia, team_logo_espn, team_wordmark
         )

# make row for each percentile metric
focusQBLong <- focusQBPctiles %>% 
  pivot_longer(cols = starts_with("pctile_"),
               names_to = "stat",
               values_to = "value") %>% 
  # create index column to reorder
  mutate(stat = recode(stat,
                       pctile_dropback_epa = "Dropback EPA",
                       pctile_scramble_epa = "Scramble EPA",
                       pctile_rush_epa = "Rush EPA",
                       pctile_avg_air_yards = "Avg Air Yds",
                       pctile_qb_fault_sack_rate = "QB Fault Sack Rate",
                       pctile_int_worthy_rate = "INT Worthy Rate",
                       pctile_catchable_ball_rate = "Catchable Ball Rate",
                       pctile_cpoe = "CPOE",
                       pctile_short_cpoe = "Short CPOE",
                       pctile_medium_cpoe = "Medium CPOE",
                       pctile_deep_cpoe = "Deep CPOE")) %>% 
  mutate(index = case_when(
    stat == "Dropback EPA" ~ 1,
    stat == "Scramble EPA" ~ 2,
    stat == "Rush EPA" ~ 3,
    stat == "Avg Air Yds" ~ 4,
    stat == "QB Fault Sack Rate" ~ 5,
    stat == "INT Worthy Rate" ~ 6,
    stat == "Catchable Ball Rate" ~ 7,
    stat == "CPOE" ~ 8,
    stat == "Short CPOE" ~ 9,
    stat == "Medium CPOE" ~ 10,
    stat == "Deep CPOE" ~ 11,
  )) %>% 
  mutate(value = round(value * 100, 1)) %>% 
  arrange(index)

# get player headshot
focus_player_headshot_url <- focusQBLong$headshot_url[1]
focus_player_headshot <- image_read(focus_player_headshot_url)
focus_player_headshot_grob <- rasterGrob(as.raster(focus_player_headshot), interpolate = TRUE)

# get team logo
focus_player_team_logo_url <- focusQBLong$team_logo_espn[1]
focus_player_team_logo <- image_read(focus_player_team_logo_url)
focus_player_team_logo_grob <- rasterGrob(as.raster(focus_player_team_logo), interpolate = TRUE)

# gray for NA values
focusQBLong <- focusQBLong %>%
  mutate(fill_color = ifelse(is.na(value), "gray40", team_color2))

plotFocusQB <- ggplot(data = focusQBLong,
                                   aes(x = reorder(stat, index),
                                       y = value, label = value,
                                       fill = team_color)) +
  geom_bar(aes(y = 100, fill = fill_color), stat = "identity", width = 1, alpha = 1) +
  geom_bar(data = focusQBLong, width = 1,
           color = "black", stat = "identity") +
  coord_curvedpolar() +
  geom_hline(yintercept = seq(0, 100, by = 100),
             color = "black", size = 0.64) +
  geom_vline(xintercept = seq(0.5, 11, by = 1),
             color = "black",
             size = 0.64) +
  geom_label(color = "black", fill = "#f7f7f7", size = 6, fontface = "bold", show.legend = F) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "gray24", color="gray24"),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#f7f7f7", face = "bold"),
        legend.key.size = unit(.5, "cm"),
        legend.box.spacing = unit(0, "mm"),
        plot.title = element_text(hjust = .5, colour = "#f7f7f7", face = "bold", size = 18),
        plot.subtitle = element_text(hjust = .5, colour = "#f7f7f7", size = 14),
        plot.caption = element_text(hjust = .5, colour = "#f7f7f7", size = 12),
        plot.caption.position = "panel",
        plot.background = element_rect(fill = "gray24", color="#f7f7f7"),
        panel.background = element_rect(fill = "gray24", color="gray24"),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold", size = 6.8, colour = "#f7f7f7"),
        axis.title = element_blank(),
        axis.text.x = element_text(face = "bold", size = 14.8),) +
  labs(title = "QB Name: Metrics by Percentile",
       subtitle = "2023 NFL Season, percentiles against QBs with at least 50 plays",
       caption = "@EthanSterbis on X
       data via nflreadR & FTN") +
  scale_fill_identity()

# overlay the images on final plot
final_plot <- ggdraw() +
  draw_plot(plotFocusQB) +
  draw_image(focus_player_headshot, x = 0.028, y = 0.784, width = 0.22, height = 0.22) +
  draw_image(focus_player_team_logo, x = 0.76, y = 0.78, width = 0.18, height = 0.18)

print(final_plot)

lastName <- str_remove(focusQB, '..')
lastName <- str_remove(lastName, "'") 
lastName <- str_to_lower(lastName)

image_file_name <- paste0(lastName, "-qb-polar-pie.jpeg")

# save plot
#ggsave(image_file_name, plot = final_plot, height = 11.2, width = 10.15, dpi = "retina", device = "jpeg", bg = "transparent")
