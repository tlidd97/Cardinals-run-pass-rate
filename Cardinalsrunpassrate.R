install.packages("nflreadr")
install.packages("tidyverse")
install.packages("gt")
install.packages("gtExtras")
install.packages("nflfastR")

library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)
library(nflfastR)

pbp <- nflreadr::load_participation(2023, include_pbp = TRUE)

names(pbp)

pbp <- pbp |>
  separate_rows(offense_players, sep = ";") |>
  select(old_game_id, play_id, posteam, defteam, week, offense_players, offense_personnel, offense_formation,
         rush, pass, wp, down, ydstogo, yardline_100, score_differential, pass_oe)

pbp <- pbp |>
  filter( rush == 1 | pass == 1)

rosters <- load_rosters(2023)

pbp <- left_join(pbp, rosters |> select(gsis_id, full_name, position), by = c('offense_players' = 'gsis_id'))

pbp |>
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |>
  group_by(full_name) |>
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
    ) |> filter(passrate >= 0.8 | passrate <= 0.2, plays >= 50) |>
  arrange(-passrate)


pbp |> 
  filter(wp >= 0.025 & wp <= 0.975, position %in% c('HB', 'WR', 'TE', 'FB')) |> 
  group_by(full_name, down) |> 
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  ) |> filter(passrate >= 0.8 | passrate <= 0.2, plays >= 20, down %in% c(1, 2)) |> 
  arrange(-passrate)

pbp |>
  filter(posteam == 'ARI', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2),
         yardline_100 >= 14) |>
  group_by(play_id) |>
  mutate(
    McBridein = any(full_name == "Trey McBride"),
    Moorein = any(full_name == 'Rondale Moore')
    ) |> slice(1) |>
  group_by(McBridein, Moorein) |>
  summarise(
    passrate = mean(pass, na.rm = T),
    plays = n(),
    PROE = mean(pass_oe, na.rm = T)
  )
 
ARI_runpass <- pbp |>
  filter(posteam == 'ARI', wp >= 0.025 & wp <= 0.975, down %in% c(1, 2),
         position %in% c('HB', 'WR', 'TE', 'FB')) |>
  group_by(full_name, position, posteam) |>
  summarise(
    snapsplayed = n(),
    passrate = mean(pass, na.rm = T),
    rushrate = mean(rush, na.rm = T),
    PROE = mean(pass_oe, na.rm = T)/100
  ) |>
  left_join(teams_colors_logos |> select(team_abbr, team_wordmark), by = c('posteam' = 'team_abbr'))

ARItbl <- ARI_runpass |>
  select(full_name, position, team_wordmark, snapsplayed, passrate, rushrate, PROE) |>
  ungroup() |>
  arrange(-snapsplayed) |>
  gt() |>
  gt_img_rows(team_wordmark) |>
  fmt_percent(columns = c(passrate, rushrate, PROE), decimals = 2) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Taylor Liddicoat | Data: nflreadr") |>
  cols_label(
    full_name = "Player",
    position = "Position",
    team_wordmark = "Offense",
    snapsplayed = "Snaps",
    passrate = "Pass Rate",
    rushrate = "Rush Rate",
    PROE = "Pass Rate Over Expected"
  ) |>
  opt_row_striping() |>
  tab_header(title = "Cardinals run/pass rate by player in 2023") |>
  gt_theme_pff()
gtsave(ARItbl, "ARIrunpasstbl.png")
 
#I am following Arjun Menon's youtube video on the MFANs channel but use the Arizona Cardinlas instead of the Buffalo Bills#
  
  
  
  
  

  
BUFtbl <- BUF_runpass |>
  select(full_name, position, team_wordmark, snapsplayed, passrate, rushrate, PROE) |>
  ungroup() |>
  arrange(-snapsplayed) |>
  gt() |>
  gt_img_rows(team_wordmark) |>
  fmt_percent(columns = c(passrate, rushrate, PROE), decimals = 2) |>
  opt_align_table_header("center") |>
  cols_align("center") |>
  tab_source_note("Table: Taylor Liddicoat | Data: nflreadr") |>
  cols_label(
    full_name = "Player",
    position = "Position",
    team_wordmark = "Offense",
    snapsplayed = "Snaps",
    passrate = "Pass Rate",
    rushrate = "Rush Rate",
    PROE = "Pass Rate Over Expected"
  ) |>
  opt_row_striping() |>
  tab_header(title = "Bills run/pass rate by player in 2023") |>
  gt_theme_pff()
gtsave(BUFtbl, "BUFrunpasstbl.png")





