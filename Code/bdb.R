rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflplotR)
library(ggrepel) # better labels
library(nflfastR)
library(ggthemes) # custom pre-built themes
library(scales)
library(signs)
library(ggrepel) # better labels
library(ggtext)
library(ggimage)
library(viridis)
library(gtExtras)
library(nflreadr)

setwd("~/BDB23")

#nflfastR
seasons <- 2021
nfl <- nflfastR::load_pbp(seasons) |>
  dplyr::filter(season_type=='REG', week<9, !is.na(epa)) |>
  #qb_dropback==1, add cp, air_epa & yac_epa... penalties??
  dplyr::select(gameId = old_game_id, playId = play_id, week, cp, cpoe, ep, epa, air_epa, yac_epa, wp, wpa, 
                secstogo = half_seconds_remaining, posteam_score, defteam_score) |>
  dplyr::mutate(gameId = as.integer(gameId),
                playId = as.integer(playId))

#PFF
pff <- read.csv("data/pffScoutingData.csv")

#Relative Pressure Rank
press <- pff |>
  group_by(gameId, playId) |>
  mutate(rushers = n_distinct(nflId[pff_role=='Pass Rush']),
         blockers = n_distinct(nflId[pff_role=='Pass Block'])) |>
  ungroup() |>
  filter(pff_role=='Pass Rush' | pff_role=='Coverage') |>
  select(c(gameId:pff_sack, rushers, blockers)) |>
  rename(hurry = pff_hurry, hit = pff_hit, sack = pff_sack) |>
  rowwise() |>  mutate(pressure = sum(hurry, hit, sack)) |>
  group_by(gameId, playId) |>
  mutate(tot_press = sum(pressure),
         tot_hurry = sum(hurry),
         tot_hit = sum(hit),
         tot_sack = sum(sack),
         r_bratio = rushers/blockers) |>
  ungroup() |>
  arrange(gameId, playId) |>
  group_by(nflId) |>
  mutate(cumpress = cumsum(pressure)) |>
  ungroup() |>
  filter(pff_role=='Pass Rush') |>
  select(-c(pff_role))
#Block Type
block <- pff |>
  filter(pff_role=='Pass Block') |>
  select(gameId, playId, offId = nflId, blockType = pff_blockType) |>
  mutate(blockType = ifelse(is.na(blockType), "NA", blockType))

#Players
players <- read.csv("data/players.csv")
#Smaller Player File
prep_players <- players |>
  select(nflId, height, weight)

#Plays
plays <- read.csv("data/plays.csv")
#Games
games <- read.csv("data/games.csv") |>
  select(gameId, homeTM = homeTeamAbbr, awayTM = visitorTeamAbbr)

plays0 <- plays |>
  inner_join(nfl, by = c('gameId', 'playId')) |>
  inner_join(games, by = c('gameId'))

rm(nfl, games, players, plays)

multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  data.table::rbindlist(lapply(filenames, data.table::fread))
}


path <- "~/BDB23/week"
DF <- multmerge(path)
DF <- as.data.frame(DF)

week <- DF |>
  left_join(pff, by = c('gameId', 'playId', 'nflId')) |>
  inner_join(plays0, by = c('gameId', 'playId'))

rm(DF, pff)
