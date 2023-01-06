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

# # Data Cleaning
# info <- week |>
#   filter(team == 'football') |>
#   select(gameId, playId, frame = frameId, event, fbx = x, fby = y) |>
#   distinct() |>
#   mutate(is_start = as.numeric(event %in% c("autoevent_ballsnap", "ball_snap")),
#          is_end = as.numeric(event %in% c("fumble", "handoff", "lateral",
#                                           "autoevent_passforward", "pass_forward",
#                                           "qb_sack", "qb_strip_sack", "run"))) |>
#   group_by(gameId, playId) |>
#   mutate(any_start = any(is_start == 1), any_end = any(is_end == 1)) |>
#   filter(any_start, any_end) |>
#   summarize(losx = fbx[which(is_start==1)[1]],
#             losy = fby[which(is_start==1)[1]],
#             start_frame = frame[which(is_start == 1)[1]],
#             end_frame = frame[which(is_end == 1 & frame > start_frame)[1]], .groups = "drop")
# 
# qb <- week |>
#   filter(pff_role == 'Pass') |>
#   select(gameId, playId, frame = frameId, qbx = x, qby = y, qbs1 = s, qbdir = dir,
#          qbId = nflId, playDirection) |>
#   left_join(info, by = c('gameId', 'playId')) |>
#   filter(!is.na(start_frame), !is.na(end_frame),
#          frame >= start_frame, frame <= end_frame) |>
#   distinct() |>
#   mutate(qbs1 = qbs * 2.04545,
#          qbx1 = ifelse(playDirection=='right', losx - qbx, qbx - losx),
#          qby1 = ifelse(playDirection=='right', qby - losy, losy - qby),
#          qbdir1 = ifelse(playDirection == "left" & qbdir < 180, qbdir+180, 
#                          ifelse(playDirection == "left" & qbdir > 180, qbdir-180, qbdir))) |>
#   select(gameId, playId, frame, qbId, qbx1, qby1, qbs, qbdir1)
# 
# rush <- week |>
#   filter(pff_role == 'Pass Rush') |>
#   select(gameId, playId, frame = frameId, defId = nflId, defx = x, defy = y, defs = s,
#          defdir = dir, playDirection) |>
#   left_join(info, by = c('gameId', 'playId')) |>
#   filter(!is.na(start_frame), !is.na(end_frame),
#          frame >= start_frame, frame <= end_frame) |>
#   distinct() |>
#   mutate(defs1 = defs * 2.04545,
#          defx1 = ifelse(playDirection=='right', losx - defx, defx - losx),
#          defy1 = ifelse(playDirection=='right', defy - losy, losy - defy),
#          defdir1 = ifelse(playDirection == "left" & defdir < 180, defdir+180, 
#                          ifelse(playDirection == "left" & defdir > 180, defdir-180, defdir))) |>
#   select(gameId, playId, frame, defId, defx1, defy1, defs1, defdir1)
# 
# block <- week |>
#   filter(pff_role == 'Pass Block') |>
#   select(gameId, playId, frame = frameId, offId = nflId, offx = x, offy = y, offs = s,
#          offdir = dir, playDirection) |>
#   left_join(info, by = c('gameId', 'playId')) |>
#   filter(!is.na(start_frame), !is.na(end_frame),
#          frame >= start_frame, frame <= end_frame) |>
#   distinct() |>
#   mutate(offs1 = offs * 2.04545,
#          offx1 = ifelse(playDirection=='right', losx - offx, offx - losx),
#          offy1 = ifelse(playDirection=='right', offy - losy, losy - offy),
#          offdir1 = ifelse(playDirection == "left" & offdir < 180, offdir+180, 
#                          ifelse(playDirection == "left" & offdir > 180, offdir-180, offdir))) |>
#   select(gameId, playId, frame, offId, offx1, offy1, offs1, offdir1) |>
#   group_by(gameId, playId, frame) |>
#   nest() |>
#   ungroup()
# 
# 
# rm(week, plays0, prep_players, press)
# 
# lev <- rush |>
#   inner_join(qb, by = c("gameId", "playId", "frame")) |>
#   inner_join(block, by = c('gameId', 'playId', 'frame')) 
# 
# rm(info, qb, rush, block)
# 
# lev1 <- lev |>
#   unnest(data) |>
#   rowwise() |>
#   mutate(dist = sqrt((defx1 - offx1)^2 + (defy1 - offy1)^2)) |>
#   ungroup() |>
#   group_by(gameId, playId, frame, defId) |>
#   slice(which.min(dist)) |>
#   ungroup()
#   
# write.csv(lev1, "data/distance.csv", row.names = FALSE)

lev1 <- read.csv("data/distance.csv")
# Find Expected Position of QB in 0.5 Seconds
lev0 <- lev1 |>
  mutate(blocked = ifelse(dist < 2, 1, 0),
         qbs = qbs1/2.04545) |>
  mutate(angle = ifelse(qbdir1 < 90, 90-qbdir1, ifelse(qbdir1 > 90 & qbdir1 < 180, qbdir1-90, ifelse(qbdir1 > 180 & qbdir1 < 270, 270-qbdir1, qbdir1-270)))) |>
  mutate(x_change = ifelse(qbdir1 < 180, sin((angle*pi)/180)*(qbs/2), -sin((angle*pi)/180)*(qbs/2))) |>
  mutate(y_change = ifelse(qbdir1 > 90 & qbdir1 < 270, -cos((angle*pi)/180)*(qbs1/2), cos((angle*pi)/180)*(qbs1/2))) |>
  mutate(x_qb_exp = qbx1+x_change) |>
  mutate(y_qb_exp = qby1+y_change)


# Blocker Leverage
pRB = sqrt((lev0$x_qb_exp - lev0$offx1)^2 + (lev0$y_qb_exp - lev0$offy1)^2)
pRD = sqrt((lev0$x_qb_exp - lev0$defx1)^2 + (lev0$y_qb_exp - lev0$defy1)^2)
pBD = sqrt((lev0$defx1 - lev0$offx1)^2 + (lev0$defy1 - lev0$offy1)^2)

block_leverage = acos((pRB^2 + pRD^2 - pBD^2)/(2*pRB*pRD))
lev0$block_lev_deg = block_leverage*180/pi



# Defender Leverage
DRp = sqrt((lev0$x_qb_exp - lev0$defx1)^2 + (lev0$y_qb_exp - lev0$defy1)^2)
DR = sqrt((lev0$qbx1 - lev0$defx1)^2 + (lev0$qby1 - lev0$defy1)^2)
DB = sqrt((lev0$defx1 - lev0$offx1)^2 + (lev0$defy1 - lev0$offy1)^2)
RB = sqrt((lev0$qbx1 - lev0$offx1)^2 + (lev0$qby1 - lev0$offy1)^2)
RR = sqrt((lev0$qbx1 - lev0$x_qb_exp)^2 + (lev0$qby1 - lev0$y_qb_exp)^2)

theta1 = acos((DB^2 + DR^2 - RB^2)/(2*DB*DR))
theta2 = acos((DR^2 + DRp^2 - RR^2)/(2*DR*DRp))

delta = theta2-theta1
lev0$rush_lev_deg = delta*180/pi

lev0$block_wt = lev0$block_lev_deg * lev0$blocked
lev0$rush_wt = lev0$rush_lev_deg * lev0$blocked

final <- lev0 |>
  select(gameId, playId, frame, defId, offId, dist_pr_pb = dist, blocked, block_lev_deg, rush_lev_deg, block_wt, rush_wt) |>
  inner_join(block, by = c('gameId', 'playId', 'offId'))

write.csv(final, "data/leverage.csv", row.names = FALSE)