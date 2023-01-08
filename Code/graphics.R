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

#Roster
roster <- nflreadr::load_rosters(seasons=2021) |>
  select(c(gsis_id, full_name))
#Players
players <- read.csv("data/players.csv")
#Leverage
leverage <- read.csv('data/leverage.csv')
#Data
total <- read.csv("data/cluster.csv")

total0 <- total |>
  filter(cmove!="OOB", !is.na(cmove), dropBackType!="DESIGNED_RUN", dropBackType!="UNKNOWN") |>
  rowwise() |>
  mutate(posdif = (posteam_score - defteam_score)/8,
         poslead = ifelse(posteam_score>defteam_score, 1, 0),
         dist_to_passer = sqrt((defx - qbx)^2 + (defy - qby)^2)) |>
  ungroup() |>
  arrange(gameId, playId, frame) |>
  group_by(gameId, playId) |>
  mutate(adj_frame = frame - min(frame),
         seconds = adj_frame/10,
         mrush = n_distinct(nflId) 
  ) |>
  ungroup() |>
  inner_join(leverage, by = c('gameId', 'playId', 'frame', 'nflId' = 'defId')) |>
  mutate(blockType = ifelse(is.na(blockType), 'NA', blockType),
         rush_wt = ifelse(is.na(rush_wt) & blocked==0, 0, rush_wt),
         yardsToEZ = ifelse(playDirection=='right', 110 - losx, losx - 10),
         qb_side = ifelse(abs(qby - 53.33) < abs(qby - 0), abs(qby - 53.33), abs(qby - 0)),
         #Relative x and y position
         qbx1 = ifelse(playDirection=='right', losx - qbx, qbx - losx),
         qby1 = ifelse(playDirection=='right', qby - losy, losy - qby),
         defx1 = ifelse(playDirection=='right', losx - defx, defx - losx),
         defy1 = ifelse(playDirection=='right', defy - losy, losy - defy)
  ) |>
  filter(!is.na(rush_wt))

rm(total, leverage)

predict <- read.csv("data/predictions.csv")

rusher <- predict |>
  inner_join(total0, by = c("gameId", "playId", "nflId", "adj_frame")) |>
  inner_join(players, by = c ("nflId", "height", "weight")) |>
  mutate(pop = test_actual - test_pred_probs) |>
  distinct() 

rusher0 <- rusher |>
  group_by(gameId, playId, nflId) |>
  summarize(
    name = last(displayName),
    officialPosition = last(officialPosition),
    meanip = mean(defip),
    ip = ifelse(0<mean(defip[seconds<=2.5]), 1, 0),
    lev = sum(rush_wt),
    sack = mean(sack),
    hit = mean(hit),
    hurry = mean(hurry),
    press = mean(test_actual),
    xpress = mean(test_pred_probs),
    xpress_25less = mean(test_pred_probs[seconds<=2.5], na.rm=T),
    xpress_25more = mean(test_pred_probs[seconds>2.5 & seconds<=5], na.rm=T),
    pop = mean(pop),
    epa = mean(epa),
    maxspeed = max(defs),
    posteam = last(postm),
    team = last(team)
  ) |>
  ungroup() |>
  mutate(lev = ifelse(is.na(lev), 0, lev),
         mean_xpress = mean(xpress, na.rm=T),
         transPOP = ifelse(pop>0, (1 - pop),
                          ifelse(pop<0, (pop + 1) * -1, 0)),
         normPOP = (transPOP + 1)/2,
         zscore = scale(xpress, center = TRUE, scale = TRUE),
         zscore_25less = scale(xpress_25less, center = TRUE, scale = TRUE),
         zscore_25more = scale(xpress_25more, center = TRUE, scale = TRUE)) |>
  group_by(gameId, playId) |>
  mutate(sack = ifelse(sack==1 & sum(sack)>1, 0.5, sack)) |>
  ungroup() |>
  group_by(nflId) |>
  summarize(
    name = last(name),
    pos = last(officialPosition),
    plays = n(),
    team = last(team),
    inside_pocket = sum(ip),
    maxspeed = mean(maxspeed),
    lev = mean(lev, na.rm=T),
    Sack = sum(sack),
    Hit = sum(hit),
    Hurry = sum(hurry),
    Pressure = sum(press),
    xPressure = sum(xpress),
    mean_xPress = sum(mean_xpress),
    POP = sum(pop),
    Zscore = mean(zscore),
    Zscore_25less = mean(zscore_25less, na.rm=T),
    Zscore_25more = mean(zscore_25more, na.rm=T),
    NormPOP = mean(normPOP),
    npop_1 = mean(normPOP[press==1]),
    npop_0 = mean(normPOP[press==0]),
    finishrate = Sack/Pressure,
    pressure_rate = Pressure/plays,
    mip = mean(meanip)
  ) |>
  ungroup() |>
  filter(plays>14) |>
  group_by(pos) |>
  mutate(mlev = mean(lev)) |>
  ungroup() |>
  mutate(resistance = mlev - (lev))

write.csv(rusher0, "rusher.csv", row.names=FALSE)

# #Shiny App Data
# 
# id <- unique(rusher0$nflId)
# 
# player <- rusher |>
#   filter(nflId %in% id, adj_frame<51) |>
#   inner_join(teams_colors_logos, by = c('team' = 'team_abbr')) |>
#   group_by(nflId, adj_frame) |>
#   summarize(
#     player = last(displayName),
#     tm = last(team),
#     last_letter = str_sub(player, start = -1),
#     prob = mean(test_pred_probs),
#     n_plays = n(),
#     se = sd(test_pred_probs) / sqrt(n_plays),
#     team_color = last(team_color)
#   ) |>
#   ungroup() |>
#   group_by(nflId) |>
#   arrange(nflId, adj_frame) |>
#   mutate(yfit = predict(loess(prob ~ adj_frame, span = .75))) |>
#   ungroup() |>
#   inner_join(roster, by = c('player' = 'full_name')) |>
#   mutate(plot_name = ifelse(last_letter=='s', paste0(player, "'"), paste0(player, "'s")),
#          seconds = adj_frame/10)
# 
# avg <- rusher |>
#   filter(adj_frame<51) |>
#   group_by(adj_frame) |>
#   summarize(
#     prob = mean(test_pred_probs),
#     n_plays = n(),
#     se = sd(test_pred_probs) / sqrt(n_plays)
#   ) |>
#   ungroup() |>
#   mutate(seconds = adj_frame/10)
# 
# write.csv(player, "data/app/player.csv", row.names=FALSE)
# write.csv(avg, "data/app/avg.csv", row.names=FALSE)

rusher0 <- read.csv("rusher.csv")

#Reactive Table
r0 <- rusher0 |>
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) |>
  select(c(name, pos, plays, team_logo_espn, maxspeed, inside_pocket, resistance, Pressure, xPressure, POP)) |>
  mutate(maxspeed = round(maxspeed, digits = 2),
         resistance = round(resistance, digits=0),
         xPressure = round(xPressure, digits=2),
         POP = round(POP, digits=2)) 

library(reactablefmtr)

reactable(r0,
          pagination = TRUE,
          highlight = TRUE,
          striped = TRUE,
          defaultSorted = "Pressure",
          defaultSortOrder = "desc",
          theme = espn(),
          defaultPageSize = 15,
          defaultColDef = colDef(align = "center"),
          columns = list(
            name = colDef(name = "Name", maxWidth = 210, 
                          html = TRUE),
            pos = colDef(name = "Pos", maxWidth = 70, 
                         style = list(fontWeight = "bold")),
            plays = colDef(name = "Plays", maxWidth = 70, 
                           style = list(fontWeight = "bold")),
            team_logo_espn = colDef(name = "Team", maxWidth = 70,
                                    cell = embed_img(height = 20, width = 20)),
            maxspeed = colDef(name = "Max Speed", maxWidth = 90,
                              format = colFormat(digits = 2), 
                              style = list(fontWeight = "bold")),
            inside_pocket = colDef(name = "Pocket", maxWidth = 90, 
                                   style = list(fontWeight = "bold")),
            resistance = colDef(name = "Resistance", maxWidth = 90, 
                                style = list(fontWeight = "bold")),
            Pressure = colDef(maxWidth = 120, 
                              cell = color_tiles(r0,colors = viridis::viridis(5, direction = -1),
                                                 bold_text = TRUE,
                                                 box_shadow = TRUE)),
            xPressure = colDef(maxWidth = 120, 
                               cell = color_tiles(r0,colors = viridis::viridis(5, direction = -1), 
                                                  bold_text = TRUE,
                                                  box_shadow = TRUE,
                                                  number_fmt = scales::number_format(accuracy = 0.01))),
            POP = colDef(maxWidth = 300,
                         cell = data_bars(
                           data = r0,
                           text_position = 'outside-end',
                           bold_text = TRUE,
                           box_shadow = TRUE,
                           fill_color = viridis::viridis(5, direction = -1),
                           number_fmt = scales::number_format(accuracy = 0.01)
                         ))
          ))




#Applications
pop <- rusher0 |>
  select(nflId, name, pos, team, plays, Hurry, Hit, Sack, Pressure, xPressure, Zscore, 
         Zscore_25less, Zscore_25more, POP, NormPOP, npop_1, npop_0, finishrate) |>
  filter(plays>=85)

#Normalized POP
npop <- pop |>
  ggplot(aes(x=npop_1, y=npop_0)) +
  #Vertical Line
  geom_vline(xintercept=mean(pop$npop_1, na.rm=T), linetype="dashed", color = "red", size=0.5) +
  #Horizontal Line
  geom_hline(yintercept=mean(pop$npop_0), linetype="dashed", color = "red", size=0.5) +
  #Points
  geom_point(shape=21, aes(color = team, fill = team, size = plays,
                           text = paste(
                             name, "\n",
                             "Team: ", team, " ",
                             "Plays: ", plays, "\n",
                             "Pressure Rate: ", paste0(round(Pressure/(plays), 2)*100, "%"), "\n",
                             "Finish Rate: ", paste0(round(finishrate, 2)*100, "%"), "\n",
                             sep = ""
                           )), show.legend=FALSE) +
  scale_size_continuous(range = c(2, 4)) +
  #Colors
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  #Labels
  labs(x = "Plays with Pressure",
       y = "Plays with No Pressure",
       title = "Normalized Pressures over Prediction (POP) per Play") +
  #Themes
  theme_fivethirtyeight() +
  theme(legend.position='none') +
  theme(axis.title = element_text(size = 12, face = "bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"))+
  #Axis Ticks
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .01)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .01))

p <- plotly::ggplotly(npop, tooltip = "text")

#Z-Score Standardization for Predicted Pressures
zscore <- pop |>
  ggplot(aes(x=Zscore_25less, y=Zscore_25more)) +
  #Vertical Line
  geom_vline(xintercept=mean(pop$Zscore_25less), linetype="dashed", color = "red", size=0.5) +
  #Horizontal Line
  geom_hline(yintercept=mean(pop$Zscore_25more), linetype="dashed", color = "red", size=0.5) +
  #Points
  geom_point(shape=21, aes(color = team, fill = team, size = plays,
                           text = paste(
                             name, "\n",
                             "Team: ", team, " ",
                             "Plays: ", plays, "\n",
                             "Normalized POP: ", round(NormPOP, 2), "\n",
                             "Z Score: ", round(Zscore, 2), "\n",
                             sep = ""
                           )), show.legend=FALSE) +
  scale_size_continuous(range = c(2, 4)) +
  #Colors
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  #Labels
  labs(x = "2.5 Seconds or Less",
       y = "2.5 - 5 Seconds",
       title = "Z-Score Standardization of Predicted Pressures") +
  #Themes
  theme_fivethirtyeight() +
  theme(legend.position='none') +
  theme(axis.title = element_text(size = 12, face = "bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"))+
  #Axis Ticks
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.1))

p2 <- plotly::ggplotly(zscore, tooltip = "text")

team <- rusher |>
  group_by(gameId, playId, nflId) |>
  summarize(
    press = mean(test_actual),
    xpress = mean(test_pred_probs),
    pop = mean(pop),
    epa = mean(epa),
    posteam = last(postm),
    team = last(team)
  ) |>
  ungroup() |>
  mutate(transPOP = ifelse(pop>0, (1 - pop),
                           ifelse(pop<0, (pop + 1) * -1, 0)),
         normPOP = (transPOP + 1)/2,
         zscore = scale(xpress, center = TRUE, scale = TRUE)) 

def <- team |>
  group_by(team) |>
  summarize(
    NormPOP = mean(normPOP),
    Zscore = mean(zscore)
  ) |>
  ungroup()

off <- team |>
  group_by(posteam) |>
  summarize(
    NormPOP = mean(normPOP),
    Zscore = mean(zscore)
  ) |>
  ungroup()

#Pass Rush Units
def |>
  ggplot(aes(x=NormPOP, y=Zscore)) +
  #Vertical Line
  geom_vline(xintercept=mean(def$NormPOP), linetype="dashed", color = "red", size=0.5) +
  #Horizontal Line
  geom_hline(yintercept=mean(def$Zscore), linetype="dashed", color = "red", size=0.5) +
  #Team Logos
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.05) +
  #Labels
  labs(x = "Normalized POP per Play",
       y = "Predicted Pressures Standardized (PPS)",
       title = "How well did Pass Rush Units Perform?",
       subtitle = "Through the first eight weeks (2021)") +
  #Themes
  theme_fivethirtyeight() +
  theme(legend.position='none') +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(face="bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size=12))+
  #Axis Ticks
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .01)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .001))

#Pass Block Units
off |>
  ggplot(aes(x=NormPOP, y=Zscore)) +
  #Vertical Line
  geom_vline(xintercept=mean(off$NormPOP), linetype="dashed", color = "red", size=0.5) +
  #Horizontal Line
  geom_hline(yintercept=mean(off$Zscore), linetype="dashed", color = "red", size=0.5) +
  #Team Logos
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.05) +
  #Labels
  labs(x = "Normalized POP per Play",
       y = "Predicted Pressures Standardized (PPS)",
       title = "How well did Pass Block Units Perform?",
       subtitle = "Through the first eight weeks (2021)") +
  #Themes
  theme_fivethirtyeight() +
  theme(legend.position='none') +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(face="bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size=12))+
  #Axis Ticks
  scale_y_reverse(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .01)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = .001))

#Probability Animation
library(gganimate)
library(cowplot)
library(lubridate)
library(sf)

p1 <- predict |>
  inner_join(total0, by = c("gameId", "playId", "nflId", "adj_frame")) |>
  inner_join(players, by = c ("nflId", "height", "weight")) |>
  mutate(oe = test_actual - test_pred_probs,
         name = paste(displayName, " ", "(", jerseyNumber, ")", sep="")) |>
  filter(gameId==2021101709, playId==349) |>
  mutate(showtime = case_when(seconds %in% c(2.9, 4.4) ~ 10,
                              TRUE ~ 1)) |>
  # uncount is a tidyr function which copies each line 'n' times
  uncount(showtime) |>
  group_by(nflId) |>
  mutate(revealtime = row_number()) |>
  ungroup()

animate.play2 <- ggplot(data = p1, aes(x=seconds, y=test_pred_probs, group=nflId)) +
  #Ball Snap
  geom_segment(aes(x=0, xend=0, y=0, yend=1), linetype = "dashed") +
  geom_text(aes(x=0, y = 0.9, label="Ball Snap", angle=90, hjust=0.5, vjust=1)) +
  #Play Action
  geom_segment(aes(x=1.3, xend=1.3, y=0, yend=1), linetype = "dashed") +
  geom_text(aes(x=1.3, y = 0.9, label="Play Action", angle=90, hjust=0.5, vjust=1)) +
  #Sack
  geom_segment(aes(x=4.4, xend=4.4, y=0, yend=1), linetype = "dashed") +
  geom_text(aes(x=4.4, y = 0.9, label="QB Sack", angle=90, hjust=0.5, vjust=1)) +
  #Probability Line & Player Name (Number)
  geom_path(aes(color=name), 
            size=1, show.legend = FALSE) +
  geom_text(aes(label=name, color=name),
            hjust = 0.5, fontface = "bold", show.legend=FALSE) +
  #titles and caption
  labs(x = "Seconds into Play",
       y = "Probability of Pressure") +
  scale_alpha_identity() +
  scale_colour_manual(values = c("#A5ACAF", "#000000", "#a6aeb0", "#000000")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = 'bold',color='black'),
        axis.text.y = element_text(face = 'bold',color='black')) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8)) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = percent) +
  scale_x_continuous(limits = c(0, 5), breaks = scales::pretty_breaks(n = 10)) +
  gganimate::transition_reveal(along = revealtime)  + 
  ease_aes('linear') + 
  NULL

play.length.ex2 <- length(unique(p1$revealtime))
animate(animate.play2, fps = 10, rewind = FALSE, nframe = play.length.ex2)

#Save
anim_save("images/prob_rusher.gif", animation = last_animation())
