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
library(gt)
library(gtExtras)
library(nflreadr)
library(gridExtra)
library(grid)

setwd("~/BDB23")

#DATA CLEANING
info <- week |>
  filter(team == 'football') |>
  select(gameId, playId, frame = frameId, event, fbx = x, fby = y) |>
  distinct() |>
  mutate(is_start = as.numeric(event %in% c("autoevent_ballsnap", "ball_snap")),
         is_end = as.numeric(event %in% c("fumble", "handoff", "lateral",
                                          "autoevent_passforward", "pass_forward",
                                          "qb_sack", "qb_strip_sack", "run"))) |>
  group_by(gameId, playId) |>
  mutate(any_start = any(is_start == 1), any_end = any(is_end == 1)) |>
  filter(any_start, any_end) |>
  summarize(losx = fbx[which(is_start==1)[1]],
            losy = fby[which(is_start==1)[1]],
            start_frame = frame[which(is_start == 1)[1]],
            end_frame = frame[which(is_end == 1 & frame > start_frame)[1]], .groups = "drop")


def <- week |>
  filter(pff_role == 'Pass Rush') |>
  select(gameId, playId, week, frame = frameId, playDirection, nflId, defx = x, defy = y, jerseyNumber, team, 
         pff_positionLinedUp, pff_hit, pff_hurry, pff_sack) |>
  distinct() |>
  rowwise() |>
  mutate(pressure = sum(pff_hit, pff_hurry, pff_sack))

pr <- def |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  group_by(gameId, playId) |>
  mutate(adj_frame = frame - min(frame),
         max_frame = max(adj_frame)) |>
  ungroup() |>
  mutate(resx = ifelse(playDirection=='right', losx - defx, defx - losx),
         resy = ifelse(playDirection=='right', defy - losy, losy - defy))

#Histogram
pr0 <- pr |>
  select(gameId, playId, pressure, max_frame) |>
  distinct() |>
  mutate(Pressure = ifelse(pressure==1, 'Pressure', 'No Pressure')) 

write.csv(pr0, "histogram.csv", row.names = FALSE)

pr0 <- read.csv("histogram.csv")

library(dineq)
pressure_decomp <- gini_decomp(x = pr0$max_frame, z = pr0$pressure)
pressure_decomp

median(pr0$max_frame[pr0$pressure==1], na.rm=T)

median(pr0$max_frame, na.rm=T)

pr0 |>
  filter(max_frame<100) |>
  ggplot(aes(x=max_frame/10, fill=Pressure)) +
  geom_histogram(alpha = 0.5, position = 'dodge', bins=95) +
  #titles and caption
  labs(x = "Seconds into Play",
       y = "Number of Plays",
       title = "Distribution of Plays with & without Pressure",
       subtitle = "Plays that lasted 10 or fewer seconds (2021)") +
  scale_fill_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(legend.title = element_text(face="bold"),
        legend.position = c(.80, .90),
        legend.direction = "vertical",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(face="bold")) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) 


# #LOOP
# loess0 <- pr |>
#   dplyr::mutate(Id = paste(gameId, "_", playId, sep="")) |>
#   filter(max_frame>19, adj_frame<25) |>
#   dplyr::select(Id, nflId, week, pff_positionLinedUp, adj_frame, resy, resx)
# 
# plays <- unique(loess0$Id)
# 
# 
# allplays <- function(plays){
#   example.play <- loess0 |>
#     filter(Id==plays)
# 
#   models <- example.play |>
#     group_by(Id, nflId) |>
#     tidyr::nest() |>
#     dplyr::mutate(
#       # Perform loess calculation on each play and rusher
#       m = purrr::map(data, loess,
#                      formula = resy ~ resx, span = .75),
#       #Retrieve the fitted and standard error values from each model
#       fit = purrr::map(m, `[[`, "fitted"),
#       se = purrr::map(m, `[[`, "s")
#     )
# 
#   # Apply fitted y's as a new column
#   results <- models |>
#     dplyr::select(-c(m)) |>
#     tidyr::unnest(cols = c(data, fit, se)) |>
#     distinct() |>
#     mutate(yfit = round(fit, 2),
#            res = round(se, 3)) |>
#     select(-c(fit, se))
#   return(results)
# }
# 
# p0=list()
# 
# for(i in seq_along(plays)){
#   p1 <- allplays(plays[i])
#   p0[[i]] = p1
# }
# 
# data = data.table::rbindlist(p0, fill = TRUE)
# data = as.data.frame(data)
# 
# write.csv(data, 'data/loess.csv', row.names = FALSE)

loess <- read.csv("data/loess.csv")

lines0 <- loess |>
  group_by(Id) |>
  mutate(max_frame = max(adj_frame)) |>
  ungroup() |>
  filter(!is.na(yfit), max_frame>19) |>
  dplyr::select(-c(resy, res, max_frame)) 

library(sf)

lines <- lines0 |> 
  group_by(Id, week, nflId) |> 
  mutate(n=n()) |>
  summarize(
    n=mean(n),
    pos = pff_positionLinedUp[adj_frame==0],
    geometry=st_sfc(st_linestring(cbind(resx, yfit)))
    ) |>
  ungroup()

lines = st_sf(lines)

#NT
lines$pos <- gsub("^NRT$", "NT", lines$pos)
lines$pos <- gsub("^NLT$", "NT", lines$pos)
#CB/S
lines$pos <- gsub("^FS$", "Secondary", lines$pos)
lines$pos <- gsub("^FS$", "Secondary", lines$pos)
lines$pos <- gsub("^LCB$", "Secondary", lines$pos)
lines$pos <- gsub("^RCB$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBiL$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBiR$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBoL$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBoR$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBL$", "Secondary", lines$pos)
lines$pos <- gsub("^SCBR$", "Secondary", lines$pos)
lines$pos <- gsub("^SSL$", "Secondary", lines$pos)
lines$pos <- gsub("^SSR$", "Secondary", lines$pos)
#LB
lines$pos <- gsub("^MLB$", "LB", lines$pos)
lines$pos <- gsub("^LILB$", "LB", lines$pos)
lines$pos <- gsub("^RILB$", "LB", lines$pos)
lines$pos <- gsub("^LLB$", "LB", lines$pos)
lines$pos <- gsub("^RLB$", "LB", lines$pos)


#Nose Tackles
nt <- lines |>
  filter(pos=='NT') 

d_nt = st_distance(nt, which="Frechet")
cl_nt = hclust(as.dist(d_nt))

nt$class = as.factor(cutree(cl_nt, 25))
nt$move = ifelse(nt$class %in% c(5, 9, 16, 20, 23, 25), 'DC',
                 ifelse(nt$class %in% c(1, 4, 10), 'OCL',
                        ifelse(nt$class %in% c(3, 6, 7, 18, 19), 'OCR',
                               ifelse(nt$class %in% c(2, 8), 'BR',
                                      ifelse(nt$class %in% c(14, 15, 17, 22), 'CLO', 
                                             ifelse(nt$class %in% c(11, 12, 13, 21), 'CRO', 
                                                    ifelse(nt$class %in% c(24), 'OOB', NA)))))))

rm(d_nt, cl_nt)

nt |>
  inner_join(loess, by = c("Id", "nflId", "week")) |>
  ggplot(aes(x=resx, y=resy, group=nflId, color=class)) +
  geom_line(aes(x = 0), color = "blue") +
  geom_line() +
  coord_flip() +
  facet_wrap(~class)

nt1 <- nt |>
  inner_join(loess, by = c("Id", "nflId", "week")) |>
  group_by(adj_frame, class) |>
  mutate(mx = median(resx),
         last_mx = last(mx),
         my = median(resy),
         last_my = last(my)) |>
  ungroup()

nt1 |>
  filter(move!='DC', move!='OOB') |>
  ggplot(aes(x=mx, y=my, group=class, color=move)) +
  geom_line(aes(x = 0), color = "blue") +
  geom_line() +
  geom_text(data = nt1 %>% group_by(class) %>%
              filter(last_mx == last(mx)), aes(label = class, x = last_mx + 0.5, y = last_my, color = move), show.legend = FALSE) + 
  coord_flip()


#Secondary
sec <- lines |>
  filter(pos=='Secondary')

d_sec = st_distance(sec, which="Frechet")
cl_sec = hclust(as.dist(d_sec))

sec$class = as.factor(cutree(cl_sec, 12))
sec$move = ifelse(sec$class %in% c(2, 3, 5, 6, 10, 12), 'SL',
                  ifelse(sec$class %in% c(1, 4, 7, 8, 11), 'SR',
                         ifelse(sec$class %in% c(9), 'OOB', NA)))


rm(d_sec, cl_sec)


#Linebackers
lb <- lines |>
  filter(pos=='LB')

d_lb = st_distance(lb, which="Frechet")
cl_lb = hclust(as.dist(d_lb))

lb$class = as.factor(cutree(cl_lb, 12))
lb$move = ifelse(lb$class %in% c(6, 10), 'LL',
                 ifelse(lb$class %in% c(2, 9), 'LR',
                        ifelse(lb$class %in% c(1, 3, 4, 5, 7, 8), 'LM',
                               ifelse(lb$class %in% c(12), 'DC', 
                                      ifelse(lb$class %in% c(11), 'OOB', NA)))))

rm(d_lb, cl_lb)

#Defensive Tackle
dlt <- lines |>
  filter(pos=='DLT')

d_dlt = st_distance(dlt, which="Frechet")
cl_dlt = hclust(as.dist(d_dlt))

dlt$class = as.factor(cutree(cl_dlt, 25))
dlt$move = ifelse(dlt$class %in% c(1, 4, 7, 9), 'OCL',
                  ifelse(dlt$class %in% c(6, 14, 17), 'OCR',
                         ifelse(dlt$class %in% c(2, 3, 5), 'BR',
                                ifelse(dlt$class %in% c(11, 15, 21), 'CLO',
                                       ifelse(dlt$class %in% c(10, 13, 22), 'CRO',
                                              ifelse(dlt$class %in% c(8, 12, 18, 19, 24, 25), 'DC',
                                                     ifelse(dlt$class %in% c(16, 20, 23), 'OOB', NA)))))))



drt <- lines |>
  filter(pos=='DRT')

d_drt = st_distance(drt, which="Frechet")
cl_drt = hclust(as.dist(d_drt))

drt$class = as.factor(cutree(cl_drt, 25))
drt$move = ifelse(drt$class %in% c(2, 11, 12), 'OCL',
                  ifelse(drt$class %in% c(3, 4, 7), 'OCR',
                         ifelse(drt$class %in% c(1, 5, 10, 19), 'BR',
                                ifelse(drt$class %in% c(8, 13, 15, 17, 21), 'CLO',
                                       ifelse(drt$class %in% c(6, 9, 14, 18, 23), 'CRO',
                                              ifelse(drt$class %in% c(20, 24, 25), 'DC',
                                                     ifelse(drt$class %in% c(16, 22), 'OOB', NA)))))))


rm(d_dlt, cl_dlt, d_drt, cl_drt)

#3-4 Edge
le <- lines |>
  filter(pos=='LE')

d_le = st_distance(le, which="Frechet")
cl_le = hclust(as.dist(d_le))

le$class = as.factor(cutree(cl_le, 25))
le$move = ifelse(le$class %in% c(2, 4, 5, 14, 18), 'BR',
                  ifelse(le$class %in% c(6, 7, 8, 9, 13, 15), 'IL',
                         ifelse(le$class %in% c(1, 3, 11, 12, 19, 25), 'CRI', 
                                ifelse(le$class %in% c(10, 16, 17, 20, 21, 23), 'CLO', 
                                       ifelse(le$class %in% c(22, 24), 'DC', NA)))))


re <- lines |>
  filter(pos=='RE')

d_re = st_distance(re, which="Frechet")
cl_re = hclust(as.dist(d_re))

re$class = as.factor(cutree(cl_re, 25))
re$move =ifelse(re$class %in% c(2, 4, 14, 15), 'BR',
                ifelse(re$class %in% c(1, 3, 7, 8, 17, 19, 24), 'IL',
                       ifelse(re$class %in% c(6, 9, 12, 16), 'CLI', 
                              ifelse(re$class %in% c(10, 11, 18, 20, 23), 'CRO', 
                                     ifelse(re$class %in% c(5, 13, 21, 22, 25), 'DC', NA)))))


rm(d_le, cl_le, d_re, cl_re)

#4-3 Edge
leo <- lines |>
  filter(pos=='LEO')

d_leo = st_distance(leo, which="Frechet")
cl_leo = hclust(as.dist(d_leo))

leo$class = as.factor(cutree(cl_leo, 25))
leo$move = ifelse(leo$class %in% c(1, 2, 3, 4, 6, 8, 10, 15, 20), 'BR',
                  ifelse(leo$class %in% c(5, 7, 11, 12, 25), 'IL',
                         ifelse(leo$class %in% c(19, 21), 'CRI',
                                ifelse(leo$class %in% c(9, 13, 16, 18), 'CLO',
                                       ifelse(leo$class %in% c(14, 17, 22), 'OOB',
                                              ifelse(leo$class %in% c(23, 24), 'DC', NA))))))


reo <- lines |>
  filter(pos=='REO')

d_reo = st_distance(reo, which="Frechet")
cl_reo = hclust(as.dist(d_reo))

reo$class = as.factor(cutree(cl_reo, 25))
reo$move = ifelse(reo$class %in% c(2, 7, 8, 10, 15), 'BR',
                  ifelse(reo$class %in% c(5, 6, 9, 12, 13, 17, 21), 'IL',
                         ifelse(reo$class %in% c(3, 4, 16, 20, 23, 24), 'CLI',
                                ifelse(reo$class %in% c(1, 11, 22), 'CRO',
                                       ifelse(reo$class %in% c(14, 18), 'OOB',
                                              ifelse(reo$class %in% c(19, 25), 'DC', NA))))))



rm(d_leo, cl_leo, d_reo, cl_reo)

#Outside Linebacker
lolb <- lines |>
  filter(pos=='LOLB')

d_lolb = st_distance(lolb, which="Frechet")
cl_lolb = hclust(as.dist(d_lolb))

lolb$class = as.factor(cutree(cl_lolb, 25))
lolb$move = ifelse(lolb$class %in% c(1, 4, 6, 7, 14, 19), 'BR',
                   ifelse(lolb$class %in% c(2, 3, 8, 11, 16, 22), 'IL',
                          ifelse(lolb$class %in% c(5, 12, 18), 'CRI',
                                 ifelse(lolb$class %in% c(9, 10, 15, 17, 20), 'CLO',
                                               ifelse(lolb$class %in% c(13, 21, 24, 25), 'DC', 
                                                      ifelse(lolb$class %in% c(23), 'OOB', NA))))))

rolb <- lines |>
  filter(pos=='ROLB')

d_rolb = st_distance(rolb, which="Frechet")
cl_rolb = hclust(as.dist(d_rolb))

rolb$class = as.factor(cutree(cl_rolb, 25))
rolb$move = ifelse(rolb$class %in% c(1, 3, 4, 5, 6, 9, 10, 11, 15, 17, 23), 'BR',
                   ifelse(rolb$class %in% c(2, 8, 12, 13, 14, 24, 25), 'IL',
                          ifelse(rolb$class %in% c(7), 'CLI',
                                 ifelse(rolb$class %in% c(16, 18, 19, 21), 'CRO',
                                        ifelse(rolb$class %in% c(20, 22), 'OOB',
                                               ifelse(rolb$class %in% c(), 'DC', NA))))))


rm(d_lolb, cl_lolb, d_rolb, cl_rolb)

# #Interior Defensive Linemen
# iDL <- as.data.frame(rbind(nt, dlt, drt)) |>
#   select(-c('geometry'))
# write.csv(iDL, "data/positions/iDL.csv", row.names = FALSE)
# 
# iDL <- read.csv("data/positions/iDL.csv")
# 
# iDL |>
#   inner_join(loess, by = c("Id", "nflId", "week")) |>
#   filter(move!="OOB") |>
#   mutate(across(pos, factor, levels=c("DLT","NT","DRT"))) |>
#   group_by(adj_frame, pos, class) |>
#   mutate(mx = median(resx),
#          my = median(resy)) |>
#   ungroup() |>
#   ggplot(aes(x=mx, y=my, group=class, color=move)) +
#   geom_line(aes(x = 0), color = "blue") +
#   geom_line() +
#   coord_flip() +
#   facet_wrap(~pos)

# #Second Level (LBs & Secondary)
# sL = as.data.frame(rbind(lb, sec)) |>
#   select(-c('geometry'))
# write.csv(sL, "data/positions/sL.csv", row.names = FALSE)
# 
# sL <- read.csv("data/positions/sL.csv")
# 
# sL |>
#   inner_join(loess, by = c("Id", "nflId", "week")) |>
#   filter(move!="OOB") |>
#   #mutate(across(pos, factor, levels=c("DLT","NT","DRT"))) |>
#   group_by(adj_frame, pos, class) |>
#   mutate(mx = median(resx),
#          my = median(resy)) |>
#   ungroup() |>
#   ggplot(aes(x=mx, y=my, group=class, color=move)) +
#   geom_line(aes(x = 0), color = "blue") +
#   geom_line() +
#   coord_flip() +
#   facet_wrap(~pos)


# #Edge
# edge <- as.data.frame(rbind(re, le, reo, leo, rolb, lolb)) |>
#   select(-c('geometry'))
# write.csv(edge, "data/positions/edge.csv", row.names = FALSE)
# 
# edge <- read.csv("data/positions/edge.csv")
# 
# edge |>
#   inner_join(loess, by = c("Id", "nflId", "week")) |>
#   filter(move!="OOB") |>
#   mutate(across(pos, factor, levels=c("LOLB","LEO","LE", "RE", "REO", "ROLB"))) |>
#   group_by(adj_frame, pos, class) |>
#   mutate(mx = median(resx),
#          my = median(resy)) |>
#   ungroup() |>
#   ggplot(aes(x=mx, y=my, group=class, color=move)) +
#   geom_line(aes(x = 0), color = "blue") +
#   geom_line() +
#   coord_flip() +
#   facet_wrap(~pos)

# cluster <- rbind(edge, iDL, sL)
# write.csv(cluster, "data/positions/cluster.csv", row.names = FALSE)



cluster <- read.csv("data/positions/cluster.csv") |>
  mutate(class2 = paste(class, "_", pos, sep=""),
         pos0 = ifelse(pos=="DLT" | pos=="DRT", "DT", 
                       ifelse(pos=="LE" | pos=="RE", "DE (3-4)", 
                              ifelse(pos=="LEO" | pos=="REO", "DE (4-3)", 
                                     ifelse(pos=="ROLB" | pos=="LOLB", "OLB", pos)))),
         pos2 = ifelse(pos=="DLT" | pos=="NT" | pos=="DRT", "Defensive Tackle", 
                       ifelse(pos=="LE" | pos=="RE" | pos=="LEO" | pos=="REO", "Defensive End", 
                              ifelse(pos=="ROLB" | pos=="LOLB", "Outside Linebacker", 
                                     ifelse(pos=="LB", "Linebacker", pos)))),
         move0 = ifelse(move %in% c("CRO", "CLO"), "CO", 
                       ifelse(move %in% c("CRI", "CLI"), "CI", 
                              ifelse(move %in% c("OCR", "OCL"), "OC", move))),
         move2 = ifelse(move0=="BR", "Bull Rush", 
                        ifelse(move0=="CI", "Crash Inside", 
                               ifelse(move0=="CO", "Crash Outside", 
                                      ifelse(move0=="IL", "Inside Leverage", 
                                             ifelse(move0=="LL", "Linebacker (Left)", 
                                                    ifelse(move0=="LM", "Linebacker (Middle)", 
                                                           ifelse(move0=="LR", "Linebacker (Right)",
                                                                  ifelse(move0=="OC", "Off Center", 
                                                                         ifelse(move0=="SL", "Secondary (Left)", 
                                                                                ifelse(move0=="SR", "Secondary (Right)", move)))))))))))



#Path Graphic
yaxis <- c("-2.5", "LOS", "+2.5", "+5", "+7.5")
ybreak <- c(-2.5, 0, 2.5, 5, 7.5)
xaxis <- c("", "+5 <-", "BATS", "-> +5", "")
xbreak = c(-10, -5, 0, 5, 10)

c1 <- cluster |>
  inner_join(loess, by = c("Id", "nflId", "week")) |>
  filter(move!="OOB", move!="DC") |>
  mutate(pos_move = paste(pos, "_", move, sep="")) |>
  group_by(adj_frame, pos, pos_move) |>
  #class2
  mutate(mx = median(resx),
         my = median(resy)) |>
  ungroup()

p <- c1 |>
  ggplot() +
  geom_line(aes(x = 0, y=my), color = "blue4", size=1) +
  geom_line(aes(x=mx, y=my, group=pos_move, color=move2), size=1, alpha=0.7) +
  scale_color_viridis(discrete=TRUE) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(legend.title = element_text(face="bold", size=10), legend.position="right", 
        legend.direction="vertical", legend.title.align=0.25) +
  labs(col="Pass Rush") +
  scale_x_continuous(limits = c(-2.5, 7.5), breaks = ybreak, labels = yaxis) +
  scale_y_continuous(limits = c(-10, 10), breaks = xbreak, labels = xaxis) +
  facet_wrap(~pos2) + 
  theme(strip.text.x = element_text(face="bold", size=10))

p1 <- p %+% subset(c1, pos2 %in% c('Defensive Tackle', 'Defensive End', 'Outside Linebacker')) + labs(x = NULL)
p2 <- p %+% subset(c1, pos2 %in% c('Linebacker', 'Secondary'))

g <- gridExtra::grid.arrange(grobs = lapply(
        list(p1, p2),
        egg::set_panel_size,
        width = unit(5, "cm"),
        height = unit(4, "cm")
))

g2 <- cowplot::ggdraw(g) + 
  # same plot.background should be in the theme of p1 and p2 as mentioned above
  theme(plot.background = element_rect(fill="grey80", color = NA))
