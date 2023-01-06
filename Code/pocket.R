rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(lubridate)
library(sf)

#Functions
source("code/functions.R")

#Filter through All Weeks
play <- week |>
  filter(week==1) |>
  mutate(Id = paste(gameId, "_", playId, sep="")) |>
  arrange(gameId, playId, frameId) |>
  group_by(playId, frameId) |>
  mutate(rushers = length(playId[which(pff_role=='Pass Rush')])) |>
  ungroup() |>
  filter(rushers!=0)

plays <- unique(play$Id)

#rm(week)

wkplays <- function(plays){
  example.play <- play |>
    filter(Id==plays)
  
  frames = sort(unique(example.play$frameId))
  pos_team = unique(example.play$possessionTeam[example.play$team==example.play$possessionTeam])
  def_team = unique(example.play$defensiveTeam[example.play$team==example.play$defensiveTeam])
  
  t0=list()
  
  for(i in seq_along(frames)){
    
    t1 <- chull_plot3(example.play, frames[i])
    t0[[i]] = t1
  }
  
  t0 = Map(cbind, t0, Id = plays, pos = pos_team, def = def_team)
  return(t0)
}

p0=list()

for(i in seq_along(plays)){
  p1 <- wkplays(plays[i])
  p0[[i]] = p1
}

list <- unlist(p0, recursive = FALSE)

data = data.table::rbindlist(list, fill = TRUE)
data = as.data.frame(data)

data_new <- data |>
  separate(Id, c("gameId","playId"), sep = "_") |>
  mutate(gameId = as.integer(gameId),
         playId = as.integer(playId)) |>
  select(c(gameId, playId, frame, everything())) |>
  unnest(value) 

write.csv(data_new, 'data/week/week1.csv', row.names = FALSE)