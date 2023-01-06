## Majority of functions come from Rajiv Shah
## player_position1, ball_position1, chull_plot_centroid, and chull_plot
## Repo: https://github.com/rajshah4/NBA_SportVu/blob/master/_functions.R

library(dplyr)
library(sp)
library(lubridate)
library(sf)

#Functions
player_position1 <- function(df, frames){
  dfall <- df %>% filter(frameId == frames)  %>% 
    filter(team!="football") %>% select (team, x, y, pff_role, jerseyNumber)
  colnames(dfall) <- c('ID','x','y', 'role','jerseyNumber')
  return(dfall)
}

ball_position1 <- function(df, frames){
  dfall <- df %>% filter(frameId == frames)  %>% 
    filter(team=="football") %>% select (team, x, y, pff_role, jerseyNumber)
  colnames(dfall) <- c('ID','x','y', 'role','jerseyNumber')
  return(dfall)
}

chull_plot_centroid <- function(df,frames) {
  ##Returns a data frame with the centroid of a convex hull
  ##Requires player_position for info
  df2 <- player_position1(df, frames)
  df_hull2 <- df2 %>% filter(((role == 'Pass Block' | role == 'Pass') | role=='Pass Rush') & ID == min(ID)) %>% select(x, y)
  df_hull3 <- df2 %>% filter(((role == 'Pass Block' | role == 'Pass') | role=='Pass Rush') & ID == max(ID)) %>% select(x, y)
  c.hull2 <- chull(df_hull2)
  c.hull3 <- chull(df_hull3)
  df2centroid <- as.data.frame(cbind(1,mean(df_hull2[c.hull2 ,]$x),mean(df_hull2[c.hull2 ,]$y)))
  df3centroid <- as.data.frame(cbind(2,mean(df_hull3[c.hull3 ,]$x),mean(df_hull3[c.hull3 ,]$y)))
  dfall <- rbind(df2centroid,df3centroid)
  colnames(dfall) <- c('ID','x','y')
  return(dfall)
}

chull_plot <- function(df, frames) {
  df2 <- player_position1(df, frames)
  df_hull2 <- df2 %>% filter(((role == 'Pass Block' | role == 'Pass') | role=='Pass Rush') & ID == min(ID)) %>% select(x, y)
  df_hull3 <- df2 %>% filter(((role == 'Pass Block' | role == 'Pass') | role=='Pass Rush') & ID == max(ID)) %>% select(x, y)
  c.hull2 <- chull(df_hull2)
  c.hull3 <- chull(df_hull3)
  c.hull2 <- c(c.hull2, c.hull2[1])
  c.hull3 <- c(c.hull3, c.hull3[1])
  df2 <- as.data.frame(cbind(1,df_hull2[c.hull2 ,]$x,df_hull2[c.hull2 ,]$y))
  df3 <- as.data.frame(cbind(2,df_hull3[c.hull3 ,]$x,df_hull3[c.hull3 ,]$y))
  dfall <- rbind(df2,df3)
  colnames(dfall) <- c('ID','x','y')
  return(dfall)
}


chull_plot2 <- function(df, frames) {
  frame = frames
  #Players
  df2 <- player_position1(df, frames)
  #QB
  qb0 <- df2 %>% filter(role == 'Pass') %>% select(x, y)
  qbx <- qb0$x
  qby <- qb0$y
  qb = map2(qbx, qby, c)
  qb = matrix(unlist(qb), ncol = 2, byrow = TRUE)
  #Convex Hulls
  df_hull2 <- df2 %>% filter(role == 'Pass Block' | role == 'Pass') %>% select(x, y)
  df_hull3 <- df2 %>% filter(role=='Pass Rush') %>% select(x, y)
  hulldef <- as.data.frame(cbind(frame, 2, df_hull3))
  colnames(hulldef) <- c('frame', 'ID','x','y')
  #Pass Rush
  xy = map2(hulldef$x, hulldef$y, c)
  xy = matrix(unlist(xy), ncol = 2, byrow = TRUE)
  c.hull2 <- chull(df_hull2)
  c.hull3 <- chull(df_hull3)
  c.hull2 <- c(c.hull2, c.hull2[1])
  c.hull3 <- c(c.hull3, c.hull3[1])
  df2 <- as.data.frame(cbind(frame, 1,df_hull2[c.hull2 ,]$x,df_hull2[c.hull2 ,]$y))
  colnames(df2) <- c('frame', 'ID','x','y')
  df3 <- as.data.frame(cbind(frame, 2,df_hull3[c.hull3 ,]$x,df_hull3[c.hull3 ,]$y))
  colnames(df3) <- c('frame', 'ID','x','y')
  #Area & Intersection
  O <- cbind(df2$x, df2$y) %>% 
    list %>% 
    st_polygon %>% 
    st_sfc
  OLw = max(df2$y) - min(df2$y)
  D <- cbind(df3$x, df3$y) %>% 
    list %>% 
    st_polygon %>% 
    st_sfc
  I = sf::st_intersection(O, D)
  Oarea <- st_area(O)
  Darea <- st_area(D)
  Iarea <- st_area(I)
  
  nxy = 1:nrow(xy)
  rif0 = list()
  ip = list()
  dist = list()

  for(i in seq_along(nxy)){
    qb1 = sf::st_point(qb[1, ])
    rif <- sf::st_point(xy[i, ])  #Gets area of convex hull
    rif0[[i]] = rif
    ip[[i]] = ifelse(is.na(as.numeric(sf::st_intersects(rif0[[i]], O))), 0, 1)
    dist[[i]] = st_distance(x = qb1,  y = rif0[[i]], by_element = TRUE)
  }

  inside_pocket <- Reduce("+", ip) 
  
  total <- list(cbind(hulldef$x, hulldef$y, ip, dist))
  total <- enframe(total)
  
  pass_rushers <- cbind(frame, qbx, qby, inside_pocket, total) 
  area <- as.data.frame(cbind(frame, Oarea, Darea, Iarea, OLw), nrow=1) |>
    left_join(pass_rushers, by = c('frame'))

  return(area)
}


chull_plot3 <- function(df, frames) {
  frame = frames
  #Players
  df2 <- player_position1(df, frames)
  #QB
  qb0 <- df2 %>% filter(role == 'Pass') %>% select(x, y)
  qbx <- qb0$x
  qby <- qb0$y
  qb = map2(qbx, qby, c)
  qb = matrix(unlist(qb), ncol = 2, byrow = TRUE)
  #Convex Hulls
  df_hull2 <- df2 %>% filter(role == 'Pass Block' | role == 'Pass') %>% select(x, y)
  df_hull3 <- df2 %>% filter(role=='Pass Rush') %>% select(x, y)
  hulldef <- as.data.frame(cbind(frame, 2, df_hull3))
  colnames(hulldef) <- c('frame', 'ID','x','y')
  #Pass Rush
  xy = map2(hulldef$x, hulldef$y, c)
  xy = matrix(unlist(xy), ncol = 2, byrow = TRUE)
  c.hull2 <- chull(df_hull2)
  c.hull3 <- chull(df_hull3)
  c.hull2 <- c(c.hull2, c.hull2[1])
  c.hull3 <- c(c.hull3, c.hull3[1])
  df2 <- as.data.frame(cbind(frame, 1,df_hull2[c.hull2 ,]$x,df_hull2[c.hull2 ,]$y))
  colnames(df2) <- c('frame', 'ID','x','y')
  df3 <- as.data.frame(cbind(frame, 2,df_hull3[c.hull3 ,]$x,df_hull3[c.hull3 ,]$y))
  colnames(df3) <- c('frame', 'ID','x','y')
  #Area & Intersection
  O <- cbind(df2$x, df2$y) %>% 
    list %>% 
    st_polygon %>% 
    st_sfc
  
  nxy = 1:nrow(xy)
  rif0 = list()
  ip = list()
  dist = list()
  
  for(i in seq_along(nxy)){
    qb1 = sf::st_point(qb[1, ])
    rif <- sf::st_point(xy[i, ])  #Gets area of convex hull
    rif0[[i]] = rif
    ip[[i]] = ifelse(is.na(as.numeric(sf::st_intersects(rif0[[i]], O))), 0, 1)
  }
  
  total <- list(cbind(hulldef$x, hulldef$y, ip))
  total <- enframe(total)
  
  pass_rushers <- as.data.frame(cbind(frame, qbx, qby, total))
  
  return(pass_rushers)
}