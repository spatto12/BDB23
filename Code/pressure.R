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

# #DATA CLEANING
# path0 <- "~/BDB23/data/week"
# DF0 <- multmerge(path0)
# DF0 <- as.data.frame(DF0)
# 
# qb <- week |>
#   filter(pff_role == 'Pass') |>
#   select(gameId, playId, week, frame = frameId, qbx = x, qby = y, qbs = s, qbdis = dis, qbdir = dir, qbo = o,
#          qbId = nflId, event, passResult, epa, playDirection, quarter, down, yardsToGo, secstogo, posteam_score, defteam_score,
#          offenseFormation, personnelO, personnelD, defendersInBox, dropBackType, pff_playAction, pff_passCoverage, pff_passCoverageType) |>
#   mutate(qbs = qbs * 2.04545) |>
#   distinct()
# 
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
# 
# def <- week |>
#   filter(pff_role == 'Pass Rush') |>
#   select(gameId, playId, frame = frameId, nflId, defx = x, defy = y, defs = s, defa = a,
#          defdis = dis, defdir = dir, defo = o, jerseyNumber, team) |>
#   inner_join(prep_players, by = c ("nflId")) |>
#   mutate(defs = defs * 2.04545,
#          defa = defa * 0.9144,
#          w_kg = weight * 0.45359237,
#          force = defa * w_kg
#          ) |>
#   distinct()
# 
# cluster <- read.csv("data/positions/cluster.csv") |>
#   separate(Id, c("gameId","playId"), "_") |>
#   mutate(gameId = as.integer(gameId),
#          playId = as.integer(playId),
#          cmove = ifelse(move %in% c("CRO", "CLO"), "CO",
#                         ifelse(move %in% c("CRI", "CLI"), "CI",
#                                ifelse(move %in% c("OCR", "OCL"), "OC", move)))) |>
#   select(-c(n, class, move))
# 
# rm(week)
# 
# total <- DF0 |>
#   select(-c(name)) |>
#   rename(defx = value.1, defy = value.2, defip = value.3, postm = pos, deftm = def) |>
#   inner_join(def, by = c('gameId', 'playId', 'frame', 'defx', 'defy')) |>
#   inner_join(qb, by = c("gameId", "playId", "frame", "qbx", "qby")) |>
#   left_join(info, by = c('gameId', 'playId')) |>
#   left_join(press, by = c('gameId', 'playId', 'nflId')) |>
#   left_join(cluster, by = c("gameId", "playId", "week", "nflId")) |>
#   filter(!is.na(start_frame), !is.na(end_frame),
#          frame >= start_frame, frame <= end_frame) |>
#   distinct()
# 
# rm(DF0, qb, def, info, cluster, press, prep_players)
# 
# write.csv(total, "data/cluster.csv", row.names = FALSE)

leverage <- read.csv('data/leverage.csv')
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

total1 <- total0 |>
  select(gameId, playId, nflId, adj_frame, pressure, dist_to_passer, cumpress, qbx1, qby1, qbs,
         defx1, defy1, defs, defip, cmove, blockType, rush_wt, block_wt, seconds, down, yardsToGo) |>
  mutate(fmove = as.numeric(as.factor(cmove))) |>
  group_by(gameId, playId, adj_frame) |>
    arrange(cumpress) |>
    mutate(player_press_rank = 1:n(),
           max_rush = max(player_press_rank)) |>
  ungroup() |>
  select(-c(cumpress, max_rush, cmove, blockType))

rm(total, total0, leverage)

library(caTools)
library(xgboost)
library(caret)
library(SHAPforxgboost)

# set.seed(1985)
# game_fold_table <- tibble(gameId = unique(total1$gameId)) |>
#   mutate(game_fold = sample(rep(1:10, length.out = n()), n()))
# 
# model <- total1 |>
#   dplyr::inner_join(game_fold_table, by = "gameId")
# 
# #Test Statistical Significance and Multicollinearity with simple Logit Model
# logit_model <- glm(pressure ~ dist_to_passer + qbx1 + qby1 + qbs + defx1 +
#                      defy1 + defs + defip + seconds + down + yardsToGo + fmove + player_press_rank + block_wt + rush_wt,
#                    data = model, family = "binomial")
# 
# summary(logit_model)
# with(summary(logit_model), 1 - deviance/null.deviance)
# car::vif(logit_model)
# 
# write.csv(model, "data/model.csv", row.names = FALSE)

model <- read.csv("data/model.csv")

xgb_prob_preds <- 
  map_dfr(unique(model$game_fold), 
          function(test_fold) {
            test_data <- model %>% filter(game_fold == test_fold)
            train_data <- model %>% filter(game_fold != test_fold)
            
            y_train <- as.integer(train_data$pressure)
            y_test <- as.integer(test_data$pressure)
            X_train <- train_data |> select(-pressure, -gameId, -playId, -nflId, - adj_frame, -game_fold)
            X_test <- test_data |> select(-pressure, -gameId, -playId, -nflId, -adj_frame, -game_fold)
            
            xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
            xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
            
            params <- list(
              objective = "binary:logistic",
              learning_rate = 0.03,
              subsample = 0.7,
              reg_lambda = 2,
              max_depth = 3
            )
            
            fit_xgb <- xgb.train(
              params,
              data = xgb_train,
              watchlist = list(valid = xgb_test),
              eval_metric = "aucpr",
              early_stopping_rounds = 100,
              print_every_n = 250,
              nrounds = 250 # early stopping
            )
            
            xgb_preds <- as.numeric(predict(fit_xgb, as.matrix(X_test), reshape = TRUE))

            # Return tibble of holdout results:
            tibble(test_pred_probs = xgb_preds,
                   test_actual = test_data$pressure,
                   adj_frame = test_data$adj_frame,
                   game_fold = test_fold,
                   gameId = test_data$gameId,
                   playId = test_data$playId,
                   nflId = test_data$nflId)
          }) 

# library(MLmetrics)
# 
# f1_scores <- sapply(seq(0.01, 0.99, .01), function(thresh) F1_Score(xgb_prob_preds$test_actual, ifelse(xgb_prob_preds$test_pred_probs >= thresh, 1, 0), positive = 1))
# which.max(f1_scores) #20
# 
# predict <- xgb_prob_preds |>
#   mutate(pred_thresh = ifelse(test_pred_probs>.20, 1, 0))
# 
# write.csv(predict, "data/predictions.csv", row.names = FALSE)

predict <- read.csv("data/predictions.csv")

#Accuracy
confusionMatrix(as.factor(predict$pred_thresh), as.factor(predict$test_actual), mode="prec_recall")

#F1 Score
recall <- 65886/(65886 + 82952) #A ?/ (A + C), C being False Negative (TPR)
precision <- 65886/(65886 + 81657) #A / (A + B), A being True Positive and B being False Positive (PPV)

f1 <- 2 * (precision * recall)/(precision + recall)
f1

# MCC
tp <- 65886
tn <- 780260
fp <- 81657
fn <- 82952

MCC <- (tn * tp - fn * fp)/sqrt((tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))
MCC

normMCC <- (MCC + 1)/2
normMCC

#Brier Score
brier <- sum((predict$test_pred_probs - predict$test_actual)**2)/nrow(predict)
brier

# #Shap Values
# fold <- unique(model$game_fold)
# 
# xgb_shap <- function(fold){
#   test_data <- model %>% filter(game_fold == fold)
#   train_data <- model %>% filter(game_fold != fold)
# 
#   y_train <- as.integer(train_data$pressure)
#   y_test <- as.integer(test_data$pressure)
#   X_train <- train_data |> select(-pressure, -gameId, -playId, -nflId, -adj_frame, -game_fold)
#   X_test <- test_data |> select(-pressure, -gameId, -playId, -nflId, -adj_frame, -game_fold)
# 
#   xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
#   xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
# 
#   params <- list(
#     objective = "binary:logistic",
#     learning_rate = 0.03,
#     subsample = 0.7,
#     reg_lambda = 2,
#     max_depth = 3
#   )
# 
#   fit_xgb <- xgb.train(
#     params,
#     data = xgb_train,
#     watchlist = list(valid = xgb_test),
#     eval_metric = "aucpr",
#     #c("error", "auc")
#     early_stopping_rounds = 100,
#     print_every_n = 250,
#     nrounds = 250 # early stopping
#   )
# 
#   shap_long <- shap.prep(xgb_model = fit_xgb, X_train = as.matrix(X_train))
# 
#   meanshap <- as.data.frame(shap_long) |>
#     select(c(variable, mean_value)) |>
#     distinct()
# 
#   meanshap$game_fold <- fold
#   return(meanshap)
# 
# }
# 
# shap0=list()
# 
# for(i in seq_along(fold)){
#   shap1 <- xgb_shap(fold[i])
#   shap0[[i]] = shap1
# }
# 
# data = data.table::rbindlist(shap0, fill = TRUE)
# data = as.data.frame(data)
# 
# write.csv(data, "data/shap.csv", row.names = FALSE)

shap <- read.csv("data/shap.csv")

shap0 <- shap |>
  group_by(variable) |>
  summarize(
    mshap = mean(mean_value)
  ) |>
  ungroup()

# Barplot
shap0 |>
  mutate(dependent = ifelse(variable=='dist_to_passer', 'Distance to QB', 
                            ifelse(variable=='defs', 'Rusher Speed (MPH)', 
                                   ifelse(variable=='player_press_rank', 'Relative Pressure Rank', 
                                          ifelse(variable=='block_wt', 'Blocker Leverage',
                                                 ifelse(variable=='seconds', 'Seconds after Snap',
                                                        ifelse(variable=='fmove', 'Rusher Path', 
                                                               ifelse(variable=='defx1', 'Relative Rusher x', 
                                                                      ifelse(variable=='defip', 'Inside Pocket', 
                                                                             ifelse(variable=='defy1', 'Relative Rusher y',
                                                                                    ifelse(variable=='down', 'Down', 
                                                                                           ifelse(variable=='rush_wt', 'Rusher Leverage', 
                                                                                                  ifelse(variable=='qbx1', 'Relative QB x',
                                                                                                         ifelse(variable=='qbs', 'QB Speed (MPH)', 
                                                                                                                ifelse(variable=='yardsToGo', 'Yards to Go',
                                                                                                                       ifelse(variable=='qby1', 'Relative QB y', NA)))))))))))))))) |>
  ggplot(aes(x=reorder(dependent, mshap), y=mshap)) + 
  coord_flip() +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  #titles and caption
  labs(x = "",
       y = "Mean SHAP Value",
       title = "Pressure Probability Model Feature Importance") +
  theme_fivethirtyeight() +
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold')) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        #plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8)) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
