library(feather)
library(tidyverse)
library(tidymodels)
library(themis)
library(vip)

df <- read_feather('~/Data Science/Horse Racing/Horse Racing - R/final_df.feather')%>%
  mutate(track_condition = if_else(track_condition == 'Good' |
                                     track_condition == 'Fast'|
                                     track_condition == 'Firm', 'Fast', 'Bad')) %>%
  mutate(track_condition = as_factor(track_condition)) %>%
  # Try this Prediction Later
  select(-money_win, -bute) %>%
  mutate(lasix = if_else(is.na(lasix), 'None', lasix),
         win = as_factor(win)) %>%
  mutate(odds_movement = replace_na(odds_movement, 0))


#######################  Baseline Accuracy to Beat:
total_races<-df %>%
  group_by(rc_track, rc_date, rc_race) %>%
  n_groups()

df %>%
  summarise(favorite_wins = sum(favorite == 1 & win == 1)/total_races)

df %>%
  summarise(baseline = sum(win==0)/n())

########## Data Procressing:

# Experiment with oversampling vs. Class Weights

splits <- initial_split(df, prop = 0.75, strata = win)
folds <- vfold_cv(training(splits), v = 10)

horse_recipe <-
  recipe(win ~ . , data = training(splits)) %>%
  update_role(rc_track, rc_race, rc_date, horse_name, new_role = 'id') %>%
  step_dummy(all_nominal(), -all_outcomes(), -rc_track, -horse_name) %>%
  step_zv(all_predictors()) %>%
  step_smote(win)

preview_data <- horse_recipe %>%
  prep() %>%
  juice()

preview_data %>%
  summarise(prop = sum(win==1)/n())

###############################################################################

# XGBoost Modeling Probably Takes Forever

xgboost_model <-
  boost_tree(trees = 1000,
             mtry = tune(),
             min_n =  tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune(),
             sample_size =  tune()) %>%
  set_engine('xgboost',
             objective = 'binary:logistic',
             set.seed = 24) %>%
  set_mode('classification')

xgb_wrk_fl <-
  workflow() %>%
  add_recipe(horse_recipe) %>%
  add_model(xgboost_model)

my_grid <- 
  grid_latin_hypercube(finalize(mtry(), training(splits)),
                   min_n(),
                   tree_depth(),
                   learn_rate(),
                   loss_reduction(),
                   sample_size = sample_prop(),
                   size = 50)

xgb_results <-
  xgb_wrk_fl %>%
  tune_grid(resamples = folds,
            grid = my_grid,
            metrics = metric_set(accuracy),
            control = control_grid(verbose = TRUE))


xgb_results %>% 
  collect_metrics() %>% 
  top_n(5, mean) %>%
  arrange(desc(mean))

xgb_results %>%
  select_best('pr_auc')


###################

levels(df$win)

rf_model <-
  rand_forest(trees = 1000) %>%
  set_engine('ranger', importance = 'impurity')%>%
  set_mode('classification')
  

rf_wrk_fl <-
  workflow() %>%
  add_recipe(horse_recipe) %>%
  add_model(rf_model)

rf_res <- rf_wrk_fl %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(pr_auc, accuracy),
    control = control_grid(save_pred = T,
                           verbose = T)
  )
  
rf_res %>%
  collect_metrics()

final_fit %>%
  collect_metrics()

final_fit %>%
  pluck('.workflow', 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 20)
