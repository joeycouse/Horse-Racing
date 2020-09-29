library(feather)
library(tidyverse)
library(tidymodels)
library(themis)

df <- read_feather('~/Data Science/Horse Racing/Horse Racing - R/final_df.feather')

horse_names <- df %>%
  select(rc_track:horse_12_horse_name)

df <- df %>%
  filter(winning_horse != 12) %>%
  mutate(track_condition = if_else(track_condition == 'Good' |
                                     track_condition == 'Fast'|
                                     track_condition == 'Firm', 'Fast', 'Bad')) %>%
  mutate(track_condition = as.factor(track_condition)) %>%
  select(-contains('horse_name')) %>%
  mutate(winning_horse = as.factor(winning_horse))

splits <- initial_split(df, prop = 0.75, strata = purse)
folds <- vfold_cv(training(splits), v = 5)

horse_recipe <-
  recipe(winning_horse ~ . , data = training(splits)) %>%
  update_role(rc_track, rc_race, rc_date, new_role = 'id') %>%
  step_dummy(all_nominal(), -all_outcomes(), -rc_track) %>%
  step_zv(all_predictors()) %>%
  step_upsample(winning_horse, over_ratio = 0.5)
  
rf <- rand_forest(trees = 1000) %>%
  set_engine('ranger') %>%
  set_mode('classification')


rf_wrk <-
  workflow() %>%
  add_recipe(horse_recipe) %>%
  add_model(rf)


rf_res <- rf_wrk %>%
  fit_resamples(resamples = folds,
                control = control_resamples(verbose = T,
                                            save_pred =  T)
  )


rf_res$.notes[[1]]

###################
xgboost_model <-
  boost_tree(trees = 1000,
             mtry = tune(),
             min_n =  tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             sample_size = tune(),
             loss_reduction = tune()) %>%
  set_engine('xgboost', seed = 24) %>%
  set_mode('classification')

xgb_wrk_fl <-
  workflow() %>%
  add_recipe(horse_recipe) %>%
  add_model(xgboost_model)


my_grid <- 
  grid_max_entropy(finalize(mtry(), training(splits)),
                   min_n(),
                   tree_depth(),
                   learn_rate(),
                   loss_reduction(),
                   sample_size = sample_prop(range = c(0.4,1)),
                   size = 6)

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


