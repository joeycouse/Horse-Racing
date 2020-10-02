library(feather)
library(tidyverse)
library(tidymodels)
library(themis)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

df <- read_feather('~/Data Science/Horse Racing/Horse Racing - R/final_df.feather') %>%
  relocate(ends_with('horse_name'), .before = everything())

horse_names <- df %>%
  select(horse_1_horse_name:horse_12_horse_name)

df <- df %>%
  filter(winning_horse != 12) %>%
  mutate(track_condition = if_else(track_condition == 'Good' |
                                     track_condition == 'Fast'|
                                     track_condition == 'Firm', 'Fast', 'Bad')) %>%
  mutate(track_condition = as.factor(track_condition)) %>%
  select(-contains('horse_name')) %>%
  mutate(winning_horse = as.factor(winning_horse))

# Baseline Accuracy to Beat:
df %>%
  summarise(favorite_wins = sum(favorite == winning_horse),
            total_races = n(),
            score_to_beat = favorite_wins/total_races)

splits <- initial_split(df, prop = 0.75, strata = purse)
folds <- vfold_cv(training(splits), v = 10)

horse_recipe <-
  recipe(winning_horse ~ . , data = training(splits)) %>%
  update_role(rc_track, rc_race, rc_date, new_role = 'id') %>%
  step_dummy(all_nominal(), -all_outcomes(), -rc_track) %>%
  step_zv(all_predictors()) %>%
  step_upsample(winning_horse, over_ratio = 0.5)

###############################################################################

# XGBoost Modeling Probably Takes Forever

xgboost_model <-
  boost_tree(trees = 1200,
             mtry = tune(),
             min_n =  tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune(),
             sample_size =  tune()) %>%
  set_engine('xgboost', 
             set.seed = 24,
             objective = "multi:softmax",
             num_class = 12) %>%
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


