library(feather)
library(tidyverse)
library(tidymodels)
library(themis)
library(vip)

# Optional to Speed Up Training

# library(doParallel)
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores)
# 
# set.seed(1234)

# To do
# Add reading of the most recent file 

df <- read_feather('~/Data Science/Horse Racing/Horse Racing - R/final_df.feather')

money_wins <-
  df %>%
  select(money_win) %>%
  mutate(.row = row_number())


# To Do
# Move some of the transformations into the read_write_file

model_df <-
  df %>%
  select(-money_win, -bute, -payout) %>%
  filter(!is.na(days_since_last_race)) %>%
  mutate(win = fct_relevel(win, '1'))

glimpse(model_df)

# To do
# investigate distribution of variables with box plots / histograms
# add addtional model types: logistic regression, neural networks, SVM?


#######################  Baseline Accuracy to Beat:

total_races <- df %>%
  group_by(rc_track, rc_date, rc_race) %>%
  n_groups()

# Favorite finishes in top 3 72.8% of the time
df %>%
  summarise(favorite_wins = sum(favorite == 1 & money_wins == 1)/total_races)

# Precision & Recall
pr_baseline <- df %>%
  summarise(Precision = sum(favorite == 1 & win ==1)/sum(favorite == 1),
            Recall = sum(favorite == 1 & win == 1)/sum(win==1))

pr_baseline_curve <- pr_curve(df, win, odds) %>%
  autoplot()

pr_baseline_curve
# Baseline Accuracy
df %>%
  summarise(baseline = sum(win==1)/n())


########## Data Pre-processing:

# Experiment with oversampling vs. Class Weights

splits <- initial_split(model_df, prop = 0.8)
folds <- vfold_cv(training(splits), v = 5)

boost_recipe <-
  recipe(win ~ . , data = training(splits)) %>%
  update_role(rc_track, rc_race, rc_date, horse_name, new_role = 'id') %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(win, skip = TRUE) %>%
  prep()


preview_data <- 
  boost_recipe %>%
  prep() %>%
  juice()

###############################################################################

# XGBoost Modeling Probably Takes Forever

xgboost_model <-
  boost_tree(trees = 1500,
             mtry = tune(),
             min_n =  tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune(),
             sample_size =  tune(),
             stop_iter = 5) %>%
  set_engine('xgboost',
             objective = 'binary:logistic') %>%
  set_mode('classification')

xgb_wrk_fl <-
  workflow() %>%
  add_recipe(horse_recipe) %>%
  add_model(xgboost_model)

my_grid <- 
  grid_max_entropy(finalize(mtry(), training(splits)),
                   min_n(),
                   tree_depth(range = c(1,10)),
                   learn_rate(),
                   loss_reduction(),
                   sample_size = sample_prop(),
                   size = 50)

xgb_results <-
  xgb_wrk_fl %>%
  tune_grid(resamples = folds,
            grid = my_grid,
            metrics = metric_set(pr_auc, accuracy),
            control = control_grid(verbose = TRUE))

xgb_results %>%
  collect_metrics() %>%
  filter(.metric == 'pr_auc') %>%
  arrange(desc(mean)) 
  
xgb_best<-
  xgb_results %>%  
  select_best('pr_auc')
  
final_wrk_fl <-
  xgb_wrk_fl %>%
  finalize_workflow(xgb_best) %>%
  last_fit(splits, metrics = metric_set(pr_auc))

final_wrk_fl %>%
  pluck('.workflow', 1)

final_wrk_fl %>%
  collect_metrics()

look<-final_wrk_fl %>%
  collect_predictions()

final_wrk_fl %>%
  pluck('.workflow', 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 20)

###################

rf_recipe <-
  recipe(win ~ . , data = training(splits)) %>%
  update_role(rc_track, rc_race, rc_date, horse_name, new_role = 'id') %>%
  step_nzv(all_numeric())

rf_model <-
  rand_forest(trees = 1000) %>%
  set_engine('ranger', importance = 'impurity')%>%
  set_mode('classification')
  
rf_wrk_fl <-
  workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

rf_res <- rf_wrk_fl %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(pr_auc, accuracy),
    control = control_grid(save_pred = T,
                           verbose = T))
  
rf_res %>%
  collect_metrics()

final_fit <-
  rf_wrk_fl %>%
  last_fit(splits, 
           metrics = metric_set(pr_auc, accuracy))

final_fit %>%
  collect_metrics()

# Add What Picking the Favorite Is

pr_curve(df, win, odds)

final_fit %>%
  collect_predictions() %>%
  inner_join(money_wins)%>%
  pr_curve(win, .pred_1) %>%
  ggplot() + 
  geom_line(aes(x = recall, y = precision), color = 'black', size = 1)+
  geom_line(data = pr_baseline_curve, aes(x = recall, y = precision),color = 'red', size = 1)+
  geom_point(data  = pr_baseline, aes(x = Recall, y = Precision), color = 'red', size = 4)+
  geom_text(data  = pr_baseline, aes(x = Recall, y = Precision, label = 'Favorite Baseline'), nudge_y = -0.07)+
  geom_hline(yintercept = 0.134, linetype = 'dashed')+
  geom_text(aes(0,0.134, label = 'Naive Baseline', show.legend = F, size = 2), vjust = -1, hjust = -0.5)
  


final_fit %>%
  pluck('.workflow', 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 20)
