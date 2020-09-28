library(feather)
library(tidymodels)
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)


df <- read_feather('~/Data Science/Horse Racing/Horse Racing - R/final_df.feather')
df_model <- df %>%
  select(purse:winning_horse)

splits <- initial_split(df_model, prop = 0.75)
folds <- vfold_cv(training(splits), v = 5)

horse_recipe <-
  recipe(winning_horse ~ . , data = training(splits)) %>%
  step_other(all_nominal(), threshold = 0.1) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_nzv(all_predictors()) %>%
  step_pca(ends_with())
  
