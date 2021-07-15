library(tidyverse)
library(broom)
library(janitor)
library(lubridate)
library(splines2)
library(ggcorrplot)
library(feather)
library(glue)



# Read in files
running_lines <- read_feather('./data/running_lines.feather')
races <- read_feather('./data/races.feather')
starters <- read_feather('./data/starters.feather')
horses <- read_feather('./data/horses.feather')
betting <- read_feather('./data/betting.feather')
special <- read_feather('./data/special.feather')

###############################

# To do

# Returning dataframes and filtering to contain matches from both datasets
starters <- starters %>%
  filter(post_position < 99, odds != 0, country == "USA")

races <- races %>%
  filter(num_horses <= 12, country == 'USA') 

races <- races %>%
  semi_join(starters, by = c('rc_track', 'rc_date', 'rc_race')) 

starters <- starters %>%
  semi_join(races, by = c('rc_track', 'rc_date', 'rc_race'))

running_lines <- running_lines %>%
  filter(!(track %in% foreign_codes$Code))


# Dependent variable generation and additional column generation
# Watch races to ensure the odds are represented correctly

starter_features <- starters %>%
  inner_join(races) %>%
  filter(non_betting_starter == 'N') %>%
  select(rc_track, rc_date, rc_race, num_horses, purse, distance, surface, track_condition,
         horse_key, horse_name,post_position, favorite, odds, finish_position, trainer_last_name, owner_last_name, final_len) %>%
  group_by(rc_track, rc_date, rc_race) %>%
  mutate(final_len = if_else(post_position == 99, Inf, final_len),
         finish_order = min_rank(final_len),
         favorite = if_else(favorite == 'Y', 1, 0),
         win = if_else(finish_order == 1 | final_len <= 10, 1, 0),
         money_win = if_else(finish_order <= 3, 1,0),
         horse_name = str_trim(horse_name),
         payout = odds/100,
         odds = 100/(odds+100),
         ) %>%
  select(-finish_position, - finish_order, - final_len) %>%
  rowwise() %>%
  mutate(owner_trainer_same = as.numeric(str_detect(owner_last_name, trainer_last_name)), .keep = 'unused') %>%
  ungroup() %>%
  group_by(rc_track, rc_date, rc_race) %>%
  mutate(odds = round(odds/sum(odds), 3)) %>%
  ungroup()

# Fitting cubic spline for distance vs speed function

# To do 
# Quantile of the horses time at that distance, recent and median
# 


spline_features <- races %>%
  select(rc_track, rc_date, rc_race, distance) %>%
  left_join(running_lines, by = c('rc_track', 'rc_date', 'rc_race')) %>%
  filter(beyer < 998, beyer > 0, 
         distance.x <= 9, distance.x >= 4,
         distance.y <= 9, distance.y >= 4, purse >0) %>%
  select(rc_track, rc_date, rc_race, track, date, race, horse, 
         distance.x, distance.y, speed, purse, final_call, final_call_len_adj, odds_position, final_time, beyer) %>%
  mutate(horse = str_trim(horse),
         log_purse = log(purse))%>%
  rename(horse_name = horse)



%>%
  nest(data = everything())%>%
  mutate(model = map(data, ~lm(speed ~ mSpline(distance.y, degree = 3), data = .)))%>%
  mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
  unnest(cols = c(final))%>%
  mutate(final_call_len_adj = if_else(final_call == 1, -1*final_call_len_adj, final_call_len_adj),
         beaten_favorite = if_else(odds_position == 1 & final_call_len_adj > 0.1, 'Y', 'N'),
         performance = case_when(final_call < odds_position ~ 'W',
                                 final_call > odds_position ~ 'L',
                                 final_call == odds_position ~ 'N'),
         place_difference = final_call - odds_position) %>%
  group_by(rc_track, rc_date, rc_race, horse_name) %>%
  summarise(median_std_resid  = round(median(.std.resid),3),
            max_std_resid = round(max(.std.resid),3),
            min_std_resid = round(min(.std.resid),3),
            recent_std_resid  = recent_stat(.std.resid),
            median_purse = median(log_purse),
            max_purse = max(log_purse),
            min_purse = min(log_purse[log_purse > 0]),
            recent_purse = recent_stat(log_purse),
            median_length_behind  = median(final_call_len_adj[final_call_len_adj < 99.99]),
            max_length_behind = max(final_call_len_adj[final_call_len_adj < 99.99]),
            min_length_behind = min(final_call_len_adj[final_call_len_adj < 99.99]),
            recent_length_behind = recent_stat(final_call_len_adj, bigger_better = FALSE),
            median_beyer = round(median(beyer)),
            max_beyer = max(beyer),
            min_beyer = min(beyer),
            recent_beyer = recent_stat(beyer),
            #improving = if_else(recent_beyer > beyer[2], 1,0),
            #improving = replace_na(improving, 0),
            dist_beyer = mean(beyer[abs(distance.x - distance.y) <= 0.5]),
            dist_beyer = replace_na(mean(beyer[abs(distance.x - distance.y) <= 1])),
            dist_beyer = replace_na(mean(beyer[abs(distance.x - distance.y) <= 2])),
            dist_beyer = replace_na(mean(beyer)),
            dist_length_behind = mean(final_call_len_adj[abs(distance.x - distance.y) <= 0.5]),
            dist_length_behind = replace_na(mean(final_call_len_adj[abs(distance.x - distance.y) <= 1])),
            dist_length_behind = replace_na(mean(final_call_len_adj[abs(distance.x - distance.y) <= 2])),
            dist_length_behind = replace_na(mean(final_call_len_adj)),
            dist_std_resid = mean(.std.resid[abs(distance.x - distance.y) <= 0.5]),
            dist_std_resid = replace_na(mean(.std.resid[abs(distance.x - distance.y) <= 1])),
            dist_std_resid = replace_na(mean(.std.resid[abs(distance.x - distance.y) <= 2])),
            dist_std_resid = replace_na(mean(.std.resid)),
            median_distance = median(distance.y),
            median_place_difference = median(place_difference),
            median_odds_position = median(odds_position),
            total_place_difference = sum(place_difference),
            total_performance = sum(performance == 'W') - sum(performance == 'L'),
            total_beaten_favorite = sum(beaten_favorite == 'Y'),
            recent_beaten_favorite = last(beaten_favorite),
            #recent_performance = last(performance),
            recent_place_difference = last(place_difference),
            races_run = n()
  )

############ Uncomment to run

# Beyer Speed Figures

# Slightly skewed normalish distrobution

# spline_features %>%
#   select(beyer) %>%
#   ggplot() +
#   aes(x = beyer) +
#   geom_histogram(bins = 30, fill = 'blue')


# Spline fit plot

# To do add facet wrap to view by purse_grade

#running_lines %>%
#   filter(beyer < 998, beyer > 0, distance <= 9, distance >= 4) %>%
#   select(rc_track, rc_date, rc_race, track, date, horse, distance, speed, purse, final_call, final_call_len_adj, final_time, beyer) %>%
#   mutate(horse = str_trim(horse)) %>%
#   rename(horse_name = horse) %>%
#   nest(data = everything()) %>%
#   mutate(model = map(data, ~lm(speed ~ mSpline(distance, degree = 3), data = .))) %>%
#   mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
#   unnest(cols = c(final)) %>%
#   sample_frac(0.10) %>%
#   ggplot() +
#   aes(x = distance, y = speed) +
#   geom_jitter()+
#   geom_line(aes(x = distance,  y = .fitted), size = 1.5, color = 'red')


# Resid vs Fitted Plot

# running_lines %>%
#   filter(beyer < 998, beyer > 0, distance <= 9, distance >= 4) %>%
#   select(rc_track, rc_date, rc_race, track, date, horse, distance, speed, purse, final_call, final_call_len_adj,
# final_time) %>%
#   mutate(horse = str_trim(horse)) %>%
#   rename(horse_name = horse) %>%
#   nest(data = everything()) %>%
#   mutate(model = map(data, ~lm(speed ~ mSpline(distance,knots = c(6,7), degree = 3), data = .))) %>%
#   mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
#   unnest(cols = c(final)) %>%
#   sample_frac(0.15) %>%
#   ggplot()+
#   aes(x = .fitted, y = .resid) +
#   geom_jitter() +
#   geom_smooth(method = 'lm', formula = y~x)

# Resid Histogram

# running_lines %>%
#   filter(beyer < 998, beyer > 0, distance <= 9, distance >= 4) %>%
#   select(rc_track, rc_date, rc_race, track, date, horse, distance, speed, purse, final_call, final_call_len_adj,
# final_time) %>%
#   mutate(horse = str_trim(horse)) %>%
#   rename(horse_name = horse) %>%
#   nest(data = everything()) %>%
#   mutate(model = map(data, ~lm(speed ~ mSpline(distance,knots = c(6,7), degree = 3), data = .))) %>%
#   mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
#   unnest(cols = c(final)) %>%
#   sample_frac(0.15) %>%
#   ggplot()+
#   aes(x = .std.resid) +
#   geom_histogram()

# races %>%
#   select(rc_track, rc_date, rc_race, distance) %>%
#   left_join(running_lines, by = c('rc_track', 'rc_date', 'rc_race')) %>%
#   filter(beyer < 998, beyer > 0, 
#          distance.x <= 9, distance.x >= 4,
#          distance.y <= 9, distance.y >= 4) %>%
#   select(rc_track, rc_date, rc_race, track, date, horse, distance.x, distance.y, speed, purse, final_call, final_call_len_adj, final_time, beyer) %>%
#   mutate(horse = str_trim(horse)) %>%
#   rename(horse_name = horse) %>%
#   nest(data = everything()) %>%
#   mutate(model = map(data, ~lm(speed ~ mSpline(distance.y, degree = 3), data = .)))%>%
#   mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
#   unnest(cols = c(final)) %>%
#   ggplot() %>%
#   aes(x = .std.resid) %>%
#   geom_histogram()

##########

all_features <- starter_features %>%
  left_join(spline_features) %>%
  group_by(rc_track, rc_date, rc_race) %>%
  filter(sum(is.na(races_run)) <= 0)%>%
  left_join(betting)  %>%
  mutate(odds_movement = round(odds - ml_odds, 3)) %>%
  select(-ml_odds) %>%
  left_join(special) %>%
  left_join(horses) %>%
  mutate(lasix = ifelse(lasix == 'l' | lasix == 'L', 'L', NA),
         average_earnings = lt_earnings/races_run) %>%
  ungroup() %>%
  select(-ends_with('pct'), -starts_with('w_'), -best_wet) %>%
  mutate(surface_earnings = if_else(surface == 'D', d_earnings, t_earnings),
         surface_money = if_else(surface == 'D', d_money, t_money),
         best_surface = if_else(surface == 'D', best_fast_dirt, best_turf),
         .keep = 'unused')


correlation <- all_features %>%
  select_if(is.numeric) %>%
  select(-odds_movement, -rc_race, -distance, -post_position, -purse) %>%
  cor() %>%
  as_tibble(rownames = 'var1') %>%
  pivot_longer(-var1, names_to = 'var2', values_to = 'correlation') %>%
  group_by(correlation) %>%
  filter(row_number() == 1, correlation != 1) %>%
  ungroup() %>%
  arrange(desc(correlation))


# PCA Dimension Reduction
###############
# 
# #Just the t_variables
# pc_t<-prcomp(all_features %>%
#          select(starts_with('t_')),
#        center = T,
#        scale. = T
#        )
# # Two PC explain 90% + variance, only using two
# summary(pc_t)
# 
# loadings_t<-pc_t$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_t = PC1,
#          PC2_t = PC2)
# 
# #Just the dist_variables
# pc_dist<-prcomp(all_features %>%
#                select(starts_with('dist_')),
#              center = T,
#              scale. = T
# )
# # Two PC explain 89% + variance, only using two
# summary(pc_dist)
# 
# loadings_dist<-pc_dist$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_dist = PC1,
#          PC2_dist = PC2)
# 
# #Just the lt_variables
# pc_lt<-prcomp(all_features %>%
#                   select(starts_with('lt_')),
#                 center = T,
#                 scale. = T
# )
# 
# # Two PC explain 85% + variance, only using two
# summary(pc_lt)
# 
# loadings_lt<-pc_lt$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_lt = PC1,
#          PC2_lt = PC2)
# 
# #Just the w_variables
# pc_w<-prcomp(all_features %>%
#                 select(starts_with('w_')),
#               center = T,
#               scale. = T
# )
# 
# # Two PC explain 90% + variance, only using two
# summary(pc_w)
# 
# loadings_w<-pc_w$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_w = PC1,
#          PC2_w = PC2)
# 
# #Just the cy_variables
# pc_cy<-prcomp(all_features %>%
#                select(starts_with('cy_')),
#              center = T,
#              scale. = T
# )
# 
# # Two PC explain 89% + variance, only using two
# summary(pc_cy)
# 
# loadings_cy<-pc_cy$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_cy = PC1,
#          PC2_cy = PC2)
# 
# #Just the py_variables
# pc_py<-prcomp(all_features %>%
#                 select(starts_with('py_')),
#               center = T,
#               scale. = T
# )
# 
# # Two PC explain 90% + variance, only using two
# summary(pc_py)
# 
# loadings_py<-pc_py$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_py = PC1,
#          PC2_py = PC2)
# 
# #Just the trk_variables
# pc_trk<-prcomp(all_features %>%
#                 select(starts_with('trk_')),
#               center = T,
#               scale. = T
# )
# 
# # Two PC explain 88% + variance, only using two
# summary(pc_trk)
# 
# loadings_trk<-pc_trk$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_trk = PC1,
#          PC2_trk = PC2)
# 
# #Just the best_variables
# pc_best<-prcomp(all_features %>%
#                  select(starts_with('best_')),
#                center = T,
#                scale. = T
# )
# 
# # One PC explain 87% + variance, only using two
# summary(pc_best)
# 
# loadings_best<-pc_best$x %>%
#   as_tibble() %>%
#   select(PC1, PC2, PC3) %>%
#   rename(PC1_best = PC1,
#          PC2_best = PC2,
#          PC3_best = PC3)
# 
# #Just the jt_variables
# pc_jt<-prcomp(all_features %>%
#                  select(starts_with('jt_')),
#                center = T,
#                scale. = T
# )
# 
# # Three PC explain 8% + variance, only using two
# summary(pc_jt)
# 
# loadings_jt<-pc_jt$x %>%
#   as_tibble() %>%
#   select(PC1, PC2, PC3) %>%
#   rename(PC1_jt = PC1,
#          PC2_jt = PC2,
#          PC3_jt = PC3)
# 
# #Just the d_variables
# pc_d<-prcomp(all_features %>%
#                 select(starts_with('d_')),
#               center = T,
#               scale. = T
# )
# 
# # Two PC explain 85% + variance, only using two
# summary(pc_d)
# 
# loadings_d<-pc_d$x %>%
#   as_tibble() %>%
#   select(PC1, PC2) %>%
#   rename(PC1_d = PC1,
#          PC2_d = PC2)

 
################


# Questioning about how much value this is adding....

# pca_features<-all_features %>%
#   select(-starts_with('cy_'),
#          -starts_with('py_'),
#          -starts_with('lt_'),
#          -starts_with('jt_'),
#          -starts_with('d_'),
#          -starts_with('best_'),
#          -starts_with('dist_'),
#          -starts_with('t_'),
#          -starts_with('w_'),
#          -starts_with('trk_')) %>%
#   bind_cols(c(loadings_lt, loadings_best, loadings_jt, loadings_dist, loadings_d, loadings_t, loadings_cy, loadings_py, loadings_trk)) %>%
#   mutate(PC1_surface = if_else(surface == 'D', PC1_d, PC1_t),
#          PC2_surface = if_else(surface == 'D', PC2_d, PC2_t)) %>%
#   select(-PC1_d, -PC2_d, -PC1_t, -PC2_t)
# 
# rm(loadings_cy, loadings_py, loadings_lt,loadings_jt, loadings_best, loadings_dist, loadings_t, loadings_w, loadings_trk, loadings_d)
# rm(pc_best, pc_cy, pc_dist, pc_jt, pc_lt, pc_py, pc_t, pc_trk, pc_w, pc_d)


# 2007 races
# pca_features %>%
#   group_by(rc_track, rc_date, rc_race) %>%
#   count() %>%
#   nrow()

# Build Final Dataset

# To Do
# Make sure all horses are represented in the race, and filter out races when all horse aren't present#
# Fix the purse representation - log of purse
# Distance of races - median distance of horse's race

final_features <- all_features %>%
  group_by(horse_key) %>%
  mutate(purse = if_else(log(purse/1000) != -Inf, log(purse/1000), mean(purse)))%>%
  ungroup() %>%
  group_by(rc_track, rc_race, rc_date) %>%
  mutate(across(ends_with('_resid'), ~ . - mean(.)), .keep = 'unused')%>%
  mutate(across(ends_with('_length_behind'), ~ . - mean(.)), .keep = 'unused')%>%
  mutate(across(ends_with('races_run'),  ~ . - mean(.)), .keep = 'unused') %>%
  mutate(across(ends_with('days_since_last_races'),  ~ . - mean(.)), .keep = 'unused')%>%
  mutate(across(ends_with('_beyer'),  ~ . - mean(.)), .keep = 'unused') %>%
  mutate(across(starts_with('best_'),  ~ . - mean(.)), .keep = 'unused') %>%
  mutate(purse_diff = purse - median_purse,
         purse_recent = purse - recent_purse,
         distance_diff = distance - median_distance, .keep = 'unused')%>%
  mutate(across(ends_with('_money'), ~ . - mean(.)), .keep = 'unused') %>%
  mutate(across(ends_with('_earnings'), ~ . - mean(.)), .keep = 'unused') %>%
  mutate(weight = weight - mean(weight), .keep = 'unused') %>%
  mutate(years_old = years_old - mean(years_old), .keep = 'unused') %>%
  mutate(days_since_last_race = days_since_last_race - mean(days_since_last_race), .keep = 'unused') %>%
  mutate(jt_all_starts = jt_all_starts - mean(jt_all_starts), .keep = 'unused') %>%
  mutate(jt_track_starts = jt_track_starts - mean(jt_track_starts), .keep = 'unused') %>%
  ungroup() %>%
  mutate(lasix = if_else(is.na(lasix), 'None', lasix),
         win = as_factor(win),
         money_win = as_factor(money_win)) %>%
  mutate(track_condition = if_else(track_condition == 'Good' |
                                     track_condition == 'Fast'|
                                     track_condition == 'Firm', 'Fast', 'Bad')) %>%
  mutate(track_condition = as_factor(track_condition)) %>%
  mutate(odds_movement = replace_na(odds_movement, 0))%>%
  mutate(across(ends_with('earnings'), ~bestNormalize::orderNorm(.x)$x.t)) %>%
  relocate(win, .after  = everything()) %>%
  relocate(money_win, .after = everything()) %>%
  relocate(horse_name, .after = 'rc_race') %>%
  mutate(win = fct_relevel(win, '1'),
         money_win = fct_relevel(money_win, '1'),
         across(where(is.character), as.factor)) %>%
  select(-horse_key, -bute) 


#rm(betting, horses, final_features, all_features, races, races_features, race_results, running_lines, spline_features, starter_features, starters, special)

date <- as.character(today())
file_name <- glue('~/Data Science/Horse Racing/Horse Racing - R/Horse Racing R/data/final_df{date}.feather')
write_feather(final_features, file_name)
