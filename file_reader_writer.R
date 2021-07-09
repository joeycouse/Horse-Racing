library(tidyverse)
library(broom)
library(janitor)
library(lubridate)
library(splines2)
library(ggcorrplot)
library(feather)

chart_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chart')
running_line_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chr')
horse_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.ch')
betting_file_paths <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.pgh')
special_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.cs')


read_chart_files <- function(file_paths, return_races=FALSE, return_starters = TRUE){
  
  all_races = data.frame()
  all_starters = data.frame()
  
  for (file_path in file_paths){
    
    print(file_path)
    
    header <- read_csv(file_path, n_max =1, col_names = FALSE, col_types = 'ccccdcc') %>%
      select(-X1) %>%
      select(c(1:4)) %>%
      rename(country = X2, rc_track = X3, rc_date = X4, num_races = X5) %>%
      mutate(rc_date = as.Date(as.character(rc_date), format = '%Y%m%d'),
             rc_track = str_trim(str_to_upper(rc_track)))
      
    
    races <- suppressWarnings(read_csv(file_path, col_names = FALSE, skip = 1, col_types = cols(.default = col_character())))%>%
      filter(X1 == 'R')%>%
      bind_cols(header, .) %>%
      rename(
        record_type = X1, rc_race = X2, breed = X3, race_type = X4, restrictions = X5, sex_restrictions = X6,
        age_restrictions = X7, division = X8, purse = X9, reverts_money = X10, available_money = X11, paid_to_others = X12, guaranteed_money = X13, added_money = X14,
        includes_one_money = X16, includes_two_money = X18, includes_three_money = X20, plus_one_money = X22, plus_two_money = X24, 
        plus_three_money = X26, min_claiming = X27,max_claiming = X28, distance = X30, distance_unit = X31, surface = X32, 
        num_horses = X34, track_condition = X43, track_variant = X45, speed_number = X46) %>%
      mutate(restrictions = as.factor(restrictions), 
             sex_restrictions = as.factor(sex_restrictions), 
             distance_unit = as.factor(distance_unit),
             surface = as.factor(surface),
             age_restrictions = as.factor(age_restrictions),
             track_condition = as.factor(track_condition)) %>%
      select(-starts_with('X')) %>%
      type_convert() %>%  
      select(country, rc_track, rc_date, rc_race, race_type, num_horses, purse, distance, distance_unit, surface, track_condition) %>%
      mutate(distance_unit = if_else(is.na(distance_unit) | distance_unit == 'M' | distance_unit == 'F', 'F', 'Y'),
             distance = round(distance/100,2)) %>%
      mutate(distance = if_else(distance_unit == 'Y', round(distance * .454545, 1), distance)) %>%
      select(-distance_unit, -race_type)
    
    all_races <- bind_rows(all_races, races)
  
    
    skip_num <- nrow(races)+1
    
    starters <- suppressWarnings(read_csv(file_path, col_names = FALSE, skip = skip_num, col_types = cols(.default = col_character()))) %>%
      filter(X1 == 'S') %>%
      bind_cols(header, .) %>%
      rename(
        record_type = X1, rc_race = X2, horse_key = X3, horse_name = X4, birthday = X5, breed = X7, sex = X8,
        medication = X24, equipment = X25, jock_last_name = X27, jock_first_name = X28, trainer_last_name = X31, 
        trainer_first_name = X32, owner_last_name = X34, owner_first_name = X35, odds = X37, non_betting_starter = X38,
        favorite = X41, post_position = X42, finish_position = X51, final_len = X63) %>%
      select(-starts_with('X')) %>%
      mutate(birthday = as.Date(as.character(birthday), format = '%Y%m%d'),
             breed = as.factor(breed),
             sex = as.factor(sex),
             medication = as.factor(medication),
             equipment = as.factor(equipment),
             non_betting_starter = as.factor(non_betting_starter),
             favorite = as.factor(favorite)) %>%
      type_convert()
    
    all_starters <- bind_rows(all_starters, starters)
    
  }
  
  if (return_races == TRUE){
    return(all_races)}
  
  else if(return_starters == TRUE){
      return(all_starters)}
}

read_running_lines <- function(file_paths){

  
  running_lines <- map_df(file_paths, ~read_csv(., trim_ws = TRUE, 
                   col_types = 'ccicccccdcccdccddccccddddddddcccddddddfccdddddddddddddccddddccddffffffcccddccccdddddddfddddddddddddd')) %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(rc_date = mdy(rc_date),
           date = mdy(date),
           track = str_to_upper(track)) %>%
    filter(final_time != -1, odds != 0, odds_position <= starters) %>%
    mutate(minutes = as.numeric(if_else(as.numeric(final_time) >= 100, str_sub(final_time, 1,1),'0')),
           seconds = as.numeric(if_else(as.numeric(final_time) >= 100, str_sub(final_time, 2,-1), as.character(final_time))),
           final_time = as.numeric(dminutes(minutes)+dseconds(seconds))) %>%
    select(-minutes, -seconds) %>%
    mutate(dist_unit = if_else(is.na(dist_unit) | dist_unit == 'M' | dist_unit == 'F', 'F', 'Y')) %>%
    mutate(distance = if_else(dist_unit == 'Y', distance * .454545, distance)) %>%
    select(-dist_unit) %>%
    mutate(speed = distance/final_time) %>%
    filter(speed <= 0.11, speed > 0.05, distance <= 20)
  
  return(running_lines)
  
}

read_horse_files <- function(file_paths){
  
  
    horse_files <- map_df(file_paths, ~read_csv(., trim_ws = TRUE, 
                   col_types = 'ccifdcfcfddcccccccccddcfffddccdddddddddddddddddddddddddddddddddddddddddcdddddddddddddddddddddddddddddddddccdcdfcdddddd')) %>%
    clean_names()%>%
    as_tibble(.) %>%
    mutate(rc_track = str_to_upper(rc_track),
           rc_date = mdy(rc_date),
           last_raced = mdy(last_raced)) %>%
    rename(lt_earnings = earnings,
           lt_starts = starts,
           lt_wins = wins,
           lt_places = places,
           lt_shows = shows) %>%
    mutate(d_born ='15', m_born = as.character(m_born)) %>%
    unite('birthday', c(y_born, m_born, d_born), sep = '-') %>%
    mutate(birthday = ymd(birthday), 
           years_old = round((rc_date - birthday)/365.25, 2),
           days_since_last_race = (rc_date - last_raced)) %>%
    select(rc_track, 
           rc_date, 
           rc_race,
           horse,
           weight,
           post_position,
           years_old,
           days_since_last_race,
           bute,
           lasix,
           cy_earnings:dist_shows,
           jt_track_starts:last_col()) %>%
    rowwise() %>%
    mutate(cy_money = sum(c_across(starts_with("cy") & -contains(c('earnings', 'starts')))),
           py_money = sum(c_across(starts_with("py") & -contains(c('earnings', 'starts')))),
           lt_money = sum(c_across(starts_with("lt") & -contains(c('earnings', 'starts')))),
           w_money = sum(c_across(starts_with("w_") & -contains(c('earnings', 'starts')))),
           t_money = sum(c_across(starts_with("t_") & -contains(c('earnings', 'starts')))),
           d_money = sum(c_across(starts_with("d_") & -contains(c('earnings', 'starts')))),
           trk_money = sum(c_across(starts_with("trk") & -contains(c('earnings', 'starts')))),
           dist_money = sum(c_across(starts_with("dist") & -contains(c('earnings', 'starts')))),
           cy_money_pct = cy_money/cy_starts,
           py_money_pct = py_money/py_starts,
           lt_money_pct = lt_money/lt_starts,
           w_money_pct = w_money/w_starts,
           t_money_pct = t_money/t_starts,
           d_money_pct = d_money/d_starts,
           trk_money_pct = trk_money/trk_starts,
           dist_money_pct = dist_money/dist_starts,
           .keep = 'unused') %>%
    ungroup() %>%
    mutate(across(cy_money_pct:dist_money_pct, ~replace_na(. , 0))) %>%
    rename(horse_name = horse) %>%
    select(-post_position) %>%
    mutate(horse_name = str_trim(horse_name))
  
    return(horse_files)
  
}

read_betting_files <- function(file_paths){
  
betting_files <- map_df(file_paths, ~read_csv(., trim_ws = TRUE, 
                                                        col_types = 'ccdcdccc')) %>%
    clean_names() %>%
    as_tibble() %>%
    filter(post_position <= 98) %>%
    select(-prog_num)%>%
    mutate(rc_date = mdy(rc_date)) %>%
    separate(ml_odds, into = c('top', 'bottom'), sep = '-', convert = TRUE) %>%
    mutate(ml_odds = bottom/(bottom+top), .keep = 'unused') %>%
    group_by(rc_track, rc_date, rc_race) %>%
    mutate(ml_odds = ml_odds/sum(ml_odds)) %>%
    rename(horse_name = horse) %>%
    select(-jockey, -post_position)

return(betting_files)
  
}

recent_stat<- function(metric, bigger_better = TRUE){
  
  
  if (length(metric) < 3) {
    result = mean(metric)
    return(result)
  }
  
  else {
    
    if(bigger_better == FALSE){
      result = (sum(metric[1:3]) - max(metric[1:3]))/2
      return(result)
    }
    
    result = (sum(metric[1:3]) - min(metric[1:3]))/2
    return(result)
    
  }
  
}

running_lines <- read_running_lines(running_line_files)
races <- read_chart_files(chart_files, return_races = T, return_starters = F)
starters <- read_chart_files(chart_files, return_races = F, return_starters = T)
horses <- read_horse_files(horse_files)
betting <- read_betting_files(betting_file_paths)
foreign_codes <- read_csv('~/Data Science/Horse Racing/Horse Racing Code/data/foreign_track_codes.csv')
special <- map_df(special_files, ~read_csv(., trim_ws = TRUE, 
                                           col_types = 'ccdccddddddddccc')) %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(rc_date = mdy(rc_date)) %>%
  rename(horse_name = horse) %>%
  select(rc_track, rc_date, rc_race, horse_name ,starts_with('best')) %>%
  select(-best_track_surface)

###############################

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

# Figure out if the bute column of horses is reading in correctly

starter_features <- starters %>%
  inner_join(races) %>%
  filter(non_betting_starter == 'N') %>%
  select(rc_track, rc_date, rc_race, num_horses, purse, distance, surface, track_condition,
         horse_name,post_position, favorite, odds, finish_position, trainer_last_name, owner_last_name, final_len) %>%
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
  select(-finish_position, - finish_order) %>%
  rowwise() %>%
  mutate(owner_trainer_same = as.numeric(str_detect(owner_last_name, trainer_last_name)), .keep = 'unused') %>%
  ungroup() %>%
  group_by(rc_track, rc_date, rc_race) %>%
  mutate(odds = round(odds/sum(odds), 3)) %>%
  ungroup()

# Fitting cubic spline for distance vs speed function

spline_features <- races %>%
  select(rc_track, rc_date, rc_race, distance) %>%
  left_join(running_lines, by = c('rc_track', 'rc_date', 'rc_race')) %>%
  filter(beyer < 998, beyer > 0, 
         distance.x <= 9, distance.x >= 4,
         distance.y <= 9, distance.y >= 4) %>%
  select(rc_track, rc_date, rc_race, track, date, horse, 
         distance.x, distance.y, speed, purse, final_call, final_call_len_adj, final_time, beyer) %>%
  mutate(horse = str_trim(horse)) %>%
  rename(horse_name = horse) %>%
  nest(data = everything()) %>%
  mutate(model = map(data, ~lm(speed ~ mSpline(distance.y, degree = 3), data = .)))%>%
  mutate(final = map2(model, data, ~augment(.x, .y)), .keep ='unused') %>%
  unnest(cols = c(final)) %>%
  mutate(final_call_len_adj = if_else(final_call == 1, -1*final_call_len_adj, final_call_len_adj)) %>%
  group_by(rc_track, rc_date, rc_race, horse_name) %>%
  summarise(median_std_resid  = round(median(.std.resid),3),
            max_std_resid = round(max(.std.resid),3),
            min_std_resid = round(min(.std.resid),3),
            recent_std_resid  = recent_stat(.std.resid),
            median_purse = median(purse),
            max_purse = max(purse),
            min_purse = min(purse),
            recent_purse = recent_stat(purse),
            median_length_behind  = median(final_call_len_adj[final_call_len_adj < 99.99]),
            max_length_behind = max(final_call_len_adj[final_call_len_adj < 99.99]),
            min_length_behind = min(final_call_len_adj[final_call_len_adj < 99.99]),
            recent_length_behind = recent_stat(final_call_len_adj, bigger_better = FALSE),
            median_beyer = round(median(beyer)),
            max_beyer = max(beyer),
            min_beyer = min(beyer),
            recent_beyer = recent_stat(beyer),
            improving = if_else(recent_beyer > beyer[2], 1,0),
            improving = replace_na(improving, 0),
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
# Make sure all horses are represented in the race, and filter out races when all horse aren't present

final_features <- all_features %>%
  group_by(rc_track, rc_race, rc_date) %>%
  mutate(across(ends_with('_resid'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(ends_with('_length_behind'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(ends_with('races_run'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(ends_with('days_since_last_races'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(ends_with('_beyer'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(starts_with('best_'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(purse_1 = purse - median_purse,
         purse_2 = purse - max_purse,
         purse_3 = purse - min_purse,
         purse_4 = purse - recent_purse, .keep = 'unused') %>%
  mutate(across(ends_with('_money'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
  mutate(across(ends_with('_earnings'), list(~ . - mean(.), ~ . - max(.) , ~ . - min(.))), .keep = 'unused') %>%
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
  mutate(odds_movement = replace_na(odds_movement, 0)) %>%
  relocate(win, .after  = everything()) %>%
  relocate(money_win, .after = everything()) %>%
  relocate(horse_name, .after = 'rc_race') %>%
  mutate(win = fct_relevel(win, '1'),
         money_win = fct_relevel(money_win, '1')) 


#rm(betting, horses, final_features, all_features, races, races_features, race_results, running_lines, spline_features, starter_features, starters, special)

date <- as.character(today())
file_name <- glue('~/Data Science/Horse Racing/Horse Racing - R/Horse Racing R/data/final_df{date}.feather')
write_feather(final_features, file_name)
