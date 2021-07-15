library(tidyverse)
library(broom)
library(janitor)
library(lubridate)
library(splines2)
library(ggcorrplot)
library(feather)
library(glue)

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
           years_old = round(as.numeric((rc_date - birthday))/365.25, 2),
           days_since_last_race = as.numeric((rc_date - last_raced))) %>%
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

write_feather(running_lines, './data/running_lines.feather')
write_feather(races, './data/races.feather')
write_feather(starters, './data/starters.feather')
write_feather(horses, './data/horses.feather')
write_feather(betting, './data/betting.feather')
write_feather(special, './data/special.feather')

