library(tidyverse)
library(janitor)
library(lubridate)
library(feather)
library(glue)

running_line_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chr')
horse_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.ch')
betting_file_paths <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.pgh')
special_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.cs')

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
    mutate(distance = if_else(dist_unit == 'Y', distance * .454545, distance),
           winning_time = final_time,
           final_time = if_else(final_call != 1, final_time + 0.2 * final_call_len_adj, final_time),
           ) %>%
    select(-dist_unit) %>%
    mutate(speed = distance/final_time) %>%
    filter(speed <= 0.11, speed > 0.05, distance <= 20) %>%
    rename(horse_name = horse)
  
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
           birthday,
           years_old,
           days_since_last_race,
           bute,
           lasix,
           sale_price,
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
    mutate(horse_name = str_trim(horse_name)) %>%
    group_by(horse_name, birthday) %>%
    mutate(horse_id = cur_group_id()) %>%
    ungroup()
  
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

running_lines <- read_running_lines(running_line_files)
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
write_feather(horses, './data/horses.feather')
write_feather(betting, './data/betting.feather')
write_feather(special, './data/special.feather')

