library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

horse_files <- 
  Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.ch') %>%
  map_df(~read_csv(., trim_ws = TRUE, 
                   col_types = 'ccifdcfcfddcccccccccddcfffddccdddddddddddddddddddddddddddddddddddddddddcdddddddddddddddddddddddddddddddddccdcdfcdddddd')) %>%
  clean_names()%>%
  as_tibble(.) %>%
  mutate(rc_track = str_to_upper(rc_track),
         rc_date = mdy(rc_date))


running_lines <- 
  Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chr') %>%
  map_df(~read_csv(., trim_ws = TRUE, 
                   col_types = 'ccicccccdcccdccddccccddddddddcccddddddfccdddddddddddddccddddccddffffffcccddccccdddddddfddddddddddddd')) %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(rc_date = mdy(rc_date),
         track = str_to_upper(track))
  

# To Do:
# Clean up Running Line File
# Condense rows into single row
# Make Function that generates additional variables
# Win percentage, last time raced, horse age, etc

chart_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chart')

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
      type_convert()
    
    all_races <- bind_rows(all_races, races)
  
    
    skip_num <- nrow(races)+1
    
    starters <- suppressWarnings(read_csv(file_path, col_names = FALSE, skip = skip_num, col_types = cols(.default = col_character()))) %>%
      filter(X1 == 'S') %>%
      bind_cols(header, .) %>%
      rename(
        record_type = X1, rc_race = X2, horse_key = X3, horse_name = X4, birthday = X5, breed = X7, sex = X8,
        medication = X24, equipment = X25, jock_last_name = X27, jock_first_name = X28, trainer_last_name = X31, 
        trainer_first_name = X32, owner_last_name = X34, owner_first_name = X35, odds = X37, non_betting_starter = X38,
        favorite = X41, post_position = X42, finish_position = X51) %>%
      select(-starts_with('X')) %>%
      mutate(birthday = as.Date(as.character(birthday), format = '%Y%m%d'),
             breed = as.factor(breed),
             sex = as.factor(sex),
             medication = as.factor(medication),
             equipment = as.factor(equipment),
             non_betting_starter = as.factor(non_betting_starter),
             favorite = as.factor(non_betting_starter)) %>%
      type_convert()
    
    all_starters <- bind_rows(all_starters, starters)
    
  }
  
  if (return_races == TRUE){
    return(all_races)}
  
  else if(return_starters == TRUE){
      return(all_starters)}
}

##################################

races <- read_chart_files(chart_files, return_races = T, return_starters = F)
starters <- read_chart_files(chart_files, return_races = F, return_starters = T)

str(running_lines)

as.tibble(running_lines)

