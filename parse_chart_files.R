library(tidyverse)
source("chart_file_col_names.R")
chart_file_paths <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/chart_files/*.chart')




header <- read_csv(chart_file_paths, n_max = 1, col_names = FALSE, col_types = 'ccccdcc', id = "file_path") |> 
  magrittr::set_colnames(header_col_names) |> 
  mutate(rc_date = as.Date(as.character(rc_date), format = '%Y%m%d'),
         rc_track = str_trim(str_to_upper(rc_track))) |> 
  select(-record_type)

chart_files <- read_csv(chart_file_paths, col_names = FALSE, skip = 1, col_types = cols(.default = col_character()), id = "file_path") |> 
  magrittr::set_colnames(races_col_names) |> 
  left_join(header, 
            by = "file_path") |> 
  select(-file_path)


#Do this on final frame not each one
%>%
  mutate(restrictions = as.factor(restrictions), 
         sex_restrictions = as.factor(sex_restrictions), 
         distance_unit = as.factor(distance_unit),
         surface = as.factor(surface),
         age_restrictions = as.factor(age_restrictions),
         track_condition = as.factor(track_condition)) %>%
  type_convert() %>%  
  select(country, rc_track, rc_date, rc_race, race_type, num_horses, purse, rc_distance, distance_unit, surface, track_condition) %>%
  mutate(distance_unit = if_else(is.na(distance_unit) | distance_unit == 'M' | distance_unit == 'F', 'F', 'Y'),
         rc_distance = round(rc_distance/100,2)) %>%
  mutate(rc_distance = if_else(distance_unit == 'Y', round(rc_distance * .454545, 1), rc_distance)) %>%
  select(-distance_unit, -race_type)



races <- 
  chart_file |> 
  filter(X1 == "R") |> 
  magrittr::set_colnames(races_col_names) |> 
  select(-record_type) |> 
  {\(x) bind_cols(header, x)}()























  






%>%
  filter(X1 == 'R')%>%
  bind_cols(header, .) %>%
  rename(
    record_type = X1, rc_race = X2, breed = X3, race_type = X4, restrictions = X5, sex_restrictions = X6,
    age_restrictions = X7, division = X8, purse = X9, reverts_money = X10, available_money = X11, paid_to_others = X12, guaranteed_money = X13, added_money = X14,
    includes_one_money = X16, includes_two_money = X18, includes_three_money = X20, plus_one_money = X22, plus_two_money = X24, 
    plus_three_money = X26, min_clm = X27, max_clm = X28, abt_dist = X29, rc_distance = X30, distance_unit = X31, surface = X32, course_type = X33,
    num_horses = X34, track_condition = X43, track_variant = X45, speed_number = X46)






chart_file |> 
  filter(X1 == "S") |> 
  separate(X64,
           into = col_names,
           sep = ",") |> 
  view()
