library(tidyverse)
source("chart_file_col_names.R")
chart_file_paths <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/chart_files/*.chart')


headers <- read_csv(chart_file_paths, n_max = 1, col_names = FALSE, col_types = 'ccccdcc', id = "file_path") |> 
  magrittr::set_colnames(header_col_names) |> 
  mutate(rc_date = as.Date(as.character(rc_date), format = '%Y%m%d'),
         rc_track = str_trim(str_to_upper(rc_track))) |> 
  select(-record_type)


chart_files <- read_csv(chart_file_paths, col_names = FALSE, skip = 1, col_types = cols(.default = col_character()), id = "file_path") |> 
  magrittr::set_colnames(races_col_names) |> 
  left_join(headers, 
            by = "file_path") |> 
  relocate(file_path, country_code, rc_track, rc_date, rc_race, num_races, day_evening, track_name, .before = everything())


races <- 
  chart_files |> 
  filter(record_type == "R") |> 
  select(-record_type) |> 
  mutate(restrictions = as.factor(restrictions), 
         sex_restrictions = as.factor(sex_restrictions), 
         distance_unit = as.factor(distance_unit),
         surface = as.factor(surface),
         age_restrictions = as.factor(age_restrictions),
         track_condition = as.factor(track_condition)) |> 
  type_convert() |> 
  mutate(distance_unit = if_else(is.na(distance_unit) | distance_unit == 'M' | distance_unit == 'F', 'F', 'Y'),
         rc_distance = round(rc_distance/100,2)) |> 
  mutate(rc_distance = if_else(distance_unit == 'Y', round(rc_distance * .454545, 1), rc_distance)) |> 
  select(-distance_unit) |> 
  relocate(file_path, country_code, rc_track, rc_date, rc_race, num_races, day_evening, track_name, .before = everything())


total_rows <-
  chart_files |> 
  filter(record_type == "S") |> 
  count(file_path, country_code, rc_track, rc_date) |> 
  rename(n_max = n)

starting_row <- 
  chart_files |> 
  filter(record_type == "R")|> 
  count(file_path, country_code, rc_track, rc_date) |> 
  mutate(skip = n + 1, .keep = "unused") 

starters_index <- 
  left_join(starting_row, total_rows) |> 
  select(file_path, skip, n_max)

starters_index <- as.list(starters_index)

starters <- 
  pmap_dfr(starters_index, 
                     ~read_csv(..1, col_names = F, skip = ..2, n_max = ..3, show_col_types = F, 
                               col_types = cols(.default = col_character()), id = "file_path")) |> 
  magrittr::set_colnames(starter_col_names) |> 
  left_join(headers, by = "file_path") |> 
  relocate(file_path, country_code, rc_track, rc_date, rc_race, num_races, day_evening, track_name, .before = everything())

footnotes <- chart_files |> 
  filter(record_type == "F") |> 
  select(file_path, country_code, rc_track, rc_date, rc_race,num_races, day_evening, track_name, footnote_num = breed_type_race, footnote = race_type) |> 
  group_by(file_path, country_code, rc_track, rc_date, rc_race) |> 
  summarise(footnote = paste(footnote, collapse = ' '))
  
write_feather(headers, './data/headers.feather')
write_feather(races, './data/races.feather')
write_feather(starters, './data/starters.feather')
write_feather(footnotes, './data/footnotes.feather')
