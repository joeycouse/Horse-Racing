chart_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/chart_files/*.chart')

chart_files[[1]]

header_col_names = c("country_code", "rc_track", "rc_date", "num_races", "day_evening", "track_name")

header <- read_csv(chart_files[[1]], n_max = 1, col_names = FALSE, col_types = 'ccccdcc') |> 
  select(-X1 ) 

colnames(header) <- header_col_names

header <- 
  header |> 
  mutate(rc_date = as.Date(as.character(rc_date), format = '%Y%m%d'),
         rc_track = str_trim(str_to_upper(rc_track))) 



chart_file <- read_csv(chart_files[[1]], col_names = FALSE, skip = 1, col_types = cols(.default = col_character()))




%>%
  filter(X1 == 'R')%>%
  bind_cols(header, .) %>%
  rename(
    record_type = X1, rc_race = X2, breed = X3, race_type = X4, restrictions = X5, sex_restrictions = X6,
    age_restrictions = X7, division = X8, purse = X9, reverts_money = X10, available_money = X11, paid_to_others = X12, guaranteed_money = X13, added_money = X14,
    includes_one_money = X16, includes_two_money = X18, includes_three_money = X20, plus_one_money = X22, plus_two_money = X24, 
    plus_three_money = X26, min_clm = X27, max_clm = X28, abt_dist = X29, rc_distance = X30, distance_unit = X31, surface = X32, course_type = X33,
    num_horses = X34, track_condition = X43, track_variant = X45, speed_number = X46) %>%
  mutate(restrictions = as.factor(restrictions), 
         sex_restrictions = as.factor(sex_restrictions), 
         distance_unit = as.factor(distance_unit),
         surface = as.factor(surface),
         age_restrictions = as.factor(age_restrictions),
         track_condition = as.factor(track_condition)) %>%
  select(-starts_with('X')) %>%
  type_convert() %>%  
  select(country, rc_track, rc_date, rc_race, race_type, num_horses, purse, rc_distance, distance_unit, surface, track_condition) %>%
  mutate(distance_unit = if_else(is.na(distance_unit) | distance_unit == 'M' | distance_unit == 'F', 'F', 'Y'),
         rc_distance = round(rc_distance/100,2)) %>%
  mutate(rc_distance = if_else(distance_unit == 'Y', round(rc_distance * .454545, 1), rc_distance)) %>%
  select(-distance_unit, -race_type)




col_names <- c("dead_heat_flag", 
               "ind_clm_price", 
               "short_com", 
               "long_com", 
               "win_payoff", 
               "place_payoff",
               "show_payoff",
               "clm",
               "clm_trn_key",
               "clm_trn_type",
               "clm_trn_last_name",
               "clm_trn_first_name",
               "clm_trn_middle_name",
               "clm_own_key",
               "clm_own_type",
               "clm_own_last_name",
               "clm_own_first_name",
               "clm_own_middle_name",
               "src_rsn",
               "disq",
               "disq_plc",
               "trb",
               "corrected_wgt",
               "overwgt",
               "ind_time",
               "speed_index",
               "breeder_name",
               "jockey_key",
               "trainer_key")

chart_file |> 
  filter(X1 == "S") |> 
  select(X64) |> 
  separate(X64,
           into = col_names,
           sep = ",") |> 
  view()
