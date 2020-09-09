library(tidyverse)
library(data.table)

horse_files <- 
  Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.ch') %>%
  map_df(~fread(.))


horse_files<-janitor::make_clean_names(horse_files)


# To Do:
# Make Function that generates additional variables
# Win percentage, last time raced, horse age, etc


chart_files <- Sys.glob('~/Data Science/Horse Racing/Horse Racing Code/data/*.chart')

file_path <- "C:/Users/joeyc/Documents/Data Science/Horse Racing/Horse Racing Code/data/ASD20200525.chart"

read_chart_files <- function(file_path){
  
  header <- read_csv(file_path, n_max =1, col_names = FALSE)
  
}

read_csv("C:/Users/joeyc/Documents/Data Science/Horse Racing/Horse Racing Code/data/ASD20200525.chart", n_max =1, col_names = FALSE)

file <- read_file("C:/Users/joeyc/Documents/Data Science/Horse Racing/Horse Racing Code/data/ASD20200525.chart")




