
# Aim of this function:
#   read the CPI data, select the variables of interest, add new computed variables if needed, and,
#   save the output as CPI_tidy.rds.

# note: this function is called from tidy_datasets.R

# TO DO
# --> scale some varialbes to per capita
# --> reverse some variables so that all are the higher, the more cyber power
# https://aaroncharlton.com/2018/01/19/transforming-variables-with-dplyr/
# https://www.marsja.se/reverse-scoring-using-r/
# https://stackoverflow.com/questions/18933441/how-to-reverse-order-a-vector
# https://statisticsglobe.com/rev-r-function
# http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/affy/html/xy2indices.html

library(foreign)
library(tidyverse)


CPI <- function(path_loadoriginal, path_savetidy){
  
#       1. read CPI Data
  print("importing CPI data... ")
  CPI <- rio::import(path_loadoriginal)
  CPI <- rio::import("../../../data/original_data/CPI/SummaryIndicators_2020March18.xlsx") # for debugging

  print("importing done")
  
  CPI_tidy <- CPI %>%
    mutate(
      country = as.factor(country)
      ) %>%
    # mutate_at(vars(skep1,skep2,skep3,skep4,skep5, // for reverse coding
    #                skep6,skep7,skep8,skep9),
    #           funs(abs(.-8)))    // subtract from var "max+1"
    arrange(country)
  # glimpse(penn_tidy)
  print("tidying done")
  
 
  
  saveRDS(CPI_tidy, file = path_savetidy)
  saveRDS(CPI_tidy, file = "../../../data/processed_data/CPI_tidy.rds")
  
  print("processed CPI data saved")
  
}
