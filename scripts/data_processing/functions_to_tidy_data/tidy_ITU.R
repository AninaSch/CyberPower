
# Aim of this function:
#   read the ITU data, select the variables of interest, add new computed variables if needed, and,
#   save the output as PENN_tidy.rds.

# note: this function is called from tidy_datasets.R

tidy_ITU <- function(path_loadoriginal, path_savetidy){
  
  #       1. read ITU Data
  print("importing ITU data... ")
  ITU <- rio::import(path_loadoriginal)
  ITU <- rio::import("../../../data/original_data/ITU/ITU_2018.xlsx") # for debugging

  print("importing done")
  
  ITU_tidy <- ITU %>%
    arrange(country)
  # glimpse(penn_tidy)
  print("tidying done")
  
  
  saveRDS(ITU_tidy, file = path_savetidy)
  saveRDS(ITU_tidy, file = "../../../data/processed_data/ITU_tidy.rds")
  
  print("processed ITU data saved")
  
}
