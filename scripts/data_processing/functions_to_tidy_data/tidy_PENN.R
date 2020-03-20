
# Aim of this function:
#   read the PENN data, select the variables of interest, add new computed variables if needed, and,
#   save the output as PENN_tidy.rds.

# note: this function is called from tidy_datasets.R

tidy_PENN <- function(path_loadoriginal, path_savetidy){
  
  #       1. read Polity Data
  print("importing Penn data... ")
  penn <- rio::import(path_loadoriginal)
  penn <- rio::import("../../../data/original_data/PENN/pwt91new.xlsx") # for debugging

  print("importing done")
  
  PENN_tidy <- penn %>%
    select(
      year,
      country,
      GDP_expentiture = rgdpe, 
      GDP_output = rgdpo,
      pop
    ) %>%
    filter(
      year == 2017
    ) %>%
    mutate(
      country = as.factor(country),
      GDPexp_capita = GDP_expentiture / pop # note: both are in millions so it works
      ) %>%
    arrange(country, year)
  # glimpse(penn_tidy)
  print("tidying done")
  
  
  saveRDS(PENN_tidy, file = path_savetidy)
  saveRDS(PENN_tidy, file = "../../../data/processed_data/PENN_tidy.rds")
  
  print("processed Penn data saved")
  
}
