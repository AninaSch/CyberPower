# This script recodes some of the variables in the data for modelling.
# The script could be integrated in "merging data"

# --- 0. Setup
library(tidyverse)
library(foreign) 
library(dplyr)

# any functions?
# source("functions/clean_countries.R") # function to clean country names

# --- 1. Load Processed Data
CPI <- readRDS("../../data/data_for_modelling/CPI_2020.rds")

# --- 2. add information that is missing for pop and GDP

# --- 3. Per capita
CPI_capita <- CPI %>%
  mutate(ecommerce_capita = ecommerce/pop, patent_app_capita= patent_application/pop)


# --- 5. Saving 
saveRDS(CPI_capita, file = "../../data/data_for_modelling/CPI_2020.rds")



# This will be done later

# --- Flip cyber power variables so that all go in the same direction (the greater, the higher the cyber power)
# reverse scores: https://www.marsja.se/reverse-scoring-using-r/
# https://stackoverflow.com/questions/26877917/reverse-scoring-items
# reverse vector: https://stackoverflow.com/questions/18933441/how-to-reverse-order-a-vector
