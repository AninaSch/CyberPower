
# Tidy All the Necessary Data.

# This "main" script reads all the separate "Scripts to Tidy Data/Data_tidy.R" scripts used to tidy the data.
# Working Directory has to be set to this file's location.

# This file take a few minutes to run

#       0. setup
library(rio) # to import and export data
library(tidyverse) # data wrangling etc.

# --- 1. tidy CPI:
source("functions_to_tidy_data/tidy_CPI.R")
path_loadoriginal = "../../data/original_data/CPI/SummaryIndicators_2020March17.xlsx"
path_savetidy = "../../data/processed_data/CPI_tidy.rds"
tidy_PENN(path_loadoriginal, path_savetidy) # import original, clean, tidy, save in processed data

# --- 2. tidy Penn:
source("functions_to_tidy_data/tidy_PENN.R")
path_loadoriginal = "../../data/original_data/PENN/pwt91new.xlsx"
path_savetidy = "../../data/processed_data/PENN_tidy.rds"
tidy_PENN(path_loadoriginal, path_savetidy) # import original, clean, tidy, save in processed data

# --- 3. tidy ITU:
source("functions_to_tidy_data/tidy_ITU.R")
path_loadoriginal = "../../data/original_data/ITU.xlsx" # path to folder only!
path_savetidy = "../../data/processed_data/ITU_tidy.rds"
tidy_ITU(path_loadoriginal, path_savetidy) # import original, clean, tidy, save in processed data

# --- 4. tidy QoG_cs:
source("functions_to_tidy_data/tidy_QoG_cs.R")
path_loadoriginal = "../../data/original_data/QoG_QualityOfGovernment/qog_std_cs_jan20.csv.csv" # path to folder only!
path_savetidy = "../../data/processed_data/QoG_cs.rds"
tidy_ITU(path_loadoriginal, path_savetidy) # import original, clean, tidy, save in processed data


