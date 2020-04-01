# This script merges the processed data together to create a dataset ready for PANEL ANALYSIS.

# duplicate Fragility & HIEF
# https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
# duplicated(), unique()
# (duplicated(isTRUE(Fragility)))

# --- 0. Setup
library(tidyverse)
# library(foreign) 

source("functions/clean_countries.R") # function to clean country names

# --- 1. Load Processed Datasets
CPI <- readRDS("../../data/processed_data/CPI_tidy.rds")
PENN <- readRDS("../../data/processed_data/PENN_tidy.rds")
ITU <- readRDS("../../data/processed_data/ITU_tidy.rds")
QoG_cs <- readRDS("../../data/processed_data/QoG_cs_tidy.rds")

# --- 2. Clean Countries Before Merging
path_to_country_dictionary = "../../data/processed_data/to_clean_countries/countries.csv"

CPI <- clean_countries(CPI, path_to_country_dictionary)
PENN <- clean_countries(PENN, path_to_country_dictionary)
ITU <- clean_countries(ITU, path_to_country_dictionary)
QoG_cs <- clean_countries(QoG_cs, path_to_country_dictionary)


# --- 3. Merging Datasets 

# --- we start by merging CPI to PENN:
# we do an left join, as we do not want countries that are not in the CPI 
CPI_PENN <- left_join(CPI, PENN, by = c("consolidated_country")) %>%
  arrange(consolidated_country) # set order by country, for aesthetics and readability

# --- then we merge ITU to CPI_PENN:
CPI_PENN_ITU <- left_join(CPI_PENN, ITU, by = c("consolidated_country")) 

# --- then we merge QoG_cs to CPI_PENN_ITU:
# to reoder columns in dataframe: http://www.datasciencemadesimple.com/re-arrange-re-order-column-dataframe-r-using-dplyr/
# CPI_PENN_ITU_QoG_cs <- left_join(CPI_PENN_ITU, QoG_cs, by = c("consolidated_country")) 

CPI_2020 <- CPI_PENN_ITU %>%
  rename(country = consolidated_country) %>%
  mutate(ecommerce_capita = ecommerce/pop, patent_app_capita= patent_application/pop) %>%
  select(country, ecommerce, ecommerce_capita, patent_application, patent_app_capita, 
         removal_google,freedom_net, infocomm_imp,
         tech_exports, skilled_employees, internet_use, cyber_prepared,
         bilat_agreement, critical_infrastructure, state_attacks,
         everything()) %>%     # reorder columns so that country is first column 
  select(-year)

# --- 5. Saving 
saveRDS(CPI_2020, file = "../../data/data_for_modelling/CPI_2020.rds")


