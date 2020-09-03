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
# ITU <- readRDS("../../data/processed_data/ITU_tidy.rds")
QoG_cs <- readRDS("../../data/processed_data/QoG_cs_tidy.rds")

# --- 2. Clean Countries Before Merging
path_to_country_dictionary = "../../data/processed_data/to_clean_countries/countries.csv"

CPI <- clean_countries(CPI, path_to_country_dictionary)
PENN <- clean_countries(PENN, path_to_country_dictionary)
# ITU <- clean_countries(ITU, path_to_country_dictionary)
QoG_cs <- clean_countries(QoG_cs, path_to_country_dictionary)

# --- 3. Merging Datasets 

# --- we start by merging CPI to PENN:
# we do an left join, as we do not want countries that are not in the CPI 
CPI_PENN <- left_join(CPI, PENN, by = c("consolidated_country")) %>%
  arrange(consolidated_country) # set order by country, for aesthetics and readability

# --- then we merge ITU to CPI_PENN:
# CPI_PENN_ITU <- left_join(CPI_PENN, ITU, by = c("consolidated_country")) 

# --- then we merge QoG_cs to CPI_PENN_ITU:
# to reoder columns in dataframe: http://www.datasciencemadesimple.com/re-arrange-re-order-column-dataframe-r-using-dplyr/
# CPI_PENN_ITU_QoG_cs <- left_join(CPI_PENN_ITU, QoG_cs, by = c("consolidated_country")) 

#remove?
# CPI_2020_all <- CPI_PENN %>%
#   rename(country = consolidated_country) %>%
#   mutate(ecommerce_capita = ecommerce/pop, patent_app_capita= patent_application/pop) %>%
#   select(country, laws, web_alexa, news_alexa, removal_google, freedom_net, 
#          state_attack, attack_objective, attack_surveillance, attack_control, attack_intelligence, attack_commercial, attack_offense,
#          tech_firm, tech_export, human_capital, cybermil_people,cyber_firm, computer_infection,
#          mobile_infection, socials_use, internet_use, surveillance_firm, shodan, military_strategy, cyber_command, CERTS, multi_agreement, bilat_agreement, softpower,
#          infocomm_imp,patent_application, patent_app_capita, broadband_speed, mobile_speed, ecommerce, ecommerce_capita,   
#           ITU,
#          everything()) %>%     # reorder columns so that country is first column 
#   select(-year)

CPI_2020 <- CPI_PENN %>%
  rename(country = consolidated_country) %>%
  mutate(ecommerce_capita = ecommerce/pop, patent_app_capita= patent_application/pop) %>%
  select(country, laws, web_alexa, news_alexa, removal_google, freedom_net, 
         infocomm_imp,patent_application, patent_app_capita, broadband_speed, mobile_speed, ecommerce, ecommerce_capita,
         state_attack, attack_objective, attack_surveillance, attack_control, attack_intelligence, attack_commercial, attack_offense,
         tech_firm, tech_export, human_capital, cybermil_people,cyber_firm, computer_infection,
         mobile_infection, socials_use, internet_use, surveillance_firm, shodan, military_strategy, cyber_command, CERTS, multi_agreement, bilat_agreement, softpower,
         ITU,
         everything()) %>%     # reorder columns so that country is first column 
  select(-year)

# --- 5. Saving 
saveRDS(CPI_2020, file = "../../data/data_for_modelling/CPI_2020.rds")

# saveRDS(CPI_2020_obj, file = "../../data/data_for_modelling/CPI_2020_obj.rds")

# Save in stata format
# CPI_2020 <- readRDS("../../data/data_for_modelling/CPI_2020.rds")
write.dta(CPI_2020,  file = "../../data/data_for_modelling/CPI_2020.dta") 



