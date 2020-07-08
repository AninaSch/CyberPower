# This script normalizes the cyber power variables and produces an aggregated cyber power measure using the package "compind"
###(data=May 12)

# CHANGES
# new data points
# 
# "removal google" to positive
# 
# remove freedom of the press
# 
# TO DO
# remove laws?
  
rm(list=ls())

# --- 0. Setup
# install.packages("GGally")

library(tidyverse)
library(foreign) 
library(dplyr)
library(Compind)
library(xtable)
library(ggplot2)
library(GGally)
library(Hmisc)
library(ggcorrplot)


# -------------- DATA IMPORT

CPI <- readRDS("../../data/data_for_modelling/CPI_2020_obj.rds") %>%
  filter(country != "North Korea") # remove North Korea for now because of missing information

CPI_countries <- readRDS("../../data/data_for_modelling/CPI_countries.rds") %>%
  filter(country != "North Korea") # remove North Korea for now because of missing information

overall <- readRDS("../../data/data_for_modelling/CPI_2020_all.rds") %>%
  select(- attack_objective, -ecommerce, -freedom_press, -patent_application) %>%
  filter(country != "North Korea") # remove North Korea for now because of missing information

# -------------- SUBSET DATA

surveillance <- CPI %>%
  select(country,laws, attack_surveillance, freedom_net, cyber_firm, socials_use, internet_use, cybermil_people)

defense <- CPI %>%
  select(country, laws, shodan, int_agreement, human_capital, cyber_firm, computer_infection, mobile_infection, internet_use, broadband_speed, mobile_speed, CERTS)

manipulation <- CPI %>%
  select(country, attack_manipulation, internet_use, socials_use, freedom_net, news_alexa, web_alexa, removal_google)

intelligence <- CPI %>%
  select(country, attack_intelligence, tech_export, human_capital, cybermil_people, tech_firm, surveillance_firm)

commercial <- CPI %>%
  select(country, attack_commercial, int_agreement, tech_firm, human_capital, cyber_firm, news_alexa, ecommerce_capita)

disruption <- CPI %>%
  select(country, attack_disruption, tech_export, cybermil_people, military_strategy, cyber_command)

norms <- CPI %>%
  select(country, laws, int_agreement, infocomm_imp, tech_firm)
  
  
# -------------- NORMALIZATION (Z-SCORE)
# https://www.researchgate.net/post/What_are_the_best_normalization_methods_Z-Score_Min-Max_etc_How_would_you_choose_a_data_normalization_method

# Normalization methods
# # 1 (default) = standardization or z-scores. The formula for calculating a z-score is is z = (x-μ)/σ, where x is the raw score, μ is the population mean, and σ is the population standard deviation
# # 2 = Min-max method
# # 3 = Ranking method. If polarity="POS" ranking is increasing, while if polarity="NEG" ranking is decreasing

colnames(CPI) # show column names CPI

# CPI_overall
data_norm = normalise_ci(overall,c(2:27),c("POS","POS","POS","POS","POS","NEG","POS","POS","POS", "POS",
                                       "POS","POS","POS","POS","POS","POS","POS","NEG","NEG","POS",
                                       "POS","POS","NEG", "POS", "POS", "POS"),
                         method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm <- cbind(CPI_countries$country, data_norm$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_overall <- CPI_norm %>% 
  gather(key, val, laws:shodan)%>%
  group_by(country) %>%
  mutate(
    sum_norm = sum(val,  na.rm = TRUE),
    mean_overall = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val)

# CPI_surveillance
data_norm1 = normalise_ci(surveillance,c(2:8),c("POS","POS","POS","POS","POS","POS","POS"),
                         method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm1 <- cbind(CPI_countries$country, data_norm1$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_surveillance <- CPI_norm1 %>% 
  gather(key, val, laws:cybermil_people)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_surveillance= mean_norm)

# CPI_defense
data_norm2 = normalise_ci(defense,c(2:12),c("POS","NEG","POS","POS","POS","NEG","NEG","POS","POS","POS", "POS"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm2 <- cbind(CPI_countries$country, data_norm2$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_defense <- CPI_norm2 %>% 
  gather(key, val, laws:mobile_speed)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_defense= mean_norm)


# CPI_manipulation
data_norm3 = normalise_ci(manipulation,c(2:8),c("POS","NEG","NEG","POS","POS","POS","POS"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm3 <- cbind(CPI_countries$country, data_norm3$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_manipulation <- CPI_norm3 %>% 
  gather(key, val, attack_manipulation:removal_google)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_manipulation= mean_norm)

# CPI_intelligence
data_norm4 = normalise_ci(intelligence,c(2:7),c("POS","POS","POS","POS","POS","POS","POS","NEG"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm4 <- cbind(CPI_countries$country, data_norm4$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_intelligence <- CPI_norm4 %>% 
  gather(key, val, attack_intelligence:surveillance_firm)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_intelligence= mean_norm)

# CPI_commercial
data_norm5 = normalise_ci(commercial,c(2:8),c("POS","POS","POS","POS","POS","POS", "POS"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm5 <- cbind(CPI_countries$country, data_norm5$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_commercial <- CPI_norm5 %>% 
  gather(key, val, attack_commercial:news_alexa)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_commercial= mean_norm)

# CPI_disruption
data_norm6 = normalise_ci(disruption,c(2:5),c("POS","POS","POS", "POS", "POS"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm6 <- cbind(CPI_countries$country, data_norm6$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_disruption <- CPI_norm6 %>% 
  gather(key, val, attack_disruption:cybermil_people)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_disruption= mean_norm)

# CPI_norms
data_norm7 = normalise_ci(norms,c(2:5),c("POS","POS","NEG", "POS"),
                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

CPI_norm7 <- cbind(CPI_countries$country, data_norm7$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_cybernorms <- CPI_norm7 %>% 
  gather(key, val, laws:tech_firm)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_cybernorms = mean_norm)

# -------------- COMBINED DATASET

CPI_scores = data.frame(CPI, score_overall = CPI_overall$mean_overall, score_surveillance = CPI_surveillance$mean_surveillance,
                        score_defense = CPI_defense$mean_defense, score_manipulation = CPI_manipulation$mean_manipulation,
                        score_intelligence = CPI_intelligence$mean_intelligence, score_commercial = CPI_commercial$mean_commercial,
                        score_disruption = CPI_disruption$mean_disruption, score_cybernorms = CPI_cybernorms$mean_cybernorms
                        )

CPI_scores <- CPI_scores %>%
  group_by(country) %>%
  mutate(score_objective = mean(c(score_surveillance, score_defense, score_manipulation, score_defense,
                                  score_commercial, score_disruption, score_cybernorms), na.rm = TRUE ))


# 
saveRDS(CPI_scores, file = "../../data/data_for_modelling/CPI_scores.rds")

rio:: export (CPI_scores, file = "../../data/data_for_modelling/CPI_scores.csv")


# -------------- OBJECTIVES X CAPABILITIES MAP


# Surveilling and Monitoring Domestic Groups [10 capabilities]
# 1.	Existence of Content, Privacy and/or Cyber Crime Laws;
# 2.	Existence of agencies dedicated to domestic content control;
# 3.	Number of state-sponsored attacks within the Council On Foreign Relations 300 Sophisticated Attacks database, (if there exist attacks likely to have resulted in domestic stability or monitoring);
# 4.	State’s Freedom of the Net Ranking;
# 5.	The number of cyber military unit personnel;
# 6.	The number of top domestic cyber security firms, if the firm advertises monitoring or surveillance;
# 7.	Percent of internet users using social media;
# 8.	Percent of individuals using the internet;
# 9.	Existence of a domestic cyber crime agency; and
# 10.	The cost of cybercrime to the state as a percentage of GDP


# A	Strengthening and Enhancing National Cyber Defenses [10 capabilities]
# 1.	The existence of domestic cyber security laws;
# 2.	Shodan results;
# 3.	The number of signed bilateral or international cyber agreements, if there are any proven to have resulted in enhancing defenses;
# 4.	The existence of ayber military strategy, if it specifically states defensive goals;
# 5.	The number of skilled employees within a country;
# 6.	The number of top domestic cyber security firms, if cyber defense or security is advertised by the firm;
# 7.	Domestic computer infection rates;
# 8.	Mobile infection rates
# 9.	Percentage of individuals using the internet; and
# 10.	Internet speeds within the country.


# B	Controlling and Manipulating the Information Environment [8 capabilities]
# 1.	Existence of agencies dedicated to foreign content control;
# 2.	Number of state-sponsored attacks within the Council On Foreign Relations 300 Sophisticated Attacks database, (if there exist attacks likely to have resulted in manipulating the information environment);
# 3.	Percent of internet users using social media;
# 4.	Percent of individuals using the internet;
# 5.	The state’s Freedom of the Net ranking;
# 6.	Top news sites in Alexa 50 that are from the state;
# 7.	Top sites in Alexa 50 overall that originate from the state; and
# 8.	Google content removal requests from the state.

# C	Intelligence Gathering and Collection in other Countries [8 capabilities]
# 1.	Existence of agencies dedicated to foreign collection;
# 2.	Number of state-sponsored attacks within the Council On Foreign Relations 300 Sophisticated Attacks database, (if there exist attacks likely to have resulted in intelligence collection);
# 3.	The existence of a cyber military strategy;
# 4.	Amount of tech exports as a percentage of manufacturing exports;
# 5.	Number of skilled industry employees;
# 6.	Number of cyber military unit personnel;
# 7.	Number of top cyber security firms, if intelligence/OSINT is advertised by the firm; and
# 8.	Number of private surveillance firms that operate or are located in the state.

# D	Commercial Gain and/or Enhancing Domestic Industry Growth [10 capabilities]
# 1.	Number of state-sponsored attacks within the Council On Foreign Relations 300 Sophisticated Attacks database, (if there exist attacks likely to have resulted in corporate espionage or technology transfer);
# 2.	Number of signed bilateral or international agreements, if commercial gain is a factor / industry is a factor;
# 3.	Number of the global top 100 tech firms based in the state;
# 4.	The number of skilled employees;
# 5.	Number of top cybersecurity firms based in the state;
# 6.	Top sites in Alexa 50 overall that originate from the state;
# 7.	Number of patent applications filed in the state;
# 8.	The state’s e-commerce sales as a percentage of the GDP;
# 9.	High tech exports of a state as a percentage of manufacturing exports; and
# 10.	[supply chain]??
#   
# E	Destroying or Disabling an Adversary’s Infrastructure and Capabilities 
# 1.	Number of state-sponsored attacks within the Council On Foreign Relations 300 Sophisticated Attacks database, (if there exist attacks likely to have resulted in the destruction or disablement of adversary);
# 2.	The existence of a cyber military strategy;
# 3.	High tech exports of a state as a percentage of manufacturing exports; and
# 4.	Number of cyber military unit personnel; 
# 
# F	Defining International Cyber Norms and Technical Standards [6 capabilities]
# 1.	The existence of content, privacy or security laws;
# 2.	The existence of a cyber military strategy;
# 3.	The scored efficacy of the state’s cyber security strategy;
# 4.	The number of signed international and bilateral agreements overall;
# 5.	High tech imports in a state; and
# 6.	The number of global top 100 tech firms in the state.


