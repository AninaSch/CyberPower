# This script normalizes the cyber power variables and produces an aggregated cyber power measure using the package "compind"
###(data=May 12)

rm(list=ls())

# -------------- OBJECTIVES X CAPABILITIES MAP

# Commercial = Commercial Gain of Enhancing Domestic Industry Growth
# Defense = Strengthening and Enhancing National Cyber Defenses
# Intelligence = Foreign Intelligence Collection for National Security
# Information Control = Controlling and Manipulating the Information Environment
# Norms = Defining International Cyber Norms and Standards
# Offense = Destroying or Disabling Adversary Infrastructure
# Surveillance = Surveilling and Monitoring Domestic Groups

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
  select(country,laws, attack_surveillance, freedom_net, surveillance_firm, socials_use, internet_use, intent_surveillance) %>%
  mutate(intent_surveillance=intent_surveillance*100)
defense <- CPI %>%
  select(country, laws, shodan,  human_capital, cyber_firm, computer_infection, mobile_infection, internet_use, broadband_speed, mobile_speed, infocomm_imp, CERTS, intent_defense) %>%
  mutate(intent_defense=intent_defense*100)
control <- CPI %>%
  select(country, attack_control, internet_use, socials_use, news_alexa, web_alexa, removal_google, intent_control) %>%
  mutate(intent_control=intent_control*100)
intelligence <- CPI %>%
  select(country, attack_intelligence, tech_export, human_capital, cybermil_people, tech_firm, surveillance_firm, intent_intelligence) %>%
  mutate(intent_intelligence=intent_intelligence*100)
commercial <- CPI %>%
  select(country, attack_commercial, tech_firm, human_capital, cyber_firm, web_alexa, ecommerce_capita, tech_export, infocomm_imp, patent_app_capita, intent_commercial) %>%
  mutate(intent_commercial=intent_commercial*100)
offense <- CPI %>%
  select(country, attack_offense, tech_export, cybermil_people, military_strategy, cyber_command, intent_offense) %>%
  mutate(intent_offense=intent_offense*100)
norms <- CPI %>%
  select(country, laws, muli_agreement, bilat_agreement, infocomm_imp, tech_firm, tech_export, softpower, intent_norms) %>%
  mutate(intent_norms=intent_norms*100)


# -------------- NORMALIZATION (MIN MAX)
# https://www.researchgate.net/post/What_are_the_best_normalization_methods_Z-Score_Min-Max_etc_How_would_you_choose_a_data_normalization_method

# Normalization methods
# # 1 (default) = standardization or z-scores. The formula for calculating a z-score is is z = (x-μ)/σ, where x is the raw score, μ is the population mean, and σ is the population standard deviation
# # 2 = Min-max method
# # 3 = Ranking method. If polarity="POS" ranking is increasing, while if polarity="NEG" ranking is decreasing

colnames(CPI) # show column names CPI

# CPI_overall
# data_norm = normalise_ci(overall,c(2:27),c("POS","POS","POS","POS","POS","NEG","POS","POS","POS", "POS",
#                                        "POS","POS","POS","POS","POS","POS","POS","NEG","NEG","POS",
#                                        "POS","POS","NEG", "POS", "POS", "POS"),
#                          method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)
# 
# CPI_norm <- cbind(CPI_countries$country, data_norm$ci_norm)  %>% # Create a matrix where x, y and z are columns 
#   rename(country= `CPI_countries$country`)
# 
# CPI_overall <- CPI_norm %>% 
#   gather(key, val, laws:shodan)%>%
#   group_by(country) %>%
#   mutate(
#     sum_norm = sum(val,  na.rm = TRUE),
#     mean_overall = mean(val,  na.rm = TRUE)
#   ) %>%
#   spread(key, val)

# CPI_surveillance
data_norm1 = normalise_ci(surveillance,c(2:8),c("POS","POS","POS","POS","POS","POS","POS"),
                         method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm1 <- cbind(CPI_countries$country, data_norm1$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_surveillance <- CPI_norm1 %>% 
  gather(key, val, laws:intent_surveillance)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_surveillance= mean_norm)


# CPI_defense
data_norm2 = normalise_ci(defense,c(2:13),c("POS","NEG","POS","POS","NEG","NEG","NEG","POS","POS","NEG", "POS", "POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm2 <- cbind(CPI_countries$country, data_norm2$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_defense <- CPI_norm2 %>% 
  gather(key, val, laws:intent_defense)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_defense= mean_norm)

# CPI_control
data_norm3 = normalise_ci(control,c(2:8),c("POS","POS","POS","POS","POS","POS","POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm3 <- cbind(CPI_countries$country, data_norm3$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_control <- CPI_norm3 %>% 
  gather(key, val, attack_control:intent_control)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_control= mean_norm)


# CPI_intelligence
data_norm4 = normalise_ci(intelligence,c(2:8),c("POS","POS","POS","POS","POS","POS","POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm4 <- cbind(CPI_countries$country, data_norm4$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_intelligence <- CPI_norm4 %>% 
  gather(key, val, attack_intelligence:intent_intelligence)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_intelligence= mean_norm)


# CPI_commercial
data_norm5 = normalise_ci(commercial,c(2:11),c("POS","POS","POS","POS","POS","POS","POS","NEG", "POS", "POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm5 <- cbind(CPI_countries$country, data_norm5$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_commercial <- CPI_norm5 %>% 
  gather(key, val, attack_commercial:intent_commercial)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_commercial= mean_norm)


# CPI_offense
data_norm6 = normalise_ci(offense,c(2:7),c("POS","POS","POS", "POS", "POS", "POS", "POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm6 <- cbind(CPI_countries$country, data_norm6$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_offense <- CPI_norm6 %>% 
  gather(key, val, attack_offense:intent_offense)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_offense= mean_norm)



# CPI_norms
data_norm7 = normalise_ci(norms,c(2:9),c("POS","POS","POS","NEG", "POS", "POS", "POS", "POS"),
                          method=2) #   method=1,z.mean=50, z.std=10)

CPI_norm7 <- cbind(CPI_countries$country, data_norm7$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

CPI_norms <- CPI_norm7 %>% 
  gather(key, val, laws:intent_norms)%>%
  group_by(country) %>%
  mutate(
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val) %>%
  select(country, mean_norms = mean_norm)




# -------------- COMBINED DATASET

# summarize(tot = round(sum(value), 1))

CPI_scores_cap = data.frame(CPI, score_surveillance = round(CPI_surveillance$mean_surveillance*100,3),
                        score_defense = round(CPI_defense$mean_defense*100,3), score_control = round(CPI_control$mean_control*100,3),
                        score_intelligence = round(CPI_intelligence$mean_intelligence*100,3), score_commercial = round(CPI_commercial$mean_commercial*100,3),
                        score_offense = round(CPI_offense$mean_offense*100,3), score_norms = round(CPI_norms$mean_norms*100,3)
                        )

CPI_scores_beta <- CPI_scores_cap %>%
  group_by(country) %>%
  mutate(score_capabilities = round(mean(c(score_surveillance, score_defense, score_control, score_intelligence,
                                  score_commercial, score_offense, score_norms), na.rm = TRUE ),3))


# CPI_scores <- CPI_scores_cap %>%
#   group_by(country) %>%
#   mutate(capint_surveillance = round((score_surveillance+(intent_surveillance*100))/2, 3),
#          capint_defense = round((score_defense+(intent_defense*100))/2, 3),
#          capint_control = round((score_control+(intent_control*100))/2, 3),
#          capint_intelligence = round((score_intelligence+(intent_intelligence*100))/2, 3),
#          capint_commercial = round((score_commercial+(intent_commercial*100))/2, 3),
#          capint_offense = round((score_offense+(intent_offense*100))/2, 3),
#          capint_norms = round((score_norms+(intent_norms*100))/2, 3)
#   ) %>%
#   mutate(score_capint = round(mean(c(capint_surveillance, capint_defense, capint_control, 
#                                      capint_intelligence, capint_commercial, capint_offense, capint_norms), na.rm = TRUE),3)) %>%
#   ungroup(country)

# 
# #
# CPI_scores_beta <- CPI_scores_cap %>%
#    group_by(country) %>%
#    mutate(capint_surveillance = round(score_surveillance*intent_surveillance, 3),
#           capint_defense = round(score_defense*intent_defense, 3),
#           capint_control = round(score_control*intent_control, 3),
#           capint_intelligence = round(score_intelligence*intent_intelligence, 3),
#           capint_commercial = round(score_commercial*intent_commercial, 3),
#           capint_offense = round(score_offense*intent_offense, 3),
#           capint_norms = round(score_norms*intent_norms, 3)
#           ) %>%
#   mutate(score_capint = round(mean(c(capint_surveillance, capint_defense, capint_control,
#                                   capint_intelligence, capint_commercial, capint_offense, capint_norms), na.rm = TRUE),3)) %>%
#   ungroup(country)


# -------------- SAVE AND EXPORT DATASET
  
  saveRDS(CPI_scores_beta, file = "../../data/data_for_modelling/CPI_scores_beta.rds")


# saveRDS(CPI_scores, file = "../../data/data_for_modelling/CPI_scores.rds")

rio:: export (CPI_scores_beta, file = "../../data/data_for_modelling/CPI_scores_beta.csv")



