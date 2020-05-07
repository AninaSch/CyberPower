# This script normalizes the cyber power variables and produces an aggregated cyber power measure using the package "compind"
###  (data= march18)

rm(list=ls())
# options(stringsAsFactors=FALSE, digits=3)
# setwd("/Users/schwarze/Documents/GitHub/CyberPower/scripts/data_processing")

# 10 STEP APPRAOCH

# STEP 1 Concept definition
# STEP 2 Indicator selection
# STEP 3 Data treatment and analysis
#       - inputation of missing values
#       - log linear inputation? 

# STEP 4 Normalization
# STEP 5 Weighting
# STEP 6 Aggregating indicators

# STEP 7 Statistical and conceptual coherence
# STEP 8 Uncertanity and sensitivity analysis

# STEP 9 Making sense of the data
# STEP 10 Visualization

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

# library("texreg")
# library("rjags")
# library("stringi")
# library("countrycode")
# library("segmented")
# library("plm")
# 
# 
# source("bgdp_functions.R")


# -------------- DATA IMPORT
CPI <- readRDS("../../data/data_for_modelling/CPI_2020_all.rds") %>%
  select(- state_attack) %>%
  filter(country != "North Korea", country != "New Zealand") # remove North Korea for now because of missing information

CPI_countries <- readRDS("../../data/data_for_modelling/CPI_countries.rds") %>%
  filter(country != "North Korea", country != "New Zealand") # remove North Korea for now because of missing information

# CPI_countries <- readRDS("../../data/data_for_modelling/CPI_countries.rds") %>%
#   select(country)
#   saveRDS(CPI_countries, file = "../../data/data_for_modelling/CPI_countries.rds")

# -------------- CORRELATION MATRIX
 
# GENERATE A CORRELATION MATRIX
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# https://rpkgs.datanovia.com/ggcorrplot/
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/

CPI.cor <- cor(CPI[, -1], method = c("pearson"))

# corr <- round(cor(CPI[, -1]), 1)
# head(corr[, 1:6])

# GENERATE A CORRELATION MATRIX WITH P VALUES
CPI.rcorr <- rcorr(as.matrix(CPI[, -1]), type = c("pearson"))
CPI.coeff = CPI.rcorr$r
CPI.p = CPI.rcorr$P

# p.mat <- cor_pmat(CPI[, -1])
# head(p.mat[, 1:4])

#  VISUALIZAITON
# https://briatte.github.io/ggcorr/  

# Pearson correlation coefficients, using pairwise observations (default method)
ggcorr(CPI[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

ggcorr(CPI[, 2:15], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
ggcorr(CPI[, 2:15], geom = "circle", method = c("pairwise", "pearson"), nbreaks = 5, min_size = 0, max_size = 6)

# problematic correlations: 
#   - patent application and ecommerce
#   - freedom of net and skilled employees


# -------------- NORMALIZATION
# normalise_ci(x, indic_col, polarity, method=1, z.mean=0, z.std=1, ties.method ="average")

# Normalization methods
# # 1 (default) = standardization or z-scores. The formula for calculating a z-score is is z = (x-μ)/σ, where x is the raw score, μ is the population mean, and σ is the population standard deviation
# # 2 = Min-max method
# # 3 = Ranking method. If polarity="POS" ranking is increasing, while if polarity="NEG" ranking is decreasing

colnames(CPI) # show column names CPI

# 1 (DEFAULT) = STANDARDIZATION OR Z-SCORE
data_norm = normalise_ci(CPI,c(2:27),c("POS","POS","POS","NEG","POS","POS","NEG","POS","POS","POS",
                                       "POS","POS","POS","POS","POS","POS","POS","POS","POS","POS",
                                       "NEG","NEG","POS","POS","POS","NEG"),
                         method=1,z.mean=50, z.std=20) #   method=1,z.mean=50, z.std=10)

summary(data_norm$ci_norm)

# dataframe with z-scores
CPI_norm <- cbind(CPI_countries$country, data_norm$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

# 2 = MIN MAX METHOD
data_minmax = normalise_ci(CPI,c(2:27),c("POS","POS","POS","NEG","POS","POS","NEG","POS","POS","POS",
                                       "POS","POS","POS","POS","POS","POS","POS","POS","POS","POS",
                                       "NEG","NEG","POS","POS","POS","NEG"),
                         method=2)
summary(data_minmax$ci_norm)

# dataframe with min-max
CPI_minmax <- cbind(CPI_countries$country, data_minmax$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

# 3 = RANKING METHOD
data_rank = normalise_ci(CPI,c(2:27),c("POS","POS","POS","NEG","POS","POS","NEG","POS","POS","POS",
                                         "POS","POS","POS","POS","POS","POS","POS","POS","POS","POS",
                                         "NEG","NEG","POS","POS","POS","NEG"),
                           method=3, ties.method = average) # other options: ties.method = c("average", "first", "last", "random", "max", "min")
summary(data_rank$ci_norm)

# dataframe with min-max
CPI_rank <- cbind(CPI_countries$country, data_rank$ci_norm)  %>% # Create a matrix where x, y and z are columns 
  rename(country= `CPI_countries$country`)

# -------------- AGGREGATE OVER SUM AND MEANS
# https://stackoverflow.com/questions/54601211/summing-values-in-r-based-on-column-value-with-dplyr

# 1 (DEFAULT) = STANDARDIZATION OR Z-SCORE
CPI_norm_summean <- CPI_norm %>% 
  gather(key, val, laws:shodan)%>%
  group_by(country) %>%
  mutate(
    sum_norm = sum(val,  na.rm = TRUE),
    mean_norm = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val)

# 2 = MIN MAX METHOD
CPI_minmax_summean <- CPI_minmax %>% 
  gather(key, val, laws:shodan)%>%
  group_by(country) %>%
  mutate(
    sum_minmax = sum(val,  na.rm = TRUE),
    mean_minmax = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val)

# 3 = RANKING METHOD
CPI_rank_summean <- CPI_rank %>% 
  gather(key, val, laws:shodan)%>%
  group_by(country) %>%
  mutate(
    sum_rank = sum(val,  na.rm = TRUE),
    mean_rank = mean(val,  na.rm = TRUE)
  ) %>%
  spread(key, val)

# EXPORT TO CSV

rio:: export (CPI_norm_summean, file = "../../data/data_for_modelling/CPI_norm_summean.csv")
rio:: export (CPI_minmax_summean, file = "../../data/data_for_modelling/CPI_minmax_summean.csv")
rio:: export (CPI_rank_summean, file = "../../data/data_for_modelling/CPI_rank_summean.csv")

# -------------- COMPLEX WEIGHTS and AGGREGATION METHODS USING Z-SCORE
# https://ggplot2.tidyverse.org/reference/geom_text.html
# check with compind manuals
# does not work when some values are missing
# methods that could be of interest: generalized mean (CI4), mean-min (CI3), MPI (CI6), taxonomic (CI7)

# PRINCIPAL METHODS

# Benefit of the Doubt approach
CI1 = ci_bod(data_norm$ci_norm,c(1:11))
CPI_CI = data.frame(CPI, CI1_est= CI1$ci_bod_est)

ggplot(data = CPI_CI, aes(x = skilled_employees, y = tech_exports, label = CPI_CI$country)) +
  geom_point(aes(colour = CI1_est),size=3) + geom_text(aes(colour = CI1_est), hjust = -0.10, nudge_x = 0.05, check_overlap = T)

# Factor analysis
CI2 = ci_factor(data_norm$ci_norm,c(1:11),method="CH", dim=3)
summ = summary(as.data.frame(CI2$ci_factor_est))
CPI_CI <- data.frame(CPI_CI, CI2_est= CI2$ci_factor_est)

print(xtable(summ,caption = "Factor Analysis scores based
             on first 11 components",label="tab_factor1"),
             include.rownames=FALSE)

ggplot(data = CPI_CI, aes(x = skilled_employees, y = tech_exports, label = CPI_CI$country)) +
  geom_point(aes(colour = CI2_est),size=3) + geom_text(aes(colour = CI2_est), hjust = -0.10, nudge_x = 0.05, check_overlap = T)

# Mean_min
CI3 = ci_mean_min(data_minmax$ci_norm, alpha=0.5, beta=1)
CPI_CI <- data.frame(CPI_CI, CI3_est= CI3$ci_mean_min_est)

ggplot(data = CPI_CI, aes(x = skilled_employees, y = tech_exports, label = CPI_CI$country)) +
  geom_point(aes(colour = CI3_est),size=3) + geom_text(aes(colour = CI3_est), hjust = -0.10, nudge_x = 0.05, check_overlap = T)

# MORE METHODS

# Generalized mean
# CI4 = ci_generalized_mean(EU_NUTS1,c(2:3),p=2) # geometric mean
CI4 = ci_generalized_mean(data_norm$ci_norm,c(1:11),p=1) # arithmetic mean
CPI_CI <- data.frame(CPI_CI, CI4_est= CI4$ci_generalized_mean_est)

# Weighting method based on geometric aggregation
CI5 = CI_geom_estimated = ci_geom_gen(data_norm$ci_norm,c(1:11),meth = "EQUAL")
summary(CI_geom_estimated$ci_mean_geom_est)

# Mazziotta-Pareto Index (MPI) method
CI6 = ci_mpi(data_norm$ci_norm, penalty="NEG")
summary(as.data.frame(CI6$ci_mpi_est))

CPI_CI <- data.frame(CPI_CI, CI6_est= CI6$ci_mpi_est)

# Wroclaw Taxonomic Method
CI7 = ci_wroclaw(data_norm$ci_norm,c(1:2))
summary(as.data.frame(CI7$ci_wroclaw_est))

CPI_CI <- data.frame(CPI_CI, CI7_est= CI7$ci_wroclaw_est)

# -------------- SENSITIVITY ANALYSIS

# COMBINE DATAFRAMES

CPI_score <- data.frame(CPI, mean_norm = CPI_norm_summean$mean_norm, 
                        mean_minmax = CPI_minmax_summean$mean_minmax,
                        mean_rank = CPI_rank_summean$mean_rank)


# CORRELATION BETWEEN GDP, ITU AND CI SCORES
ggcorr(CPI_score[, c("GDPexp_capita","itu_2018", "mean_norm", "mean_minmax", "mean_rank")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")


# correlation between GDP, ITU and CI scores
# ggcorr(CPI_CI[, c("GDPexp_capita","itu_2018", "CI1_est", "CI2_est", "CI3_est")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")






# TO DO
# - detect outliers of CI
# - Kernel distribution of CI
# - compare with other composite indicators method papers

# INPUT FROM COIN
# outliers? Composite indicators can be sensitive to outliers in data. treat outliers using Winsorisation? 
# Box Cox: For indicators that still have skew and kurtosis values beyond the thresholds
# Scatterplots

