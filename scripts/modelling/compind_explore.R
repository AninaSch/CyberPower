# This script normalizes the cyber power variables and produces an aggregated cyber power measure using the package "compind"

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
library(tidyverse)
library(foreign) 
library(dplyr)
library(Compind)
library(xtable)

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
CPI <- readRDS("../../data/data_for_modelling/CPI_2020.rds")


# -------------- NORMALIZATION
# normalise_ci(x, indic_col, polarity, method=1, z.mean=0, z.std=1, ties.method ="average")

# Standard z-scores normalisation 
data_norm = normalise_ci(CPI,c(2:13),c("NEG","POS","NEG","POS","POS","POS","POS","NEG","POS","POS","POS","POS"),method=1,z.mean=0, z.std=1)
summary(data_norm$ci_norm)

# -------------- WEIGHTS and AGGREGATION
# check with compind manuals

# Benefit of the Doubt approach
CI1 = ci_bod(data_norm$ci_norm,c(1:12))
CPI_CI = data.frame(CPI, CI1_est= CI1$ci_bod_est)

ggplot(data = CPI_CI, aes(x = removal_google, y = freedom_net)) +
  geom_point(aes(colour = CI_est),size=3)

# Factor analysis
CI2 = ci_factor(data_norm$ci_norm,c(1:12),method="CH", dim=3)
summ = summary(as.data.frame(CI2$ci_factor_est))

CPI_CI <- data.frame(CPI_CI, CI2_est= CI2$ci_factor_est)
print(xtable(summ,caption = "Factor Analysis scores based
             on first 12 components",label="tab_factor1"),
      include.rownames=FALSE)

# Generalized mean
# CI3 = ci_generalized_mean(EU_NUTS1,c(2:3),p=2) # geometric mean
CI3 = ci_generalized_mean(data_norm$ci_norm,c(1:3),p=1) # arithmetic mean
CPI_CI <- data.frame(CPI_CI, CI3_est= CI3$ci_generalized_mean_est)

# Min-Max
CI4 = ci_mean_min(data_norm$ci_norm, alpha=0.5, beta=1)
CPI_CI <- data.frame(CPI_CI, CI4_est= CI4$ci_mean_min_est)

# Weighting method based on geometric aggregation
CI5 = CI_geom_estimated = ci_geom_gen(data_norm$ci_norm,c(1:3),meth = "EQUAL")
summary(CI_geom_estimated$ci_mean_geom_est)

# Mazziotta-Pareto Index (MPI) method
CI6 = ci_mpi(data_norm$ci_norm, penalty="NEG")
summary(as.data.frame(CI6$ci_mpi_est))

# Wroclaw Taxonomic Method
CI7 = ci_wroclaw(data_norm$ci_norm,c(1:2))
summary(as.data.frame(CI7$ci_wroclaw_est))


# -------------- STATISTICAL AND CONCEPTUAL COHERENCE




# -------------- SENSITIVITY ANALYSIS



# -------------- VISUALIZATION


