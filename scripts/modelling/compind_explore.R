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
CPI <- readRDS("../../data/data_for_modelling/CPI_2020.rds") %>%
  filter(country != "North Korea") # remove North Korea for now because of missing information

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
# # 1 (default) = standardization or z-scores
# # 2 = Min-max method
# # 3 = Ranking method. If polarity="POS" ranking is increasing, while if polarity="NEG" ranking is decreasing

# Standard z-scores normalisation with restricted number of indicators
data_norm = normalise_ci(CPI,c(3, 5, 6:14),c("POS","POS","NEG","POS","POS","POS","POS","POS","POS","POS","POS"),method=1,z.mean=0, z.std=1)
summary(data_norm$ci_norm)


# -------------- WEIGHTS and AGGREGATION
# https://ggplot2.tidyverse.org/reference/geom_text.html

# check with compind manuals

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

# Min-Max
CI3 = ci_mean_min(data_norm$ci_norm, alpha=0.5, beta=1)
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

# Wroclaw Taxonomic Method
CI7 = ci_wroclaw(data_norm$ci_norm,c(1:2))
summary(as.data.frame(CI7$ci_wroclaw_est))


# -------------- SENSITIVITY ANALYSIS

# correlation between GDP, ITU and CI scores
ggcorr(CPI_CI[, c("GDPexp_capita","itu_2018", "CI1_est", "CI2_est", "CI3_est")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# TO DO
# - detect outliers of CI
# - Kernel distribution of CI
# - compare with other composite indicators method papers

# INPUT FROM COIN
# outliers? Composite indicators can be sensitive to outliers in data. treat outliers using Winsorisation? 
# Box Cox: For indicators that still have skew and kurtosis values beyond the thresholds
# Scatterplots

