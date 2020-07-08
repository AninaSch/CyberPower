# This script normalizes the cyber power variables and produces an aggregated cyber power measure using the package "compind"
###  (data= march18)

# can we combine indicators that do not correlate?
#   https://www.researchgate.net/post/How_to_check_statistically_whether_two_or_more_variables_can_be_combined_into_one_derived_variable

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
library(ggrepel) # ggrepel provides geoms for ggplot2 to repel overlapping text labels: geom_text_repel() geom_label_repel()
# https://github.com/slowkow/ggrepel
# library radarchart
library(fmsb)
library(colormap)



# source("bgdp_functions.R")

# -------------- DATA IMPORT

CPI_scores <- readRDS("../../data/data_for_modelling/CPI_scores.rds") 
  # group_by(country) %>%
  # mutate(score_capabilities = mean(c(score_surveillance, score_defense, score_control, score_defense,
  #                                 score_commercial, score_offense, score_norms), na.rm = TRUE ))

cor(CPI_scores$broadband_speed, CPI_scores$internet_use, use = "complete.obs")

# UPDATE

surveillance <- CPI_scores %>%
  select(country,laws, attack_surveillance, freedom_net, surveillance_firm, socials_use, internet_use)
defense <- CPI_scores %>%
  select(country, laws, shodan,  human_capital, cyber_firm, computer_infection, mobile_infection, internet_use, broadband_speed, mobile_speed, infocomm_imp, CERTS)
control <- CPI_scores %>%
  select(country, attack_control, internet_use, socials_use, news_alexa, web_alexa, removal_google)
intelligence <- CPI_scores %>%
  select(country, attack_intelligence, tech_export, human_capital, cybermil_people, tech_firm, surveillance_firm)
commercial <- CPI_scores %>%
  select(country, attack_commercial, tech_firm, human_capital, cyber_firm, web_alexa, ecommerce_capita, tech_export, infocomm_imp, patent_app_capita)
offense <- CPI_scores %>%
  select(country, attack_offense, tech_export, cybermil_people, military_strategy, cyber_command)
norms <- CPI_scores %>%
  select(country, laws, int_agreement, infocomm_imp, tech_firm, tech_export)

# -------------- THEME

# overview of best themes: 
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# https://www.statology.org/best-ggplot2-themes/

# how to create your own themes:
# https://rpubs.com/mclaire19/ggplot2-custom-themes

# https://ggplot2.tidyverse.org/reference/theme.html
# https://joeystanley.com/blog/custom-themes-in-ggplot2
# https://ggplot2-book.org/polishing.html
# https://bookdown.org/rdpeng/RProgDA/building-a-new-theme.html
# https://emanuelaf.github.io/own-ggplot-theme.html

crimson_theme <- function() {
  # font <- "Georgia"   #assign font family up front
  theme_minimal() %+replace%    #replace elements we want to change
  theme(
    # add border 1)
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "#A51C30", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "#A51C30", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    
    # modify text, axis and colour 4) and 5)
    # axis.text = element_text(colour = "black", face = "italic", family = "Times New Roman"),
    # axis.title = element_text(colour = "black", family = "Times New Roman"),
    axis.ticks = element_line(colour = "black"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}

# -------------- CORRELATION MATRIX
 
# GENERATE A CORRELATION MATRIX
# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# https://rpkgs.datanovia.com/ggcorrplot/
# https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/

CPI.cor <- cor(CPI_scores[, -1], method = c("pearson"))
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

# surveillance
ggcorr(surveillance[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# ggcorr(CPI[, 2:15], geom = "circle", method = c("pairwise", "pearson"), nbreaks = 5, min_size = 0, max_size = 6)
# defense
ggcorr(defense[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# control
ggcorr(control[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# intelligence
ggcorr(intelligence[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# commercial
ggcorr(commercial[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# offense
ggcorr(offense[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")
# norms
ggcorr(norms[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# problematic correlations: 
#   - patent application and ecommerce
#   - freedom of net and skilled employees

# -------------- SENSITIVITY ANALYSIS

# CORRELATION BETWEEN GDP, ITU AND CI SCORES
ggcorr(CPI_scores[, c("GDPexp_capita","itu_2018", "score_overall")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

ggcorr(CPI_scores[, c("GDPexp_capita","itu_2018", "score_overall")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# -------------- VISUALIZATION - CORR PLOTS

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://www.data-to-viz.com/caveat/spider.html
# https://edav.info/cleveland.html
# https://stackoverflow.com/questions/40542685/how-to-jitter-remove-overlap-for-geom-text-labels


# CORR PLOT "Correlation between Belfer Cyber Index and ITU Cyber Index"
ggplot(data = CPI_scores, aes(x = score_capint, y = ITU, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3,color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  # geom_point(aes(),size=3,color="#A51C30") + geom_text(aes(), hjust = -0.10, nudge_x = 0.05, check_overlap = T) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "ITU Cyber Index") 

# CORR PLOT "Correlation between Belfer Cyber Index and GDP per capita"
ggplot(data = CPI_scores, aes(x = score_capint, y = GDPexp_capita, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3, color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "GDP per capita") 

# ggplot(data = CPI_scores, aes(x = score_capabilities, y = itu_2018, label = CPI_scores$country)) +
#   geom_point(aes(colour = GDPexp_capita),size=3) + geom_text(aes(colour = GDPexp_capita), hjust = -0.10, nudge_x = 0.05, check_overlap = T)


# -------------- VISUALIZATION - LOLLIPOP SCORES

#  OVERALL CAPABILITY*INTENT SCORE
capint <- CPI_scores %>% ungroup(country) %>%
  select(country, score_capint) %>% mutate(country = fct_reorder(country, score_capint)) %>% arrange(-score_capint)
p_capint <- ggplot(data = capint, aes(x = country, y = score_capint)) +
  geom_point(size = 3, color = "#A51C30") +
  geom_segment(aes(xend = country, y = 0, yend = score_capint)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle(" ") +
  coord_flip() +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, 100, by = 10))  + 
  theme(
    # plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(colour="black", size = 12),
    axis.title.x=element_text(colour="black", size = 15, face = "bold") ,
    axis.text.y=element_text(colour="black", size = 15)
  ) 


# ggplot(CPI_scores, aes(x = score_capint, y = reorder(country, score_capint))) +
#   geom_point( size=3, color="#A51C30") +
#   crimson_theme() +
#   scale_x_continuous(limits = c(10, 100)) +
#   labs(x = "National Cyber Power Index" , y = "", title = "") + theme_minimal() +
#   theme(plot.title = element_text(size = 20, face = "bold"),
#         axis.text.x = element_text(colour="black", size = 12),
#         axis.title.x=element_text(colour="black", size = 15, face = "bold"),
#         axis.text.y=element_text(colour="black", size = 15)) 


#### ------------------------------------ BAR PLOTS CAPABILITY * INTENT  --------------------


# define a them for the plot:
crimson_theme <- function() {
  # font <- "Georgia"   #assign font family up front
  theme_minimal() + # %+replace%    #replace elements we want to change
    theme(
      #   # add border 1)
      #   panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
      #   # color background 2)
      #   panel.background = element_rect(fill = "aliceblue"),
      #   # modify grid 3)
      #   panel.grid.major.x = element_line(colour = "#A51C30", linetype = 3, size = 0.5),
      #   panel.grid.minor.x = element_blank(),
      #   panel.grid.major.y =  element_line(colour = "#A51C30", linetype = 3, size = 0.5),
      #   panel.grid.minor.y = element_blank(),
      # 
      #   # modify text, axis and colour 4) and 5)
      #   # axis.text = element_text(colour = "black", face = "italic", family = "Times New Roman"),
      #   # axis.title = element_text(colour = "black", family = "Times New Roman"),
      #   axis.ticks = element_line(colour = "black"),
      #   # legend at the bottom 6)
      #   legend.position = "bottom"
    )
}


# select and order data
data2 <- CPI_scores %>%
  select(country, capint_norms, capint_surveillance, capint_defense, capint_control, capint_intelligence, capint_commercial, capint_offense) %>%
  ungroup(country) %>%
  mutate(country=factor(country, country)) %>% # what does this do?
  mutate(average = rowSums(.[2:8])/7) # sum to compute order for fct_reorder

size_dot = 2
crimson_red = "#A51C30"
dot_color = crimson_red

average <- data2 %>% select(country, average) %>% mutate(country = fct_reorder(country, average)) %>% arrange(-average)
p_avg <- ggplot(data = average, aes(x = country, y = average)) +
  geom_point(size = size_dot, color = dot_color) +
  geom_segment(aes(xend = country, y = 0, yend = average)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("National Cyber Power Index") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title.position  = "plot") # left align title
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))


surveillance <- data2 %>% select(country, capint_surveillance) %>% mutate(country = fct_reorder(country, capint_surveillance)) %>% arrange(-capint_surveillance)
p_surveillance <- ggplot(data = surveillance, aes(x = country, y = capint_surveillance)) +
  geom_point(size = size_dot, color = "#ed1b34") +
  geom_segment(aes(xend = country, y = 0, yend = capint_surveillance)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Surveillance") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,85), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

defense <- data2 %>% select(country, capint_defense) %>% mutate(country = fct_reorder(country, capint_defense)) %>% arrange(-capint_defense)
p_defense <- ggplot(data = defense, aes(x = country, y = capint_defense)) +
  geom_point(size = size_dot, color = "#000000") +
  geom_segment(aes(xend = country, y = 0, yend = capint_defense)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Defense") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,85), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

control <- data2 %>% select(country, capint_control) %>% mutate(country = fct_reorder(country, capint_control)) %>% arrange(-capint_control)
p_control <- ggplot(data = control, aes(x = country, y = capint_control)) +
  geom_point(size = size_dot, color = "#4db848") +
  geom_segment(aes(xend = country, y = 0, yend = capint_control)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Information Control") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

intelligence <- data2 %>% select(country, capint_intelligence) %>% mutate(country = fct_reorder(country, capint_intelligence)) %>% arrange(-capint_intelligence)
p_intelligence <- ggplot(data = intelligence, aes(x = country, y = capint_intelligence)) +
  geom_point(size = size_dot, color = "#4e88c7") +
  geom_segment(aes(xend = country, y = 0, yend = capint_intelligence)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Intelligence") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

commerce <- data2 %>% select(country, capint_commercial) %>% mutate(country = fct_reorder(country, capint_commercial)) %>% arrange(-capint_commercial)
p_commerce <- ggplot(data = commerce, aes(x = country, y = capint_commercial)) +
  geom_point(size = size_dot, color = "#fcb315") +
  geom_segment(aes(xend = country, y = 0, yend = capint_commercial)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Commerce") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

offense <- data2 %>% select(country, capint_offense) %>% mutate(country = fct_reorder(country, capint_offense)) %>% arrange(-capint_offense)
p_offense <- ggplot(data = offense, aes(x = country, y = capint_offense)) +
  geom_point(size = size_dot, color = "#00aaad") +
  geom_segment(aes(xend = country, y = 0, yend = capint_offense)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Offense") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

norms <- data2 %>% select(country, capint_norms) %>% mutate(country = fct_reorder(country, capint_norms)) %>% arrange(-capint_norms)
p_norms <- ggplot(data = norms, aes(x = country, y = capint_norms)) +
  geom_point(size = size_dot, color = "#946eb7") +
  geom_segment(aes(xend = country, y = 0, yend = capint_norms)) +
  xlab("") +
  ylab("National Cyber Power Score") +
  ggtitle("Norms") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0,81), expand = c(0,0)) # +
# theme(plot.title.position  = "plot") # left align title

# Bring together on one page
library(patchwork)

(p_avg | p_surveillance | p_defense | p_control) / (p_intelligence | p_commerce | p_offense | p_norms)


# (p_surveillance | p_defense | p_control | p_intelligence ) / ( p_commerce | p_offense | p_norms | plot_spacer() )
# p_avg + p_norms + p_surveillance







