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
    axis.title = element_text(size = 15),
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
corr_ITU <- ggplot(data = CPI_scores, aes(x = score_capint, y = ITU, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3,color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  # geom_point(aes(),size=3,color="#A51C30") + geom_text(aes(), hjust = -0.10, nudge_x = 0.05, check_overlap = T) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "ITU Cyber Index") 

ggsave(corr_ITU, file = "../../findings/Corr_NCPI_ITU.pdf")


# CORR PLOT "Correlation between Belfer Cyber Index and GDP per capita"
corr_GDP <-ggplot(data = CPI_scores, aes(x = score_capint, y = GDPexp_capita, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3, color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "GDP per capita (in USD)") 

ggsave(corr_GDP, file = "../../findings/Corr_NCPI_GDP.pdf")


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
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0,103), expand = c(0,0)) +
  theme(
    # plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(colour="black", size = 12),
    axis.title.x=element_text(colour="black", size = 15) ,
    axis.text.y=element_text(colour="black", size = 15)
  ) 

ggsave(p_capint, file = "../../findings/National_Cyber_Power_Index.pdf")


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

CPI_capint <- CPI_scores %>%
  select(country, capint_norms, capint_surveillance, capint_defense, capint_control, capint_intelligence, capint_commercial, capint_offense) %>%
  ungroup(country) %>%
  mutate(country=factor(country, country)) %>% # what does this do?
  mutate(average = rowSums(.[2:8])/7) # sum to compute order for fct_reorder

size_dot = 2
crimson_red = "#A51C30"
dot_color = crimson_red

lolli <- function(df, column, title, clr){
  ggplot(data = df, aes(x = country, y = df[[column]])) +
    geom_point(size = size_dot, color = clr) +
    geom_segment(aes(xend = country, y = 0, yend = df[[column]])) +
    coord_flip() +
    crimson_theme() +
    scale_y_continuous(breaks = seq(0, 100, by = 20), limits = c(0,103), expand = c(0,0)) +
    xlab("") +
    ylab("National Cyber Power Score") +
    ggtitle(title)
}

# 
# select_reorder <- function(.data,...) {
#   .data %>%
#     select( ...) %>%
#     mutate(country = fct_reorder(.data[[country]], .data[[column]])) %>%
#     arrange (-.data[[column]])
# }

# AVERAGE
average <- CPI_capint %>% select(country, average) %>% mutate(country = fct_reorder(country, average)) %>% arrange(-average)
# 
# country = "country"
# column = "average"
# average <- data %>% select_reorder(country, column)

p_avg <- lolli(df = average, column = "average", title = "National Cyber Power Index", clr = dot_color)

# SURVEILLANCE
surveillance <- CPI_capint %>% select(country, capint_surveillance) %>% mutate(country = fct_reorder(country, capint_surveillance)) %>% arrange(-capint_surveillance)

# country = "country"
# column = "capint_surveillance"
# surveillance <- data %>% select_reorder(country, column)

p_surveillance <- lolli(df = surveillance, column = "capint_surveillance" , title = "Surveillance", clr = "#ed1b34")

# DEFENSE
defense <- CPI_capint %>% select(country, capint_defense) %>% mutate(country = fct_reorder(country, capint_defense)) %>% arrange(-capint_defense)

# country = "country"
# column = "capint_defense"
# defense <- data %>% select_reorder(country, column)

p_defense <- lolli(df = defense, column = "capint_defense" , title = "Defense", clr = "#000000")

# CONTROL
control <- CPI_capint %>% select(country, capint_control) %>% mutate(country = fct_reorder(country, capint_control)) %>% arrange(-capint_control)

# country = "country"
# column = "capint_control"
# control <- data %>% select_reorder(country, column )

p_control <- lolli(df = control, column = "capint_control" , title  = "Information Control", clr = "#4db848")

# INTELLIGENCE
intelligence <- CPI_capint %>% select(country, capint_intelligence) %>% mutate(country = fct_reorder(country, capint_intelligence)) %>% arrange(-capint_intelligence)

# country = "country"
# column = "capint_intelligence"
# intelligence <- data %>% select_reorder(country, column)
 
p_intelligence <- lolli(df = intelligence, column = "capint_intelligence" , title = "Intelligence", clr = "#4e88c7")

# COMMERCE
commerce <- CPI_capint %>% select(country, capint_commercial) %>% mutate(country = fct_reorder(country, capint_commercial)) %>% arrange(-capint_commercial)

# country = "country"
# column = "capint_commercial"
# commerce <- data %>% select_reorder(country, column)

p_commerce <- lolli(df = commerce, column  = "capint_commercial", title = "Commerce", clr = "#fcb315")

# OFFENSE
offense <- CPI_capint %>% select(country, capint_offense) %>% mutate(country = fct_reorder(country, capint_offense)) %>% arrange(-capint_offense)

# country = "country"
# column = "capint_offense"
# offense <- data %>% select_reorder(country, column )

p_offense <- lolli(df = offense, column = "capint_offense", title = "Offense", clr = "#00aaad")

# NORMS
norms <- CPI_capint %>% select(country, capint_norms) %>% mutate(country = fct_reorder(country, capint_norms)) %>% arrange(-capint_norms)

# country = "country"
# column = "capint_norms"
# norms <- data %>% select_reorder(country, column)

p_norms <- lolli(df = norms, column = "capint_norms" , title = "Norms", clr = "#946eb7")

# Bring together on one page
library(patchwork)

(p_avg | p_surveillance | p_defense | p_control) / (p_intelligence | p_commerce | p_offense | p_norms)

final <- (p_avg | p_surveillance | p_defense | p_control) / (p_intelligence | p_commerce | p_offense | p_norms)

ggsave(final, file = "../../findings/National_Cyber_Power_Index_Objectives.pdf")

# ggsave(p, file="output.png", width=10, height=10)


#### ------------------------------------ SCATTERPLOT INTENT * CAPABILITIES  --------------------
# 
# https://stackoverflow.com/questions/24190431/adding-quadrants-to-r-scatterplots-and-lines-pointing-from-plots-to-their-respe/35510338
# https://stackoverflow.com/questions/50801794/split-geom-point-into-quadrants-and-color-by-group

# https://stackoverflow.com/questions/42062547/ggplot2-mixing-of-labels-and-colours-using-scale-colour-manual
# https://michaeltoth.me/a-detailed-guide-to-ggplot-colors.html
# Once you have your color palette, you can use the scale_color_manual function to map the levels in your dataset to different colors in your generated color palette.
# Side note: Can you guess? Yes, again, there is a similar function called scale_fill_manual that we would use if we were instead graphing bars or other fillable shapes.

# https://ggplot2.tidyverse.org/reference/geom_text.html

####  OVERALL PLOT

# First, calculate the mid-point for the x and y axes:

CPI_quadrant <- CPI_scores %>%
  select(score_capabilities, score_intent, country, pop) 

x_mid <- mean(c(max(CPI_quadrant$score_capabilities, na.rm = TRUE), 
                min(CPI_quadrant$score_capabilities, na.rm = TRUE)))
y_mid <- mean(c(max(CPI_quadrant$score_intent, na.rm = TRUE), 
                min(CPI_quadrant$score_intent, na.rm = TRUE)))
# 
# x_mid <- 50
# y_mid <- 0.5

# Now, I'll use dplyr to assign each point to a quadrant with mutate, and then pipe that into ggplot2:

my_palette <- c( "#4db848", "#4e88c7", "#fcb315", "#ed1b34")
  
p_quad <- CPI_quadrant %>%
        mutate(quadrant = case_when(score_capabilities > x_mid & score_intent > y_mid   ~ "High Capability &\n High Intent",
                              score_capabilities <= x_mid & score_intent > y_mid  ~ "Low Capability &\n High Intent",
                              score_capabilities <= x_mid & score_intent <= y_mid ~ "Low Capability &\n Low Intent",
                              TRUE                                         ~ "High Capability &\n Low Intent")) %>%
        ggplot(aes(x = score_capabilities, y = score_intent, label = CPI_quadrant$country, color = quadrant)) +
            # ggplot(aes(x = score_capabilities, y = score_intent, color = quadrant, label = CPI_quadrant$country, size=pop)) +
            geom_vline(xintercept = x_mid) + # plot vertical line
            geom_hline(yintercept = y_mid) + # plot horizontal line
            geom_point() +
            crimson_theme() +
            theme_minimal() +
            theme(axis.title = element_text(size = 15)) +
            geom_point(aes(),size=3) + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05, color="black") +
            ggtitle("") +
            labs(x = "Cyber Capability Index" , y = "Cyber Intent Index") +
            theme(legend.position="bottom") + 
            scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(-0.03,1.03), expand = c(0,0)) +
            scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(-3,103), expand = c(0,0)) +
            scale_color_manual(values = my_palette, name = "Quadrant", 
                               labels = c("Higher Capability &\n Higher Intent","Higher Capability &\n Lower Intent", "Lower Capability &\n HIgher Intent", "Lower Capability &\n Lower Intent")) +
            annotate("text", x=90, y=0.95, label= "Higher Capability &\n Higher Intent", size=6, color="#4db848", fontface = 'bold') +
            annotate("text", x=10, y=0.95, label= "Lower Capability &\n Higher Intent", size=6, color="#fcb315", fontface = 'bold') +
            annotate("text", x=90, y=0.05, label= "Higher Capability &\n Lower Intent", size=6, color="#4e88c7", fontface = 'bold') +
            annotate("text", x=10, y=0.05, label= "Lower Capability &\n Lower Intent", size=6, color="#ed1b34", fontface = 'bold')  +
            guides(color = FALSE) 

ggsave(p_quad, file = "../../findings/Quad_Cap_Intent.pdf")


#### FUNCTION

CPI_quadrant <- CPI_scores %>%
  select(country, pop, score_capabilities, score_intent, 
         score_surveillance, score_defense, score_control, score_intelligence, score_commercial, score_offense, score_norms,
         intent_surveillance, intent_defense, intent_control, intent_intelligence, intent_commercial, intent_offense, intent_norms)

# %>%
#   mutate(quadrant = case_when(score_capabilities > x_mid & score_intent > y_mid   ~ "High Capability &\n High Intent",
#                               score_capabilities <= x_mid & score_intent > y_mid  ~ "Low Capability &\n High Intent",
#                               score_capabilities <= x_mid & score_intent <= y_mid ~ "Low Capability &\n Low Intent",
#                               TRUE                                         ~ "High Capability &\n Low Intent")) 

x_mid <- mean(c(max(CPI_quadrant$score_capabilities, na.rm = TRUE), 
                min(CPI_quadrant$score_capabilities, na.rm = TRUE)))
y_mid <- mean(c(max(CPI_quadrant$score_intent, na.rm = TRUE), 
                min(CPI_quadrant$score_intent, na.rm = TRUE)))

my_palette <- c( "#4db848", "#4e88c7", "#fcb315", "#ed1b34")


intent_times_capab <-   function (.data, ...) {
  .data  %>%
  mutate(quadrant = case_when(.data[[capabilities]] > x_mid & .data[[intent]] > y_mid   ~ "High Capability &\n High Intent",
                              .data[[capabilities]] <= x_mid & .data[[intent]] > y_mid  ~ "Low Capability &\n High Intent",
                              .data[[capabilities]] <= x_mid & .data[[intent]] <= y_mid ~ "Low Capability &\n Low Intent",
                                TRUE                                         ~ "High Capability &\n Low Intent")) %>%  
  ggplot(aes(x = .data[[capabilities]], y =  .data[[intent]], label = .data[[label]] , color = quadrant)) +
    geom_vline(xintercept = x_mid) + # plot vertical line
    geom_hline(yintercept = y_mid) + # plot horizontal line
    geom_point() +
    crimson_theme() +
    theme_minimal() +
    theme(axis.title = element_text(size = 15)) +
    geom_point(aes(),size=3) + 
    geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05, color="black") +
    ggtitle("") +
    labs(x = labs_x , y = labs_y) +
    theme(legend.position="bottom") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.10), limits = c(-0.03,1.03), expand = c(0,0)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(-3,103), expand = c(0,0)) +
    scale_color_manual(values = my_palette, name = "Quadrant",
                     labels = c("Higher Capability &\n Higher Intent","Higher Capability &\n Lower Intent", "Lower Capability &\n Higher Intent", "Lower Capability &\n Lower Intent")) +
    annotate("text", x=90, y=0.95, label= "Higher Capability &\n Higher Intent", size=6, color="#4db848", fontface = 'bold') +
    annotate("text", x=10, y=0.95, label= "Lower Capability &\n Higher Intent", size=6, color="#fcb315", fontface = 'bold') +
    annotate("text", x=90, y=0.05, label= "Higher Capability &\n Lower Intent", size=6, color="#4e88c7", fontface = 'bold') +
    annotate("text", x=10, y=0.05, label= "Lower Capability &\n Lower Intent", size=6, color="#ed1b34", fontface = 'bold')  +
    guides(color = FALSE)
  }

#### OBJECTIVES

#### SURVEILLANCE
capabilities = "score_surveillance"
intent = "intent_surveillance"
labs_x = "Surveillance Capability Score"
labs_y =  "Surveillance Intent Score"
label = "country"
quadrant = "quandrant"

p_quad_surveillance <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_surveillance, file = "../../findings/Quad_Cap_Intent_surveillance.pdf", width = 17, height = 12, device='pdf')

# ggsave(p_quad_surveillance, file = "../../findings/Quad_Cap_Intent_surveillance.png", width = 17, height = 12, dpi = 150, units = "cm", device='png')



#### DEFENSE
capabilities = "score_defense"
intent = "intent_defense"
labs_x = "Defense Capability Score"
labs_y =  "Defense Intent Score"
label = "country"
quadrant = "quandrant"

p_quad_defense <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_defense, file = "../../findings/Quad_Cap_Intent_defense.pdf", width = 17, height = 12)


#### INFORMATION CONTROL
capabilities = "score_control"
intent = "intent_control"
labs_x = "Info Control Capability Score"
labs_y =  "Info Control Intent Score"
label = "country"
quadrant = "quandrant"

my_palette <- c("#4db848", "#fcb315", "#ed1b34")

p_quad_control <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_control, file = "../../findings/Quad_Cap_Intent_control.pdf")


#### INTELLIGENCE
capabilities = "score_intelligence"
intent = "intent_intelligence"
labs_x = "Intelligence Capability Score"
labs_y =  "Intelligence Intent Score"
label = "country"
quadrant = "quandrant"

my_palette <- c( "#4db848", "#4e88c7", "#fcb315", "#ed1b34")

p_quad_intel <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)


ggsave(p_quad_intel, file = "../../findings/Quad_Cap_Intent_intel.pdf")


#### COMMERCE
capabilities = "score_commercial"
intent = "intent_commercial"
labs_x = "Commerce Capability Score"
labs_y =  "Commerce Intent Score"
label = "country"
quadrant = "quandrant"

my_palette <- c( "#4e88c7", "#fcb315", "#ed1b34")

p_quad_commerce <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_commerce, file = "../../findings/Quad_Cap_Intent_commerce.pdf")


#### OFFENSE
capabilities = "score_offense"
intent = "intent_offense"
labs_x = "Offense Capability Score"
labs_y =  "Offense Intent Score"
label = "country"
quadrant = "quandrant"

my_palette <- c( "#4db848", "#4e88c7", "#fcb315", "#ed1b34")

p_quad_offense <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_offense, file = "../../findings/Quad_Cap_Intent_offense.pdf")


#### NORMS
capabilities = "score_norms"
intent = "intent_norms"
labs_x = "Norms Capability Score"
labs_y =  "Norms Intent Score"
label = "country"
quadrant = "quandrant"

my_palette <- c("#4db848", "#fcb315", "#ed1b34")

p_quad_norms <- CPI_quadrant %>%  intent_times_capab(capabilities, intent, labs_x, labs_y, label, quadrant)

ggsave(p_quad_norms, file = "../../findings/Quad_Cap_Intent_norms.pdf")

# 
# # Bring together on one page
# library(patchwork)
# 
# (p_quad | p_quad_surveillance | p_quad_defense | p_quad_control)
# 
# quad <-  (p_quad | p_quad_surveillance | p_quad_defense | p_quad_control) / (p_quad_intel | p_quad_commerce | p_quad_offense | p_quad_norms)


#### ------------------------------------ FUNCTION FOR BARPLOTS INTENT  --------------------

# https://r4ds.had.co.nz/functions.html
# https://swcarpentry.github.io/r-novice-inflammation/02-func-R/
#   https://www.statmethods.net/management/userfunctions.html
# http://adv-r.had.co.nz/Functions.html
#   https://www.tutorialspoint.com/r/r_functions.htm

# https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function










