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
  # mutate(score_capabilities = mean(c(score_surveillance, score_defense, score_manipulation, score_defense,
  #                                 score_commercial, score_disruption, score_cybernorms), na.rm = TRUE ))

cor(CPI_scores$broadband_speed, CPI_scores$internet_use, use = "complete.obs")

# UPDATE

surveillance <- CPI_scores %>%
  select(country,laws, attack_surveillance, freedom_net, surveillance_firm, socials_use, internet_use)
defense <- CPI_scores %>%
  select(country, laws, shodan,  human_capital, cyber_firm, computer_infection, mobile_infection, internet_use, broadband_speed, mobile_speed, infocomm_imp, CERTS)
manipulation <- CPI_scores %>%
  select(country, attack_manipulation, internet_use, socials_use, news_alexa, web_alexa, removal_google)
intelligence <- CPI_scores %>%
  select(country, attack_intelligence, tech_export, human_capital, cybermil_people, tech_firm, surveillance_firm)
commercial <- CPI_scores %>%
  select(country, attack_commercial, tech_firm, human_capital, cyber_firm, web_alexa, ecommerce_capita, tech_export, infocomm_imp, patent_app_capita)
disruption <- CPI_scores %>%
  select(country, attack_disruption, tech_export, cybermil_people, military_strategy, cyber_command)
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

# manipulation
ggcorr(manipulation[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# intelligence
ggcorr(intelligence[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# commercial
ggcorr(commercial[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# disruption
ggcorr(disruption[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# cybernorms
ggcorr(cybernorms[, -1], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# problematic correlations: 
#   - patent application and ecommerce
#   - freedom of net and skilled employees

# -------------- SENSITIVITY ANALYSIS

# CORRELATION BETWEEN GDP, ITU AND CI SCORES
ggcorr(CPI_scores[, c("GDPexp_capita","itu_2018", "score_overall")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

ggcorr(CPI_scores[, c("GDPexp_capita","itu_2018", "score_overall")], method = c("pairwise", "pearson"), label=T, hjust = 0.75, size = 3, color = "grey50")

# -------------- VISUALIZATION - PLOTS

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://www.data-to-viz.com/caveat/spider.html
# https://edav.info/cleveland.html
# https://stackoverflow.com/questions/40542685/how-to-jitter-remove-overlap-for-geom-text-labels


# CORR PLOT "Correlation between Belfer Cyber Index and ITU Cyber Index"
ggplot(data = CPI_scores, aes(x = score_capabilities, y = ITU, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3,color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  # geom_point(aes(),size=3,color="#A51C30") + geom_text(aes(), hjust = -0.10, nudge_x = 0.05, check_overlap = T) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "ITU Cyber Index") 

# CORR PLOT "Correlation between Belfer Cyber Index and GDP per capita"


ggplot(data = CPI_scores, aes(x = score_capabilities, y = GDPexp_capita, label = CPI_scores$country)) +
  crimson_theme() +
  geom_point(aes(),size=3, color="#A51C30") + geom_text_repel(aes(), hjust = -0.10, nudge_x = 0.05) +
  ggtitle("") +
  labs(x = "National Cyber Power Index (NCPI)" , y = "GDP per capita") 

# ggplot(data = CPI_scores, aes(x = score_capabilities, y = itu_2018, label = CPI_scores$country)) +
#   geom_point(aes(colour = GDPexp_capita),size=3) + geom_text(aes(colour = GDPexp_capita), hjust = -0.10, nudge_x = 0.05, check_overlap = T)


#  OVERALL CAPABILITY SCORE
ggplot(CPI_scores, aes(x = score_capabilities, y = reorder(country, score_capabilities))) +
  geom_point( size=3, color="#A51C30") +
  crimson_theme() +
  scale_x_continuous(limits = c(10, 100)) +
  labs(x = "Capabilities Scores" , y = "", title = "") + theme_minimal() +
theme(plot.title = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(colour="black", size = 12),
      axis.title.x=element_text(colour="black", size = 15, face = "bold"),
      axis.text.y=element_text(colour="black", size = 15)) 

#  OVERALL CAPABILITY*INTENT SCORE
ggplot(CPI_scores, aes(x = score_capint, y = reorder(country, score_capint))) +
  geom_point( size=3, color="#A51C30") +
  crimson_theme() +
  scale_x_continuous(limits = c(10, 100)) +
  labs(x = "National Cyber Power Index" , y = "", title = "") + theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.x=element_text(colour="black", size = 15, face = "bold"),
        axis.text.y=element_text(colour="black", size = 15)) 


# check lollipop graph
CPI_scores %>%
  arrange(score_capabilities) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(rowname=factor(country, levels=country)) %>%   # This trick update the factor levels
  ggplot( aes(x=country, y=score_capabilities)) +
  geom_segment(aes(x=country ,xend=country, y=0, yend=score_capabilities), color="grey") +
  geom_point( size=4, color="blue") +
  coord_flip() + theme_minimal()


# -------------- VISUALIZATION - RADARCHART
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/stars.html
# https://www.data-to-viz.com/caveat/spider.html

# USA 
USA <- CPI_scores %>%
  ungroup(country) %>%
  filter(country== "United States") %>%
  select(score_surveillance, score_defense, score_manipulation, score_intelligence, score_commercial, score_disruption, score_cybernorms)

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# data1 <- rbind(rep(120,10) , rep(0,10) , data1)
USA <- rbind(rep(108,6) , rep(36,6) , USA)

# radarchart(USA)

# Custom the radarChart !
par(mar=c(0,0,0,0))
radarchart(USA, axistype=1, title = "USA",
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(30,120,20), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

# Commercial = Commercial Gain of Enhancing Domestic Industry Growth
# Defense = Strengthening and Enhancing National Cyber Defenses
# Intelligence = Foreign Intelligence Collection for National Security
# Information Control = Controlling and Manipulating the Information Environment
# Norms = Defining International Cyber Norms and Standards
# Offense = Destroying or Disabling Adversary Infrastructure
# Surveillance = Surveilling and Monitoring Domestic Groups

# 15 PLOTS 
all <- CPI_scores %>%
  ungroup(country) %>%
  select(country, Surveillance = score_surveillance, Defense = score_defense, `Info Control` = score_manipulation, 
         Intelligence = score_intelligence, Commercial = score_commercial, Offense = score_disruption, Norms = score_cybernorms) %>%
  filter(country == "Australia" | country == "Brazil" | country == "Canada" | country == "China" | country == "Egypt" | country == "Estonia" |
           country == "France" | country == "Germany" | country == "India" | country == "Israel" | country == "Italy" | country == "Japan" | 
           country == "Lithuania" | country == "Malaysia" | country == "Netherlands" ) %>%
  select (-country)

all <- rbind(rep(100,10) , rep(30,10) , all)

# Prepare color

# colors_border=colormap(colormap=colormaps$viridis, nshades=15, alpha=1)
# colors_in=colormap(colormap=colormaps$viridis, nshades=15, alpha=0.3)

# Prepare title
mytitle <- c("Australia",  "Brazil",  "Canada", "China", "Egypt", "Estonia", "France","Germany", "India", 
             "Israel", "Italy", "Japan","Lithuania", "Malaysia" , "Netherlands"  )

# Split the screen in 6 parts
par(mar=rep(0.8,4))
par(mfrow=c(5,3))

# Loop for each plot
for(i in 1:15){
  
  # Custom the radarChart !
  radarchart( all[c(1,2,i+2),], axistype=1, 
              
              #custom polygon
              pcol='#A51C30', pfcol=rgb(0.94, 0.97, 1 , 0.8) , plwd=4, plty=1 ,
              # pcol=rgb(0.165,0.28,0.48, 1), pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4, plty=1 ,
              # pcol=colors_border[i] , pfcol=colors_in[i] , plwd=4, plty=1 ,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(30,100,10), cglwd=0.8,
              #custom labels
              vlcex=1.0,
              #title
              title=mytitle[i]
  )
}


# 15 PLOTS 
all <- CPI_scores %>%
  ungroup(country) %>%
  select(country, Surveillance = score_surveillance, Defense = score_defense, `Info Control` = score_manipulation, 
         Intelligence = score_intelligence, Commercial = score_commercial, Offense = score_disruption, Norms = score_cybernorms) %>%
    filter(country == "Russia" | country == "Saudi Arabia" | country == "Singapore" | country == "South Korea" | country == "Spain" | country == "Sweden" |
           country == "Switzerland" | country == "Turkey" | country == "Ukraine" | country == "United Kingdom"
         | country == "United States" | country == "Vietnam") %>%
  select (-country)

all <- rbind(rep(100,10) , rep(30,10) , all)

# Prepare color
# colors_border=colormap(colormap=colormaps$viridis, nshades=15, alpha=1)
# colors_in=colormap(colormap=colormaps$viridis, nshades=15, alpha=0.3)

# Prepare title
mytitle <- c("Russia",  "Saudi Arabia",  "Singapore", "South Korea", "Spain", "Sweden", 
             "Switzerland","Turkey", "Ukraine", "United Kingdom", "United States", "Vietnam")

# Split the screen in 6 parts
par(mar=rep(0.8,4))
par(mfrow=c(5,3))


# Loop for each plot
for(i in 1:12){
  
  # Custom the radarChart !
  radarchart( all[c(1,2,i+2),], axistype=1, 
              
              #custom polygon
              pcol='#A51C30', pfcol=rgb(0.94, 0.97, 1 , 0.8) , plwd=4, plty=1 ,
              # pcol=colors_border[i] , pfcol=colors_in[i] , plwd=4, plty=1 , 
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(30,100,10), cglwd=0.8,
              #custom labels
              vlcex=1.0,
              #title
              title=mytitle[i]
  )
}


