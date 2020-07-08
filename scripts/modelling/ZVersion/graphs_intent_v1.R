# This script computes graphs for the itnent scoring

# TO DO
# ADD OVERALL INTENT SCORE


# can we combine indicators that do not correlate?
#   https://www.researchgate.net/post/How_to_check_statistically_whether_two_or_more_variables_can_be_combined_into_one_derived_variable

rm(list=ls())

# -------------- SETUP

library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
# library(viridisLite) # this package might cause problesm
library(viridis)

# -------------- DATA IMPORT

CPI_scores <- readRDS("../../data/data_for_modelling/CPI_scores.rds") 
# group_by(country) %>%
# mutate(score_capabilities = mean(c(score_surveillance, score_defense, score_control, score_defense,
#                                 score_commercial, score_offense, score_norms), na.rm = TRUE ))

# -------------- VISUALIZATION - CIRCULAR BARPLOT INTENT 
# https://www.data-to-viz.com/graph/circularbarplot.html
# https://www.r-graph-gallery.com/295-basic-circular-barplot.html
# https://www.d3-graph-gallery.com/circular_barplot

# select and order data
tmp <- CPI_scores %>%
  select(country, intent_surveillance) %>%
  # filter(!is.na(intent_surveillance)) %>%
  arrange(desc(intent_surveillance)) %>%
  ungroup(country) %>%
  mutate(country=factor(country, country)) # what does this do?

# Set a number of 'empty bar'
empty_bar=10

# Add lines to the initial tmpset
to_add = matrix(NA, empty_bar, ncol(tmp))
# to_add = matrix(NA, empty_bar, ncol(tmp_or))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

# Get the name and the y position of each label
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$country <- gsub("United States", "US", label_tmp$country)
label_tmp$country <- paste(label_tmp$country, " (", label_tmp$intent_surveillance,")", sep="")

# Make the plot
ggplot(tmp, aes(x=as.factor(id), y=intent_surveillance)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  # geom_bar(stat="identity", fill=alpha("#69b3a2", 0.8)) +
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(-300,500) +   # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_tmp, aes(x=id, y=intent_surveillance+200, label=country ), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE ) +
  geom_text( aes(x=24, y= -300, label="Who's surveillance intent is highest?"), color="black", inherit.aes = FALSE)


# -------------- VISUALIZATION - STACKED CIRCULAR BARPLOT INTENT 

# https://tidyr.tidyverse.org/reference/pivot_longer.html
# https://forcats.tidyverse.org/reference/fct_reorder.html # reorder factors
# https://ggplot2.tidyverse.org/reference/scale_manual.html # These functions allow you to specify your own set of mappings from levels in the data to aesthetic values.

CPI_scores <- readRDS("../../data/data_for_modelling/CPI_scores.rds") 

# select and order data
tmp <- CPI_scores %>%
  select(country, intent_surveillance, intent_defense, intent_control, intent_intelligence, intent_commercial, intent_offense, intent_norms) %>%
  
  # filter(!is.na(intent_surveillance)) %>%
  # arrange(desc(intent_surveillance)) %>%
  ungroup(country) %>%
  mutate(country=factor(country, country)) %>% # what does this do?
  mutate(sum_tot = rowSums(.[2:8])) # sum to compute order for fct_reorder

tmp$country <- fct_reorder(tmp$country, -tmp$sum_tot) # reorder the factor levels by tot
tmp <- tmp %>% select(-sum_tot) # remove not useful total

# pivot data
tmp <- tmp %>% 
  pivot_longer(-country, names_to = "observation", values_to = "value",   values_drop_na = FALSE) %>%
  mutate(value = (value/7)*100) # divide by 7, so that sum = average

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(tmp$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(tmp$country)*nObsType, ncol(tmp)) )
colnames(to_add) <- colnames(tmp)
to_add$country <- rep(levels(tmp$country), each=empty_bar*nObsType )
tmp <- rbind(tmp, to_add)
tmp <- tmp %>% arrange(country, observation)
tmp$id <- rep( seq(1, nrow(tmp)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- tmp %>% 
  group_by(id, country) %>%
  summarise(tot = round(sum(value), 1)) # put summarise instead of summarize

number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

label_data$country <- gsub("United States", "US", label_data$country)
label_data$country <- gsub("United Kingdom", "UK", label_data$country)

label_data$country <- paste(label_data$country, " (", label_data$tot,")", sep="")

# prepare a data frame for base lines
base_data <- tmp %>% 
  group_by(country) %>% 
  summarise(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# my_palette <- c("#ec8f9c", "#93a1ad", "#cbdb2a", "#95b5df", "#ffde2d", "#77ced9", "#bb89ca")
my_palette <- c("#ed1b34", "#000000", "#4db848", "#4e88c7", "#fcb315", "#00aaad", "#946eb7")

# https://www.seas.harvard.edu/office-communications/brand-style-guide/color-palette
# grey, black, green, blue, warm yellow, turquoise, purple

# Make the plot
p <- ggplot(tmp) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.8) +
  scale_fill_manual(values = my_palette, name = "Objectives", labels = c("Surveillance","Defense", "Control", "Intelligence", "Commerce", "Offense", "Norms")) +
  # scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(tmp$id),6), y = c(0, 20,40, 60, 80, 100), label = c("0", "20", "40", "60", "80", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1)+
  # ylim(-50, max(label_data$tot, na.rm=T)) + # chnage value to change size of circle
  ylim(-80, max(110, na.rm=T)) + # chnage value to change size of circle
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +

  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+3, label=country), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE, hjust=label_data$hjust) +
  # geom_text(data=label_data, aes(x=id, y=tot+10, label=country, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE ) 
# geom_text(data=base_data, aes(x = title, y = -18, label=country), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

# Save at png
ggsave(p, file="output.png", width=10, height=10)


#### ------------------------------------ BAR PLOTS --------------------

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
  select(country, intent_norms, intent_surveillance, intent_defense, intent_control, intent_intelligence, intent_commercial, intent_offense) %>%
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
  ylab("Intent Score") +
  ggtitle("Intent Index") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
  # theme(plot.title.position  = "plot") # left align title
  # theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

surveillance <- data2 %>% select(country, intent_surveillance) %>% mutate(country = fct_reorder(country, intent_surveillance)) %>% arrange(-intent_surveillance)
p_surveillance <- ggplot(data = surveillance, aes(x = country, y = intent_surveillance)) +
  geom_point(size = size_dot, color = "#ed1b34") +
  geom_segment(aes(xend = country, y = 0, yend = intent_surveillance)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Surveillance") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

defense <- data2 %>% select(country, intent_defense) %>% mutate(country = fct_reorder(country, intent_defense)) %>% arrange(-intent_defense)
p_defense <- ggplot(data = defense, aes(x = country, y = intent_defense)) +
  geom_point(size = size_dot, color = "#000000") +
  geom_segment(aes(xend = country, y = 0, yend = intent_defense)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Defense") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

control <- data2 %>% select(country, intent_control) %>% mutate(country = fct_reorder(country, intent_control)) %>% arrange(-intent_control)
p_control <- ggplot(data = control, aes(x = country, y = intent_control)) +
  geom_point(size = size_dot, color = "#4db848") +
  geom_segment(aes(xend = country, y = 0, yend = intent_control)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Information Control") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

intelligence <- data2 %>% select(country, intent_intelligence) %>% mutate(country = fct_reorder(country, intent_intelligence)) %>% arrange(-intent_intelligence)
p_intelligence <- ggplot(data = intelligence, aes(x = country, y = intent_intelligence)) +
  geom_point(size = size_dot, color = "#4e88c7") +
  geom_segment(aes(xend = country, y = 0, yend = intent_intelligence)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Intelligence") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

commerce <- data2 %>% select(country, intent_commercial) %>% mutate(country = fct_reorder(country, intent_commercial)) %>% arrange(-intent_commercial)
p_commerce <- ggplot(data = commerce, aes(x = country, y = intent_commercial)) +
  geom_point(size = size_dot, color = "#fcb315") +
  geom_segment(aes(xend = country, y = 0, yend = intent_commercial)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Commerce") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

offense <- data2 %>% select(country, intent_offense) %>% mutate(country = fct_reorder(country, intent_offense)) %>% arrange(-intent_offense)
p_offense <- ggplot(data = offense, aes(x = country, y = intent_offense)) +
  geom_point(size = size_dot, color = "#00aaad") +
  geom_segment(aes(xend = country, y = 0, yend = intent_offense)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Offense") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title  = element_text(hjust = -0.45, vjust=2.12))

norms <- data2 %>% select(country, intent_norms) %>% mutate(country = fct_reorder(country, intent_norms)) %>% arrange(-intent_norms)
p_norms <- ggplot(data = norms, aes(x = country, y = intent_norms)) +
  geom_point(size = size_dot, color = "#946eb7") +
  geom_segment(aes(xend = country, y = 0, yend = intent_norms)) +
  xlab("") +
  ylab("Intent Score") +
  ggtitle("Norms") +
  coord_flip() +
  crimson_theme() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20), limits = c(0,1.05), expand = c(0,0)) # +
# theme(plot.title.position  = "plot") # left align title
  
# Bring together on one page
library(patchwork)

(p_avg | p_surveillance | p_defense | p_control) / (p_intelligence | p_commerce | p_offense | p_norms)
# p_avg + p_norms + p_surveillance


  
  