
# Aim of this script: check if the data and data processing from the PENN tables are correct.

# Variables of interest in the PENN table:
    # year,
    # country,
    # GDP_expentiture = rgdpe, 
    # GDP_output = rgdpo,
    # pop

library(rio)
library(tidyverse)

penn_raw <- rio::import("../../../Data/Original Data/PENNWorldTable/pwt91new.xlsx") # our raw data



# questions:
# - difference btw pwt91new.xlsx and pwt91.xlsx ? -> consider deleting pwt91.xlsx to avoid confusion.
# - exact source (URL) of the data? 


# a function to compare if two values are equal:
compare_two_values <- function(valueA, valueB){
  if ( valueA != valueB ) {
    print("!!! ISSUE HERE !!!")
  } else {print("OK")}
}


# --- TEST 1 : similarity btw new online PENN data and our raw data (pwt91new.xlsx) ---

# new data for the check downloaded from there: https://www.rug.nl/ggdc/docs/pwt91.xlsx 
# version 9.1 downloaded 26 August 2019
# (follow DOI: https://doi.org/10.15141/S50T0R)
penn_new_raw_data <- rio::import(
  file = "../../../Data/Original Data/PENNWorldTable/pwt91_datacheck.xlsx",
  which = "Data" # select the sheet where the data is
)

# OK: same size
compare_two_values(dim(penn_raw)[1], dim(penn_new_raw_data)[1])
compare_two_values(dim(penn_raw)[2], dim(penn_new_raw_data)[2])

# Ok: same total pop
compare_two_values(penn_raw$pop %>% sum(na.rm = TRUE), 
                   penn_new_raw_data$pop %>% sum(na.rm = TRUE))

# Ok: same total rgdpe
compare_two_values(penn_raw$rgdpe %>% sum(na.rm = TRUE), 
                   penn_new_raw_data$rgdpe %>% sum(na.rm = TRUE))

# Ok: same total rgdpo
compare_two_values(penn_raw$rgdpo %>% sum(na.rm = TRUE), 
                   penn_new_raw_data$rgdpo %>% sum(na.rm = TRUE))

# Ok: same total number of missing values
compare_two_values(sum(is.na(penn_raw)), 
                   sum(is.na(penn_new_raw_data)))




# --- TEST 2 : comparison btw our raw data (pwt91new.xlsx) and the processed data (PENN_tidy.rds)

# the tidying was done with:
# /Users/dla/Documents/Data Science and Viz/Legitimacy_and_ViolentExtremism/LegitimacyTerrorism/Scripts/Data Processing/Functions to Tidy Data/tidy_PENN.R
PENN_tidy <- rio::import("../../../Data/Processed Data/PENN_tidy.rds") # our processed data

# the only thing that were done is selecting the variables of interest and filter for year >= 1970

penn_new_raw_data_1970 = penn_new_raw_data %>% filter(year >= 1970)

# OK: same size
compare_two_values(dim(PENN_tidy)[1], dim(penn_new_raw_data_1970)[1]) # same number of rows

# Ok: same total pop
compare_two_values(PENN_tidy$pop %>% sum(na.rm = TRUE), 
                   penn_new_raw_data_1970$pop %>% sum(na.rm = TRUE))

# Ok: same total rgdpe
compare_two_values(PENN_tidy$GDP_expentiture %>% sum(na.rm = TRUE), 
                   penn_new_raw_data_1970$rgdpe %>% sum(na.rm = TRUE))

# Ok: same total rgdpo
compare_two_values(PENN_tidy$GDP_output %>% sum(na.rm = TRUE), 
                   penn_new_raw_data_1970$rgdpo %>% sum(na.rm = TRUE))

# Ok: same total number of missing values
compare_two_values(sum(is.na(PENN_tidy)), 
                   sum(is.na(penn_new_raw_data_1970 %>% select(country, year, pop, rgdpe, rgdpo))))


# If there were a lot of missing years, the filtering by year could be an issue:
# OK, no missing years
sum(is.na(penn_raw$year)) 
table(penn_raw$year, useNA = "ifany") # a beautiful 182 country per year




# --- TEST 3 : comparison btw our processed data (PENN_tidy.rds) and the data used for modelling that is
#               merged with the GTD and other datasets (GTD_polity_PENN_PRIO_WGI_2000.rds)

final_dataset <- rio::import("../../../Data/Data for Modelling/GTD_polity_PENN_PRIO_WGI_2000.rds") # our merged dataset, used for the modelling
final_PENN <- final_dataset %>% select(
  year,
  consolidated_country,
  GDP_expentiture,
  GDP_output,
  pop
) # only the PENN variables

# filtering the penn_new_raw_data to see if we get the same data as the final one:
final_countries <- final_PENN$consolidated_country %>% unique()
final_years <- final_PENN$year %>% unique()
new_modelling_PENN_data <- penn_new_raw_data %>%
  select(year, country, rgdpe, rgdpo, pop) %>%
  filter(year %in% final_years,
         country %in% final_countries)

# The 2 datasets are of different size. Theoretically this should only be the case because 
# of the countries in GTD that are not in the PENN table (e.g. Afghanistan)

# thus the sum of pop and rgdpe/o should be the same:
# ISSUE: not same total pop
compare_two_values(final_PENN$pop %>% sum(na.rm = TRUE), 
                   new_modelling_PENN_data$pop %>% sum(na.rm = TRUE))
# ISSUE: not same total rgdpe
compare_two_values(final_PENN$GDP_expentiture %>% sum(na.rm = TRUE), 
                   new_modelling_PENN_data$rgdpe %>% sum(na.rm = TRUE))
# ISSUE: not same total rgdpo
compare_two_values(final_PENN$GDP_output %>% sum(na.rm = TRUE), 
                   new_modelling_PENN_data$rgdpo %>% sum(na.rm = TRUE))

# cause: final_PENN %>% filter(is.na(pop)) %>% dim() # ==> 414 + 2322 = 2736, != 2862 ...
# WHY?

# let's try to drop the NA from the final_PENN that we used:
final_PENN_noNA <- final_PENN %>% drop_na()
# issue: this one is larger than new_modelling_PENN_data... why?

# In the final data filtered to keep only the countries with PENN data, we still have more countries:
(final_PENN_noNA$consolidated_country %>% unique() %>% length()) > 
  (new_modelling_PENN_data$country %>% unique() %>% length())

# let's see what are these countries in excess:
countries_in_PENN <- new_modelling_PENN_data$country %>% unique()
final_PENN_strange_countries <- final_PENN_noNA %>% 
  filter(!(consolidated_country %in% countries_in_PENN))

# those countries are: Bolivia, Bosnia-Herzegovina, Congo DRC, Moldova, Tanzania, Venezuela, Vietnam.
# they were renamed with the clean_countries() function. 
# those 7 countries (*18years) should be the countries that are missing in the final count:
414 + 2322 + (7*18) == 2862
# ==> OK!




# --- TEST 4 : careful proofreading of the code, focusing on PENN.

# Looks ok to me.


# --- TEST 5 : comparison of direction of effect when taking only the PENN and GTD

# select only n_events and PENN data from final dataset:
final_PENN_GTD <- final_dataset %>% select(
  year,
  consolidated_country,
  n_events,
  GDP_expentiture,
  GDP_output,
  pop
  ) %>% # only the PENN variables
  mutate(
    numeric_country = as.numeric(consolidated_country)
  )
  
# correlation matrix:
cor(x = select(final_PENN_GTD, -consolidated_country), use = "complete.obs")


# simple linear regressions:
my_formula = 'n_events ~ pop' # (pop) effect is positive
my_formula = 'n_events ~ GDP_expentiture' # effect is positive
my_formula = 'n_events ~ GDP_output' # effect is positive
my_formula = 'n_events ~ year + pop' # effect is positive
my_formula = 'n_events ~ year + GDP_expentiture' # effect is positive
my_formula = 'n_events ~ year + GDP_output' # effect is positive
my_formula = 'n_events ~ year + log(pop)' # effect is positive
my_formula = 'n_events ~ year + log(GDP_expentiture)' # effect is positive
my_formula = 'n_events ~ year + log(GDP_output)' # effect is positive

lrm <- lm(my_formula, data = final_PENN_GTD)
summary(lrm)


# Using the pglm panel analysis and a negative binomial:
library(pglm) # model panel data
my_pglm <- pglm(n_events ~ log(GDP_expentiture) + log(pop),
               final_PENN_GTD,
               family = negbin, model = "within", print.level = 3, method = "nr",
               index = c('consolidated_country', 'year'),
               start = NULL, R = 20 )
summary(my_pglm)
# GDP Expenditure effect is positive,
# meaning that country with larger GDP exp. get more terrorism events.
# and Popularion effect is negative,
# meaning that countries with larger pop get less terrorism events.
# THIS CONTRADICT SOME PREVIOUS FINDINGS...

# same with a lag:
my_pglm <- pglm(n_events ~ lag(log(GDP_expentiture)) + lag(log(pop)),
                final_PENN_GTD,
                family = negbin, model = "within", print.level = 3, method = "nr",
                index = c('consolidated_country', 'year'),
                start = NULL, R = 20 )
summary(my_pglm) # ==> SAME RESULTS

# same without log (CANNOT DO AS NOT CONVERGING)
# my_pglm <- pglm(n_events ~ GDP_expentiture + pop,
#                 final_PENN_GTD,
#                 family = negbin, model = "within", print.level = 3, method = "nr",
#                 index = c('consolidated_country', 'year'),
#                 start = NULL, R = 20 )
# summary(my_pglm)

library(MASS) # to estimate negative binomial (as not included in "basic glm)
my_formula = 'n_events ~ log(pop)' # effect is positive (without log, no convergence)
my_formula = 'n_events ~ GDP_expentiture' # effect is positive (without log, no convergence)
my_formula = 'n_events ~ GDP_output' # effect is positive (without log, no convergence)
my_formula = 'n_events ~ year + log(pop)' # effect is positive
my_formula = 'n_events ~ year + log(GDP_expentiture)' # effect is positive
my_formula = 'n_events ~ year + log(GDP_output)' # effect is positive
summary(m1 <- glm.nb(my_formula, data = final_PENN_GTD))
# ALL EFFECTS ARE POSITIVE...


# Why is the population effect negative in the pglm panel analysis negative binomial then?

# Would the effect reverse if we remove countries with few n_events?
# (does not make sense to do that like this, just a test)
countries_with_few_events <- final_PENN_GTD %>% 
  group_by(consolidated_country) %>% 
  summarise(n_events_tot = sum(n_events)) %>%
  filter(n_events_tot < 18) %$%
  consolidated_country %>%
  unique()
# I then remove this 66 countries and re-try the panel analysis:

my_pglm <- pglm(n_events ~ lag(log(GDP_expentiture)) + lag(log(pop)),
                filter(final_PENN_GTD, !(consolidated_country %in% countries_with_few_events)),
                family = negbin, model = "within", print.level = 3, method = "nr",
                index = c('consolidated_country', 'year'),
                start = NULL, R = 20 )
summary(my_pglm) # ==> SAME DIRECTION OF THE EFFECTS.
  
  


cor(final_dataset$n_events, final_dataset$effectiveness, use = "complete.obs")
cor(final_dataset$n_events, final_dataset$accountability, use = "complete.obs")


my_pglm <- pglm(n_events ~ lag(effectiveness),
                filter(final_dataset, !(consolidated_country %in% countries_with_few_events)),
                family = negbin, model = "within", print.level = 3, method = "nr",
                index = c('consolidated_country', 'year'),
                start = NULL, R = 20 )
summary(my_pglm) 

