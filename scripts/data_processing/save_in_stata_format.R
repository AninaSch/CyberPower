# save data for modelling in Stata format:

library(foreign)

CPI_2020 <- readRDS("../../data/data_for_modelling/CPI_2020.rds")
write.dta(CPI_2020,  file = "../../data/data_for_modelling/CPI_2020.dta") 
