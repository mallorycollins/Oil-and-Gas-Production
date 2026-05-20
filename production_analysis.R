library(tidyverse)
library(readr)


#read in data 
prod21 <- read_csv("OilGasProduction2021.csv")
prod22 <- read_csv("OilGasProduction2022.csv")
prod23 <- read_csv("OilGasProduction2023.csv")

prod <- rbind(prod21, prod22, prod23)
rm(prod21, prod22, prod23)


#all of the wells in Greene county in a given month
greene <- prod %>%
  filter(COUNTY == "Greene") %>%
  group_by(PERIOD_ID) %>% 
  summarize(sum(GAS_QUANTITY, na.rm = TRUE))
  
  
    


  
  
  