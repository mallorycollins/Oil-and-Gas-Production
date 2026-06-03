library(tidyverse)
library(readr)
library(latex2exp)
library(stringr)

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
  summarize(sum(GAS_QUANTITY, na.rm = TRUE))%>% #gas production in Mcf or 1,000 cubic feet
  mutate(dt= ym(paste0("20", str_remove(PERIOD_ID, "P$")))) %>%
  rename(gas= `sum(GAS_QUANTITY, na.rm = TRUE)`)%>%
  select(dt, gas)
  #inserted long variable name, that has spaces. Work around is having grave ticks. Gets rid of autopopulated variable name
  
write_csv(greene, "greene(t_gas_production.csv")

gg <- ggplot(greene) +
  geom_line(aes(x=dt, y=gas)) +
  xlab("Date") +
  ylab(TeX('Gas Production ($\\; times 10^6\\ Mcf$)')) + #slashes gives spaces 
  theme(panel.background = element_rect(fill= "white", colour = "black"), 
    axis.text = element_text(face = "plain", size= 14), 
    axis.title =element_text(face = "plain", size = 14))
    
ggsave("Greene(ty_gas_prod.eps", gg, device = "eps")
  
#Data analysis of Washington county
    
washington <- prod %>%
  filter(COUNTY == "Washington") %>%
  group_by(PERIOD_ID) %>% 
  summarize(sum(GAS_QUANTITY, na.rm = TRUE))%>% #gas production in Mcf or 1,000 cubic feet
  mutate(dt= ym(paste0("20", str_remove(PERIOD_ID, "P$")))) %>%
  rename(gas= `sum(GAS_QUANTITY, na.rm = TRUE)`)%>%
  select(dt, gas)
write_csv(washington, "washington(t_gas_production.csv")

gg <- ggplot(washington) +
  geom_line(aes(x=dt, y=gas)) +
  xlab("Date") +
  ylab(TeX('Gas Production ($\\; times 10^6\\ Mcf$)')) +  
  theme(panel.background = element_rect(fill= "white", colour = "black"), 
        axis.text = element_text(face = "plain", size= 14), 
        axis.title =element_text(face = "plain", size = 14))

ggsave("washington(ty_gas_prod.eps", gg, device = "eps") 
ggsave("washington(ty_gas_prod.jpeg", gg, device = "jpeg")
  