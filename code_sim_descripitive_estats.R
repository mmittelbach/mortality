#'@author Maria Mittelbach 
#'@title code_sim_descripitive_estats
#'@description This descriptive analysis for the SIM data set


#Packages
library(dplyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(brpop)

#Set up
dir <- getwd()
code_data_path <- paste0(dir,"\\code") %>% str_replace_all("/", "\\\\\\") 
input_data_path <- paste0(dir,"\\input") %>% str_replace_all("/", "\\\\\\") 
output_data_path <- paste0(dir,"\\output") %>% str_replace_all("/", "\\\\\\") 
maps_data_path <- paste0(dir,"\\maps") %>% str_replace_all("/", "\\\\\\") 

# Open data 

load(paste0(output_data_path,"\\data_deaths_rate.Rdata"))

# Creating Regions variable 
data_deaths_rate$region <- NA
data_deaths_rate_region <- data_deaths_rate %>% 
  mutate(region = case_when(state %in% c("RO","AC","AM","RR","PA","AP","TO")~ "North",
                            state %in% c("MA","PI","CE","RN","PB","PE","AL","SE","BA")~ "Northeast",
                            state %in% c("MG","ES","RJ","SP") ~ "Southeast", 
                            state %in% c("PR","SC","RS") ~ "South",
                            state %in% c("MS","MT","GO","DF") ~ "Midwest"))

gc()
# Creating data year-region
data_deaths_rate_map <- data_deaths_rate_region %>%
  group_by(year,region)%>%
  summarise(population = sum(population),
            number_deaths = sum(number_deaths))%>%
  mutate(deaths_rate = 100000*number_deaths/population)

# Creating graph 

ggplot(data_deaths_rate_map, 
       aes(x = year, y = deaths_rate, group = region, 
           color = factor(region))) +
  geom_line()+ 
  labs(color='Regions') + 
  xlab("Year") +
  ylab("")+
  theme_classic()

ggsave(paste0(maps_data_path,"\\death_rates_regions.pdf"))





