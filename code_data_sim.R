#'@author Maria Mittelbach 
#'@title code_data_sim
#'@description This code reads and creates some descriptive analysis for the SIM data set

#Packages
library(sf)
library(dplyr)
library(tidyverse)
library(microdatasus)
library(lubridate)
library(readxl)
library(stringi)
library(RColorBrewer)
library(brpop)

# Set up
YEAR_START <- 2007
YEAR_END <- 2019
UFS <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS",
         "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", 
         "SP", "SE", "TO")
dir <- getwd()
code_data_path <- paste0(dir,"\\code") %>% str_replace_all("/", "\\\\\\") 
input_data_path <- paste0(dir,"\\input") %>% str_replace_all("/", "\\\\\\") 
output_data_path <- paste0(dir,"\\output") %>% str_replace_all("/", "\\\\\\") 
maps_data_path <- paste0(dir,"\\maps") %>% str_replace_all("/", "\\\\\\") 



# Function treat data and save temporary file 
download_sim <- function(UF){
  print(UF)
  # open data
  df_sim_ra <- fetch_datasus(year_start = YEAR_START, 
                      year_end = YEAR_END,
                      uf = UF,
                      information_system = "SIM-DO") 
  
  # save intermediate file
  saveRDS(df_sim_ra, paste0(input_data_path,"/SIM/", UF  , ".rds"))
}


# Saving raw data
walk(UFS, download_sim)

# Function that cleans data and counts how many deaths per municipality of residence
deaths_sim <- function(UF){
  data <- readRDS(paste0(input_data_path,"/SIM/", UF  , ".rds"))%>% 
    mutate(date                      = as.character(DTOBITO),
           year                      = stri_sub(date,-4),
           month                     = stri_sub(date, from=3, length=2), 
           code_muni_residence       = CODMUNRES,
           code_muni_occurrence      = CODMUNOCOR,
           code_neighboor_residence  = CODBAIRES,
           code_neighboor_occurrence = CODBAIOCOR,
           state                     = UF)%>%
    group_by(year,month,code_muni_residence,state)%>% 
    summarise(number_deaths = n())
  
  return(data)
} 


# Getting count of the deaths
data_deaths <- map(UFS, deaths_sim) %>%
  do.call(rbind,.)   


#Open data of population
population_municipalities_raw <- brpop::mun_pop()
population_municipalities <- population_municipalities_raw %>% 
  filter(year >= YEAR_START & year <= YEAR_END & 
           age_group == "Total") %>% 
  select(-age_group)%>%
  mutate(code_muni_residence = as.factor(mun),
         year = as.character(year),
         population = pop)

# Combining bases and creating the rate :
data_deaths_rate <- inner_join(data_deaths,population_municipalities)%>%
  mutate(deaths_rate = 100000*number_deaths/population)

save(data_deaths_rate, file = paste0(output_data_path,"data_deaths_rate.Rdata"))



# Analyzing data:
## Summary
data_deaths_rate %>% summary()

## Histogram: 
data_deaths_rate$deaths_rate %>% hist()

## Creating Maps
load(paste0(input_data_path,"\\shapefiles\\clean_muniDivision2010.Rdata"))

#Aggregating by year
map_deaths_rate <- data_deaths_rate %>%
  group_by(year,code_muni_residence)%>%
  summarise(deaths_rate = sum(deaths_rate,na.rm = T))

shape_muni <- clean.muniDivision2010 %>% 
  mutate(code_muni_residence = str_sub(as.character(muni_code), end=-2))%>%
  select(code_muni_residence)

## Combining bases
map_deaths_rate <- inner_join(map_deaths_rate,shape_muni)

## Map
pal <- brewer.pal(7, "OrRd")

creating_map <- function(y_){
  df <- map_deaths_rate %>%
    filter(year== 2007 | year == 2019) 

  ggplot(df)+ 
    geom_sf(aes(geometry = geom,fill = deaths_rate),color = NA)+ 
    labs(fill='Death rate')+
    theme_void()+
    scale_fill_gradient(low="#FFF5EE", high="violetred4",
                        labels = c("0","250","500","750","1000","1250","1500","1750","2000"),
                        breaks = seq(from = 0, to = 2000, by = 250))+
    facet_wrap(~year)
  
  ggsave(paste0(maps_data_path,"\\death_rates.pdf"))
}

creating_map(2007)
creating_map(2019)
