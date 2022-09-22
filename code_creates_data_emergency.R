#'@author Maria Mittelbach 
#'@title code_creates_data_emergency
#'@description This code creates the data set with emergency facilities

#Packages
library(owdbr)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(rlang)
library(readr)
library(sf)
library(RSQLite)
library(lubridate)
library(microdatasus)

#Set Ups:
dir <- getwd()
code_data_path <- paste0(dir,"\\code") %>% str_replace_all("/", "\\\\\\") 
input_data_path <- paste0(dir,"\\input") %>% str_replace_all("/", "\\\\\\") 
output_data_path <- paste0(dir,"\\output") %>% str_replace_all("/", "\\\\\\") 
year <- c(2007:2019)
month <- c(1:12)
d1 <- crossing(month, year)

first_date <- lubridate::dmy("01/01/2007")
first_year <- "2007"
#Open data 
## Municipalities
load(paste0(input_data_path,"\\shapefiles\\clean_muniDivision2010.Rdata"))

## All emergency facilities
load(paste0(output_data_path,"\\emergency_all.Rdata"))

## Emergency facilities that open and didn't closed between 2007 to 2019

load(paste0(output_data_path,"\\emergency_open_final.Rdata"))

## Death rates 
load(paste0(output_data_path,"\\data_deaths_rate.Rdata"))

#Getting all municipalities 
muni_all <- clean.muniDivision2010 %>% 
  st_drop_geometry()%>%
  rename(state = state_name)

#Removing any municipality that appears at the initial establishment database
list_muni_emergency<- cnes_panel %>%
  ungroup()%>%
  select(muni_code,state)%>% 
  unique()

muni_never_treated <- anti_join(muni_all,list_muni_emergency)%>% 
  mutate(dates = NA)

#Combining with the treated units
## Getting muni name 
muni_treated <- left_join(emergency_open_final,muni_all)%>% 
  select(-dummy_changed)

df_final_emergency <- rbind(muni_never_treated,muni_treated)%>%
  mutate(dummy_treated = ifelse(is.na(dates),0,1),
         muni_code_6   = substring(as.character(muni_code),1, nchar(as.character(muni_code))-1),
         dates = lubridate::dmy(paste0("01/",dates)),
         group = ifelse(is.na(dates),0,1+interval(first_date, dates) %/% months(1)))%>% 
  select(group,muni_code_6)
  
#Creating yearly data
df_final_emergency_yearly <- rbind(muni_never_treated,muni_treated)%>%
  mutate(dummy_treated = ifelse(is.na(dates),0,1),
         muni_code_6   = substring(as.character(muni_code),1, nchar(as.character(muni_code))-1),
         year = lubridate::year(lubridate::dmy(paste0("01/",dates))),
         group = ifelse(is.na(year),0,1+ as.numeric(year) - 2007))%>% 
  select(group,muni_code_6)



# Cleaning death rates data 

df_deaths <- data_deaths_rate %>% 
  mutate(dates = paste0(month,"/",year),
         muni_code_6  = as.character(code_muni_residence))%>%
  ungroup()%>%
  rename(death_rate = deaths_rate) %>% 
  select(year,month,dates,muni_code_6,death_rate, population)%>% 
  mutate(dates = lubridate::dmy(paste0("01/",dates)),
         period = ifelse(is.na(dates),0,1+interval(first_date, dates) %/% months(1)))

#Combining dates to year 
df_deaths_yearly <- data_deaths_rate %>% 
  mutate(muni_code_6  = as.character(code_muni_residence))%>%
  group_by(year,muni_code_6)%>% 
  summarise(deaths_rate = sum(deaths_rate,na.rm = T),
            population = population)%>%
  mutate(period = ifelse(is.na(year),0,1+ as.numeric(year) - 2007))%>%
  filter(population != 0)


# Combining death rates with treatment 
df_final_yearly <- left_join(df_final_emergency_yearly,df_deaths_yearly)%>%
  mutate(muni_code_6 = as.numeric(muni_code_6))

df_final <- left_join(df_final_emergency,df_deaths)%>%
  mutate(muni_code_6 = as.numeric(muni_code_6))

save(df_final,file = paste0(output_data_path,"\\df_final.Rdata"))
save(df_final_yearly,file = paste0(output_data_path,"\\df_final_yearly.Rdata"))

