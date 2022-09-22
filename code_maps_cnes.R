#'@author Maria Mittelbach 
#'@title code_maps_cnes
#'@description This code creates a map with number of health establishments in each municipality in Brasil


#Packages
library(owdbr)
library(dbplyr)
library(dplyr)
library(tidyverse)
library(rlang)
library(readr)
library(sf)
library(RSQLite)
library(microdatasus)

#Set Ups:
dir <- getwd()
code_data_path <- paste0(dir,"\\code") %>% str_replace_all("/", "\\\\\\") 
input_data_path <- paste0(dir,"\\input") %>% str_replace_all("/", "\\\\\\") 
output_data_path <- paste0(dir,"\\output") %>% str_replace_all("/", "\\\\\\") 
year <- c(2007:2019)
month <- c(1:12)
d1 <- crossing(month, year)



#Open data from CNES
##download: 2022-07-07
##source: https://basedosdados.org/dataset/br-ms-cnes?bdm_table=estabelecimento
##temporal cover: 2007-2019 monthly data
## Download at then SQL 

#Listing raw data 
names <- list.files(paste0(input_data_path,"\\estabelecimentos_urgencia"), full.names = TRUE)


# Open sample data 
open_cleans <- function(file_name){
  df_cnes_raw <- read.csv(file = file_name)
  df_cnes<- df_cnes_raw %>%
    rename(muni_code = id_municipio,
           state     = sigla_uf,
           year      = ano,
           month     = mes)%>% 
    group_by(muni_code,state,year,month)%>%
    summarise(number_emergency_facilities = sum(indicador_instalacao_urgencia), 
              dummy_emergency = ifelse(number_emergency_facilities== 0, 0,1))
  
  return(df_cnes)
}

# Getting count of emergency facilities in a municipality
cnes_panel <- map(names, open_cleans) %>%
  do.call(rbind,.)

#Saving data
save(cnes_panel, file = paste0(output_data_path,"\\emergency_all.Rdata"))



# Creating dummies for each month and year
cnes_panel_dummy <-cnes_panel %>% 
  mutate(date = paste0(month,"/",year))%>%
  ungroup() %>%
  select(-number_emergency_facilities, -year,-month)%>%
  pivot_wider(names_from = date, values_from = dummy_emergency)%>%
  mutate_if(is.numeric, ~ ifelse(is.na(.),0,.))

# Creating data frame with municipalities that some emergency facility entered and did
# not got out.

emergency_df <-  cnes_panel_dummy %>% 
  mutate(dates_with_emergency = select(., `1/2007`:`12/2019`) %>% 
           rowSums(na.rm = TRUE))%>% 
  filter(dates_with_emergency!=156)

for(i in c(3:157)){
  name <- colnames(emergency_df[,i + 1]) 
  emergency_df[, paste0("change_",name)]  <- emergency_df[,i + 1] - emergency_df[,i]
}
  
emergency_final <- emergency_df %>% 
  select(muni_code,state,contains('change_'))%>% 
    pivot_longer(cols = -c(muni_code,state), names_to = 'dates',
                 values_to = "dummy_changed")%>%
  mutate(dates =  gsub("change_","",dates))%>%
  filter(dummy_changed != 0)%>%
  arrange(dates, muni_code) %>%
  filter(duplicated(muni_code) == FALSE)

emergency_open_final <- emergency_final %>%
  filter(dummy_changed == 1)
#Saving data
save(emergency_open_final, file = paste0(output_data_path,"\\emergency_open_final.Rdata"))

emergency_close_final <- emergency_final %>%
  filter(dummy_changed == -1)
#Saving data
save(emergency_close_final, file = paste0(output_data_path,"\\emergency_close_final.Rdata"))

#Removing any municipality that appears at the initial establishment database

## Creating Maps
# Creating a base with many municipalities: 
emergency_facilities <-  cnes_panel_dummy %>% 
  mutate(dates_with_emergency = select(., `1/2007`:`12/2019`) %>% 
           rowSums(na.rm = TRUE))%>% 
  filter(dates_with_emergency==156)%>%
  select(muni_code,state)%>%
  mutate(dummy_changed = 0)

all_muni <- rbind(emergency_open_final,emergency_close_final)%>%
  select(muni_code,state,dummy_changed)%>%
  rbind(.,emergency_facilities)%>%
  mutate(dummy_changed = ifelse(dummy_changed == -1,
                                "Close",
                                ifelse(dummy_changed == 1,
                                       "Open",
                                       ifelse(dummy_changed == 0,
                                              "Open in every period",
                                              dummy_changed))))

#Open sf
load(paste0(input_data_path,"\\shapefiles\\clean_muniDivision2010.Rdata"))
    
#Combining 
map_emergency <- left_join(clean.muniDivision2010,all_muni)

#Ploting map
ggplot() + 
geom_sf(data=filter(map_emergency, !is.na(dummy_changed)), 
        aes(fill= factor(dummy_changed)), alpha = NA)+
  scale_fill_manual(values = c('#8BBEE8FF', "#D7A9E3FF", "#A8D5BAFF"))+ 
  guides(fill=guide_legend(title="Status of emergency facilities"))+
  theme_void()
        
  
ggsave(paste0(maps_data_path,"\\emergency_facilities.pdf"))


