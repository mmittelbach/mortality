################################################################################
#'@author Maria Mittelbach                                                     #
#'@description This code creates the information of hospital opening           #
#'@title 01_data_hospitals                                                     #
#'@date  August 2022                                                           #
################################################################################

############################## 
# 0 - Open files 
##############################
## Hospitals
cnes_muni_month_panel <- read_csv(paste0(input_data_path,"\\cnes_muni_month_panel.csv"))

## Municipalities
load(paste0(input_data_path,"\\shapefiles\\clean_muniDivision2010.Rdata"))

############################## 
# 2 - Some estatistics 
##############################


############################## 
# 1 - Creating graphs
##############################

# 1.1 Aggregate #############################

##1. Aggregating data
cnes_total_hospitals_dt<- cnes_muni_month_panel %>%
  filter(total_hosp_beds != 0 & mes == 12) %>%
  group_by(ano)%>% 
  summarise(small  = sum(number_small,na.rm = T),
            big    = sum(number_big,na.rm = T),
            medium = sum(number_medium,na.rm = T))

cnes_total_hospitals <- cnes_total_hospitals_dt%>% 
  pivot_longer(cols = c('small','big','medium'),
               names_to = "size",
               values_to = "Number")

## Proportion of hospitals 
cnes_total_hospitals_prop <-  cnes_total_hospitals_dt %>% 
  mutate(total = small + big + medium,
         prop_small  = 100*small/total,
         prop_medium = 100*medium/total,
         prop_big    = 100*big/total)%>% 
  select(ano,prop_small,prop_medium,prop_big)


##2. Ruining bar graph
ggplot(cnes_total_hospitals, aes(ano, Number)) +   
  geom_bar(aes(fill = size), position = "stack", stat="identity")


# 1.2 By region #############################

##1. Aggregating data by region
cnes_region_hospitals<- cnes_muni_month_panel %>%
  mutate(region  = case_when(sigla_uf %in% c('AM','RR','AP','PA','TO','RO','AC') ~ 'North',
                            sigla_uf %in% c('MA','PI','CE','RN','PE','PB','SE','AL','BA') ~ 'Northeast',
                            sigla_uf %in% c('MT','MS','DF','GO') ~ 'Midwest',
                            sigla_uf %in% c('SP','RJ','ES','MG') ~ 'Southeast',
                            sigla_uf %in% c('PR','RS','SC')      ~ 'South')) %>% 
  filter(total_hosp_beds != 0 & mes == 12) %>%
  group_by(ano, region)%>% 
  summarise(small  = sum(number_small,na.rm = T),
            big    = sum(number_big,na.rm = T),
            medium = sum(number_medium,na.rm = T)) %>% 
  pivot_longer(cols = c('small','big','medium'),
               names_to = "size",
               values_to = "Number")

##2. Ruining bar graph
ggplot(cnes_region_hospitals, aes(ano, Number)) +   
  geom_bar(aes(fill = size), width=0.6,position = "stack", stat="identity") +
  facet_wrap(. ~ region, labeller=label_both)


# 1.2 By size #############################

##1. Aggregating data by region
cnes_region_hospitals<- cnes_muni_month_panel %>%
  mutate(region  = case_when(sigla_uf %in% c('AM','RR','AP','PA','TO','RO','AC') ~ 'North',
                             sigla_uf %in% c('MA','PI','CE','RN','PE','PB','SE','AL','BA') ~ 'Northeast',
                             sigla_uf %in% c('MT','MS','DF','GO') ~ 'Midwest',
                             sigla_uf %in% c('SP','RJ','ES','MG') ~ 'Southeast',
                             sigla_uf %in% c('PR','RS','SC')      ~ 'South')) %>% 
  filter(total_hosp_beds != 0 & mes == 12) %>%
  group_by(ano, region)%>% 
  summarise(small  = sum(number_small,na.rm = T),
            big    = sum(number_big,na.rm = T),
            medium = sum(number_medium,na.rm = T)) %>% 
  pivot_longer(cols = c('small','big','medium'),
               names_to = "size",
               values_to = "Number")
##2. Ruining bar graph
ggplot(cnes_region_hospitals, aes(ano, Number)) +   
  geom_bar(aes(fill = size), width=0.6,position = "stack", stat="identity") +
  facet_wrap(. ~ region, labeller=label_both)


