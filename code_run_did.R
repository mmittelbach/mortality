#'@author Maria Mittelbach 
#'@title code_creates_data_emergency
#'@description This code does staggered DiD

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
library(did)

#Open data
load(paste0(output_data_path,"\\df_final_yearly.Rdata"))

df_final_year <- df_final_yearly %>%
  filter(year == "2007"|year == "2008"|year == "2009"|year == "2010"|year == "2011")
#Estimating ATT
out <- att_gt(yname = "deaths_rate",
              gname = "group",
              idname = "muni_code_6",
              tname = "period",
              xformla = ~ 1 ,
              data = df_final_year,
              est_method = "reg",
              allow_unbalanced_panel = TRUE)

es <- aggte(out, type = "dynamic",na.rm = TRUE)


group_effects <- aggte(out, type = "group")

ggdid(es)

ggsave(paste0(maps_data_path,"\\death_rates_regression_1.pdf"))

