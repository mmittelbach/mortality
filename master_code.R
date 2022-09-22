################################################################################
#'@author Maria Mittelbach                                                     #
#'@description This is the master file for the mortality project               #
#'@title master_code                                                           #
#'@date  August 2022    
# mariamittelbach@gmail.com                                                     #
################################################################################

##############################
# 0 - Load librairies
##############################
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
library(read.dbc)

############################## 
# 1 - Source file 
##############################
dir <- getwd()
code_data_path <- paste0(dir,"\\code") %>% str_replace_all("/", "\\\\\\") 
input_data_path <- paste0(dir,"\\input") %>% str_replace_all("/", "\\\\\\") 
output_data_path <- paste0(dir,"\\output") %>% str_replace_all("/", "\\\\\\") 

############################## 
# 2 - Set ups 
##############################
year <- c(2005:2019)
month <- c(1:12)
d1 <- crossing(month, year)

first_date <- lubridate::dmy("01/01/2005")
first_year <- "2005"

############################## 
# 3 - Codes
##############################

# 3.1 Data preparation




