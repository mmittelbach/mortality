################################################################################
#'@author Maria Mittelbach                                                     #
#'@description This code downloads the data from SIH/DataSus                   #
#'@title code_download_sih                                                     #
#'@date  September 2022    
# mariamittelbach@gmail.com                                                     #
################################################################################

##############################
# 0 - Load libraries
##############################
library(read.dbc)
library(foreign)
library(tidyverse)

############################## 
# 1 - Source file 
##############################
rm(list=ls())

dir <- getwd()
code_data_path    <- paste0(dir,"/code") 
input_data_path   <- paste0(dir,"/input")
output_data_path  <- paste0(dir,"/output") 


############################## 
# 2 - Set ups 
##############################


prefix <- list("RD") 
month  <- list("01","02","03","04","05","06","07","08","09","10","11","12")
year   <- seq(2008,2019)
st     <- list("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")

url1 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  for(y in year){
    for(m in month) {
      for(s in 1:length(st)){
  
        url <- paste0(url1,prefix,st[s],substring(y,3,4),m,".dbc")
        download.file(url, destfile = paste0(input_data_path,"/sih/dbc/","sih_",y,"_",m,"_",st[s],".dbc"), mode = "wb")
        rd <- read.dbc(paste0(input_data_path,"/sih/dbc/","sih_",y,"_",m,"_",st[s],".dbc"))
        saveRDS(rd, file = paste0(input_data_path,"/sih/","sih_",y,"_",m,"_",st[s],".rds"))
        rm(rd)
      }
    }
  }
