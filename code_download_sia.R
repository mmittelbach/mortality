################################################################################
#'@author Maria Mittelbach                                                     #
#'@description This code downloads the data from SIA/DataSUS                   #
#'@title code_download_sia                                                     #
#'@date  September 2022                                                        #
# mariamittelbach@gmail.com                                                    #
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
prefix        <- list("PA") 
month         <- list("01","02","03","04","05","06","07","08","09","10","11","12")
year          <- seq(2008,2019)
st            <- list("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
url_after2008 <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
 
############################## 
# 3 - Running code 
##############################
# Download in dbf format, convert to dbc and rds
for(y in year){
  for(s in st){
    for(m in 1:length(month)){
      for(ext in c('','a','b','c')) { # for SP only?
        filename <- paste0("PA",s,substring(y,3,4),month[m],ext)
        url <- paste0(url_after2008,filename,".dbc")
        
        err <- try(download.file(url, destfile = paste0(input_data_path,"/sia/dbc/","sia_",y,"_",m,"_",st[s],".dbc"), mode = "wb"))
        if (class(err) == "try-error") next

        sia_pa <- read.dbc(paste0(input_data_path,"/sia/dbc/","sia_",y,"_",m,"_",st[s],".dbc"))
        saveRDS(sia_pa, file = paste0(input_data_path,"/sia/","sia_",y,"_",m,"_",st[s],".rds"))
        rm(sia_pa)
      }
    }
  }
}
