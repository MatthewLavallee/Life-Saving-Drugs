library(here)
library(data.table)
library(tidyverse)
library(magrittr)

dir.create(here("trunk","derived","brfss_denoms"), showWarnings = FALSE)
exp<-fread(here("trunk","raw","BRFSS","expansion.csv"))
  #read in expansion file from KFF
exp[,`:=`(`_STATE`=cdlTools::fips(State),
           year_implemented=as.numeric(gsub("^.+?(\\d{4}).*", "\\1",Description)))]
  #add fips code and break out year implemented
exp[is.na(year_implemented)]$year_implemented<-0
  #if na, make 0

fwrite(exp,here("trunk","derived","brfss_denoms","medicaid_year_expansion.csv"))

rm(list=ls())
