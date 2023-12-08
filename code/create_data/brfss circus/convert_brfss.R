library(data.table)
library(tidyverse)
library(magrittr)
library(here)
library(haven)

dir.create(here("trunk","derived","brfss_denoms"), showWarnings = FALSE)

for (xyear in c(2004:2020)) {
  df<-read_xpt(here("trunk","raw","BRFSS",paste0("brfss_",xyear,".XPT")))
  fwrite(df,here("trunk","derived","brfss_denoms",paste0("brfss_",xyear,".txt")))
}
  #write brfss to .txt for faster loading
rm(list=ls())
         