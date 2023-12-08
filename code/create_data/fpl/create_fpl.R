library(data.table)
library(datapasta)
library(tidyverse)
library(magrittr)
library(rvest)
library(here)

dir.create(here("trunk","raw","FPL"),showWarnings = F)

download.file(url="https://aspe.hhs.gov/sites/default/files/documents/8e7483715719918e8fddb58ab23f606d/Guidelines-1983-2022.xlsx",destfile = here("trunk","raw","FPL","fpls.xlsx"),mode="wb")

dfr<-vector()
for (i in 1:3) {
  df<-readxl::read_excel(here("trunk","raw","FPL","fpls.xlsx"),sheet=i+1)
  df%<>%as.data.table()
  df1<-df[4:43]
  names(df1)<-df[3]%>%as.character()
  df1<-df1[,lapply(.SD,as.numeric),.SDcols=names(df1)[1:10]]
  
  df1[,`:=`(`9 Persons`=`8 Persons`+`Additional $`,
            `10 Persons`=`8 Persons`+2*`Additional $`,
            `11 Persons`=`8 Persons`+3*`Additional $`,
            `12 Persons`=`8 Persons`+4*`Additional $`,
            `13 Persons`=`8 Persons`+5*`Additional $`,
            `14 Persons`=`8 Persons`+6*`Additional $`,
            `15 Persons`=`8 Persons`+7*`Additional $`,
            `16 Persons`=`8 Persons`+8*`Additional $`,
            `17 Persons`=`8 Persons`+9*`Additional $`,
            `18 Persons`=`8 Persons`+10*`Additional $`,
            `19 Persons`=`8 Persons`+11*`Additional $`,
            `20 Persons`=`8 Persons`+12*`Additional $`,
            `21 Persons`=`8 Persons`+13*`Additional $`,
            `22 Persons`=`8 Persons`+14*`Additional $`,
            `23 Persons`=`8 Persons`+15*`Additional $`,
            `24 Persons`=`8 Persons`+16*`Additional $`)]
  
  df1<-
    df1%>%
    pivot_longer(cols=!c("Year","Additional $"),values_to = "FPL",names_to="hh_size")%>%
    mutate(hh_size=gsub(" .*","",hh_size),states=c("all","Alaska","Hawaii")[i])
  dfr%<>%rbind(df1)
}
dfr%<>%data.table()

h<-c(state.name[c(1,3:10,12:50)],"District of Columbia")
dfr%<>%
  mutate(state=case_when(states=="Hawaii"~list("Hawaii"),
                         states=="Alaska"~list("Alaska"),
                         T~list(h)))%>%
  unnest(state)%>%select(!c(states))

dfr%<>%mutate(fips=cdlTools::fips(state))
fwrite(dfr,here("trunk","raw","FPL","fpl.csv"))

rm(list=ls())
