library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dir.create(here("trunk","derived","medicare data"),showWarnings = F)

medicaid<-fread(here("trunk","raw","Monthly Medicaid Data","data","medicaid_june.txt"))
medicare<-rbindlist(map(list.files(here("trunk","raw","Monthly Medicare Data"),full.names = T),fread),idcol="id")
lxw<-tibble(year=gsub(".csv","",gsub("m","",list.files(here("trunk","raw","Monthly Medicare Data")))),id=1:11)
medicare<-merge(medicare,lxw,by="id")

xw<-fread(here("trunk","derived","medicaid data","medicaid_days_xw.txt"))
brfss<-fread(here("trunk","derived","brfss_denoms","brfss_denoms.txt"))
names(brfss)[5]<-"year"
dfpop<-brfss%>%merge(medicaid[,.(year,State,medicaid_pop=enroll)],by=c("State","year"))%>%merge(medicare[,.(State=Location,year=as.integer(year),medicare_pop=Total)],by=c("State","year"),all.x = T)

dfpop[,`:=`(medicaid_estimate=medicaid_perc*as.numeric(medicaid_pop),
            medicare_estimate=medicare_perc*as.numeric(medicare_pop))]

df<-fread(here("trunk","derived","medicare data","relevant_medicare.txt"))
df<-df[Prscrbr_Geo_Lvl=="State"]
df[,Region:=Prscrbr_Geo_Desc]


df%<>%merge(dfpop[,.(Region=State,Year=year,medicare_estimate,medicare_perc)],by=c("Region","Year"))
#some obs lost because are not part of 50-states

fwrite(df,here("trunk","derived","medicare data","unagg_medicare_with_medicare_estimates.txt"))

df$days_supplied<-as.numeric(gsub(x=df$Tot_30day_Fills,",",""))

df_agg<-df[,.(days_supplied=sum(days_supplied*30),
              med_estimate=mean(medicare_estimate),
              med_perc=mean(medicare_perc)),.(Year,Region,drug_class=type)]
fwrite(df_agg,here("trunk","derived","medicare data","medicare_with_medicare_estimates.txt"))
rm(list=ls())
