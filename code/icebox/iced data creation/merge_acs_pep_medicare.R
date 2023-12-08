library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dfpep_agg<-fread(here("trunk","derived","acs pep","pep_aggregated.txt"))
acs_agg<-fread(here("trunk","derived","acs pep","acs_aggregated.txt"))
brfss<-fread(here("trunk","derived","brfss_denoms","brfss_denoms.txt"))

df_pop<-acs_agg[,.(YEAR,perc_medicare,Region)]%>%merge(dfpep_agg,by=c("YEAR","Region"))

df_pop[,med_estimate:=perc_medicare*population]
df_pop%<>%merge(brfss[,.(Region=State,perc_diab=medicare_perc,YEAR=Year)],by=c("YEAR","Region"))

df_pop[,med_estimate:=med_estimate*perc_diab]

df<-fread(here("trunk","derived","medicare data","relevant_medicare.txt"))
df<-df[Prscrbr_Geo_Lvl=="State"]
df[,Region:=Prscrbr_Geo_Desc]


df%<>%merge(df_pop[,.(Region,Year=YEAR%>%as.numeric(),med_estimate)],by=c("Region","Year"))

fwrite(df,here("trunk","derived","medicare data","unagg_medicare_with_medicare_estimates.txt"))

df$days_supplied<-as.numeric(gsub(x=df$Tot_30day_Fills,",",""))

df_agg<-df[,.(days_supplied=sum(days_supplied*30),
      med_estimate=mean(med_estimate)),.(Year,Region,drug_class=type)]
fwrite(df_agg,here("trunk","derived","medicare data","medicare_with_medicare_estimates.txt"))
rm(list=ls())
