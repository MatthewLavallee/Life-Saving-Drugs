library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dfpep_agg<-fread(here("trunk","derived","acs pep","pep_aggregated.txt"))
acs_agg<-fread(here("trunk","derived","acs pep","acs_aggregated.txt"))
xw<-fread(here("trunk","derived","medicaid data","medicaid_days_xw.txt"))
brfss<-fread(here("trunk","derived","brfss_denoms","brfss_denoms.txt"))
df_pop<-acs_agg[,.(YEAR,perc_medicaid,Region)]%>%merge(dfpep_agg,by=c("YEAR","Region"))

df_pop[,med_estimate:=perc_medicaid*population]
df_pop%<>%merge(brfss[,.(Region=State,perc_diab=medicaid_perc,YEAR=Year)],by=c("YEAR","Region"))
df_pop[,med_estimate:=med_estimate*perc_diab]
df<-fread(here("trunk","derived","medicaid data","relevant_sdud.txt"))
df[,Region:=usdata::abbr2state(State)]

df%<>%merge(df_pop[,.(Region,Year=YEAR%>%as.numeric(),med_estimate)],by=c("Region","Year"))
#set of NAs are dropped here, they're national estimates
#Year 2020 & 2021 are dropped, missing from ACS currently
df%<>%merge(xw[,.(`Product Code`=prod_code,`Package Size`=pack_code,days_30,is30)],by=c("Product Code","Package Size"))
fwrite(df,here("trunk","derived","medicaid data","unagg_sdud_with_medicaid_estimates.txt"))
df<-df[,.(
   `Units Reimbursed`=sum(`Units Reimbursed`,na.rm=T),
   days_supplied=sum(`Units Reimbursed`*days_30/`Package Size`,na.rm=T),
   `Number of Prescriptions`=sum(`Number of Prescriptions`,na.rm=T),
   `Total Amount Reimbursed`=sum(`Total Amount Reimbursed`,na.rm=T),
   `Medicaid Amount Reimbursed`=sum(`Medicaid Amount Reimbursed`,na.rm=T),
   `Non Medicaid Amount Reimbursed`=sum(`Non Medicaid Amount Reimbursed`,na.rm=T),
   med_estimate=mean(med_estimate)),.(Region,drug_class,Year,is30)]

fwrite(df,here("trunk","derived","medicaid data","sdud_with_medicaid_estimates.txt"))

rm(list=ls())
