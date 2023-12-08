library(data.table)
library(tidyverse)
library(magrittr)
library(here)

dir.create(here("trunk","derived","medicaid data"),showWarnings = F)
#should exist if running in order


medicaid<-fread(here("trunk","raw","Monthly Medicaid Data","data","medicaid_june.txt"))
medicare<-rbindlist(map(list.files(here("trunk","raw","Monthly Medicare Data"),full.names = T),fread),idcol="id")
lxw<-tibble(year=gsub(".csv","",gsub("m","",list.files(here("trunk","raw","Monthly Medicare Data")))),id=1:11)
medicare<-merge(medicare,lxw,by="id")

xw<-fread(here("trunk","derived","medicaid data","medicaid_days_xw.txt"))
brfss<-fread(here("trunk","derived","brfss_denoms","brfss_denoms.txt"))
names(brfss)[5]<-"year"
dfpop<-brfss%>%
  merge(medicaid[,.(year,State,medicaid_pop=enroll)],by=c("State","year"))%>%
  merge(medicare[,.(State=Location,year=as.integer(year),medicare_pop=Total)],by=c("State","year"),all.x = T)

dfpop[,`:=`(medicaid_estimate=medicaid_perc*as.numeric(medicaid_pop),
             medicare_estimate=medicare_perc*as.numeric(medicare_pop))]

df<-fread(here("trunk","derived","medicaid data","relevant_sdud.txt"))
df[,Region:=usdata::abbr2state(State)]

df%<>%merge(dfpop[,.(Region=State,Year=year,medicaid_estimate,medicaid_perc)],by=c("Region","Year"))
#set of NAs are dropped here, they're national estimates

df%<>%merge(xw[,.(`Product Code`=prod_code,`Package Size`=pack_code,days_30,is30)]%>%unique(),by=c("Product Code","Package Size"))

fwrite(df,here("trunk","derived","medicaid data","unagg_sdud_with_medicaid_estimates.txt"))
df<-fread(here("trunk","derived","medicaid data","unagg_sdud_with_medicaid_estimates.txt"))
dfa<-df[,.(
  `Units Reimbursed`=sum(`Units Reimbursed`,na.rm=T),
  days_supplied=sum(`Units Reimbursed`*days_30/`Package Size`,na.rm=T),
  `Number of Prescriptions`=sum(`Number of Prescriptions`,na.rm=T),
  `Total Amount Reimbursed`=sum(`Total Amount Reimbursed`,na.rm=T),
  `Medicaid Amount Reimbursed`=sum(`Medicaid Amount Reimbursed`,na.rm=T),
  `Non Medicaid Amount Reimbursed`=sum(`Non Medicaid Amount Reimbursed`,na.rm=T),
  med_estimate=mean(medicaid_estimate),
  med_perc=mean(medicaid_perc)),.(Region,drug_class,Year)]
df3<-df[is30==1,.(
  `Units Reimbursed`=sum(`Units Reimbursed`,na.rm=T),
  days_supplied=sum(`Units Reimbursed`*days_30/`Package Size`,na.rm=T),
  `Number of Prescriptions`=sum(`Number of Prescriptions`,na.rm=T),
  `Total Amount Reimbursed`=sum(`Total Amount Reimbursed`,na.rm=T),
  `Medicaid Amount Reimbursed`=sum(`Medicaid Amount Reimbursed`,na.rm=T),
  `Non Medicaid Amount Reimbursed`=sum(`Non Medicaid Amount Reimbursed`,na.rm=T),
  med_estimate=mean(medicaid_estimate),
  med_perc=mean(medicaid_perc)),.(Region,drug_class,Year)]
df3[,is30:="30/60/90"]
dfa[,is30:="all"]
df<-rbind(dfa,df3)
fwrite(df,here("trunk","derived","medicaid data","sdud_with_medicaid_estimates.txt"))

rm(list=ls())
