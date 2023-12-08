library(data.table)
library(tidyverse)
library(magrittr)
library(here)

dir.create(here("trunk","derived","final datasets"),showWarnings = F)

dfs<-fread(here("trunk","derived","medicaid data","sdud_with_medicaid_estimates.txt"))
dfm<-fread(here("trunk","derived","medicare data","medicare_with_medicare_estimates.txt"))

df<-rbind(dfs[,.(Region,Year,drug_class,is30,days_supplied,med_estimate,med_perc,Insurer="Medicaid")],dfm[,.(Region,Year,drug_class,is30="30/60/90",days_supplied,med_estimate,med_perc,Insurer="Medicare")])
#days supplied is already multiplied by 30 (i.e. 60 days supplied is 2 30 day refills).

df%<>%rbind(df[,.(days_supplied=sum(days_supplied),drug_class="GLP-1 & SGLT-2"),.(Year,Region,med_estimate,med_perc,Insurer,is30)])
names(df)<-c("state","year","drug","is30","d_sup","pop","med_perc","prg")
fwrite(df,here("trunk","derived","final datasets","annual_df.txt"))
rm(list=ls())
