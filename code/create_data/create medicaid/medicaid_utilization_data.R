library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dir.create(here("trunk","derived","medicaid data"),showWarnings = F)
  #create directory
sdud_data<-list.files(here("trunk","raw","SDUD_database"),pattern = paste0(as.character(2003:2020),collapse = "|"),full.names = T)
#load in CMS data from https://data.medicaid.gov/datasets?theme[0]=State%20Drug%20Utilization&fulltext=state%20drug%20utilization&sort=modified

names(fread(sdud_data[18],colClasses=rep("character",15),nrows=10))
#names aren't capitalized for 2005:2013
#suppression is spelled wrong in 2020 data

dfs<-rbindlist(map(sdud_data[c(3:11)],fread,colClasses=rep("character",15)))
dfss<-rbindlist(map(sdud_data[c(1:2,12:17)],fread,colClasses=rep("character",15)))
dfs2020<-fread(sdud_data[18],colClasses=rep("character",15))

names(dfs)<-names(dfss)
names(dfs2020)<-names(dfss)
dfs<-rbind(dfs,dfss,dfs2020)

gdata::keep(dfs,sure=T)
dfd<-fread(here("trunk","derived","ndcs","relevant_ndc_fixed.txt"))

#manually creating a prop name list because Medicare data lacks NDCs
glp1<-c("adlyxin","bydureon","byetta","ozempic","rybelsus","saxenda","soliqua","trulicity","victoza","wegovy","xultophy")
  #wegovy is missing from medicare drugs, by prop and nonprop names, because it was implemented in 2021
sglt2<-c("farxiga","glyxambi","invokamet","invokana","jardiance","qtern","synjardy","trijardy","xigduo")
  #

dfs[,drug_class:=case_when(grepl(`Product Name`,pattern=paste0(sglt2,collapse = "|"),ignore.case = T)~"SGLT-2",
                           grepl(`Product Name`,pattern=paste0(glp1,collapse = "|"),ignore.case = T)~"GLP-1")]

df<-dfs[!is.na(drug_class)]

fwrite(df,here("trunk","derived","medicaid data","relevant_sdud.txt"))
rm(list=ls())
