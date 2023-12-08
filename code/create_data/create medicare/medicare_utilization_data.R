library(data.table)
library(tidyverse)
library(magrittr)
library(here)

dir.create(here("trunk","derived","medicare data"),showWarnings = F)

medicare<-list.files(here("trunk","raw","Medicare Part D"),pattern = "medicare_1",full.names = T)

dfm<-rbindlist(map(medicare,fread),idcol="file")
table(dfm$file)

dfm[,Year:=factor(file,labels=basename(paste0("20",gsub(gsub(medicare,pattern=".*_",replacement=""),pattern="\\..*",replacement=""))))]
  #create year

dfm%>%head()
  #brnd_name & gnrc_name are the result of linking ndc codes

dfd<-fread(here("trunk","derived","ndcs","relevant_ndc_fixed.txt"))
dfd$drug_type%>%table
dfd[drug_type=="SGLT-2"]$prop_name%>%table()
  #manually creating a prop name list due to coding differences in 
glp1<-c("adlyxin","bydureon","byetta","ozempic","rybelsus","saxenda","soliqua","trulicity","victoza","wegovy","xultophy")
  #wegovy is missing from medicare drugs, by prop and nonprop names, because it was implemented in 2021
sglt2<-c("farxiga","glyxambi","invokamet","invokana","jardiance","qtern","synjardy","trijardy","xigduo")
  #trijardy is missing from medicare drugs because it is implemented in 2020

dfm$Gnrc_Name%>%table()
df1<-dfm[grepl(Brnd_Name,pattern=paste0(glp1,collapse="|"),ignore.case = T)]
df2<-dfm[grepl(Brnd_Name,pattern=paste0(sglt2,collapse="|"),ignore.case = T)]
df1$type<-"GLP-1"
df2$type<-"SGLT-2"
df<-rbind(df1,df2)
fwrite(df,here("trunk","derived","medicare data","relevant_medicare.txt"))

rm(list=ls())
