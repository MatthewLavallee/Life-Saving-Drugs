library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dfpep<-readxl::read_xlsx(here("trunk","raw","Census","pep_2010to2019.xlsx"))
dfpep_2020<-readxl::read_xlsx(here("trunk","raw","Census","pep_2020.xlsx"))
dfpep1<-fread(here("trunk","raw","Census","pep_2000to2010.csv"))
dfpep1<-dfpep1[c(10:60),c(1,3:12)]
names(dfpep1)<-c("Region","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009")
dfpep1[,Region:=sub(x=Region,pattern="^.*\\.","")]

dfpep1[,`:=`(`2000`=as.numeric(gsub(pattern=",",replacement="",`2000`)),
             `2001`=as.numeric(gsub(pattern=",",replacement="",`2001`)),
             `2002`=as.numeric(gsub(pattern=",",replacement="",`2002`)),
             `2003`=as.numeric(gsub(pattern=",",replacement="",`2003`)),
             `2004`=as.numeric(gsub(pattern=",",replacement="",`2004`)),
             `2005`=as.numeric(gsub(pattern=",",replacement="",`2005`)),
             `2006`=as.numeric(gsub(pattern=",",replacement="",`2006`)),
             `2007`=as.numeric(gsub(pattern=",",replacement="",`2007`)),
             `2008`=as.numeric(gsub(pattern=",",replacement="",`2008`)),
             `2009`=as.numeric(gsub(pattern=",",replacement="",`2009`)))]


dfpep%<>%as.data.table()
dfpep%<>%janitor::row_to_names(row_number = 1)
dfpep<-dfpep[1:56]
dfpep%<>%as.data.table()

dfpep_2020%<>%as.data.table()
dfpep_2020%<>%janitor::row_to_names(row_number = 1)
dfpep_2020<-dfpep_2020[1:56]
dfpep_2020%<>%as.data.table()
dfpep%<>%merge(dfpep_2020,by=c("Region"))

dfpep[,Region:=sub(x=Region,pattern="^.*\\.","")]
#get rid of leading period for State names
dfpep%<>%merge(dfpep1,"Region")

dfpep_agg<-dfpep[,.(Region,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`)]%>%pivot_longer(cols=c(`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`),names_to="YEAR",values_to="population")

fwrite(dfpep_agg,here("trunk","derived","acs pep","pep_aggregated.txt"))

rm(list=ls())
