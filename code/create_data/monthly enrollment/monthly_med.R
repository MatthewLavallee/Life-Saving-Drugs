library(tidyverse)
library(pdftools)
library(here)
library(magrittr)
library(data.table)

  #no directory necessary since this is being read to a "raw" file

df <- pdf_text(here("trunk","raw","Monthly Medicaid Data","reports","june_2004.pdf"))
tb<-str_split(df,"\n",simplify=T)
tb<-tb[10,5:57]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|y1997|y1998|y1999|y2000|y2001|y2002|y2003|y2004"
text_con <- textConnection(table)
df97_04 <- read.csv(text_con, sep = "|")

df <- pdf_text(here("trunk","raw","Monthly Medicaid Data","reports","june_2006_2013.pdf"))
tb<-str_split(df,"\n",simplify=T)
tb<-tb[16,2:54]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|y2006|y2007|y2008|y2009|y2010|y2011|y2012|y2013"
  #fixing headers
text_con <- textConnection(table)
df06_13 <- read.csv(text_con, sep = "|")
df<-merge(df97_04,df06_13,by="State")
df%<>%as.data.table()
cols<-names(df)
r<-function(x){gsub(",","",x)}
df[,(cols):=lapply(.SD,r),.SDcols=cols]
cols[-1]
df[,(cols[-1]):=lapply(.SD,as.numeric),.SDcols=cols[-1]]
df[,y2005:=y2004+((y2006-y2004)/2)]

y2020<-fread("https://data.medicaid.gov/api/1/datastore/query/6165f45b-ca93-5bb5-9d06-db29c692a360/0/download?conditions%5B0%5D%5Bresource%5D=t&conditions%5B0%5D%5Bproperty%5D=preliminary_updated&conditions%5B0%5D%5Bvalue%5D=U&conditions%5B0%5D%5Boperator%5D=%3D&conditions%5B1%5D%5Bresource%5D=t&conditions%5B1%5D%5Bproperty%5D=report_date&conditions%5B1%5D%5Bvalue%5D=06%2F01%2F2020&conditions%5B1%5D%5Boperator%5D=%3D&format=csv")

y2019<-fread("https://data.medicaid.gov/api/1/datastore/query/6165f45b-ca93-5bb5-9d06-db29c692a360/0/download?conditions%5B0%5D%5Bresource%5D=t&conditions%5B0%5D%5Bproperty%5D=preliminary_updated&conditions%5B0%5D%5Bvalue%5D=U&conditions%5B0%5D%5Boperator%5D=%3D&conditions%5B1%5D%5Bresource%5D=t&conditions%5B1%5D%5Bproperty%5D=report_date&conditions%5B1%5D%5Bvalue%5D=06%2F01%2F2019&conditions%5B1%5D%5Boperator%5D=%3D&format=csv")

y2018<-fread("https://data.medicaid.gov/api/1/datastore/query/6165f45b-ca93-5bb5-9d06-db29c692a360/0/download?conditions%5B0%5D%5Bresource%5D=t&conditions%5B0%5D%5Bproperty%5D=preliminary_updated&conditions%5B0%5D%5Bvalue%5D=P&conditions%5B0%5D%5Boperator%5D=%3D&conditions%5B1%5D%5Bresource%5D=t&conditions%5B1%5D%5Bproperty%5D=report_date&conditions%5B1%5D%5Bvalue%5D=06%2F01%2F2018&conditions%5B1%5D%5Boperator%5D=%3D&format=csv")

y2017<-fread("https://data.medicaid.gov/api/1/datastore/query/6165f45b-ca93-5bb5-9d06-db29c692a360/0/download?conditions%5B0%5D%5Bresource%5D=t&conditions%5B0%5D%5Bproperty%5D=preliminary_updated&conditions%5B0%5D%5Bvalue%5D=P&conditions%5B0%5D%5Boperator%5D=%3D&conditions%5B1%5D%5Bresource%5D=t&conditions%5B1%5D%5Bproperty%5D=report_date&conditions%5B1%5D%5Bvalue%5D=06%2F01%2F2017&conditions%5B1%5D%5Boperator%5D=%3D&format=csv")
df%<>%pivot_longer(cols=c(-State),names_to="year",values_to="enroll")
df%<>%as.data.table()
df[,`:=`(year=gsub("y","",year),enroll=enroll*1000)]

df17_20<-rbind(y2020,y2019,y2018,y2017)
df17_20<-df17_20[,.(State=state_name,enroll=total_medicaid_enrollment,year=year(report_date))]
df%<>%rbind(df17_20)
dfa<-df

df <- pdf_text("https://www.medicaid.gov/medicaid/downloads/updated-june-2016-enrollment-data.pdf")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[1,12:44]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df16 <- read.csv(text_con, sep = "|")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[3,12:31]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df16%<>%rbind(read.csv(text_con,sep="|"))
df16$year<-"2016"

df <- pdf_text("https://www.medicaid.gov/medicaid/downloads/updated-june-2015-enrollment-data.pdf")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[1,10:40]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df15 <- read.csv(text_con, sep = "|")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[3,10:31]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df15%<>%rbind(read.csv(text_con,sep="|"))
df15$year<-"2015"

df <- pdf_text("https://www.medicaid.gov/medicaid/downloads/updated-june-2014-enrollment-report.pdf")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[1,13:41]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df14 <- read.csv(text_con, sep = "|")
tb<-str_split(df,"\n",simplify=T)
tb<-tb[3,13:36]
table <- str_replace_all(tb, "\\s{2,}", "|")
table[1]<-"State|Marketplace|May2016_med|june2016|p_ch|avg_month|net_ch|p_ch_year"
text_con <- textConnection(table)
df14%<>%rbind(read.csv(text_con,sep="|"))
df14$year<-"2014"
df14

df14_16<-rbind(df14,df15,df16)
df14_16%<>%as.data.table()
dfa
df14_16<-df14_16[,.(State,year,enroll=june2016)]
df14_16[,`:=`(enroll=gsub(",","",enroll))]
df14_16[,`:=`(State=gsub("*","",gsub("^","",State,fixed=T),fixed=T))]

df<-rbind(dfa,df14_16)

df[State=="North Dakota"&year=="2014"]$enroll<-"79067"
  #adding july estimate of medicaid & chip enrollment since it's missing in June

fwrite(df,here("trunk","raw","Monthly Medicaid Data","data","medicaid_june.txt"))

rm(list=ls())
