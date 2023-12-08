library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

#ACS hasn't released 2020 estimates yet, but it looks like they'll be a mess when they are released
# https://www.census.gov/library/working-papers/2021/acs/2021_CensusBureau_01.html
# looks like IPUMS will release 2020 ACS in January https://forum.ipums.org/t/acs-2020-release-schedule/4325/5

#Micro data is meant to be representative at a state-level but raw counts aren't revised. Will be necessary to merge with PEP data 

# HINSCAID indicates whether, at the time of interview, persons were covered by Medicaid, Medical Assistance, or any other kind of government-assistance plan for those with low incomes or a disability.
# HINSCARE indicates whether, at the time of interview, persons were covered by Medicare.
acs_ddi <- read_ipums_ddi(here("trunk","raw","IPUMS","usa_00007.xml"))
acs <- read_ipums_micro(acs_ddi)
acs%>%names()
acs%<>%as.data.table()
acs[,`:=`(medicaid=case_when(HINSCAID==2&HINSCARE!=2~1,TRUE~0),
          medicare=case_when(HINSCARE==2&HINSCAID!=2~1,TRUE~0))]
acs_agg<-acs[,.(medicaid=sum((PERWT*medicaid)),
                medicare=sum((PERWT*medicare)),
                denom=sum(PERWT),
                perc_medicaid=sum((PERWT*medicaid))/sum(PERWT),
                perc_medicare=sum((PERWT*medicare))/sum(PERWT)),.(STATEFIP,YEAR)]

acs_agg[,`:=`(Region=case_when(
  STATEFIP==01~"Alabama",
  STATEFIP==02~"Alaska",
  STATEFIP==04~"Arizona",
  STATEFIP==05~"Arkansas",
  STATEFIP==06~"California",
  STATEFIP==08~"Colorado",
  STATEFIP==09~"Connecticut",
  STATEFIP==10~"Delaware",
  STATEFIP==11~"District of Columbia",
  STATEFIP==12~"Florida",
  STATEFIP==13~"Georgia",
  STATEFIP==15~"Hawaii",
  STATEFIP==16~"Idaho",
  STATEFIP==17~"Illinois",
  STATEFIP==18~"Indiana",
  STATEFIP==19~"Iowa",
  STATEFIP==20~"Kansas",
  STATEFIP==21~"Kentucky",
  STATEFIP==22~"Louisiana",
  STATEFIP==23~"Maine",
  STATEFIP==24~"Maryland",
  STATEFIP==25~"Massachusetts",
  STATEFIP==26~"Michigan",
  STATEFIP==27~"Minnesota",
  STATEFIP==28~"Mississippi",
  STATEFIP==29~"Missouri",
  STATEFIP==30~"Montana",
  STATEFIP==31~"Nebraska",
  STATEFIP==32~"Nevada",
  STATEFIP==33~"New Hampshire",
  STATEFIP==34~"New Jersey",
  STATEFIP==35~"New Mexico",
  STATEFIP==36~"New York",
  STATEFIP==37~"North Carolina",
  STATEFIP==38~"North Dakota",
  STATEFIP==39~"Ohio",
  STATEFIP==40~"Oklahoma",
  STATEFIP==41~"Oregon",
  STATEFIP==42~"Pennsylvania",
  STATEFIP==44~"Rhode Island",
  STATEFIP==45~"South Carolina",
  STATEFIP==46~"South Dakota",
  STATEFIP==47~"Tennessee",
  STATEFIP==48~"Texas",
  STATEFIP==49~"Utah",
  STATEFIP==50~"Vermont",
  STATEFIP==51~"Virginia",
  STATEFIP==53~"Washington",
  STATEFIP==54~"West Virginia",
  STATEFIP==55~"Wisconsin",
  STATEFIP==56~"Wyoming"))]
#relabel states for merging with PEP
#Medicaid/Medicare data isn't available pre-2008
fwrite(acs_agg,here("trunk","derived","acs pep","acs_aggregated.txt"))

rm(list=ls())
