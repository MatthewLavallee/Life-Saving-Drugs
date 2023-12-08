library(data.table)
library(tidyverse)
library(magrittr)
library(ipumsr)
library(here)

dir.create(here("trunk","derived","ndcs"), showWarnings = FALSE)

dfpr<-fread(here("trunk","raw","FDA_ndc_database","product.txt"),colClasses = rep("character",20),quote="")
  #load in product file from FDA's NDC database https://www.fda.gov/drugs/drug-approvals-and-databases/national-drug-code-directory
  #there's an entry with quotes around it 87k lines in, so `quote=""` is specified.

dfpr<-dfpr[,.(
  prod_id=PRODUCTID,
  prod_ndc=PRODUCTNDC,
  prod_type_name=PRODUCTTYPENAME,
  prop_name=PROPRIETARYNAME,
  prop_suf=PROPRIETARYNAMESUFFIX,
  nonprop_name=NONPROPRIETARYNAME,
  dosage=DOSAGEFORMNAME,
  route_name=ROUTENAME,
  market_date_start=STARTMARKETINGDATE,
  market_date_end=ENDMARKETINGDATE,
  market_cat_name=MARKETINGCATEGORYNAME,
  app_num=APPLICATIONNUMBER,
  labeler_company=LABELERNAME,
  active_name=SUBSTANCENAME,
  active_strength=ACTIVE_NUMERATOR_STRENGTH,
  active_unit=ACTIVE_INGRED_UNIT,
  pharm_class=PHARM_CLASSES,
  DEAD_schedule_num=DEASCHEDULE,
  ndc_exclude=NDC_EXCLUDE_FLAG,
  record_expire=LISTING_RECORD_CERTIFIED_THROUGH
)]
  #change names of horrible FDA variables

glp1_class<-str_subset(dfpr$pharm_class,pattern="GLP-1")%>%unique()
sglt2_class<-str_subset(dfpr$pharm_class,pattern="Sodium-Glucose Transporter 2 Inhibitors")%>%unique()
glp1<-dfpr[pharm_class%in%glp1_class]
sglt2<-dfpr[pharm_class%in%sglt2_class]
#subset to GLP-1s/SGLT-2 by pharm_class

glp1$drug_type<-"GLP-1"
sglt2$drug_type<-"SGLT-2"
dfd<-rbind(glp1,sglt2)
  #bind GLP-1s and SGLT-2s into one dataframe for subsetting SDUD

fwrite(dfd,here("trunk","derived","ndcs","relevant_ndc.txt"))
  #export GLP-1 and SGLT-2 NDC codes.

rm(list=ls())
