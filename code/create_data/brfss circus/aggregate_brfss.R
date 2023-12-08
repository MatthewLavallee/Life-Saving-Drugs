library(data.table)
library(tidyverse)
library(magrittr)
library(here)
library(haven)

dir.create(here("trunk","derived","brfss_denoms"), showWarnings = FALSE)
  #should already be made if running in order

`%!in%`<-negate(`%in%`)
dfc<-c()
for (xyear in c(2004:2020)) {
  df<-fread(here("trunk","derived","brfss_denoms",paste0("brfss_",xyear,".txt")))
  exp<-fread(here("trunk","derived","brfss_denoms","medicaid_year_expansion.csv"))
  
  #CHILDREN - How many children less than 18 years of age live in your household?
  # 1:87 - Number of children. Notes: _ _ = Number of children.
  # 88 - None.
  # 99 - Refused.
  # Blank - Not asked or Missing.
  #NUMADULT - Number of adults in the household.
  # 1 - Number of adults in the household.
  # 2 - Number of adults in the household.
  # 3 - Number of adults in the household.
  # 4 - Number of adults in the household.
  # 5 - Number of adults in the household.
  # 6:99 - 6 or more
  # Blank - Missing
  
  df[is.na(NUMADULT)]$NUMADULT<-1
  df[is.na(CHILDREN)]$CHILDREN<-0
  #replace num children that are missing with zero, as they're missing.
  #replace num adults that are missing with 1, as an adult likely needed to answer the survey.
  df$CHILDREN%<>%as.double()
  df$NUMADULT%<>%as.double()
  df[,`:=`(
    hh_size=case_when(
      CHILDREN%in%1:87&NUMADULT%in%0:5~CHILDREN+NUMADULT,
      CHILDREN%in%1:87&NUMADULT%in%6:99~CHILDREN+6,
      CHILDREN%in%c(0,88,99)&NUMADULT%in%0:5~0+NUMADULT,
      CHILDREN%in%c(0,88,99)&NUMADULT%in%6:99~6))]
  
  fpl<-fread(here("trunk","raw","FPL","fpl.csv"))
  names(fpl)[c(4,6)]<-c("fpl","_STATE")
  fpl%<>%filter(Year==xyear)
  
  df%<>%merge(fpl,by=c("_STATE","hh_size"))
  #create poverty thresholds by hh size
  #Alaska and Hawaii have different poverty rates
  #https://aspe.hhs.gov/2014-poverty-guidelines
  
  df[,`:=`(inc_max=case_when(
    INCOME2==1~9999,
    INCOME2==2~14999,
    INCOME2==3~19999,
    INCOME2==4~24999,
    INCOME2==5~34999,
    INCOME2==6~49999,
    INCOME2==7~74999,
    INCOME2==8~75000,
    INCOME2%in%c(77,99)~9999999
  ))]
  #translate income measure to max amounts
  
  df%<>%merge(exp,by="_STATE")
  #merge expansion status dataframe, we drop Guam and Puerto Rico as they're not in BRFSS.
  
  if(xyear%in%c(2004:2010)){
    df[,`:=`(fpl_mult=case_when(year_implemented<xyear~1.38,TRUE~1),
             diab_real=case_when(DIABETE2==1~1,TRUE~0),
             is_30p=case_when(`_AGEG5YR`%!in%c(1,2,14)~1,TRUE~0),
             is_65=case_when(`_AGE65YR`==2~1,TRUE~0))]
  }else 
    if(xyear%in%c(2011:2018)){
      df[,`:=`(fpl_mult=case_when(year_implemented<xyear~1.38,TRUE~1),
               diab_real=case_when(DIABETE3==1~1,TRUE~0),
               is_30p=case_when(`_AGEG5YR`%!in%c(1,2,14)~1,TRUE~0),
               is_65=case_when(`_AGE65YR`==2~1,TRUE~0))]
    }else{
      df[,`:=`(fpl_mult=case_when(year_implemented<xyear~1.38,TRUE~1),
               diab_real=case_when(DIABETE4==1~1,TRUE~0),
               is_30p=case_when(`_AGEG5YR`%!in%c(1,2,14)~1,TRUE~0),
               is_65=case_when(`_AGE65YR`==2~1,TRUE~0))]
    }
  
  df[,`:=`(below_fpl=case_when(inc_max<fpl*fpl_mult~1,TRUE~0))]
  
  if(xyear%in%c(2004:2010)){
    df[,`:=`(is_medicaid_est_weighted=case_when(is_65==0~below_fpl*`_FINALWT`,TRUE~0),
             is_medicare_est_weighted=case_when(below_fpl==0~is_65*`_FINALWT`,TRUE~0))]
  }else{
    df[,`:=`(is_medicaid_est_weighted=case_when(is_65==0~below_fpl*`_LLCPWT`,TRUE~0),
             is_medicare_est_weighted=case_when(below_fpl==0~is_65*`_LLCPWT`,TRUE~0))]
  }
  
  df_agg<-df[,.(medicaid_perc=sum((is_medicaid_est_weighted*diab_real*is_30p))/sum(is_medicaid_est_weighted),
               medicare_perc=sum((is_medicare_est_weighted*diab_real*is_30p))/sum(is_medicare_est_weighted),
               Year=xyear),.(`_STATE`,State)]
  dfc%<>%rbind(df_agg)
}
fwrite(dfc,here("trunk","derived","brfss_denoms","brfss_denoms.txt"))

rm(list=ls())
