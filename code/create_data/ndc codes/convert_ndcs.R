library(data.table)
library(tidyverse)
library(magrittr)

dir.create(here("trunk","derived","ndcs"), showWarnings = FALSE)
  #should already be made if running in order 

dfd<-fread(here("trunk","derived","ndcs","relevant_ndc.txt"))
  # NDC codes are 10 digit for packaging (FDA NDC database) and 11 digit for billing (SDUD data). Converting from 10-digit to 11-digit is explained in ~/docs/NDC conversion to 11 digits.pdf, an example is provided below
  # 4-4-2 example: 9999-9999-99 -> 09999-9999-99 
  # 5-3-2 example: 99999-999-99 -> 99999-0999-99
  # 5-4-1 example: 99999-9999-9 -> 99999-9999-09
  
  # The FDA data doesn't contain the package size digits (final 2), so all coding is: 4-4, 5-3, or 5-4.

dfd[,`:=`(first_ndc=sub(x=prod_ndc,"-.*",""),
          second_ndc=sub(x=prod_ndc,".*-",""))]

dfd[,`:=`(nchar_first_ndc=nchar(first_ndc),
          nchar_second_ndc=nchar(second_ndc))]
  #break out first and second parts of FDA NDC codes & count characters in each set

dfd[,`:=`(fixed_ndc=case_when(
  nchar_first_ndc==5&nchar_second_ndc==4~paste0(first_ndc,second_ndc),
  nchar_first_ndc==5&nchar_second_ndc==3~paste0(first_ndc,0,second_ndc),
  nchar_first_ndc==4&nchar_second_ndc==4~paste0(0,first_ndc,second_ndc)
))]
  #convert ot 11 digits for matching with SDUD
fwrite(dfd,here("trunk","derived","ndcs","relevant_ndc_fixed.txt"))

rm(list=ls())
