library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)

###set up####
dir.create(here("trunk","analysis","figures","Table 1"),showWarnings = F)
df<-fread(here("trunk","derived","final datasets","annual_df.txt"))
df[,rate:=(d_sup)/pop]
df$drug<-factor(df$drug,levels = c("GLP-1","SGLT-2","GLP-1 & SGLT-2"))

df[is.na(rate)]

df<-df[is30!="all"]
#filter out Medicaid "Alls"

df[,`:=`(rank=rank(-rate),
         quintile=ntile(-rate,5)),.(year,prg,drug)]
#create rank, 1 is high rates, 50 is low rates
#creates quintile, 1 is high, 5 is low to keep consistent

###tab 1.1####
dft<-
  df[quintile%in%c(1,5),
     .(rate=mean(rate,na.rm=T)*1000),
     .(year,prg,drug,quintile)]%>%
  pivot_wider(names_from = quintile,values_from=rate)%>%
  pivot_wider(names_from = prg,values_from=c(`1`,`5`))

dft%<>%
  as.data.table()

dft<-
  dft[,
      .(year,
        drug,
        `1_Medicaid`,
        `5_Medicaid`,
        `1_Medicare`,
        `5_Medicare`)][order(year)]
dft<-
  dft[drug=="GLP-1 & SGLT-2",
      .(Year=year,
        `Top Quintile1`=round(`1_Medicaid`,2),
        `Bottom Quintile1`=round(`5_Medicaid`,2),
        `Top Quintile`=round(`1_Medicare`,2),
        `Bottom Quintile`=round(`5_Medicare`,2))]
fwrite(dft,here("trunk","analysis","figures","Table 1","tab1.1.csv"))

wb<-createWorkbook()
addWorksheet(wb,"tab")
writeData(wb,"tab",x="Table 1: Days Supplied Per 1000 Eligible Enrollees (Both GLP-1 & SGLT-2)",startRow = 1,startCol = 1)
hs<-createStyle(valign = "center",
                halign = "center",
                fgFill="#5081bd",
                fontColour = "#f3f6f4")
writeData(wb,"tab",x="Medicaid",startRow = 2,startCol = 2)
writeData(wb,"tab",x="Medicare",startRow = 2,startCol = 4)
addStyle(wb,"tab",style=hs,rows=1,cols=1)
addStyle(wb,"tab",style=hs,rows=2,cols=2)
addStyle(wb,"tab",style=hs,rows=2,cols=4)

writeDataTable(wb,
               sheet="tab",
               dft,
               colNames = T,
               firstColumn = T,
               tableStyle = "TableStyleMedium16",
               startCol = 1,
               startRow = 3)

setColWidths(wb,"tab",cols=1:5,widths=20)
mergeCells(wb,"tab",cols=1:5,rows=1)
mergeCells(wb,"tab",cols=2:3,rows=2)
mergeCells(wb,"tab",cols=4:5,rows=2)

saveWorkbook(wb,here("trunk","analysis","figures","Table 1","tab1.1.xlsx"),overwrite = T)


###tab 1.2####
dft=df[drug=="GLP-1 & SGLT-2",.(r_m=mean(rate,na.rm = T)*1000,r_sd=sd(rate,na.rm = T)*1000),.(year,prg)]
dft=dft[,.(year,
           prg,
           r_m,
           r_sd,
           t=paste0(round(r_m,2)," (",round(r_sd,2),")"))][order(year,prg)]
dft=dft[,.(Year=year,prg,t)]%>%pivot_wider(names_from = prg,values_from=t)
fwrite(dft,here("trunk","analysis","figures","Table 1","tab1.2.csv"))

wb<-createWorkbook()
addWorksheet(wb,"tab")
writeData(wb,"tab",x="Table 1: Days Supplied Per 1000 Eligible Enrollees (GLP-1 & SGLT-2)",startRow = 1,startCol = 1)
hs<-createStyle(valign = "center",
                halign = "center",
                fgFill="#5081bd",
                fontColour = "#f3f6f4")
addStyle(wb,"tab",style=hs,rows=1,cols=1)


writeDataTable(wb,
               sheet="tab",
               dft,
               colNames = T,
               firstColumn = T,
               tableStyle = "TableStyleMedium16",
               startCol = 1,
               startRow = 2)
setColWidths(wb,"tab",cols=1,widths=7.5)
setColWidths(wb,"tab",cols=2:3,widths=17.5)
mergeCells(wb,"tab",cols=1:3,rows=1)
saveWorkbook(wb,here("trunk","analysis","figures","Table 1","tab1.2.xlsx"),overwrite = T)


###tab 1.3####
dft=df[drug=="GLP-1 & SGLT-2",.(r_m=mean(rate,na.rm = T)*1000,r_sd=sd(rate,na.rm = T)*1000),.(quintile,year)]
dft=dft[,.(year,
           r_m,
           r_sd,
           quintile,
           t=paste0(round(r_m,2),"\n(",round(r_sd,2),")"))][order(year,quintile)]
dft=dft[,.(Year=year,t,quintile)]%>%pivot_wider(names_from = quintile,values_from=t)
names(dft)<-c("Year","Top Quintile","Second Quintile","Third Quintile","Fourth Quintile","Bottom Quintile")
dft%<>%filter(Year%in%2013:2019)
fwrite(dft,here("trunk","analysis","figures","Table 1","tab1.3.csv"))

wb<-createWorkbook()
addWorksheet(wb,"tab")
writeData(wb,"tab",x="Table 1: Days Supplied Per 1000 Eligible Enrollees (GLP-1 & SGLT-2)",startRow = 1,startCol = 1)
hs<-createStyle(valign = "center",
                halign = "center",
                fgFill="#5081bd",
                fontColour = "#f3f6f4")
addStyle(wb,"tab",style=hs,rows=1,cols=1)


writeDataTable(wb,
               sheet="tab",
               dft,
               colNames = T,
               firstColumn = T,
               tableStyle = "TableStyleMedium16",
               startCol = 1,
               startRow = 2)
setColWidths(wb,"tab",cols=1,widths=7.5)
setColWidths(wb,"tab",cols=2:6,widths=17.5)
mergeCells(wb,"tab",cols=1:6,rows=1)
saveWorkbook(wb,here("trunk","analysis","figures","Table 1","tab1.3.xlsx"),overwrite = T)


###tab 1.4####
dft=df[!is.na(quintile)]
dft=dft[drug=="GLP-1 & SGLT-2"&year%in%2013:2019,.(
  r_m=mean(rate,na.rm = T)*1000,
  r_sd=sd(rate,na.rm = T)*1000,
  p_m=mean(pop),
  p_sd=sd(pop),
  d_m=mean(med_perc)*100,
  d_sd=sd(med_perc)*100
  ),.(quintile,prg)]
dft=dft[,.(prg,
           quintile,
           t=paste0(round(r_m,2),"\n(",round(r_sd,2),")"),
           p=paste0(round(p_m,2),"\n(",round(p_sd,2),")"),
           d=paste0(round(d_m,2),"\n(",round(d_sd,2),")"))][order(prg,quintile)]
dft%<>%pivot_longer(cols=c(t,p,d),names_to = "var",values_to = "val")%>%pivot_wider(names_from = quintile,values_from = val)%>%as.data.table()
names(dft)<-c("program","variable","Top Quintile","Second Quintile","Third Quintile","Fourth Quintile","Bottom Quintile")
dft[,variable:=case_when(variable=="t"~"Rate of Days Supplied\nper 1000 Enrollees",
                         variable=="p"~"Population of Program",
                         variable=="d"~"Percent Diabetic\nin Program")]

fwrite(dft,here("trunk","analysis","figures","Table 1","tab1.4.csv"))
###tab 1.5####
dft=df[!is.na(quintile)]

dfa<-fread(here("trunk","raw","AHRF","ahrf.txt"))

dfa1<-dfa[,.(state=F00008,
             fips=F00011,
             c_health_ctr_n=as.numeric(`F13221-13`),
              #number of community mental health centers for 2013
             c_health_ctr_fed_n=as.numeric(`F13320-13`),
              #number of federally qualified health centers for 2013
             medicare_sp_13=as.numeric(`F15297-13`)*.01,
              #per capita medicare spending for 2013
             fed_MD_r=as.numeric(`F11269-14`),
              #total fed MDs, hosp residents
             fed_MD_s=as.numeric(`F11270-14`),
              #total fed MDs, FT staff
             fed_MD_o=as.numeric(`F11271-14`)
              #total fed MDs, other
             )]
dfa2=dfa1[,.(
  c_health_ctr_n=sum(c_health_ctr_n,na.rm=T),
  c_health_ctr_fed_n=sum(c_health_ctr_fed_n,na.rm=T),
  medicare_sp_13=mean(medicare_sp_13,na.rm=T),
  fed_MD_r=sum(fed_MD_r,na.rm=T),
  fed_MD_s=sum(fed_MD_s,na.rm=T),
  fed_MD_o=sum(fed_MD_o,na.rm=T),
  fed_MD_t=sum(fed_MD_o+fed_MD_s+fed_MD_r,na.rm=T)
),.(state)]

dft%<>%merge(dfa2,by="state")
  #we lose DC in this merge

dfe<-fread(here("trunk","raw","State Health Expenditure Data Files","MEDICAID_AGGREGATE14.CSV"))
dfe<-dfe[Item=="Medicaid/Personal Health Care (Millions of Dollars)",.(state=State_Name,Medicaid_spend=(Y2013+Y2012+Y2011+Y2010+Y2009)/5)]
dft%<>%merge(dfe,by="state")

dfe<-fread(here("trunk","raw","State Health Expenditure Data Files","MEDICAID_PER_ENROLLEE14.CSV"))
dfe<-dfe[Item=="Medicaid/Personal Health Care ($)",.(state=State_Name,Medicaid_spend_percap=(Y2013+Y2012+Y2011+Y2010+Y2009)/5)]
dft%<>%merge(dfe,by="state")

dfe<-fread(here("trunk","raw","State Health Expenditure Data Files","MEDICARE_PER_ENROLLEE14.CSV"))
dfe<-dfe[Item=="Medicare/Personal Health Care ($)",.(state=State_Name,Medicare_spend_percap=(Y2013+Y2012+Y2011+Y2010+Y2009)/5)]
dft%<>%merge(dfe,by="state")

dfe<-fread(here("trunk","raw","State Health Expenditure Data Files","MEDICARE_AGGREGATE14.CSV"))
dfe<-dfe[Item=="Medicare/Personal Health Care (Millions of Dollars)",.(state=State_Name,Medicare_spend=(Y2013+Y2012+Y2011+Y2010+Y2009)/5)]
dft%<>%merge(dfe,by="state")

msd <- function(x) {
  return(paste0(round(mean(x),2),"\n(",round(sd(x),2),")"))
}

dft1=dft[drug=="GLP-1 & SGLT-2"&year%in%2013:2019,
        .(`Medicaid Spending\n(Total, Millions of $)`=msd(Medicaid_spend),
          `Medicare Spending\n(Total, Millions of $)`=msd(Medicare_spend),
          `Medicaid Spending\n(Per Capita, $)`=msd(Medicaid_spend_percap),
          `Medicare Spending\n(Per Capita, $)`=msd(Medicare_spend_percap),
          Enrollment=msd(pop),
          `Total Federally Qualified\nCommunity Health Clinics`=msd(c_health_ctr_fed_n),
          `Total Federal Medical Doctors`=msd(fed_MD_t)),
        .(quintile,prg)]%>%
  pivot_longer(!c(prg,quintile),values_to = "val",names_to = "var")%>%
  pivot_wider(names_from = quintile,values_from = val)%>%as.data.table()
dft1<-dft1[,.(prg,var,`1`,`2`,`3`,`4`,`5`)]

fwrite(dft1,here("trunk","analysis","figures","Table 1","tab1.5.csv"))
