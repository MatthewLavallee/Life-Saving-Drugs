library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(usmap)
library(sf)

df<-fread(here("trunk","derived","final datasets","annual_df.txt"))
df[,rate:=(days_supplied)/med_estimate]
df$drug_class<-factor(df$drug_class,levels = c("GLP-1","SGLT-2","GLP-1 & SGLT-2"))
names(df)[1]<-"state"
cols<-c("1"="azure4","2"="azure3","3"="dodgerblue1","4"="dodgerblue3","5"="dodgerblue4","NA"="white")

df[,ut_type:=paste0(Insurer,is30)]
df[,ut_type:=case_when(ut_type=="Medicaid30/60/90"~"Medicaid (30-Day Fills)",
                       ut_type=="Medicare1"~"Medicare (30-Day Fills)",
                       ut_type=="Medicaidall"~"Medicaid (All)")]
df[,`:=`(sd=sd(rate,na.rm=T),max=max(rate,na.rm=T)),.(Year,ut_type,drug_class)]

p1<-ggplot(df)+
  geom_boxplot(aes(y=rate,x=as.character(Year)))+
  geom_text(aes(y=max+4,x=as.character(Year),label=round(sd,2)),angle=45,size=3)+
  facet_grid(ut_type~drug_class,
             scale="free_y")+
  theme_clean()+
  theme(axis.text.x = element_text(angle=-70))+
  labs(
  x="Year",
  y="Days Supplied per Enrollee",
  title="Days Supplied by Program, Drug Class, and Fill Type",
  caption="Numbers are standard deviations for program, drug class, and fill type.")
p1
ggsave(p1,file=here("trunk","analysis","figures","boxplots","drug_class_insurance_rates.png"))
knitr::plot_crop(here("trunk","analysis","figures","boxplots","drug_class_insurance_rates.png"))

p2<-ggplot(df)+
  geom_boxplot(aes(y=rate,x=as.character(Year)))+
  geom_text(aes(y=max+4,x=as.character(Year),label=round(sd,2)),angle=45,size=3)+
  facet_grid(drug_class~ut_type,
             scale="free_y")+
  theme_clean()+
  theme(axis.text.x = element_text(angle=-70))+
  labs(
    x="Year",
    y="Days Supplied per Enrollee",
    title="Days Supplied by Program, Drug Class, and Fill Type",
    caption="Numbers are standard deviations for program, drug class, and fill type.")
p2
ggsave(p2,file=here("trunk","analysis","figures","boxplots","insurance_drug_class_rates.png"))
knitr::plot_crop(here("trunk","analysis","figures","boxplots","insurance_drug_class_rates.png"))


px<-ggplot(df[,.(sd,Year,ut_type,drug_class)]%>%unique())+
  geom_point(aes(y=sd,x=as.character(Year),col=ut_type))+
  facet_grid(~drug_class)+
  theme_clean()+
  theme(axis.text.x = element_text(angle=-70))+
  scale_color_manual(values=c("azure4","dodgerblue1","dodgerblue4"))+
  labs(
    x="Year",
    y="Standard Deviation in Days Supplied Rates",
    title="Variance in Days Supplied by Program, Drug Class, and Fill Type",
    col="Program (Fill Type)")
px
ggsave(px,file=here("trunk","analysis","figures","boxplots","sd_plot.png"))
knitr::plot_crop(here("trunk","analysis","figures","boxplots","sd_plot.png"))


p3<-ggplot(df[drug_class!="GLP-1 & SGLT-2"])+geom_boxplot(aes(y=rate,x=as.character(Year),col=drug_class))+
  theme_clean()+
  facet_grid(~ut_type)+
  scale_color_manual(values=c("dodgerblue4","azure4"))+
  theme(axis.text.x = element_text(angle=-90))+
  labs(
    x="Year",
    y="Days Supplied per Enrollee",
    title="Days Supplied by Program, Drug Class, and Fill Type",
    col="Drug Class")
p3
ggsave(p3,file=here("trunk","analysis","figures","boxplots","by_30day_rates.png"))
knitr::plot_crop(here("trunk","analysis","figures","boxplots","by_30day_rates.png"))
