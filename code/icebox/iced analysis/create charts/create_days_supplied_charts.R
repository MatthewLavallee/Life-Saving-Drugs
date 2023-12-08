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

p1<-ggplot(df,aes(y=rate,x=as.integer(Year),group=state))+
  geom_line(col="grey")+
  facet_grid(drug_class~ut_type,scales="free_y")+
  theme_clean()+
  scale_x_discrete(limits=c(2005:2020))+
  theme(legend.position = "none",axis.text.x = element_text(angle=-90))+
  labs(
  x="Year",
  y="Days Supplied per Enrollee",
  title="Days Supplied by Program, Drug Class, and Fill Type")
p1
ggsave(p1,file=here("trunk","analysis","figures","line charts","days_supplied_by_drug_class.png"))
knitr::plot_crop(here("trunk","analysis","figures","line charts","days_supplied_by_drug_class.png"))


p2<-ggplot(df,aes(y=rate,x=as.integer(Year),group=state))+
  geom_line(col="grey")+
  facet_grid(ut_type~drug_class,scales="free_y")+
  theme_clean()+
  scale_x_discrete(limits=c(2005:2020))+
  theme(legend.position = "none",axis.text.x = element_text(angle=-90))+
  labs(
    x="Year",
    y="Days Supplied per Enrollee",
    title="Days Supplied by Program, Drug Class, and Fill Type")
p2
ggsave(p2,file=here("trunk","analysis","figures","line charts","days_supplied_by_insurer.png"))
knitr::plot_crop(here("trunk","analysis","figures","line charts","days_supplied_by_insurer.png"))

df[,rank:=rank(-rate),.(ut_type,drug_class,Year)]
dfa<-dcast(df[,.(ut_type,drug_class,Year,rank,state)],ut_type+state+drug_class~Year,value.var = "rank")

p3<-ggplot(dfa,aes(y=`2019`,x=`2013`,col=drug_class))+
  geom_point()+
  facet_grid(~ut_type)+
  theme_clean()+
  scale_color_manual(values=c("azure4","dodgerblue1","dodgerblue4"))+
  labs(
    x="2013 Rank",
    y="2019 Rank",
    title="Transition of Relative Rank by Program, Drug Class, and Fill Type")

p3
ggsave(p3,file=here("trunk","analysis","figures","line charts","rank_withinprogram.png"))
knitr::plot_crop(here("trunk","analysis","figures","line charts","rank_withinprogram.png"))

dfb<-dcast(df[ut_type%in%c("Medicaid (All)","Medicare (30-Day Fills)"),.(ut_type,drug_class,Year,rank,state)],state+drug_class+Year~ut_type,value.var = "rank")

p4<-ggplot(dfb[Year%in%2013:2019],aes(y=`Medicaid (All)`,x=`Medicare (30-Day Fills)`,col=drug_class))+
  geom_point()+
  facet_wrap(~Year)+
  theme_clean()+
  scale_color_manual(values=c("azure4","dodgerblue1","dodgerblue4"))+
  labs(
    x="Medicare Rank",
    y="Medicaid Rank",
    title="Medicaid Vs Medicare Ranks by Drug Class and Year",
    col="Drug Class")

p4
ggsave(p4,file=here("trunk","analysis","figures","line charts","rank_betweenprogram.png"))
knitr::plot_crop(here("trunk","analysis","figures","line charts","rank_betweenprogram.png"))


dfb[,rankdiff:=`Medicaid (All)`-`Medicare (30-Day Fills)`]

p5<-ggplot(dfb[Year%in%2013:2019]%>%unique(),aes(x=Year,y=rankdiff,col=drug_class))+
  geom_line(aes(group=state))+
  theme_clean()+
  theme(legend.position = "none")+
  facet_grid(~drug_class)+
  scale_color_manual(values=c("azure4","dodgerblue1","dodgerblue4"))+
  labs(
    x="Year",
    y="Difference in Rank",
    title="Difference in Medicaid (All) and Medicare (30-Day Fill) Ranks by Drug Class and Year",
    col="Drug Class")

p5
ggsave(p5,file=here("trunk","analysis","figures","line charts","diff_rank.png"))
knitr::plot_crop(here("trunk","analysis","figures","line charts","diff_rank.png"))
rm(list=ls())
