library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)

###set up####
dir.create(here("trunk","analysis","figures","Figure 1"),showWarnings = F)
df<-fread(here("trunk","derived","final datasets","annual_df.txt"))
df[,rate:=(d_sup)/pop]
df$drug<-factor(df$drug,levels = c("GLP-1","SGLT-2","GLP-1 & SGLT-2"))

df[is.na(rate)]
#north dakota is missing it's 2014 medicaid numbers in the annual report, so it's na here
df<-df[is30!="all"]
  #filter out Medicaid "Alls"

df[,`:=`(rank=rank(-rate),
         quintile=ntile(-rate,5)),.(year,prg,drug)]
  #create rank, 1 is high rates, 50 is low rates
  #creates quintile, 1 is high, 5 is low to keep consistent

###fig 1####
p1<-df[quintile%in%c(1,5),.(rate=mean(rate)),.(year,prg,drug,quintile)]%>%
  ggplot()+
  geom_line(aes(x=year,y=rate,col=as.character(quintile)))+
  geom_jitter(data=df[quintile%in%c(1,5)],aes(x=year,y=rate),col="lightgrey",alpha=.3)+
  facet_grid(drug~prg,scales="free")+
  labs(x="Year",
       y="Per Enrollee Days Supplied",
       subtitle="Days Supplied by the Top Quintile vs the Bottom Quintile")+
  theme_clean()+
  theme(legend.position = "none")+
  scale_color_manual(values=c("firebrick","dodgerblue4"))
p1
ggsave(p1,file=here("trunk","analysis","figures","Figure 1","fig1.1.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 1","fig1.1.png"))

###fig 1.2####
p2<-df[,.(rate=mean(rate,na.rm=T)),.(year,prg,drug)]%>%
  ggplot()+
  geom_line(aes(x=year,y=rate,col=prg))+
  geom_jitter(data=df,aes(x=year,y=rate),col="lightgrey",alpha=.15)+
  facet_grid(drug~.,scales="free")+
  labs(x="Year",
       y="Per Enrollee Days Supplied",
       subtitle="Mean Days Supplied by Program")+
  theme_clean()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values=c("firebrick","dodgerblue4"))
p2
ggsave(p2,file=here("trunk","analysis","figures","Figure 1","fig1.2.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 1","fig1.2.png"))

###fig 1.3####
p3<-df[,.(rate=var(rate,na.rm=T)),.(year,prg,drug)]%>%
  ggplot()+
  geom_line(aes(x=year,y=rate,col=prg))+
  geom_jitter(data = df,aes(x=year,y=rate),col="lightgrey",alpha=.25,size=1)+
  facet_grid(drug~.,scales="free")+
  labs(x="Year",
       y="Days Supplied Per Enrollee",
       subtitle="Figure 1: Variance in Days Supplied by Program & Drug Class (2005-2020)")+
  theme_clean()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values=c("firebrick","dodgerblue4"))
p3
ggsave(p3,file=here("trunk","analysis","figures","Figure 1","fig1.3.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 1","fig1.3.png"))

rm(list=ls())


