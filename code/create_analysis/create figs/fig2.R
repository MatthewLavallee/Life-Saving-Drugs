library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(openxlsx)
library(ggalluvial)
library(patchwork)

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

mk_alluv <- function(df,year1,year2) {
  dt=df[year%in%c(year1,year2),.(year,state,prg,quintile,drug)]
  dt=dt%>%pivot_wider(names_from=year,values_from=quintile)%>%as.data.table()
  #pivot to wide alluvia
  y1<-rollup(dt,.(first_year_tot=.N),as.character(year1))
  dt%<>%merge(y1[1:dim(y1)[1]-1],by=as.character(year1))
  #merge year 1 totals, filter out last sum row
  y2<-rollup(dt,.(last_year_tot=.N),as.character(year2))
  dt%<>%merge(y2[1:dim(y2)[1]-1],by=as.character(year2))
  #merge year 2 totals, filter out last sum row
  return(dt)
}

###prep 2.1####
#drop new jersey (which is missing for 2019) & DC which is missing in 2013
  #dropping them early keeps the quintiles more consistent
dt<-mk_alluv(df[!state%in%c("New Jersey","District of Columbia")],2013,2019)

dt[,.N,.(`2013`,`2019`,first_year_tot,last_year_tot)][,.(
  `2013`,
  `2019`,
  first_year_tot,
  last_year_tot,
  year1perc=round((N/first_year_tot)*100,2))]

###make 2.1####
  #drop new jersey (which is missing for 2019) & DC which is missing in 2013

p2<-ggplot(data=dt[drug=="GLP-1 & SGLT-2"],aes(axis1=as.character(`2013`),axis2=as.character(`2019`)))+
  stat_alluvium(aes(fill=as.character(`2013`)))+
  geom_stratum()+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size= 2.5)+
  facet_grid(~prg)+
  theme_clean()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())+
  labs(subtitle="Figure X: Quintile Rank Transition of Per Capita Days-Supplied 2013 to 2019")

p2
ggsave(p2,file=here("trunk","analysis","figures","Figure 2","fig2.1.png"),
       height=10,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 2","fig2.1.png"))

###make 2.2####
x=dt[drug=="GLP-1 & SGLT-2"&prg=="Medicaid"]
xx<-x[,.(tot_trans=.N),.(`2013`,`2019`)]
x%<>%merge(xx,by=c("2013","2019"))
ggplot(data=x,aes(axis1=as.character(`2013`),axis2=as.character(`2019`)))+
  stat_alluvium(aes(fill=as.character(`2013`)))+
  geom_stratum()+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size= 2.5)+
  geom_text(stat="flow",
            aes(label = after_stat(n)),
            size= 2.5)
  

p3<-ggplot(data=dt[drug=="GLP-1 & SGLT-2"&prg=="Medicaid"],aes(axis1=as.character(`2013`),axis2=as.character(`2019`)))+
  stat_alluvium(aes(fill=as.character(`2013`)))+
  geom_stratum()+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size= 2.5)+
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())+
  labs(subtitle="Medicaid, 2013 to 2019")

p4<-ggplot(data=dt[drug=="GLP-1 & SGLT-2"&prg=="Medicare"],aes(axis1=as.character(`2013`),axis2=as.character(`2019`)))+
  stat_alluvium(aes(fill=as.character(`2013`)))+
  geom_stratum()+
  scale_fill_brewer(type = "qual", palette = "Set1")+
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size= 2.5)+
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.line.x=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())+
  labs(subtitle="Medicare, 2013 to 2019")

p5<-
  (p3+p4)+
  plot_annotation(tag_prefix = 'Fig X.',
                  tag_levels = "A",
                  tag_suffix = ':',
                  title="Figure X: Quintile Rank Transition")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0))
p5
ggsave(p5,file=here("trunk","analysis","figures","Figure 2","fig2.2.png"),
       height=10,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 2","fig2.2.png"))

rm(list=ls())