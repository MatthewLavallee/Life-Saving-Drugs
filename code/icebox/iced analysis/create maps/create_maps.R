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


for (I in c("Medicaid (30-Day Fills)","Medicare (30-Day Fills)","Medicaid (All)")) {
  for (d in c("GLP-1","SGLT-2","GLP-1 & SGLT-2")) {
    p1<-plot_usmap(data = df[ut_type==I&drug_class==d], values = "rate", color = "black")+
      facet_wrap(~Year)+
      theme_void()+
      theme(legend.position = "bottom")+
      guides(fill=guide_colorbar(title.position="top"))+
      scale_fill_continuous(high="dodgerblue4",low="white", name = "Days Supplied Per Enrollee")+
      labs(title=paste0(d," Days Supplied Per Enrollee in ",I))
    ggsave(p1,file=here("trunk","analysis","figures","maps","rate maps","raw rate",paste0(I,"_",d,"_rate_map.png")))
    knitr::plot_crop(here("trunk","analysis","figures","maps","rate maps","raw rate",paste0(I,"_",d,"_rate_map.png")))
  }
}
#days supplied per enrollee maps

for (d in c("GLP-1","SGLT-2","GLP-1 & SGLT-2")) {
  p2<-plot_usmap(data = df[order(Year),.SD[1],.(state,drug_class,ut_type)][drug_class==d][,.(Year=Year%>%as.character(),state,ut_type)], values = "Year", color = "black")+
    facet_wrap(~ut_type)+
    theme_void()+
    theme(legend.position = "bottom")+
    guides(fill=guide_legend(title.position="top",nrow=2, byrow=TRUE,override.aes = list()))+
    scale_fill_manual(values=c("azure4","dodgerblue1","dodgerblue3","dodgerblue4","azure3"), name = "Quintile of Days Supplied Rate")+
    labs(title=paste0("Initial Year of ",d," Utilization by State & Program"))
  p2
  ggsave(p2,file=here("trunk","analysis","figures","maps","initial utilization maps",paste0("initial_",d,"map.png")))
  knitr::plot_crop(here("trunk","analysis","figures","maps","initial utilization maps",paste0("initial_",d,"map.png"))) 
}
#creates maps of initial utilization 

rm(list=ls())