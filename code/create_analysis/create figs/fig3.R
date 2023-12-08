library(data.table)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(here)
library(usmap)
library(sf)
library(patchwork)

###set up####
dir.create(here("trunk","analysis","figures","Figure 3"))
df<-fread(here("trunk","derived","final datasets","annual_df.txt"))
df[,rate:=(d_sup)/pop]
df$drug<-factor(df$drug,levels = c("GLP-1","SGLT-2","GLP-1 & SGLT-2"))

cpal<-RColorBrewer::brewer.pal(5,"Dark2")
df[is.na(rate)]
#none missing
df<-df[is30!="all"]
#filter out Medicaid "Alls"

df[,`:=`(rank=rank(-rate),
         quintile=ntile(-rate,5)),.(year,prg,drug)]
#create rank, 1 is high rates, 50 is low rates
#creates quintile, 1 is high, 5 is low to keep consistent


###map 3.1 prep####
dft=df[year%in%c(2013,2019)& drug=="GLP-1 & SGLT-2",
       .(state,
         min=round(min(rate,na.rm=T)*1000,2),
         max=round(max(rate,na.rm=T)*1000,2)),
       .(year,prg,drug,quintile=as.factor(quintile))]

x<-c(2013,"Medicaid","GLP-1 & SGLT-2",NA,"District of Columbia",NA,NA)%>%t()%>%as.data.table()
x%<>%rbind(x)
x[1,1]<-2019
names(x)<-names(dft)
dft1<-dft%>%rbind(x)
x<-c(2013,"Medicare","GLP-1 & SGLT-2",NA,"District of Columbia",NA,NA)%>%t()%>%as.data.table()
x%<>%rbind(x)
x[1,1]<-2019
names(x)<-names(dft)
dft1<-dft1%>%rbind(x)
dft=dft1

dft[,quintile_fixed:=paste0(quintile,", (",min," - ",max,")")]
###map 3.1 map####
p1<-plot_usmap(data=dft,values="quintile",col="black")+
  theme_void()+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position="top",nrow=2, byrow=TRUE,override.aes = list()))+
  scale_fill_manual(values=c("azure4","azure3","dodgerblue1","dodgerblue3","dodgerblue4"), name = "Quintile of Days Supplied Rate",na.translate=FALSE)+facet_wrap(year~prg)+
  labs(title="Quintile Rank of Days Supplied by Program & Year")
p1
  #duplciated values are for NA DC
ggsave(p1,file=here("trunk","analysis","figures","Figure 3","fig3.1.png"),
       height=7.54,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 3","fig3.1.png"))
###map 3.2 map####
for (i in 2:5) {
  if(i%in%2:3){
    p="Medicaid"
  }else{
    p="Medicare"
  }
  if(i%in%c(2,4)){
    y="2013"
  }else{
    y="2019"
  }

  assign(paste0("p",i),plot_usmap(data=dft[year==y&prg==p&!is.na(min)],values="quintile_fixed",col="black")+
    theme_void()+
    theme(legend.position = "left")+
    scale_fill_manual(values=c("azure4","azure3","dodgerblue1","dodgerblue3","dodgerblue4","green"), name = "",na.translate=FALSE)+
    labs(title=paste0(p,", ",y)))
}

p6<-
  (p2+p3+p4+p5)+
  plot_annotation(tag_prefix = 'Fig X.',
                  tag_levels = "A",
                  tag_suffix = ':',
                  title="Figure X: Days Supplied Per Capita")& 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0))
p6
#duplciated values are for NA DC

ggsave(p6,file=here("trunk","analysis","figures","Figure 3","fig3.2.png"),
       height=10,width=9.06,units=c("in"))
knitr::plot_crop(here("trunk","analysis","figures","Figure 3","fig3.2.png"))
