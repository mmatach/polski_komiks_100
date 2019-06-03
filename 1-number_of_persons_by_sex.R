library(readxl)
library(streamgraph)
library(ggplot2)
library(tibble)
library(tidyverse)
#----------------------------------
#1number_of_persons_by_sex
#----------------------------------
plk_os<-readxl::read_xlsx("polski_komiks.xlsx")
plk_os$nazwisko<-NULL
plk_os_k<-plk_os[which(plk_os$plec=="K"),]
plk_os_m<-plk_os[which(plk_os$plec=="M"),]
plk_os_nw<-plk_os[which(plk_os$plec=="nw"),]
#remove info about sex
plk_os_k$plec<-NULL
plk_os_m$plec<-NULL
plk_os_nw$plec<-NULL
#change number of comics - so it would count persons in each year, not number of comics
plk_os_k[plk_os_k >1] <-1
plk_os_m[plk_os_m >1] <-1
plk_os_nw[plk_os_nw >1] <-1
#sum plk_k and plk_m, give name to column
plk_os_k_df<-as.data.frame(colSums(plk_os_k, na.rm = TRUE))
names(plk_os_k_df)<-c("ilosc")
plk_os_m_df<-as.data.frame(colSums(plk_os_m, na.rm = TRUE))
names(plk_os_m_df)<-c("ilosc")
plk_os_nw_df<-as.data.frame(colSums(plk_os_nw, na.rm = TRUE))
names(plk_os_nw_df)<-c("ilosc")
#add K and M column
plk_os_k_df<-cbind(plk_os_k_df, data.frame(plec="K"))
plk_os_m_df<-cbind(plk_os_m_df, data.frame(plec="M"))
plk_os_nw_df<-cbind(plk_os_nw_df, data.frame(plec="nw"))
#add years as columns
rok <- rownames(plk_os_k_df)
rownames(plk_os_k_df) <- NULL
plk_os_k_df<-cbind(rok, plk_os_k_df)
rownames(plk_os_m_df) <- NULL
plk_os_m_df<-cbind(rok, plk_os_m_df)
rownames(plk_os_nw_df) <- NULL
plk_os_nw_df<-cbind(rok, plk_os_nw_df)
#combine three dataframes
plk_os_df_rbind<-rbind(plk_os_m_df, plk_os_k_df, plk_os_nw_df)
#re-order
plk_os_df_rbind<-plk_os_df_rbind[order(plk_os_df_rbind$rok),]
#factor to non-factor trap
rok_num<-as.numeric(as.character(plk_os_df_rbind$rok))
plk_os_df_rbind<-cbind(plk_os_df_rbind, rok_num)
plk_os_df_rbind$rok<-NULL

##code for Spiral chart
#change position of each type K/M/nw
plk_spread<-spread(plk_os_df_rbind, plec, ilosc)
#new column - adding
plk_spread<-mutate(plk_spread, Ile=K+M+nw)
#drop not needed columns
plk_spread$M<-NULL
plk_spread$K<-NULL
plk_spread$nw<-NULL
#adding id column
plk_spread$id <- seq.int(nrow(plk_spread))
#chart
ggplot(plk_spread, aes(plk_spread$id %% 25, 
                       2*plk_spread$id + Ile/2, height = Ile, fill = Ile)) + 
  geom_tile() + 
  scale_y_continuous(limits = c(0, NA)) + #narrowness
  scale_x_continuous(breaks = NULL, minor_breaks = NULL, labels = rok_num) +
  coord_polar() +  
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())


##for stacked bar chart
Kto<-plk_os_df_rbind$plec
Ile<-plk_os_df_rbind$ilosc
Rok<-plk_os_df_rbind$rok_num

# Stacked bar chart
ggplot(plk_os_df_rbind, aes(fill=Kto, y=Ile, x=Rok)) + 
  geom_bar(stat="identity", width=0.8)+
  scale_fill_manual(values=c("#214667", "#ee4642", "black"))+
  scale_y_continuous(breaks=c(0,50,100,150, 200, 250, 300, 350,400), position="right")+
  scale_x_continuous(breaks=c(1918,1930, 1940, 1950, 1960, 1970, 1980,1990, 2000, 2010, 2018))+
  geom_text(aes(label=Ile),vjust=1.5, colour="white", size=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())
# ggtitle("Liczba polskich twórców i twórczyń")
#  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015,2020), limits=c(1990,2020))
