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
#change number of comics
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

#NOT USED - streamgraph
streamgraph(plk_os_df_rbind, key="plec", value="ilosc", date="rok_num")%>%
  sg_fill_manual(c("#9F3881", "#61B6C5", "black"))%>%
  sg_axis_y(0)%>%
  sg_axis_x(10, "rok", "%Y")

#NOT USED - Stacked Percent bar chart
#for stacked bar chart
Kto<-plk_os_df_rbind$plec
Ile<-plk_os_df_rbind$ilosc
Rok<-plk_os_df_rbind$rok_num

ggplot(plk_os_df_rbind, aes(fill=Kto, y=Ile, x=Rok)) + 
  geom_bar( stat="identity", position="fill")+
  scale_fill_manual(values=c("#3b5998", "#9F3881", "black"))+
  scale_x_continuous(breaks=c(1990,1995,2000,2005,2010,2015,2020), limits=c(1989,2020))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())+
  ggtitle("Liczba polskich twórców i twórczyn [proporcje]")
