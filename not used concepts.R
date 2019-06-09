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
#----------------------------------
#----------------------------------
library(readxl)
library(ggplot2)
library(tibble)
library(tidyverse)
#----------------------------------
#2 how many comics by surname - not used method
#----------------------------------
#1st method - removing 'sex' column, rowsums, dividing into groups
plk_naz<-readxl::read_xlsx("polski_komiks.xlsx")
plk_naz_plec<-plk_naz$plec
plk_naz$plec<-NULL
#move surnames and add rows
plk_naz<-column_to_rownames(plk_naz, var = "nazwisko")
as.data.frame(plk_naz)
plk_naz_added<-as.data.frame(rowSums(plk_naz, na.rm=TRUE))
#add surnames
nazwiska<-rownames(plk_naz)
rownames(plk_naz_added) <- NULL
plk_added_z_naz <- cbind(plk_naz_added, nazwiska)
colnames(plk_added_z_naz)<-c("IloscPrac", "Nazwisko")
#eliminating people with x number of comics
plk_added_z_naz #podstawowy
plk_elim5_20<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=5 & plk_added_z_naz$IloscPrac<=20),]
plk_elim15<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=15),] #pow 15


#bar chart - surnames by 90 degrees
ggplot(plk_elim15, aes(y=plk_elim15$IloscPrac, x=plk_elim15$Nazwisko)) + 
  geom_bar( stat="identity")+
  geom_text(aes(label=plk_elim15$IloscPrac),position = position_stack(0.80), colour="white", size=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.text.x.bottom = element_text(angle=90, hjust=0.90, vjust=0.4))

#lollipop - whole chart by 90 degrees
#214667 blue #ee4642 coral ##5a5a5a grey
ggplot(plk_elim15, aes(y=plk_elim15$IloscPrac, x=fct_rev(plk_elim15$Nazwisko))) + 
  geom_segment( aes(xend=plk_elim15$Nazwisko, yend=0) , size=0.7, color="#214667")  +
  geom_point(size=5, color="#214667", fill=alpha("#214667", 0.2), alpha=0.7, shape=21)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_line(colour="#d4d4d4"),
        panel.background=element_blank(),  
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  coord_flip()
