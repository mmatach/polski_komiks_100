library(readxl)
library(ggplot2)
library(tibble)
library(tidyverse)
#----------------------------------
#2 how many comics by surname
#----------------------------------
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
plk_elim20<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=20),] #pow 20
plk_elim10<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=10),] #pow 10
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
ggplot(plk_elim20, aes(y=plk_elim20$IloscPrac, x=fct_rev(plk_elim20$Nazwisko))) + 
  geom_segment( aes(xend=plk_elim20$Nazwisko, yend=0) , size=0.7, color="orange")  +
  geom_point(size=5, color="orange", fill=alpha("orange", 0.2), alpha=0.7, shape=21)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_line(colour="#d4d4d4"),
        panel.background=element_blank(),  
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  coord_flip()

