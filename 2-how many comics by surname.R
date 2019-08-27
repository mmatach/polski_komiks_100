library(readxl)
library(ggplot2)
library(tibble)
library(tidyverse)
#----------------------------------
#2 how many comics by surname 
#2nd method - adding rows while keeping 'sex/plec' column, then color-code them
#----------------------------------
plk_naz<-readxl::read_xlsx("polski_komiks.xlsx")
plec<-plk_naz$plec
#move surnames and add rows
plk_naz<-column_to_rownames(plk_naz, var = "nazwisko")
as.data.frame(plk_naz)
plk_naz_added<-as.data.frame(rowSums(plk_naz[, -1],  na.rm=TRUE))
plk_naz_added<-cbind(plk_naz_added, plec)
#add surnames
nazwiska<-rownames(plk_naz)
rownames(plk_naz_added) <- NULL
plk_added_z_naz <- cbind(plk_naz_added, nazwiska)
colnames(plk_added_z_naz)<-c("IloscPrac", "Plec", "Nazwisko")
plk_added_z_naz <- as.data.frame(plk_added_z_naz)
#eliminating people with x number of comics
plk_added_z_naz #podstawowy
plk_elim5_20<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=5 & plk_added_z_naz$IloscPrac<=20),]
plk_elim15<-plk_added_z_naz[(plk_added_z_naz$IloscPrac>=15),] #>15


#bar chart - surnames by 90 degrees
ggplot(plk_elim15, aes(y=plk_elim15$IloscPrac, x=plk_elim15$Nazwisko)) + 
  geom_bar( stat="identity")+
  geom_text(aes(label=plk_elim15$IloscPrac),position = position_stack(0.80), colour="white", size=2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.text.x.bottom = element_text(angle=90, hjust=0.90, vjust=0.4))

#lollipop - whole chart by 90 degrees
ggplot(plk_elim15, aes(y=plk_elim15$IloscPrac, x=fct_rev(plk_elim15$Nazwisko))) + 
  geom_segment( aes(xend=plk_elim15$Nazwisko, yend=0) , size=0.7, color="#214667")  +
  geom_point(size=5, color="#214667", fill=alpha("#214667", 0.2), alpha=0.7, shape=21)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_line(colour="#d4d4d4"),
        panel.background=element_blank(),  
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  coord_flip()

#lollipop - whole chart by 90 degrees with color-coded sex (but only for 2 types)
#409685 green #ee4642 coral #787575 grey
ggplot(plk_elim15, aes(y=plk_elim15$IloscPrac, x=fct_rev(plk_elim15$Nazwisko))) + 
  geom_segment(aes(xend=plk_elim15$Nazwisko, yend=0 ), 
               color=ifelse(plk_elim15$Plec %in% c("M"), "#214667", "#ee4642"), 
               size=0.7) +
  geom_point(color=ifelse(plk_elim15$Plec %in% c("M"), "#214667", "#ee4642"), 
             fill=ifelse(plk_elim15$Plec %in% c("M"), "#214667", "#ee4642"), 
             size=5, alpha=0.7, shape=21) +
  geom_text(aes(label=IloscPrac), 
             color=ifelse(plk_elim15$Plec %in% c("M"), "#214667", "#ee4642"), size=3, hjust=-1)+
  geom_text(aes(label=Nazwisko), 
            color=ifelse(plk_elim15$Plec %in% c("M"), "#214667", "#ee4642"), size=3, hjust=-0.5)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_line(colour="#d4d4d4"),
        panel.background=element_blank(),  
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  coord_flip()

#for Adobe, colors for 3 types
p<-ggplot(plk_elim15, aes(y=IloscPrac, x=fct_rev(Nazwisko), color=Plec)) + 
  geom_segment(aes(xend=plk_elim15$Nazwisko, yend=0 ), size=0.7) +
  geom_point(size=5, alpha=0.7, shape=19) +
  geom_text(aes(label=IloscPrac), size=3, hjust=-1)+
  geom_text(aes(label=Nazwisko), size=3, hjust=-1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_blank(), panel.grid.major.x=element_blank(),
        panel.background=element_blank(),  
        axis.ticks.y = element_blank(), axis.ticks.x = element_blank())+
  coord_flip()
p

cols <- c("M"="#409685", "K"="#ee4642", "nw"="#787575")
p+scale_colour_manual(values = cols)
