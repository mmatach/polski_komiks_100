library(readxl)
library(ggplot2)
library(tibble)
library(tidyverse)
#----------------------------------
#3 ile osob w danym przedziale ilosci komiksow (=1, 2-4, 5-9, 10-14, 15+) z podzialem na plec
#3 how many people, with how many comics, in bins (=1, 2-4, 5-9, 10-14, 15+), based on sex 
#----------------------------------
plk_przedzialy<-readxl::read_xlsx("polski_komiks.xlsx")
plk_przedzialy_plec<-as.data.frame(plk_przedzialy$plec)
plk_przedzialy$plec<-NULL
#przesuwanie nazwisk i dodawanie rzedami
#move surnames to rownames, sum all years
plk_przedzialy<-column_to_rownames(plk_przedzialy, var = "nazwisko")
plk_przedzialy_sumy<-as.data.frame(rowSums(plk_przedzialy, na.rm=TRUE))
#dodaj ilosc komiksow i plec, zmien nazwy kolumn
#add amount of comics and sex type, change columns' names
plk_przedzialy_z_plcia<-cbind(plk_przedzialy_sumy, plk_przedzialy_plec)
colnames(plk_przedzialy_z_plcia)<-c("IloscOsob", "Plec")
#sprawdzic kompletnosc zestawu
#check if data are complete
plk_przedzialy_z_plcia[!complete.cases(plk_przedzialy_z_plcia$Plec),]
#przedzialy
#bins
plk_przed_elim1<-plk_przedzialy_z_plcia[(plk_przedzialy_z_plcia$IloscOsob==1),] #=1
plk_przed_elim2_5<-plk_przedzialy_z_plcia[(plk_przedzialy_z_plcia$IloscOsob>=2 & plk_przedzialy_z_plcia$IloscOsob<=5),] #2-5
plk_przed_elim6_14<-plk_przedzialy_z_plcia[(plk_przedzialy_z_plcia$IloscOsob>=6 & plk_przedzialy_z_plcia$IloscOsob<=14),] #10-14
plk_przed_elim15<-plk_przedzialy_z_plcia[(plk_przedzialy_z_plcia$IloscOsob>=15),] #15+
#usunac nazwiska z rzedow
#remove surnames from rows
rownames(plk_przed_elim1)<-NULL
rownames(plk_przed_elim2_5)<-NULL
rownames(plk_przed_elim6_14)<-NULL
rownames(plk_przed_elim15)<-NULL

#zamienic wszystkie wartosci na 1
#turn every value into 1
plk_przed_elim2_4[plk_przed_elim2_5 >1] <-1
plk_przed_elim6_14[plk_przed_elim6_14 >1] <-1
plk_przed_elim15[plk_przed_elim15 >1] <-1

#counting people in each group in bins
p1<-plk_przed_elim1 %>% group_by(Plec) %>% summarise(a=sum(IloscOsob))
p2<-plk_przed_elim2_5 %>% group_by(Plec) %>% summarise(a=sum(IloscOsob))
p3<-plk_przed_elim6_14 %>% group_by(Plec) %>% summarise(a=sum(IloscOsob))
p4<-plk_przed_elim15 %>% group_by(Plec) %>% summarise(a=sum(IloscOsob))

#wykresy
#charts
ggplot(plk_przed_elim1, aes(fill=Plec, y=IloscOsob, x=Plec))+
  scale_fill_manual(values=c("#3b5998", "#9F3881", "black"))+
  geom_bar( stat="identity")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())

ggplot(plk_przed_elim1_5, aes(fill=Plec, y=IloscOsob, x=Plec)) +
  scale_fill_manual(values=c("#3b5998", "#9F3881", "black"))+
  geom_bar( stat="identity")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())

ggplot(plk_przed_elim6_14, aes(fill=Plec, y=IloscOsob, x=Plec)) +
  scale_fill_manual(values=c("#3b5998", "#9F3881", "black"))+
  geom_bar( stat="identity")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())

ggplot(plk_przed_elim15, aes(fill=Plec, y=IloscOsob, x=Plec)) +
  scale_fill_manual(values=c("#3b5998", "#9F3881", "black"))+
  geom_bar( stat="identity")+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        panel.grid.major.y=element_line(colour="#babfc4"),
        panel.background=element_blank(), panel.grid.major.x=element_blank(),
        axis.ticks.y = element_blank())
