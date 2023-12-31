bulk=read.csv("BULK.csv")
human=subset(bulk, bulk$species =="human")
animal=subset(bulk, bulk$species =="herbivore" | bulk$species =="camelid"| bulk$species =="caprine"|  bulk$species =="ovicaprid"|  bulk$species == "goat"|  bulk$species == "sheep"|   bulk$species == "wild sheep"| bulk$species == "ibex"|  bulk$species =="bos"|  bulk$species =="cattle"| bulk$species =="boar"| bulk$species =="pig"| bulk$species =="carp"| bulk$species =="fish"| bulk$species == "pike"| bulk$species == "sazan"| bulk$species =="deer"|  bulk$species =="red deer"|  bulk$species =="roe deer"| bulk$species == "wild deer"|  bulk$species =="donkey"| bulk$species == "hemione"|  bulk$species =="horse"| bulk$species == "gazella"|  bulk$species =="saiga"|  bulk$species =="bear"|  bulk$species =="cat"|  bulk$species =="dog"| bulk$species == "wolf"| bulk$species == "tortoise")
plot(bulk$carbon, bulk$nitrogen, main="Carbon and Nitrogen Isotopic Ratios", xlab="δ13C‰", ylab="δ15N‰", col="lightblue", pch=20)
ggplot(bulk, mapping=aes(x=carbon, y=nitrogen)) + geom_point() + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios") 
ggplot(bulk, mapping=aes(x=carbon, y=nitrogen, col=species)) + geom_point() + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios")
install.packages("ggplot2")
library(ggplot2)
install.packages("RColorBrewer") 
library(RColorBrewer) 
install.packages("ggpubr")
library("ggpubr")
ggplot(bulk, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point() + bgcolor("#BFD5E3") + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios") 
ggplot(bulk, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point() + bgcolor("linen") + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-30, -5), ylim=c(0,20), expand=FALSE)
socioeconomic=subset(bulk, bulk$SOCIOECONOMIC == "NOMADIC"|bulk$SOCIOECONOMIC == "URBAN")
ggplot(socioeconomic, mapping=aes(x=carbon, y=nitrogen, col=SOCIOECONOMIC)) + geom_point(size=1) + bgcolor("linen") + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios for Nomadic and Urban Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(2.5,20), expand=FALSE)
ggplot(environment, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=0.65) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios for Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,20), expand=FALSE)
ggplot(chronology, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=0.75) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios for Different Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(0,20), expand=FALSE)
chronology=subset(bulk, !bulk$CHRONOLOGY%in%c("MODERN", "pre-BRONZE AGE"))
animals=subset(bulk, !bulk$species%in%c("human",""))
ggplot(animals, mapping=aes(x=carbon, y=nitrogen, col=species)) + geom_point(size=0.75) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios for Different Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE)
ggplot(human, mapping=aes(x=carbon, y=nitrogen, col="orange")) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(5,20), expand=FALSE)
ggplot(human, mapping=aes(x=carbon, y=nitrogen, col="orange")) + geom_point(size=1, show.legend=FALSE) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(5,20), expand=FALSE)
ggplot(human, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(5,20), expand=FALSE)
environment=subset(bulk, bulk$ENVIRONMENT == "A - SEMI-ARID STEPPE"|bulk$ENVIRONMENT == "B - FOREST STEPPE"| bulk$ENVIRONMENT == "C - WEST STEPPE"| bulk$ENVIRONMENT == "D - MOUNTAIN/FOREST STEPPE"| bulk$ENVIRONMENT == "E - ARID/MOUNTAIN STEPPE"| bulk$ENVIRONMENT == "F - DESERT OASES"| bulk$ENVIRONMENT == "G - ARID DESERT")
ggplot(animalenvironment, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=0.9) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE)
animalenvironment=subset(environment, !environment$species%in%c("human",""))
humanchronology=subset(chronology, chronology$species =="human")
ggplot(humanchronology, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Different Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(5,20), expand=FALSE)
animalchronology=subset(chronology, !chronology$species%in%c("human",""))
ggplot(animalchronology, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Different Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(0,17.5), expand=FALSE)
ggplot(humansocioeconomic2, mapping=aes(x=carbon, y=nitrogen, col=SOCIOECONOMIC)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Urban or Nomadic settings")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(7.5,20), expand=FALSE)
humansocioeconomic2=subset(socioeconomic2, socioeconomic2$species=="human")
animalsocioeconomic2=subset(socioeconomic2, !socioeconomic2$species%in%c("human",""))
ggplot(animalsocioeconomic2, mapping=aes(x=carbon, y=nitrogen, col=SOCIOECONOMIC)) + geom_point(size=0.9) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Urban or Nomadic settings")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE)
ggplot(humanbronzeage, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Bronze Age Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-12.5), ylim=c(7.5,19), expand=FALSE) +font("title", size=11)
animalbronzeage=subset(bronzeage, !bronzeage$species%in%c("human",""))
animalenvbage=subset(animalenvironment, animalenvironment$CHRONOLOGY=="BRONZE AGE")
ggplot(animalenvbage, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Bronze Age Animals in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(0,15), expand=FALSE) +font("title", size=11)
ggplot(humanironage, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Iron Age Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(7.5,20), expand=FALSE) +font("title", size=11)
ironage=subset(bulk, bulk$CHRONOLOGY=="IRON AGE")
humanironage=subset(ironage, ironage$species=="human")
lateant=subset(bulk, bulk$CHRONOLOGY=="LATE ANTIQUITY")
humanlateant=subset(lateant, lateant$species=="human")
medieval=subset(bulk, bulk$CHRONOLOGY=="MEDIEVAL")
humanmedieval=subset(medieval, medieval$species=="human")
ggplot(humanlateant, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Late Antiquity Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-30,-5), ylim=c(0,20), expand=FALSE) +font("title", size=11)
ggplot(humanlateant, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Late Antiquity Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(10,17.5), expand=FALSE) +font("title", size=11)
ggplot(humanmedieval, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Medieval Humans in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-20,-10), ylim=c(7.5,17.5), expand=FALSE) +font("title", size=11)
ggplot(anenvironage, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=1.6) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Iron Age Animals in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(0,15), expand=FALSE) +font("title", size=11)
anenvironage=subset(animalenvironment,animalenvironment$CHRONOLOGY=="IRON AGE")
ggplot(anenvlateant, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Late Antiquity Animals in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-12.5), ylim=c(2.5,15), expand=FALSE) +font("title", size=11)
anenvlateant=subset(animalenvironment, animalenvironment$CHRONOLOGY=="LATE ANTIQUITY")
anenvmedieval=subset(animalenvironment, animalenvironment$CHRONOLOGY=="MEDIEVAL")
ggplot(anenvmedieval, mapping=aes(x=carbon, y=nitrogen, col=ENVIRONMENT)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Medieval Animals in Different Environments")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(5,17.5), expand=FALSE) +font("title", size=11)
ggplot(humanA, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Semi-Arid Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(7.5,20), expand=FALSE) +font("title", size=11)
humanA=subset(human, human$ENVIRONMENT=="A - SEMI-ARID STEPPE")
ggplot(humanB, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Forest Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-15), ylim=c(7.5,17.5), expand=FALSE) +font("title", size=11)
humanB=subset(human, human$ENVIRONMENT=="B - FOREST STEPPE")
humanC=subset(human, human$ENVIRONMENT=="C - WEST STEPPE")
ggplot(humanC, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in West Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-20,-12.5), ylim=c(10,20), expand=FALSE) +font("title", size=11)
humanD=subset(human, human$ENVIRONMENT=="D - MOUNTAIN/FOREST STEPPE")
ggplot(humanD, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Mountain/Forest Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(7.5,15), expand=FALSE) +font("title", size=11)
humanE=subset(human, human$ENVIRONMENT=="E - ARID MOUNTAIN STEPPE")
humanF=subset(human, human$ENVIRONMENT=="F - DESERT OASES")
ggplot(humanF, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Desert Oases Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(7.5,17.5), expand=FALSE) +font("title", size=11)
humanG=subset(human, human$ENVIRONMENT=="G - ARID DESERT")
ggplot(humanG, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=3) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Arid Desert Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-15), ylim=c(10,17.5), expand=FALSE) +font("title", size=11)
animalsA=subset(animals, animals$ENVIRONMENT=="A - SEMI-ARID STEPPE")
ggplot(animalsA, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Semi-Arid Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.55,-10), ylim=c(2.5,20), expand=FALSE) +font("title", size=11)
animalsB=subset(animals, animals$ENVIRONMENT=="B - FOREST STEPPE")
ggplot(animalsB, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Forest Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-15), ylim=c(2.5,17.5), expand=FALSE) +font("title", size=11)
animalsC=subset(animals, animals$ENVIRONMENT=="C - WEST STEPPE")
ggplot(animalsC, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1.5) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in West Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(2.5,20), expand=FALSE) +font("title", size=11)
animalsD=subset(animals, animals$ENVIRONMENT=="D - MOUNTAIN/FOREST STEPPE")
ggplot(animalsD, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1.5) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Mountain/Forest Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE) +font("title", size=11)
animalsE=subset(animals, animals$ENVIRONMENT=="E - ARID/MOUNTAIN STEPPE")
ggplot(animalsE, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Arid/Mountain Steppes Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE) +font("title", size=11)
animalsF=subset(animals, animals$ENVIRONMENT=="F - DESERT OASES")
ggplot(animalsF, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1.5) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Desert Oases Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(2.5,17.5), expand=FALSE) +font("title", size=11)
animalsG=subset(animals, animals$ENVIRONMENT=="G - ARID DESERT")
ggplot(animalsG, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=3) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Animals in Arid Desert Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(5,17.5), expand=TRUE) +font("title", size=11)
urban=subset(bulk, bulk$SOCIOECONOMIC=="URBAN")
urbanhuman=subset(urban, urban$species=="human")
ggplot(urbanhuman, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Urban Societies Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(7.5,15), expand=FALSE) +font("title", size=11)
nomadic=subset(bulk, bulk$SOCIOECONOMIC=="NOMADIC")
nomadichuman=subset(nomadic, nomadic$species=="human")
ggplot(nomadichuman, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1.5) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans in Nomadic Societies Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-20,-10), ylim=c(7.5,20), expand=FALSE) +font("title", size=11)
ggplot(begash, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Begash Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(0,15), expand=FALSE) +font("title", size=11)
begash=subset(bulk, bulk$site=="Begash")
turgen=subset(bulk, bulk$site=="Turgen"| bulk$site=="Turgen 2"| bulk$site=="Turgen-2")
humanturgen=subset(turgen, turgen$species=="human")
ggplot(humanturgen, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Turgen Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-20,-12.5), ylim=c(10,14), expand=FALSE) +font("title", size=11)
Kainarbulak=subset(bulk, bulk$site=="Kainar Bulak"| bulk$site=="Kainar Bulak-1"| bulk$site=="Kainarbulak-1"| bulk$site=="Kainarbulak-2")
ggplot(Kainarbulak, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Kainarbulak Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(0,15), expand=FALSE) +font("title", size=12)
ggplot(GeoktchikDepe, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Geoktchik Depe Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-30,-5), ylim=c(0,20), expand=FALSE) +font("title", size=11)
GeoktchikDepe=subset(bulk, bulk$site=="Geoktchik Depe")
ggplot(humanBestamak, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans at Bestamak Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-15), ylim=c(7.5,15), expand=FALSE) +font("title", size=13)
Bestamak=subset(bulk, bulk$site=="Bestamak")
humanBestamak=subset(Bestamak, Bestamak$species=="human")
Aktogai=subset(bulk, bulk$site=="Aktogai")
ggplot(Aktogai, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Humans at Aktogai Across Periods")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-20,-15), ylim=c(10,17.5), expand=FALSE) +font("title", size=13)
A=subset(bulk, bulk$ENVIRONMENT=="A - SEMI-ARID STEPPE")
ggplot(A, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1, show.legend=FALSE) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Semi-Arid Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-10), ylim=c(2.5,20), expand=FALSE) +font("title", size=11)
ggplot(A, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Semi-Arid Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-30,-5), ylim=c(0,20), expand=FALSE) +font("title", size=11)

B=subset(bulk, bulk$ENVIRONMENT=="B - FOREST STEPPE")
ggplot(B, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Forest Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-15), ylim=c(2.5,17.5), expand=FALSE) +font("title", size=11)
C=subset(bulk, bulk$ENVIRONMENT=="C - WEST STEPPE")
ggplot(C, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=2) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of West Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-22.5,-12.5), ylim=c(2.5,20), expand=FALSE) +font("title", size=13)
D=subset(bulk, bulk$ENVIRONMENT=="D - MOUNTAIN/FOREST STEPPE")
ggplot(D, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Mountain/Forest Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-30,-12.5), ylim=c(0,15)) +font("title", size=11)
E=subset(bulk, bulk$ENVIRONMENT=="E - ARID/MOUNTAIN STEPPE")
ggplot(E, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Arid/Mountain Steppe Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-27.5,-10), ylim=c(0,17.5), expand=FALSE) +font("title", size=11)
F=subset(bulk, bulk$ENVIRONMENT=="F - DESERT OASES")
ggplot(F, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Desert Oases Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(2.5,17.5), expand=FALSE) +font("title", size=12)
G=subset(bulk, bulk$ENVIRONMENT=="G - ARID DESERT")
ggplot(G, mapping=aes(x=carbon, y=nitrogen, col=site)) + geom_point(size=1) + labs(x="δ13C‰", y="δ15N‰", title="Carbon and Nitrogen Isotopic Ratios of Arid Desert Sites")+ theme_bw() + scale_y_continuous(position = "left") + coord_cartesian(xlim = c(-25,-10), ylim=c(5,20), expand=FALSE) +font("title", size=13)

animal=subset(animal, !animal$CHRONOLOGY%in%c("pre-BRONZE AGE", "MODERN", ""))
aC=ggplot(animal, mapping=aes(x=carbon, y=nitrogen, col=CHRONOLOGY)) + geom_point(aes(col=CHRONOLOGY)) + xlab(expression(paste(delta^{13}, "C (\u2030)"))) + ylab(expression(paste(delta^{15}, "N (\u2030)"))) + theme_bw() + scale_color_manual(values=c("darkslategray", "gold4", "cornsilk3", "navajowhite3"))
aC + facet_wrap(vars(CHRONOLOGY), nrow=2) + theme(strip.background = element_blank()) + theme(legend.position = "none", plot.margin=margin(10, 10, 10, 10)) + scale_x_continuous(breaks = seq(-22, -10, by = 2))


