library(ggplot2)
library(reshape2)

cram_2019_short<-read.csv("/Volumes/GoogleDrive-116069313024281609105/My Drive/Active Grants/SUNA_Data/corrected_files/SUNA_Cor/SUNA_CRAM_2019_20210701_full_short.csv")

cram_2019_short$dtp<-as.POSIXct(cram_2019_short$dtp,tz="UTC")

cram_2019_keep<-cram_2019_short[,grep("interp_*",names(cram_2019_short))]
cram_2019_keep$dtp<-cram_2019_short$dtp

cram_melt<-melt(cram_2019_keep,id="dtp")

cram_melt$wavelength<-as.numeric(gsub("interp_","",cram_melt$variable,fixed=TRUE))
cram_melt<-subset(cram_melt,wavelength>=200)


cram_melt_onestamp<-subset(cram_melt,dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))

ggplot(cram_melt_onestamp,aes(wavelength,value))+
geom_point(size=2)+
geom_line(size=2)

cram_melt_onestamp_2<-subset(cram_melt,dtp==as.POSIXct("2019-07-27 10:00:46",tz="UTC")|dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))

ggplot(cram_melt_onestamp_2,aes(x=wavelength,y=value,color=as.factor(dtp)))+
geom_point(size=2)+
geom_line(size=2)




ggplot(cram_melt,aes(x=wavelength,y=value,color=as.factor(dtp)))+
theme(legend.position="NONE")+
geom_point(size=2)+
geom_line(size=2)