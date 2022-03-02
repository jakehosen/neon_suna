library(dplyr)
library(streamMetabolizer)
library(splitstackshape)
library(ggplot2)
library(lubridate)
library(reshape2)
library(neonUtilities)
library(progress)
library(patchwork)
library(doBy)
#library(box)

theme_ts_space<-theme_grey() +
		theme(
#		panel.grid.major = element_blank(),
#		panel.grid.minor = element_blank(),
		panel.background = element_rect(fill="white", colour="black", size=2),
#		legend.key       = element_blank(),
#		legend.text      = element_text(size=20.5),
#		legend.text      = element_blank(),
#		legend.title     = element_text(size=20.5),
		axis.text.x = element_text(size=22,colour="black",hjust=1,angle=45),
		axis.text.y = element_text(size=22,colour="black",vjust=.3),
		axis.title.x = element_text(size=22),
		axis.title.y = element_text(size=22,vjust=-1),
#		plot.title = element_text(hjust = 0.5,size=22,face="bold"),		
#		legend.position  = "left",
		legend.position  = "none",
		plot.margin = unit(c(1,1,1,1), "cm"),
		panel.border = element_rect(colour = "black", fill=NA, size=2)
		)






working<-"~/neon_suna"
site_id<-"Huron_CSMI_July"
data<-"/Volumes/GoogleDrive/My Drive/SUNA_Data/triaxus/Triaxus_2015_2017_Data/Suna/2017/Huron CSMI July"



#setwd(paste("~/gdrive/SUNA_data/SUNA_NEON/",site_id,sep=""))
#sunas<-list.files(pattern="*.csv")
#getwd()
#options(box.path = "~/neon_suna/box/")
#box::use(plots)

setwd(data)
sunas<-list.files(pattern="*.csv")

#suna_cal<-readRDS("/Users/jhosen/gdrive/fdom/SUNA Calibration pdfs/suna_cal_comb.rds")
suna_cal<-read.csv("/Users/jhosen/gdrive/SUNA_Data/SUNA Calibration pdfs/suna_0839_20200312.csv")
suna_cal_t<-as.data.frame(t(suna_cal))
names(suna_cal_t)<-suna_cal_t[1,]






compiled_suna<-data.frame()
sunas_length<-length(sunas)
for(i in 1:sunas_length){
#i<-1
print(i)
suna<-read.csv(paste(sunas[i]),skip=14,header=FALSE)
#head(suna)

#suna_d0<-cSplit(suna,3, ",")
#names(suna)<-c("suna_id","date","time","nitrate","nitrate_mgl","a254","a350","bromide_trace","spec_average_dark","dark_signal_average","int_time",suna_cal$Wavelength,"sensor_temp","spec_temp","lamp_temp","lamp_time","rel_hum","main_volt","lamp_volt","int_volt","main_current","fit_aux_1","fit_aux_2","fit_base_1","fit_base_2","fit_RMSE","CTD_Time","CTD_Salinity","CTD_Temp","CTD_Pressure","checksum")
names(suna)<-c("suna_id","date","time","nitrate","nitrate_mgl","a254","a350","bromide_trace","spec_average_dark","dark_signal_average","int_time",seq(190,394,length.out=256),"sensor_temp","spec_temp","lamp_temp","lamp_time","rel_hum","main_volt","lamp_volt","int_volt","main_current","fit_aux_1","fit_aux_2","fit_base_1","fit_base_2","fit_RMSE","CTD_Time","CTD_Salinity","CTD_Temp","CTD_Pressure","checksum")

#suna<-subset(suna,a254!=0)

#suna<-subset(suna,a254>=0)
suna<-suna[suna$"254">1000,]

elength<-nchar(suna$date)
slength<-elength-2
doy<-substr(suna$date,slength,elength)
year<-substr(suna$date,1,4)
    
day<-as.POSIXct(paste(as.Date(as.numeric(doy)-1,origin=paste(year,"-01-01",sep="")),"00:00:00"),tz="UTC")
#suna$day<-as.POSIXct("2019-05-10 00:00:00",tz="UTC")
suna$dtp<-day + (3600*as.numeric(suna$time))
#attr(suna$dtp,"tzone") <- "Etc/GMT+8"

    

offse<-c(t(suna_cal_t[3,]))
suna2<-(suna[,12:267]/suna$int_time)-suna$dark_signal_average
#suna_d0_norm<-sweep(suna2,2,FUN="/",offse)
suna_d0_norm<-suna2
names(suna_d0_norm)<-paste("n_",names(suna_cal_t),sep="")

    

#print("test")
suna_d0_int<-data.frame()
for(j in 1:nrow(suna_d0_norm)){	
#    print(j)
	flip<-as.data.frame(t(suna_d0_norm[j,]))
	names(flip)<-c("abs")
	flip$wl_nm<-gsub("n_","",row.names(flip),fixed=TRUE)
	flip_int<-as.data.frame(t(approx(flip$wl_nm,flip$abs,xout=seq(189,394,1),rule=2)$y))
	names(flip_int)<-paste("interp_",seq(189,394,1),sep="")
	suna_d0_int<-bind_rows(suna_d0_int,flip_int)
}
suna_d<-bind_cols(suna,suna_d0_int)
#suna_d$date<-as.character(suna_d[,c("date")])
#suna_d$time<-as.character(suna_d[,c("time")])
suna_d2<-suna_d %>% mutate_if(is.numeric,as.character)
suna_d2$cal<-sunas[i]
compiled_suna<-bind_rows(compiled_suna,suna_d2)
}
print("done")

saveRDS(compiled_suna,"/Volumes/GoogleDrive/My Drive/SUNA_Data/triaxus/Triaxus_2015_2017_Data/Suna/2017/huron_csmi_jul2017_compiled.rds")
compiled_suna<-readRDS("/Volumes/GoogleDrive/My Drive/SUNA_Data/triaxus/Triaxus_2015_2017_Data/Suna/2017/huron_csmi_jul2017_compiled.rds")
    

plot(compiled_suna$interp_254,compiled_suna$a254)


    
#compiled_suna$cal<-"suna_0839"

suna_interp<-compiled_suna[,grepl("^inter",names(compiled_suna))] %>% mutate_if(is.character, ~as.numeric(.))
#head(suna_interp)

for(i in 1:ncol(suna_interp)){
	suna_interp[,i]<-suna_interp[,i]/max(suna_interp[,i])
}


other_suna<-compiled_suna[,c("suna_id","date","time","nitrate","nitrate_mgl","a254","a350","bromide_trace","spec_average_dark","dark_signal_average","int_time","sensor_temp","spec_temp","lamp_temp","lamp_time","rel_hum","main_volt","lamp_volt","int_volt","main_current","fit_aux_1","fit_aux_2","fit_base_1","fit_base_2","fit_RMSE","CTD_Time","CTD_Salinity","CTD_Temp","CTD_Pressure","checksum","cal","dtp")]

suna_cols<-bind_cols(other_suna,suna_interp)
suna_cols$a254<-as.numeric(suna_cols$a254)
suna_cols$a350<-as.numeric(suna_cols$a350)
suna_cols$nitrate<-as.numeric(suna_cols$nitrate)
suna_cols$nitrate_mgl<-as.numeric(suna_cols$nitrate_mgl)
suna_cols$sensor_temp<-as.numeric(suna_cols$sensor_temp)



suna_cols<-subset(suna_cols,a254<10)

suna_cols_f<-data.frame()
for(j in 1:length(sunas)){
    suna_cols_t<-subset(suna_cols,cal==sunas[j])

a350_mod<-lm(a350~log10(interp_350)+I(log10(interp_350)^2)+I(log10(interp_350)^3),suna_cols_t)
a254_mod<-lm(a254~log10(interp_254)+I(log10(interp_254)^2)+I(log10(interp_254)^3),suna_cols_t)

    cor_cols<-grep("^interp_",names(suna_cols_t))
    
for(i in 1:length(cor_cols)){
	prepdata<-data.frame(interp_350=suna_cols_t[,cor_cols[i]],interp_254=suna_cols_t[,cor_cols[i]])
	a350_pred<-predict(a350_mod,newdata=prepdata)
	a254_pred<-predict(a254_mod,newdata=prepdata)
	suna_cols_t[,c(paste(names(suna_cols_t)[cor_cols[i]],"_c350",sep=""))]<-a350_pred
	suna_cols_t[,c(paste(names(suna_cols_t)[cor_cols[i]],"_c254",sep=""))]<-a254_pred
}
 suna_cols_f<-bind_rows(suna_cols_f,suna_cols_t)   
}

s275295_comp<-data.frame()
suna_cols_f_interp<-suna_cols_f[,grepl("_c254",names(suna_cols_f),fixed=TRUE)]
for(i in 1:nrow(suna_cols_f_interp)){
	datz<-melt(suna_cols_f_interp[i,],id.vars=NULL)
	datz$wavelength_nm<-gsub("interp_","",datz$variable)
	datz$wavelength_nm<-gsub("_c254","",datz$wavelength_nm)	
	datz<-subset(datz,wavelength_nm>=275&wavelength_nm<=295)
	datz$am1<-datz$value*100*2.3025851
	datz$lnam1<-log(datz$am1)
		if(sum(!is.na(datz$lnam1))>2){
			s275295<-lm(datz$lnam1~datz$wavelength_nm)$coefficients[2]*-1
			}else{s275295<-NA}
			s275295_temp<-data.frame(s275295=s275295)
			s275295_comp<-bind_rows(s275295_comp,s275295_temp)
	#		pb$tick()
	}	
suna_cols_f$a254<-suna_cols_f$a254+0.03
suna_cols_f$s275295<-s275295_comp$s275295
suna_cols_f$s275295z<-abs(((suna_cols_f$s275295-mean(suna_cols_f$s275295,na.rm=TRUE))/(sd(suna_cols_f$s275295,na.rm=TRUE)))/100)+0.6
suna_cols_fm<-melt(subset(suna_cols_f,date=="2017199"&hour(dtp)<12)[,c("dtp","a254","s275295z")],id.vars="dtp")
suna_cols_fm$variable<-gsub("a254","Absorbance at 254nm",suna_cols_fm$variable,fixed=TRUE)
suna_cols_fm$variable<-gsub("s275295z","Spectral Slope (275-295nm)",suna_cols_fm$variable,fixed=TRUE)

	ggplot(suna_cols_fm,aes(dtp,value))+geom_point(size=2)+
	theme(legend.position = "none",
	strip.text = element_text(size = 20))+
	facet_wrap(.~variable,scales="free_y",ncol=1)+
	xlab("")+
	ylab("")




	suna_cols_fm<-melt(subset(suna_cols_f,date=="2017199"&hour(dtp)>12)[,c("dtp","a254","s275295z")],id.vars="dtp")
	suna_cols_fm$variable<-gsub("a254","Absorbance at 254nm",suna_cols_fm$variable,fixed=TRUE)
	suna_cols_fm$variable<-gsub("s275295z","Spectral Slope (275-295nm)",suna_cols_fm$variable,fixed=TRUE)

		ggplot(suna_cols_fm,aes(dtp,value))+geom_point(size=2)+
		theme(legend.position = "none",
		strip.text = element_text(size = 20))+
		facet_wrap(.~variable,scales="free_y",ncol=1)+
		xlab("")+
		ylab("")
