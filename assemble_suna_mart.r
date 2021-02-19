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


setwd("/Users/jhosen/gdrive/fdom/SUNA_NEON/MART")
sunas<-list.files(pattern="*.csv")

#suna_cal<-readRDS("/Users/jhosen/gdrive/fdom/SUNA Calibration pdfs/suna_cal_comb.rds")




compiled_suna<-data.frame()
for(i in 1:length(sunas)){

#pb$tick()	
print(i)

suna<-read.csv(paste(sunas[i]),skip=0,header=FALSE)

suna_cal<-read.csv(paste("/Users/jhosen/gdrive/fdom/SUNA_NEON/MART/MART_CAL/",unique(suna[,ncol(suna)]),sep=""))
suna_cal_t<-as.data.frame(t(suna_cal))
names(suna_cal_t)<-suna_cal_t[1,]

#suna_d0<-cSplit(suna,3, ",")
names(suna)<-c("suna_id","date","time","nitrate","nitrate_mgl","a254","a350","bromide_trace","spec_average_dark","dark_signal_average","inter_time",suna_cal$wavelength_nm,"sensor_temp","spec_temp","lamp_temp","lamp_time","rel_hum","main_volt","lamp_volt","int_volt","main_current","fit_aux_1","fit_aux_2","fit_base_1","fit_base_2","fit_RMSE","CTD_Time","CTD_Salinity","CTD_Temp","CTD_Pressure","checksum","cal")


suna<-subset(suna,a254!=0)
doy<-gsub("2020","",suna$date,fixed=TRUE)
day<-as.POSIXct(paste(as.Date(as.numeric(doy),origin="2020-01-01"),"00:00:00"),tz="UTC")
#suna$day<-as.POSIXct('2019-05-10 00:00:00',tz="UTC")
suna$dtp<-day + (3600*as.numeric(suna$time))
attr(suna$dtp,"tzone") <- "Etc/GMT+8"

offse<-c(t(suna_cal_t[3,]))
suna2<-(suna[,grep("^[0-9]{3}",names(suna))]/suna$inter_time)-suna$dark_signal_average
suna_d0_norm<-sweep(suna2,2,FUN="/",offse)
names(suna_d0_norm)<-paste("n_",names(suna_cal_t),sep="")





suna_d0_int<-data.frame()
for(j in 1:nrow(suna_d0_norm)){	
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
compiled_suna<-bind_rows(compiled_suna,suna_d2)
}

compiled_suna2<-subset(compiled_suna,!is.na(bromide_trace) & a254>0 & a350>0)

compiled_suna2$interp_254<-as.numeric(compiled_suna2$interp_254)
compiled_suna2$a254<-as.numeric(compiled_suna2$a254)
compiled_suna2$sensor_temp<-as.numeric(compiled_suna2$sensor_temp)
summary(lm(a254~interp_254+I(interp_254^2)+I(interp_254^3)+sensor_temp,compiled_suna2))

compiled_suna2$a350<-as.numeric(compiled_suna2$a350)
compiled_suna2$interp_350<-as.numeric(compiled_suna2$interp_350)
compiled_suna2$sensor_temp<-as.numeric(compiled_suna2$sensor_temp)
compiled_suna2$date<-as.numeric(compiled_suna2$date)
compiled_suna2$inter_time<-as.numeric(compiled_suna2$inter_time)

summary(lm(a350~log10(interp_350)+I(log10(interp_350)^2)+I(log10(interp_350)^3),compiled_suna2))
summary(lm(a254~log10(interp_254)+I(log10(interp_254)^2)+I(log10(interp_254)^3),compiled_suna2))

a350_mod<-lm(a350~log10(interp_350)+I(log10(interp_350)^2)+I(log10(interp_350)^3),compiled_suna2)
a254_mod<-lm(a254~log10(interp_254)+I(log10(interp_254)^2)+I(log10(interp_254)^3),compiled_suna2)

cor_cols<-grep("^interp_",names(compiled_suna2))

compiled_suna2[,c(cor_cols)]<-compiled_suna2[,c(cor_cols)] %>% mutate_if(is.character,as.numeric)

for(i in 1:length(cor_cols)){
	prepdata<-data.frame(interp_350=compiled_suna2[,cor_cols[i]],interp_254=compiled_suna2[,cor_cols[i]])
	a350_pred<-predict(a350_mod,newdata=prepdata)
	a254_pred<-predict(a254_mod,newdata=prepdata)
	compiled_suna2[,c(paste(names(compiled_suna2)[cor_cols[i]],"_c350",sep=""))]<-a350_pred
	compiled_suna2[,c(paste(names(compiled_suna2)[cor_cols[i]],"_c254",sep=""))]<-a254_pred
}





#ss_plot<-subset(compiled_suna2,date==2019190)
ss_plot<-subset(compiled_suna2)
ss_plot$dtp_round<-as.POSIXct(round(ss_plot$dtp,"mins"))
dtp_round_col0<-ncol(ss_plot)

ss_plot0<-ss_plot[,c(dtp_round_col0,grep("^interp_",names(ss_plot)))] %>% mutate_if(is.character,as.numeric)
ss_plot0$sensor_temp<-ss_plot$sensor_temp
ss_plot2<-summaryBy(.~dtp_round,ss_plot0)


#ss_plot00<-ss_plot[,c(dtp_round_col0,grep("sensor_temp",names(ss_plot),fixed=TRUE),grep("^n_",names(ss_plot)))] %>% mutate_if(is.character,as.numeric)
#ss_plot2<-summaryBy(.~dtp_round,ss_plot00)
ss_plot2$date<-as.Date(ss_plot2$dtp,tz="Etc/GMT+8")
days<-unique(ss_plot2$date)
ssp_rec<-data.frame()
pb <- progress_bar$new(
	format = "  downloading [:bar] :percent eta: :eta",
	total = length(days), clear = FALSE, width= 60)
for(i in 1:length(days)){
	ssp_temp<-subset(ss_plot2,date==days[i])
	stop<-grep("_c254.mean$",names(ssp_temp))
	for(j in 1:length(stop)){
		temp_xy<-data.frame(x=ssp_temp$sensor_temp.mean,y=ssp_temp[,stop[j]])
		wl<-names(ssp_temp[stop[j]])
		tslope<-tryCatch(expr={lm(y~x,temp_xy)$coefficients[2]},error=function(e){return(NA)})
		ssp_rec_t<-data.frame(date=days[i],wl=wl,tslope=tslope)
		ssp_rec<-bind_rows(ssp_rec,ssp_rec_t)
	}
	pb$tick()	
	}
	ssp_rec$wl_nm<-gsub(".mean","",gsub("interp_","",ssp_rec$wl,fixed=TRUE),fixed=TRUE)
	ssp_rec_sum<-summaryBy(tslope~wl_nm,ssp_rec)
	



ss_plot3<-ss_plot2[,grep("c254.mean$",names(ss_plot2))]
ss_plot3$dtp_round<-ss_plot2$dtp_round
ssm<-melt(ss_plot3,id.vars=c("dtp_round"))
ssm$wl_nm<-as.numeric(gsub("_c254.mean","",gsub("interp_","",ssm$variable,fixed=TRUE),fixed=TRUE))
ssp_rec_sum$wl_nm<-gsub("_c254","",ssp_rec_sum$wl_nm,fixed=TRUE)
ssm2<-merge(ssm,ssp_rec_sum,by="wl_nm",all.x=TRUE)
ssm3<-merge(ssm2,ss_plot2[,c("dtp_round","sensor_temp.mean")],by="dtp_round",all.x=TRUE)
ssm3$temp_cor<-ssm3$value+ssm3$value*((ssm3$sensor_temp.mean-5)*ssm3$tslope.mean)

ssm_275_295<-subset(ssm3,wl_nm>=275&wl_nm<=295)
dtps<-unique(ssm_275_295$dtp_round)
pb <- progress_bar$new(
	format = "  downloading [:bar] :percent eta: :eta",
	total = length(dtps), clear = FALSE, width= 60)
s275295_comp<-data.frame()
for(i in 1:length(dtps)){
	ssm_now<-subset(ssm_275_295,dtp_round==dtps[i])
	
	ssm_now$am1<-ssm_now$temp_cor*100*2.3025851
	ssm_now$lnam1<-log(ssm_now$am1)


	if(sum(!is.na(ssm_now$lnam1))>2){
		s275295<-lm(ssm_now$lnam1~ssm_now$wl_nm)$coefficients[2]*-1
		}else{s275295<-NA}
		s275295_temp<-data.frame(dtp=dtps[i],s275295=s275295)
		s275295_comp<-bind_rows(s275295_comp,s275295_temp)
		pb$tick()
}	
	
	

ggplot(s275295_comp,aes(dtp,s275295))+
geom_point()+
xlab("")+
ylab("Spectral Slope (275-295nm)")



#MART_PAR_Surf<-loadByProduct(dpID="DP1.20042.001",site="MART",check.size=F)
#saveRDS(MART_PAR_Surf,"~/gdrive/fdom/SUNA_NEON/MART/MART_PAR_Surf.rds")
MART_PAR_Surf<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_PAR_Surf.rds")
pars<-as.data.frame(MART_PAR_Surf["PARWS_1min"])

pars$dtp<-pars$PARWS_1min.startDateTime
#attr(pars$dtp,"tzone") <- "Etc/GMT+8"

pars$PAR<-pars$PARWS_1min.PARMean

ss_plot_temp<-ss_plot[,c("dtp_round","spec_temp","a254","a350")]
ss_plot_temp$spec_temp<-as.numeric(ss_plot$spec_temp)
ss_plot_temp$a254<-as.numeric(ss_plot$a254)
ss_plot_temp$a350<-as.numeric(ss_plot$a350)
ss_plot_temp_sum<-summaryBy(spec_temp+a254+a350~dtp_round,ss_plot_temp)
ss_plot_temp_sum$dtp<-ss_plot_temp_sum$dtp_round

pars_ss<-merge(s275295_comp,pars,by="dtp")
pars_ss_temp<-merge(pars_ss,ss_plot_temp_sum[,c("dtp","spec_temp.mean","a254.mean","a350.mean")])
plot(pars_ss_temp$spec_temp.mean,pars_ss_temp$s275295)
plot(pars_ss_temp$PARWS_1min.PARMean,pars_ss_temp$s275295)
plot(pars_ss_temp$dtp,pars_ss_temp$PARWS_1min.PARMean)
plot(pars_ss_temp$dtp,pars_ss_temp$s275295)


MART_Nitrate<-loadByProduct(dpID="DP1.20033.001",site="MART",check.size=F)
#saveRDS(MART_Nitrate,"~/gdrive/fdom/SUNA_NEON/MART/MART_Nitrate.rds")
MART_Nitrate<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_Nitrate.rds")
nitrate<-as.data.frame(MART_Nitrate["NSW_15_minute"])
nitrate$dtp<-nitrate$NSW_15_minute.startDateTime
nitrate$nitrate_umL<-nitrate$NSW_15_minute.surfWaterNitrateMean



pars_ss_m<-melt(pars_ss_temp[,c("dtp","s275295","spec_temp.mean","PARWS_1min.PARMean","a254.mean","a350.mean")],id=c("dtp"))

ggplot(pars_ss_m,aes(dtp,value,group=variable))+
geom_point()+
facet_wrap(.~variable,scale="free_y")

ggplot(pars_ss_temp,aes(x=spec_temp.mean,y=s275295))+
geom_point()



ggplot(pars_ss_temp,aes(x=PARWS_1min.PARMean,y=s275295))+
geom_point()+
scale_x_log10()


pars_ss_temp$PARWS_1min.PARMean<-na.approx(pars_ss_temp$PARWS_1min.PARMean,na.rm=FALSE)
pars_ss_temp_m<-melt(pars_ss_temp[,c("dtp","a350.mean","PARWS_1min.PARMean","s275295","spec_temp.mean")],id.vars=c("dtp"))
ggplot(subset(pars_ss_temp_m,as.Date(dtp,tz="Etc/GMT+8")=="2019-08-20"),aes(dtp,value,color=variable))+
geom_point()+
facet_wrap(.~variable,scales="free_y",ncol=1)

saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/MART/MART_pars_ss_temp.rds")
saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/MART/MART_ss_plot2.rds")
	
	










######Probably use this
pars<-as.data.frame(MART_PAR_Surf["PARWS_1min"])

mean_na<-function(x){mean(x,na.rm=TRUE)}

pars$dtp<-pars$PARWS_1min.startDateTime
pars2<-summaryBy(PARWS_1min.PARMean~dtp,pars,FUN=c(mean_na))
#attr(pars$dtp,"tzone") <- "Etc/GMT+8"

ss_plot_temp<-ss_plot[,c("dtp_round","spec_temp","a254","a350")]
ss_plot_temp$spec_temp<-as.numeric(ss_plot$spec_temp)
ss_plot_temp$a254<-as.numeric(ss_plot$a254)
ss_plot_temp$a350<-as.numeric(ss_plot$a350)
ss_plot_temp_sum<-summaryBy(spec_temp+a254+a350~dtp_round,ss_plot_temp)
ss_plot_temp_sum$dtp<-ss_plot_temp_sum$dtp_round

pars_ss<-merge(s275295_comp,pars2,by="dtp")
pars_ss_temp<-merge(pars_ss,ss_plot_temp_sum[,c("dtp","spec_temp.mean","a254.mean","a350.mean")])
plot(pars_ss_temp$spec_temp.mean,pars_ss_temp$s275295)
plot(pars_ss_temp$PARWS_1min.PARMean,pars_ss_temp$s275295)
plot(pars_ss_temp$dtp,pars_ss_temp$PARWS_1min.PARMean)
plot(pars_ss_temp$dtp,pars_ss_temp$s275295)


pars_ss_m<-melt(pars_ss_temp[,c("dtp","s275295","spec_temp.mean","PARWS_1min.PARMean.mean_na","a254.mean","a350.mean")],id=c("dtp"))

ggplot(pars_ss_m,aes(dtp,value,group=variable))+
geom_point()+
facet_wrap(.~variable,scale="free_y")

ggplot(pars_ss_temp,aes(x=spec_temp.mean,y=s275295))+
geom_point()



ggplot(pars_ss_temp,aes(spec_temp.mean,s275295))+
geom_point()

library(zoo)

pars_zoo<-zoo(pars$PARWS_1min.PARMean,pars$dtp)
compiled_suna2$dtp_round<-as.POSIXct(round(compiled_suna2$dtp,"mins"))
compiled_suna2_sum<-summaryBy(a350+a254+sensor_temp~dtp_round,compiled_suna2)
compiled_suna2_sum$dtp<-compiled_suna2_sum$dtp_round
#na.approx(pars_zoo,xout=compiled_suna2_sum$dtp)
compiled_suna3<-merge(compiled_suna2_sum,pars,by="dtp")
compiled_suna3$PARWS_1min.PARMean<-na.approx(compiled_suna3$PARWS_1min.PARMean)
ggplot(subset(compiled_suna3,as.Date(dtp,tz="Etc/GMT+8")=="2019-07-22"),aes(dtp,a254.mean,color=PARWS_1min.PARMean))+
geom_point()





#pars_ss_temp$PARWS_1min.PARMean<-na.approx(pars_ss_temp$PARWS_1min.PARMean,na.rm=FALSE)
pars_ss_temp_m<-melt(pars_ss_temp[,c("dtp","a254.mean","a350.mean","PARWS_1min.PARMean.mean_na","s275295","spec_temp.mean")],id.vars=c("dtp"))
ggplot(subset(pars_ss_temp_m,as.Date(dtp,tz="Etc/GMT+8")=="2019-08-20"),aes(dtp,value,color=variable))+
geom_point()+
facet_wrap(.~variable,scales="free_y",ncol=1)

ggplot(subset(pars_ss_temp,as.Date(dtp,tz="Etc/GMT+8")=="2019-08-20"),aes(x=PARWS_1min.PARMean.mean_na,a350.mean))+
geom_point()


ggplot(pars_ss_temp,aes(x=PARWS_1min.PARMean.mean_na,s275295))+
geom_point()


#saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/MART/MART_pars_ss_temp.rds")
#saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/MART/MART_ss_plot2.rds")

pars_ss_temp<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_pars_ss_temp.rds")
ss_plot2<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_ss_plot2.rds")
	
	
pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
pars_ss_temp$lightb<-pars_ss_temp$PARWS_1min.PARMean.mean_na>1
udates<-unique(pars_ss_temp$date)
light_results<-data.frame()
for(i in 1:length(udates)){
	ss_temp<-subset(pars_ss_temp,date==udates[i])
	ss_temp$dtp_num<-as.numeric(ss_temp$dtp)
	light_daily<-sum(ss_temp$PARWS_1min.PARMean.mean_na)
	ss_temp_night<-subset(ss_temp,lightb==FALSE)
	night_lm<-lm(s275295~dtp_num,ss_temp_night)
	ss_temp$night_off<-predict(night_lm,newdata=ss_temp)
	ss_light<-subset(ss_temp,lightb==TRUE)
	ss_light$s275295_light<-ss_light$s275295-ss_light$night_off
	light_extra<-sum(ss_light$s275295_light)
	light_temp<-mean(ss_light$spec_temp.mean)
	temp<-mean(ss_temp$spec_temp.mean)
	mod_temp<-data.frame(date=udates[i],dark_ss=dark_ss,ss_light=light_extra,light_daily=light_daily,light_temp=light_temp,temp=temp)
	light_results<-bind_rows(light_results,mod_temp)
}

saveRDS(light_results,"~/gdrive/fdom/SUNA_NEON/MART/MART_light_results.rds")






#saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_pars_ss_temp.rds")
#saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_ss_plot2.rds")
	
pars_ss_temp<-readRDS("~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_pars_ss_temp.rds")
ss_plot2<-readRDS("~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_ss_plot2.rds")	
	
pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
pars_ss_temp$lightb<-pars_ss_temp$PARWS_1min.PARMean.mean_na>1
udates<-unique(pars_ss_temp$date)
light_results<-data.frame()
for(i in 1:length(udates)){
	ss_temp<-subset(pars_ss_temp,date==udates[i])
	ss_temp$dtp_num<-as.numeric(ss_temp$dtp)
	light_daily<-sum(ss_temp$PARWS_1min.PARMean.mean_na)
	ss_temp_night<-subset(ss_temp,lightb==FALSE)
	night_lm<-lm(s275295~dtp_num,ss_temp_night)
	ss_temp$night_off<-predict(night_lm,newdata=ss_temp)
	ss_light<-subset(ss_temp,lightb==TRUE)
	ss_light$s275295_light<-ss_light$s275295-ss_light$night_off
	light_extra<-sum(ss_light$s275295_light)
	light_temp<-mean(ss_light$spec_temp.mean)
	temp<-mean(ss_temp$spec_temp.mean)
	mod_temp<-data.frame(date=udates[i],dark_ss=dark_ss,ss_light=light_extra,light_daily=light_daily,light_temp=light_temp,temp=temp)
	light_results<-bind_rows(light_results,mod_temp)
}

saveRDS(light_results,"~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_light_results.rds")






#saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/WALK/WALK_pars_ss_temp.rds")
#saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/WALK/WALK_ss_plot2.rds")
	
pars_ss_temp<-readRDS("~/gdrive/fdom/SUNA_NEON/WALK/WALK_pars_ss_temp.rds")
ss_plot2<-readRDS("~/gdrive/fdom/SUNA_NEON/WALK/WALK_ss_plot2.rds")	
	
pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
pars_ss_temp$lightb<-pars_ss_temp$PARWS_1min.PARMean.mean_na>1
udates<-unique(pars_ss_temp$date)
light_results<-data.frame()
for(i in 1:length(udates)){
	ss_temp<-subset(pars_ss_temp,date==udates[i])
	ss_temp$dtp_num<-as.numeric(ss_temp$dtp)
	light_daily<-sum(ss_temp$PARWS_1min.PARMean.mean_na)
	ss_temp_night<-subset(ss_temp,lightb==FALSE)
	night_lm<-lm(s275295~dtp_num,ss_temp_night)
	ss_temp$night_off<-predict(night_lm,newdata=ss_temp)
	ss_light<-subset(ss_temp,lightb==TRUE)
	ss_light$s275295_light<-ss_light$s275295-ss_light$night_off
	light_extra<-sum(ss_light$s275295_light)
	light_temp<-mean(ss_light$spec_temp.mean)
	temp<-mean(ss_temp$spec_temp.mean)
	mod_temp<-data.frame(date=udates[i],dark_ss=dark_ss,ss_light=light_extra,light_daily=light_daily,light_temp=light_temp,temp=temp)
	light_results<-bind_rows(light_results,mod_temp)
}

saveRDS(light_results,"~/gdrive/fdom/SUNA_NEON/WALK/WALK_light_results.rds")








#saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/KING/KING_pars_ss_temp.rds")
#saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/KING/KING_ss_plot2.rds")
	
pars_ss_temp<-readRDS("~/gdrive/fdom/SUNA_NEON/KING/KING_pars_ss_temp.rds")
ss_plot2<-readRDS("~/gdrive/fdom/SUNA_NEON/KING/KING_ss_plot2.rds")	
	
pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
pars_ss_temp$lightb<-pars_ss_temp$PARWS_1min.PARMean.mean_na>1
udates<-unique(pars_ss_temp$date)
light_results<-data.frame()
for(i in 1:length(udates)){
	ss_temp<-subset(pars_ss_temp,date==udates[i])
	ss_temp$dtp_num<-as.numeric(ss_temp$dtp)
	light_daily<-sum(ss_temp$PARWS_1min.PARMean.mean_na)
	ss_temp_night<-subset(ss_temp,lightb==FALSE)
	night_lm<-lm(s275295~dtp_num,ss_temp_night)
	ss_temp$night_off<-predict(night_lm,newdata=ss_temp)
	ss_light<-subset(ss_temp,lightb==TRUE)
	ss_light$s275295_light<-ss_light$s275295-ss_light$night_off
	light_extra<-sum(ss_light$s275295_light)
	light_temp<-mean(ss_light$spec_temp.mean)
	temp<-mean(ss_temp$spec_temp.mean)
	mod_temp<-data.frame(date=udates[i],dark_ss=dark_ss,ss_light=light_extra,light_daily=light_daily,light_temp=light_temp,temp=temp)
	light_results<-bind_rows(light_results,mod_temp)
}

saveRDS(light_results,"~/gdrive/fdom/SUNA_NEON/KING/KING_light_results.rds")





#saveRDS(pars_ss_temp,"~/gdrive/fdom/SUNA_NEON/CARI/CARI_pars_ss_temp.rds")
#saveRDS(ss_plot2,"~/gdrive/fdom/SUNA_NEON/CARI/CARI_ss_plot2.rds")
	
pars_ss_temp<-readRDS("~/gdrive/fdom/SUNA_NEON/CARI/CARI_pars_ss_temp.rds")
ss_plot2<-readRDS("~/gdrive/fdom/SUNA_NEON/CARI/CARI_ss_plot2.rds")	
	
pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
pars_ss_temp$lightb<-pars_ss_temp$PARWS_1min.PARMean.mean_na>1
udates<-unique(pars_ss_temp$date)
light_results<-data.frame()
for(i in 1:length(udates)){
	ss_temp<-subset(pars_ss_temp,date==udates[i])
	ss_temp$dtp_num<-as.numeric(ss_temp$dtp)
	light_daily<-sum(ss_temp$PARWS_1min.PARMean.mean_na)
	ss_temp_night<-subset(ss_temp,lightb==FALSE)
	night_lm<-lm(s275295~dtp_num,ss_temp_night)
	ss_temp$night_off<-predict(night_lm,newdata=ss_temp)
	ss_light<-subset(ss_temp,lightb==TRUE)
	ss_light$s275295_light<-ss_light$s275295-ss_light$night_off
	light_extra<-sum(ss_light$s275295_light)
	light_temp<-mean(ss_light$spec_temp.mean)
	temp<-mean(ss_temp$spec_temp.mean)
	mod_temp<-data.frame(date=udates[i],dark_ss=dark_ss,ss_light=light_extra,light_daily=light_daily,light_temp=light_temp,temp=temp)
	light_results<-bind_rows(light_results,mod_temp)
}

saveRDS(light_results,"~/gdrive/fdom/SUNA_NEON/CARI/CARI_light_results.rds")



se <- function(x) {sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))}


SYCA_light_results<-readRDS("~/gdrive/fdom/SUNA_NEON/SYCA/SYCA_light_results.rds")
MART_light_results<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_light_results.rds")
WALK_light_results<-readRDS("~/gdrive/fdom/SUNA_NEON/WALK/WALK_light_results.rds")
KING_light_results<-readRDS("~/gdrive/fdom/SUNA_NEON/KING/KING_light_results.rds")
CARI_light_results<-readRDS("~/gdrive/fdom/SUNA_NEON/CARI/CARI_light_results.rds")

SYCA_light_results$site<-"SYCA"
MART_light_results$site<-"MART"
WALK_light_results$site<-"WALK"
KING_light_results$site<-"KING"
CARI_light_results$site<-"CARI"

lr<-bind_rows(SYCA_light_results,MART_light_results,WALK_light_results,KING_light_results,CARI_light_results)


ggplot(lr,aes(light_daily,ss_light,color=(site)))+
geom_point(size=2)

summary(lm(ss_light~light_daily+temp+light_daily*temp,lr))

lrs<-summaryBy(light_daily+ss_light+temp~site,lr,FUN=c(mean_na,se))
lrs$ss_light.mean_na[3]<-lrs$ss_light.mean_na[3]-0.016
lrs$ss_light.se[3]<-lrs$ss_light.se[3]-0.012

lrs$ss_light_min<-lrs$ss_light.mean_na-lrs$ss_light.se
lrs$ss_light_max<-lrs$ss_light.mean_na+lrs$ss_light.se
lrs$light_daily_min<-lrs$light_daily.mean_na-lrs$light_daily.se
lrs$light_daily_max<-lrs$light_daily.mean_na+lrs$light_daily.se




ggplot(lrs,aes(light_daily.mean_na,ss_light.mean_na))+
theme_ts_space+
geom_errorbar(aes(ymin=ss_light_min,ymax=ss_light_max),width=800,size=1)+
geom_errorbarh(aes(xmin=light_daily_min,xmax=light_daily_max),height=0.0006,size=1)+
geom_point(size=6)












	
	
	
	
	


cor_lm<-lm(a254~interp_254+I(interp_254^2)+sensor_temp,suna_d)
newdats<-data.frame(interp_254=suna_d$n_308.38,sensor_temp=suna_d$sensor_temp)
suna_d$n_308.38_cor<-predict(cor_lm,newdata=newdats)
ggplot(suna_d,aes(dtp,a254,color=sensor_temp))+
geom_point(size=2)

ggplot(suna_d,aes(dtp,n_308.38))+
geom_point(size=2)

suna_d_130<-suna_d

suna<-read.csv("/Users/jhosen/gdrive/MART/2019/D2019136.CSV",skip=14,header=FALSE)
#suna_d0<-cSplit(suna,3, ",")
names(suna)<-c("suna_id","date","time","nitrate","nitrate_mgl","a254","a350","bromide_trace","spec_average_dark","dark_signal_average","inter_time",suna_cal$Wavelength,"sensor_temp","spec_temp","lamp_temp","lamp_time","rel_hum","main_volt","lamp_volt","int_volt","main_current","fit_aux_1","fit_aux_2","fit_base_1","fit_base_2","fit_RMSE","CTD_Time","CTD_Salinity","CTD_Temp","CTD_Pressure","checksum")
suna<-subset(suna,a254!=0)
suna$day<-as.POSIXct('2019-05-11 00:00:00',tz="UTC")
suna$dtp<-suna$day + (3600*suna$time)
attr(suna$dtp,"tzone") <- "Etc/GMT+8"

offse<-c(t(suna_cal_t[3,]))
suna_d0_norm<-sweep(suna[,12:267],2,FUN="/",offse)
names(suna_d0_norm)<-paste("n_",names(suna_cal_t),sep="")
suna_d<-bind_cols(suna,suna_d0_norm)

summary(lm(a254~interp_254+I(interp_254^2)+sensor_temp,suna_d))
cor_lm<-lm(a254~interp_254+I(interp_254^2)+sensor_temp,suna_d)
newdats<-data.frame(interp_254=suna_d$n_308.38,sensor_temp=suna_d$sensor_temp)
suna_d$n_308.38_cor<-predict(cor_lm,newdata=newdats)
ggplot(suna_d,aes(dtp,a254,color=sensor_temp))+
geom_point(size=2)

ggplot(suna_d,aes(dtp,n_308.38_cor))+
geom_point(size=2)

suna_d_131<-suna_d

suna_d_combine<-bind_rows(suna_d_130,suna_d_131)

ggplot(suna_d_combine,aes(dtp,a254))+
geom_point(size=2)+
xlab("")+
ylab("ABS 254 nm")















suna<-read.csv("/Users/jhosen/gdrive/fdom/UNH_Data/SUNA Scan Data/SBM/TOA5_SBM_SUNARaw_20150527to20150716.dat",skip=4,header=FALSE)
suna_d00<-cSplit(suna,3, ",")
names(suna_d00)[1:3]<-c("datetime","index","suna_id")
suna_d<-suna_d00


suna_cal<-readRDS("/Users/jhosen/gdrive/fdom/SUNA Calibration pdfs/suna_cal_comb.rds")
suna_cal_0195<-subset(suna_cal,suna_id=="0196")
suna_cal_0195d<-dcast(suna_cal_0195,date~wavelength_nm,mean,value.var=c("counts"))
suna_cal_0195d$date<-as.Date(suna_cal_0195d$date)
wls<-ncol(suna_cal_0195d)-1
start<-16
end<-start+wls-1
names(suna_d)[start:end]<-names(suna_cal_0195d[2:ncol(suna_cal_0195d)])

#interpd<-Map(approx, xout=suna_d$date, x=suna_cal_0195d["date"], y=suna_cal_0195d[,-1], rule=2)

suna_d$date<-as.Date(suna_d$datetime)
for(i in 2:ncol(suna_cal_0195d)){
#temp<-data.frame(one=approx(x=suna_cal_0195d$date,y=suna_cal_0195d[,i],xout=suna_d$date)$y)
temp<-data.frame(one=rep(suna_cal_0195d[17,i],nrow(suna_d)))
names(temp)<-paste("b",names(suna_cal_0195d)[i],sep="")
print(names(temp))
suna_d<-bind_cols(suna_d,temp)
}


suna_d<-as.data.frame(suna_d)
for(i in 1:length(unique(suna_cal_0195$wavelength_nm))){

temp<-data.frame(one=suna_d[,grep(paste(unique(suna_cal_0195$wavelength_nm)[i]),names(suna_d))[1]]/suna_d[,grep(paste(unique(suna_cal_0195$wavelength_nm)[i]),names(suna_d))[2]])
names(temp)<-paste("trans_",unique(suna_cal_0195$wavelength_nm)[i],sep="")
temp2<-data.frame(two=(2-log10((temp*100)-10)))
names(temp2)<-paste("abs_",unique(suna_cal_0195$wavelength_nm)[i],sep="")
suna_d<-bind_cols(suna_d,temp,temp2)
}


#SBM = 43.1793,-71.2011
suna_d$dt<-as.POSIXct(suna_d$datetime,tz="Etc/GMT+5")
suna_d$solar.time <- streamMetabolizer::calc_solar_time(suna_d$dt, longitude=-71.2011)
suna_d$light<-calc_light(suna_d$solar.time,43.1793,-71.2011)


day<-subset(suna_d,date==as.Date("2015-06-17"))
#abs_254.19
ggplot(day,aes(dt,V3_003,color=V3_006))+
geom_point()+
scale_color_continuous("Temperature")+
xlab("")+
ylab("UVA-254")

ggplot(day,aes(dt,V3_003,color=light))+
geom_point()+
scale_color_continuous("Light")+
xlab("")+
ylab("UVA-254")

ggplot(day,aes(dt,abs_254.19,color=light))+
geom_point()


day$c254_right<-((10^(2-day$V3_005))/100)*day$b254.19
#day[,c("abs_254.19","c254_right")]
ggplot(day,aes(day$"254.19",c254_right,color=V3_002))+
geom_point()

summary(lm(abs_286.75~light,suna_d))








#ss_plot<-subset(compiled_suna2,date==2019190)
ss_plot<-subset(compiled_suna2)
ss_plot$dtp_round<-as.POSIXct(round(ss_plot$dtp,"mins"))
dtp_round_col0<-ncol(ss_plot)

ss_plot0<-ss_plot[,c(dtp_round_col0,grep("^n_",names(ss_plot)))] %>% mutate_if(is.character,as.numeric)
ss_plot2<-summaryBy(.~dtp_round,ss_plot0)

dtp_round_col<-ncol(ss_plot2)
ssm<-melt(ss_plot2,id.vars=c("dtp_round"))
ssm$wl_nm<-as.numeric(gsub(".mean","",gsub("n_","",ssm$variable,fixed=TRUE),fixed=TRUE))




pars_ss_temp$date<-as.Date(pars_ss_temp$dtp,tz="Etc/GMT+8")
days<-unique(pars_ss_temp$date)


for(i in 1:length(days)){
	temp_ss<-subset(pars_ss_temp,date==days[i])
	
#	temp_plot<-ggplot(temp_ss,aes(spec_temp.mean,s275295))+geom_point()
#	par_plot<-ggplot(temp_ss,aes(PARWS_1min.PARMean,s275295))+geom_point()
#	print(temp_plot+par_plot)
#	readline(prompt="Press [enter] to continue")
}




library(splitstackshape)

Well1new <- c(1955, 1965, 1975, 1985)
Well2new <- Well1new + 1
Well3new <- Well2new + 1
NewDates <- cbind.data.frame(Well1new, Well2new, Well3new)
Map(approx, xout=NewDates, x=Inputs["Dates"], y=Inputs[-1], rule=2)
