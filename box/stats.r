
#' @export
lmer_extr = function(lmer_obj) {
	n<-as.numeric(summary(lmer_obj)$devcomp$dims[c("n")])
	aicc<-AICc(lmer_obj)
	caic<-cAIC(lmer_obj)$caic
	log_lik<-as.numeric(summary(lmer_obj)$logLik)
	marg_r2<-rsquared(lmer_obj)[5]
	cond_r2<-rsquared(lmer_obj)[6]
	xx<-summary(lmer_obj)$call[2]
	call<-gsub("()","",xx,fixed=TRUE)
	stats<-cbind.data.frame(call,n,aicc,caic,log_lik,marg_r2,cond_r2)
	return(stats)
}


#' @export
mean_na = function(x) {mean(x,na.rm=TRUE)}