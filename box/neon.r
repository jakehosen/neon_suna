
#' @export
loadr = function(site,prodID,wd) {    
if(!file.exists(paste(wd,"/",site,"_",prodID,".rds",sep=""))){
    
    }
#MART_Nitrate<-loadByProduct(dpID="DP1.20033.001",site="MART",check.size=F)
#saveRDS(MART_Nitrate,"~/gdrive/fdom/SUNA_NEON/MART/MART_Nitrate.rds")
MART_Nitrate<-readRDS("~/gdrive/fdom/SUNA_NEON/MART/MART_Nitrate.rds")
    