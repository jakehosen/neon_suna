library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(neonUtilities)

sampling_dates<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NEON_field_data.csv')



nd<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/msb_neon_incubation/neon_sum_data_240207.csv')

sw<-readRDS('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NeonAncillaryData/sw_chemical_properties.rds')

head(sw$swc_externalLabAbsorbanceScan)
head(sw$swc_externalLabDataByAnalyte)


sd<-unique(nd[,c("site","deployDate")])

swd<-unique(sw$swc_externalLabDataByAnalyte[,c("siteID","collectDate")])

sd$month<-month(sd$deployDate)
sd$year<-year(sd$deployDate)


swd$month<-month(swd$collectDate)
swd$year<-year(swd$collectDate)


sd$siteID<-sd$site

swdd<-merge(sd,swd,by=c("siteID","month","year"),all.x=TRUE)

write.csv(swdd,"~/swdd.csv",row.names=FALSE)

NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJqaG9zZW5AcHVyZHVlLmVkdSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTkxNzQ0MTIxNSwiaWF0IjoxNzU5NzYxMjE1LCJlbWFpbCI6Impob3NlbkBwdXJkdWUuZWR1In0.6_se_h_9cRnjBbNVqKz_rdU8yKGv9d7jR6ATctsRdhPPurwQGO36d-dMys_6LUWTXKtDBTh4vmz9gaVFiLUxvA"


#wq_data <- loadByProduct(dpID = "DP1.20093.001", package = "basic", check.size = FALSE, include.provisional = TRUE, token = NEON_TOKEN)

saveRDS(wq_data,'/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NeonAncillaryData/wq_data_20251110.rds')

wq_data<-readRDS('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NeonAncillaryData/wq_data_20251110.rds')

grab_sensor<-wq_data[[11]]
grab_chem<-wq_data[[9]]
grab_uvabs<-wq_data[[8]]
grab_alk<-wq_data[[7]]



grab_chem$date<-as.Date(grab_chem$collectDate)
sampling_dates$date<-as.Date(sampling_dates$UTC_DateTime)
#sampling_dates$siteID<-sampling_dates$Site_ID




#unique(subset(grab_chem,siteID=="CARI" & year==2023)$collectDate)

grab_chem<-as.data.frame(grab_chem)

gccast<-reshape2::dcast(grab_chem[,c("siteID","date","analyte","analyteConcentration")],siteID+date~analyte,value.var="analyteConcentration",fun.aggregate=mean)


gccast[gccast$siteID=="REDB"&gccast$date=="2023-07-05","date"]<-NA
#gccast[gccast$siteID=="REDB"&gccast$date=="2023-07-18","date"]


library(data.table)

# Convert to data.tables
setDT(sampling_dates)
setDT(gccast)

# Set keys: site (exact match) + datetime (rolling match)
setkey(sampling_dates, siteID, date)
setkey(gccast, siteID, date)

# Rolling join within each site
merged <- gccast[sampling_dates, roll = "nearest"]
merged2<-as.data.frame(merged)
#merged$TOC

saveRDS(merged2,"/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NeonAncillaryData/")


redbgcc<-as.data.frame(subset(gccast,siteID=="REDB"))

redbgcc


sd_gc<-merge(sampling_dates,grab_chem,by=c("siteID","date"),all.x=TRUE)
sd_gc_cast<-dcast(sd_gc[,c("siteID","date","collectDate","analyte","analyteConcentration")],siteID+date+collectDate~analyte,value.var="analyteConcentration",fun.aggregate = mean)

#nec<-read.csv("/Users/jdh/neon_suna/r_scripts/neon_experiment_collectdates.csv")
#nec$deployDate<-as.Date(nec$deployDate,format="%m/%d/%y")
#sd$deployDate<-as.Date(sd$deployDate)
#nd$deployDate<-as.Date(nd$deployDate)

nd$siteID<-nd$site

nd2<-merge(nd,nec,by=c("siteID","deployDate"),all.x=TRUE)
nd2$date<-as.Date(nd2$collectDate,format="%m/%d/%y %H:%M")

swdata<-as.data.frame(sw$swc_externalLabDataByAnalyte)
swdata$date<-as.Date(swdata$collectDate)
sampling_dates$date<-as.Date(sampling_dates$UTC_DateTime)




swwide<-dcast(swdata[,c("siteID","date","collectDate","analyteConcentration","analyte")], siteID + date+ collectDate ~ analyte, mean,value.var="analyteConcentration") # average effect of time


nd3<-merge(nd2,swwide,by=c("siteID","date"),all.x=TRUE)



nd3c<-dcast(nd3[,c("siteID","season","treatment","doc_conc")],siteID+season~treatment,mean)
nd3_ss<-dcast(nd3[,c("siteID","season","treatment","SR")],siteID+season~treatment,mean)
nd3_comp5f<-dcast(nd3[,c("siteID","season","treatment","comp5f")],siteID+season~treatment,mean)
nd3_comp4f<-dcast(nd3[,c("siteID","season","treatment","comp4f")],siteID+season~treatment,mean)
nd3_comp3f<-dcast(nd3[,c("siteID","season","treatment","comp3f")],siteID+season~treatment,mean)
nd3_comp2f<-dcast(nd3[,c("siteID","season","treatment","comp2f")],siteID+season~treatment,mean)
nd3_comp1f<-dcast(nd3[,c("siteID","season","treatment","comp1f")],siteID+season~treatment,mean)


neon_meta<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/Active Grants/MSB NEON SUNA Project/NEON_Field_Site_Metadata_20250828.csv')
neon_meta$siteID<-neon_meta$field_site_id

nd3c$filt_photo<-nd3c$FPAR-nd3c$Ini
nd3c$photo<-nd3c$UPAR-nd3c$Ini
nd3c$resp_doc<-nd3c$UC-nd3c$FC

nd3c$photo_per_change<-nd3c$photo/nd3c$Ini*100

nd3c$s275_295_ini<-nd3_ss$Ini
nd3c$comp5f_ini<-nd3_comp5f$Ini
nd3c$comp4f_ini<-nd3_comp4f$Ini
nd3c$comp3f_ini<-nd3_comp3f$Ini
nd3c$comp2f_ini<-nd3_comp2f$Ini
nd3c$comp1f_ini<-nd3_comp1f$Ini

nd3c$uv_deg_doc<-nd3c$FU-nd3c$FC
nd3c2<-subset(nd3c,uv_deg_doc>-1.5)

nd3c30<-merge(nd3c2,neon_meta,by="siteID")



getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}



nd3c30<-merge(nd3c2,neon_meta,by="siteID")
nd3c30$season<-getSeason(nd3c30$deployDate)
swwide$season<-getSeason(swwide$date)

nd3c300<-merge(nd3c30,nec,by=c("siteID","deployDate"),all.x=TRUE)
nd3c300$date<-as.Date(nd3c300$collectDate,format="%m/%d/%y %H:%M")


nd3c3000<-merge(nd3c300,swwide,by=c("siteID","date"),all.x=TRUE)
nd3c3<-merge(nd3c3000,nd,by=c("siteID","deployDate"),all.x=TRUE)

nd3c3$uva_254<-nd3c3$"UV Absorbance (254 nm)"

nd3c3$field_mean_annual_temperature_C<-as.numeric(gsub("°C","",nd3c3$field_mean_annual_temperature_C,fixed=TRUE))

ggplot(nd3c3,aes(comp3f_ini,uv_deg_doc,color=season))+ geom_point(size=5)
summary(lm(comp3f_ini~uv_deg_doc,data=nd3c3))

ggplot(nd3c2,aes(comp1f_ini,resp_doc,color=season))+ geom_point(size=5)
summary(lm(resp_doc~field_mean_annual_temperature_C,data=nd3c3))

ggplot(nd3c3,aes(field_mean_annual_temperature_C,resp_doc,color=season))+ geom_point(size=3)


ggplot(nd3c,aes(site,fpar_net))+
geom_point(aes(color=season))

nd3c$fpar_net<-nd3c$FPAR-nd3c$Pre

# Function to create wide format with Ini-corrected doc concentrations
create_ini_corrected_wide <- function(df) {
  
  # Step 1: Calculate mean doc_conc for each treatment within site-deployDate
  # (average all replicates first)
  df_means <- df %>%
    group_by(site, deployDate, treatment) %>%
    summarise(
      doc_conc_mean = mean(doc_conc, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Step 2: Apply Ini correction to the mean values
  df_corrected <- df_means %>%
    group_by(site, deployDate) %>%
    mutate(
      # Find3 the Ini treatment's mean doc_conc for this site-deployDate combination
      ini_baseline = doc_conc_mean[treatment == "Ini"][1],  # Get Ini mean value
      # Subtract baseline from all treatment means (Ini will become 0)
      doc_conc_ini_corrected = doc_conc_mean - ini_baseline
    ) %>%
    ungroup()
  
  # Step 3: Pivot to wide format - each treatment becomes a column
  df_wide <- df_corrected %>%
    select(site, deployDate, treatment, doc_conc_ini_corrected) %>%
    pivot_wider(
      names_from = treatment,
      values_from = doc_conc_ini_corrected,
      names_prefix = "doc_"
    ) %>%
    arrange(site, deployDate)
  
  return(df_wide)
}

# Create the wide format dataframe
data_wide <- create_ini_corrected_wide(nd3)

# Display the results
print("Ini-corrected DOC concentrations in wide format:")
print(data_wide)

# Save to CSV
write_csv(data_wide, "ini_corrected_doc_wide_format.csv")

# Show summary information
print(paste("\nDataframe dimensions:", nrow(data_wide), "rows x", ncol(data_wide), "columns"))
print(paste("Unique site-deployDate combinations:", nrow(data_wide)))
print(paste("Treatment columns available:", paste(names(data_wide)[!names(data_wide) %in% c("site", "deployDate")], collapse = ", ")))

# Verify Ini correction (all doc_Ini values should be 0)
if ("doc_Ini" %in% names(data_wide)) {
  print(paste("\nVerification - All doc_Ini values should be 0:"))
  print(paste("Range of doc_Ini values:", round3(min(data_wide$doc_Ini, na.rm = TRUE), 6), "to", round3(max(data_wide$doc_Ini, na.rm = TRUE), 6)))
}

