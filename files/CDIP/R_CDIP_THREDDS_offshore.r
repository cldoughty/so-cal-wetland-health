setwd("C:/Users/chery/Google Drive/1 Research Projects/4 So Cal Wetlands Remote Sensing/Time Series/Data/CDIP")

library(ncdf4)
library(metR)
library(udunits2)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#####
#EX. this gets data from 1 prediction site outside Upper Newport Bay wetlands

# 71 HARVEST, CA
#historic (all) data
url_71 = 'https://thredds.cdip.ucsd.edu//thredds/dodsC/cdip/archive/071p1/071p1_historic.nc'

ncfile_HC <- ncdf4::nc_open(url_71)

#Spec metadata for vars
print(ncfile_HC$var)

# vars (hindcast only, repeat for nowcast)
df_waveHS_HC <- ReadNetCDF(ncfile_HC, vars = "waveHs", out = "data.frame") #significant wave height in meter
df_waveTp_HC <- ReadNetCDF(ncfile_HC, vars = "waveTp", out = "data.frame") #peak wave period in second
df_waveTa_HC <- ReadNetCDF(ncfile_HC, vars = "waveTa", out = "data.frame") #average wave period in second
df_waveDp_HC <- ReadNetCDF(ncfile_HC, vars = "waveDp", out = "data.frame") #peak wave direction in degreeT
df_waveDm_HC <- ReadNetCDF(ncfile_HC, vars = "waveDm", out = "data.frame") #bulk Sxy wave direction in degreeT
df_waveSxy_HC <- ReadNetCDF(ncfile_HC, vars = "waveSxy", out = "data.frame") #alongshore radiation stress in meter^2
df_waveSxx_HC <- ReadNetCDF(ncfile_HC, vars = "waveSxx", out = "data.frame") #onshore radiation stress in meter^2
df_waveFlagPrimary_HC <- ReadNetCDF(ncfile_HC, vars = "waveFlagPrimary", out = "data.frame") #primary wave QC flag
#not used: df_waveEnergyDensity_HC <- ReadNetCDF(ncfile_HC, vars = "waveEnergyDensity", out = "data.frame") #band energy density in meter^2 second - Dimensions: waveFrequency by waveTime


# 45 OCEANSIDE OFFSHORE, CA
# 92 SAN PEDRO, CA







#####
#EX. this gets data from 10 prediction site outside Upper Newport Bay wetlands UpperNewportBay
#loop through 10 neighboring prediction sites (points) around central prediction point at wetland mouth to get mean and SD CDIP wave data
reg = 'VE'
num = 376 # with region, the prediction site point label at the center of the estuary mouth
numSeq = seq(num-5, num+4, by = 1)
numSeq

df_wave = NULL
for(i in 1:length(numSeq)) {
  # url for hindacast data 
  url_HC = paste('https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/', #change VE for other 
                 reg,
                 numSeq[i], 
                 '_hindcast.nc', sep="")
  
  # url for nowcast data (2016 +)
  url_NC = paste('https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/',
                 reg,
                 numSeq[i], 
                 '_nowcast.nc', sep="")
  
  #open NetCDF file
  ncfile_HC <- ncdf4::nc_open(url_HC)
  ncfile_NC <- ncdf4::nc_open(url_NC)
  
  #Extract wave data
  #Hindcast
  df_waveHS_HC <- ReadNetCDF(ncfile_HC, vars = "waveHs", out = "data.frame") #significant wave height in meter
  df_waveTp_HC <- ReadNetCDF(ncfile_HC, vars = "waveTp", out = "data.frame") #peak wave period in second
  df_waveTa_HC <- ReadNetCDF(ncfile_HC, vars = "waveTa", out = "data.frame") #average wave period in second
  df_waveDp_HC <- ReadNetCDF(ncfile_HC, vars = "waveDp", out = "data.frame") #peak wave direction in degreeT
  df_waveDm_HC <- ReadNetCDF(ncfile_HC, vars = "waveDm", out = "data.frame") #bulk Sxy wave direction in degreeT
  df_waveSxy_HC <- ReadNetCDF(ncfile_HC, vars = "waveSxy", out = "data.frame") #alongshore radiation stress in meter^2
  df_waveSxx_HC <- ReadNetCDF(ncfile_HC, vars = "waveSxx", out = "data.frame") #onshore radiation stress in meter^2
  df_waveFlagPrimary_HC <- ReadNetCDF(ncfile_HC, vars = "waveFlagPrimary", out = "data.frame") #primary wave QC flag
  
  #Nowcast
  df_waveHS_NC <- ReadNetCDF(ncfile_NC, vars = "waveHs", out = "data.frame") #significant wave height in meter
  df_waveTp_NC <- ReadNetCDF(ncfile_NC, vars = "waveTp", out = "data.frame") #peak wave period in second
  df_waveTa_NC <- ReadNetCDF(ncfile_NC, vars = "waveTa", out = "data.frame") #average wave period in second
  df_waveDp_NC <- ReadNetCDF(ncfile_NC, vars = "waveDp", out = "data.frame") #peak wave direction in degreeT
  df_waveDm_NC <- ReadNetCDF(ncfile_NC, vars = "waveDm", out = "data.frame") #bulk Sxy wave direction in degreeT
  df_waveSxy_NC <- ReadNetCDF(ncfile_NC, vars = "waveSxy", out = "data.frame") #alongshore radiation stress in meter^2
  df_waveSxx_NC <- ReadNetCDF(ncfile_NC, vars = "waveSxx", out = "data.frame") #onshore radiation stress in meter^2
  df_waveFlagPrimary_NC <- ReadNetCDF(ncfile_NC, vars = "waveFlagPrimary", out = "data.frame") #primary wave QC flag
  
  #cbind hindcast and nowcast wave data per site #
  #Hindcast all wave data
  df_wave_HC = df_waveHS_HC %>%
    left_join(df_waveTp_HC, by='waveTime') %>%
    left_join(df_waveTa_HC, by='waveTime') %>%
    left_join(df_waveDp_HC, by='waveTime') %>%
    left_join(df_waveDm_HC, by='waveTime') %>%
    left_join(df_waveSxy_HC, by='waveTime') %>%
    left_join(df_waveSxx_HC, by='waveTime') %>%
    left_join(df_waveFlagPrimary_HC, by='waveTime')
  
  #Nowcast all wave data
  df_wave_NC = df_waveHS_NC %>%
    left_join(df_waveTp_NC, by='waveTime') %>%
    left_join(df_waveTa_NC, by='waveTime') %>%
    left_join(df_waveDp_NC, by='waveTime') %>%
    left_join(df_waveDm_NC, by='waveTime') %>%
    left_join(df_waveSxy_NC, by='waveTime') %>%
    left_join(df_waveSxx_NC, by='waveTime') %>%     
    left_join(df_waveFlagPrimary_NC, by='waveTime')
  
  #bind all 
  df_wave_temp = df_wave_HC %>%
    bind_rows(df_wave_NC) %>%
    arrange(waveTime)
  
  df_wave = rbind.data.frame(df_wave, df_wave_temp)
}

#Aggregate data by month
df_wave_monthly = df_wave %>%
  mutate(Date_ym = format(waveTime, "%Y-%m")) %>%
  arrange(Date_ym) %>%
  group_by(Date_ym) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/UpperNewportBay_monthly.csv", row.names=FALSE)
