
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

#Single file test
#'https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/VE376_hindcast.nc'
#hindcast data
url_HC = 'https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/VE376_hindcast.nc'
ncfile_HC <- ncdf4::nc_open(url_HC)
#nowcast data
url_NC = 'https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/VE376_nowcast.nc'
ncfile_NC <- ncdf4::nc_open(url_NC)
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/UpperNewportBay_monthly.csv", row.names=FALSE)


#####
#####
#AguaHedionda
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
#num = 376 # with region, the prediction site point label at the center of the mouth
numSeq = seq(830, 841, by = 1)

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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/AguaHedionda_monthly.csv", row.names=FALSE)

#####
#AlisoCanyonCreek
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D'
num = 1009 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/AlisoCanyonCreek_monthly.csv", row.names=FALSE)

#####
#AnaheimBay
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'OC'
num = 642 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/AnaheimBay_monthly.csv", row.names=FALSE)

#####
#BallonaWetlands
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'L0'
num = 646 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/BallonaWetlands_monthly.csv", row.names=FALSE)

#####
#BatiquitosLagoon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 769 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/BatiquitosLagoon_monthly.csv", row.names=FALSE)

#####
#BellCanyonCreek
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'B0'
num = 455 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/BellCanyonCreek_monthly.csv", row.names=FALSE)

#####
#BolsaChica
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'OC'
num = 555 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/BolsaChica_monthly.csv", row.names=FALSE)

#####
#BuenaVistaLagoon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 865 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/BuenaVistaLagoon_monthly.csv", row.names=FALSE)

#####
#Carpinteria
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'B00'
num = 63 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/Carpinteria_monthly.csv", row.names=FALSE)

#####
#DevereuxLagoon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'B0'
num = 419 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/DevereuxLagoon_monthly.csv", row.names=FALSE)

#####
#FrenchLagoon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D100'
num = 7 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/FrenchLagoon_monthly.csv", row.names=FALSE)

#####
#GoletaSlough
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'B0'
num = 355 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/GoletaSlough_monthly.csv", row.names=FALSE)

#####
#KendallFrost
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 362 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/KendallFrost_monthly.csv", row.names=FALSE)

#####
#LasFloresCreek
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D10'
num = 44 # with region, the prediction site point label at the center of the mouth
numSeq = seq(num-5, num+4, by = 1)
numSeq

df_wave = NULL
for(i in 1:length(numSeq)) {
  # url for hindacast data 
  url_HC = paste('https://thredds.cdip.ucsd.edu/thredds/dodsC/cdip/model/MOP_alongshore/',
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/LasFloresCreek_monthly.csv", row.names=FALSE)

#####
#LasPulgasCanyon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D10'
num = 47 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/LasPulgasCanyon_monthly.csv", row.names=FALSE)

#####
#LomaAltaSlough
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 880 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/LomaAltaSlough_monthly.csv", row.names=FALSE)

#####
#LosCerritosWetlands
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'OC'
numSeq = seq(645, 668, by = 1)
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/LosCerritosWetlands_monthly.csv", row.names=FALSE)

#####
#LosPenasquitos
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 589 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/LosPenasquitos_monthly.csv", row.names=FALSE)

#####
#MalibuCreek
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'L0'
num = 903 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/MalibuCreek_monthly.csv", row.names=FALSE)

#####
#MuguLagoon
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'VE'
num = 152 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/MuguLagoon_monthly.csv", row.names=FALSE)

#####
#OrmondBeach
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'VE'
num = 258 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/OrmondBeach_monthly.csv", row.names=FALSE)

#####
#SanDieguito
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 637 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SanDieguito_monthly.csv", row.names=FALSE)

#####
#SanElijo
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 682 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SanElijo_monthly.csv", row.names=FALSE)

#####
#SanLuisReyEstuary
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 929 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SanLuisReyEstuary_monthly.csv", row.names=FALSE)

#####
#SanOnofreCreek
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D'
num = 1194 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SanOnofreCreek_monthly.csv", row.names=FALSE)

#####
#SantaAnaWetlands
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'OC'
num = 646 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SantaAnaWetlands_monthly.csv", row.names=FALSE)

#####
#SantaClaraRiver
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'VE'
num = 397 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SantaClaraRiver_monthly.csv", row.names=FALSE)

#####
#SantaMargaritaEstuary
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 965 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SantaMargaritaEstuary_monthly.csv", row.names=FALSE)

#####
#SweetwaterMarsh
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D0'
num = 228 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/SweetwaterMarsh_monthly.csv", row.names=FALSE)

#####
#TalbertMarsh
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'OC'
num = 468 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/TalbertMarsh_monthly.csv", row.names=FALSE)

#####
#Tijuana
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'D000'
num = 20 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/Tijuana_monthly.csv", row.names=FALSE)

#####
#UpperNewportBay
#loop through 10 nearest prediction sites (points) outside wetland mouth to get mean and SD CDIP wave data
reg = 'VE'
num = 376 # with region, the prediction site point label at the center of the mouth
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
  filter(waveFlagPrimary<2) %>%
  summarise_all(list(mean=mean, sd=sd, min=min, max=max)) #, na.rm = TRUE

#Write csv to wetland Site name
write.csv(df_wave_monthly, "./MOP Nearshore Pts/UpperNewportBay_monthly.csv", row.names=FALSE)
