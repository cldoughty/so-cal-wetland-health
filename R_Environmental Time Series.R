setwd("files")

library(dplyr) 
require(tidyr) 
require(tidyverse)
library(lubridate)
require(ggplot2)
library(ggsci)
library(RColorBrewer)
library(viridis)
library(patchwork)
library(zoo)
library(scPDSI)
library(SPEI)
library(lfstat) 

# Environmental data prep: aggregate to monthly
#####
# NOAA Sea Level Data
NOAAstations = c('SantaBarbara', 'SanDiego', 'LaJolla', 'LosAngeles', 'SantaMonica')
ts_WLmonthly_all = NULL
for (i in 1:length(NOAAstations)){
  ts_WLhourly = read.csv(paste("./NOAA WL Data/", NOAAstations[i], "/", NOAAstations[i], "_WL_Hourly.csv", sep=''), head=TRUE, sep=",", stringsAsFactors=FALSE)

  #Aggregating water level data by month
  ts_WLmonthly = ts_WLhourly %>%
    mutate(Date_time = parse_date_time(x = data.t, orders = c("%m-%d-%Y %H:%M:%S", "%m-%d-%Y", "%m-%d-%Y %H:%M", "%m/%d/%Y %H:%M:%S"))) %>%
    mutate(Date_ym = format(as.Date(Date_time), "%Y-%m")) %>%
    arrange(Date_ym) %>%
    group_by(Date_ym) %>%
    summarise(SeaLevel_mean = mean(data.v)) %>%
    mutate(NOAA = NOAAstations[i]) %>%
    select(NOAA, Date_ym, SeaLevel_mean)
  
  ts_WLmonthly_all=rbind.data.frame(ts_WLmonthly_all, ts_WLmonthly)
}   

dateStart=as.Date("1984-01-01")
dateEnd=as.Date("2019-12-31")
DateSeq=seq.Date(dateStart, dateEnd, by="month")
DateSeq
Date_ymSeq = format(as.Date(DateSeq), "%Y-%m")
Date_ymSeq


#Fill in missing data
#Regional averages and filled using spline
ts_WLmonthly_all_avgs = ts_WLmonthly_all %>%
  group_by(Date_ym) %>%
  summarise(SeaLevel_mean=mean(SeaLevel_mean)) %>%
  mutate(SeaLevel_mean_RegAvg_filled=na.spline(SeaLevel_mean)) %>%
  select(-SeaLevel_mean)

#Create a month sequence for full time period
DateSeq=seq.Date(as.Date("1984-01-01"), as.Date("2019-12-31"), by="month")
Date_ymSeq = format(as.Date(DateSeq), "%Y-%m")
Date_ymSeq

ts_WLmonthly_all_filled <- ts_WLmonthly_all %>% 
  ungroup() %>%
  complete(nesting(NOAA), Date_ym = Date_ymSeq)%>%
  left_join(ts_WLmonthly_all_avgs, by="Date_ym") %>%
  #Swap SL for SL_filled
  mutate(SeaLevel_mean_filled=ifelse(is.na(SeaLevel_mean), SeaLevel_mean_RegAvg_filled, SeaLevel_mean))

ts_WLmonthly_all=ts_WLmonthly_all_filled


#####
#USGS Streamflow Data
USGSstreamflow_files = list.files(path="./USGS Streamflow Data",
                        pattern=".txt.csv", full.names = T)
ts_Streamflow_monthly_all = NULL

for(i in 1:length(USGSstreamflow_files)){
  ts_Streamflow_hourly = read.csv(USGSstreamflow_files[i], head=TRUE, sep=",")
  USGSsite_no = ts_Streamflow_hourly[1,2]
  
  ts_Streamflow_monthly = ts_Streamflow_hourly %>%
    mutate(Date=as.POSIXct(Date, origin="1970-01-01", tz="America/Los_Angeles", format="%Y-%m-%d")) %>%
    mutate(Date_ym = format(as.Date(Date), "%Y-%m")) %>%
    arrange(Date_ym) %>%
    group_by(Date_ym) %>%
    summarise(Discharge_mean_cms = sum(Discharge_mean_cfs*0.0283168)) %>%
    mutate(Site_no = USGSsite_no) %>%
    select(Site_no,Date_ym,Discharge_mean_cms)
  
  ts_Streamflow_monthly_all = rbind.data.frame(ts_Streamflow_monthly_all, ts_Streamflow_monthly)
}


#####
#PRISM Temp and Precip - calculate PDSI and SPEI
#this data is per site
input <- read.csv(file="../Time Series Sites.csv", head=TRUE, sep=",")
Sites=input$Site
Sites = levels(Sites)

# A function to help calculate PSDI 
funcPDSI = function(P, PE) {
  temp = pdsi(P,PE, AWC=100) 
  out = temp$X
}
# A funtion to help calulate PSI
funcSPI = function(P) {
  temp = spi(P,1) 
  out = temp$fitted 
}
#A function to help caluclate PSEI 
funcSPEI = function(P, PE) {
  temp = spei(P-PE,1) 
  out = temp$fitted 
}

for (i in 1:length(Sites)){
  filename = paste("./PRISM Data/", Sites[i], '.csv', sep = '')
  ts_raw = read.csv(filename, head=TRUE, sep=",",  skip = 10)
  ts_prepped = ts_raw %>% 
      rename_all(~ gsub("//.", "", .))
  
  siteLat = subset(input, Site==Sites[i], select=lat)

  ts_TempPrecip_monthly = ts_prepped %>%
    
    mutate(Date_ymd = as.Date(as.yearmon(Date, "%Y-%m"))) %>%
    mutate(Year = year(Date_ymd)) %>%
    mutate(Date_ym = Date) %>%
    mutate(WaterYear = water_year(Date_ymd, origin = "usgs", assign = "end")) %>%
    # PE (mm) from Thornwaite's
    mutate(PE = thornthwaite(tmeandegreesC, siteLat[1,1])) %>%
    #Calulate scPDSI
    mutate(PDSI = funcPDSI(pptmm,PE)) %>% 
    #Calulate SPI
    mutate(SPI = funcSPI(pptmm)) %>%
    #Calulate SPI
    mutate(SPEI = funcSPEI(pptmm, PE)) %>%
    select(Date, Date_ym, Date_ymd, Year, WaterYear, everything())
  
  write.csv(ts_TempPrecip_monthly, paste("./PRISM Data/", Sites[i], '_monthly.csv', sep = ''), row.names=FALSE)
}

#####
#ONI
ts_ONI = read.csv("./ONI/Monthly Oceanic Nino Index.csv", head=TRUE, sep=",")

#####
#PDO
ts_PDO = read.csv("./PDO/PDO.csv", head=TRUE, sep=",")

#####
#MEI (multivariate Enso Index) - bimonthly
ts_MEI = read.csv("./MEI/MEI.csv", head=TRUE, sep=",")

#####
# Visualize Environmental data
#####
# NOAA Sea Levels
#Fix the Santa Barbara gap for graphing
ts_WLmonthly_all2 = rbind.data.frame(ts_WLmonthly_all, cbind.data.frame(NOAA='SantaBarbara', Date_ym='1998-03', SeaLevel_mean=NA, SeaLevel_mean_RegAvg_filled=NA, SeaLevel_mean_filled=NA))
ts_WLmonthly_all3 = ts_WLmonthly_all2 %>% 
  mutate(Date = ymd(paste(Date_ym, "-01", sep="")))

#Spec filled NAs
ggplot() +
  geom_line(data = ts_WLmonthly_all3, aes(x=Date, y=SeaLevel_mean, color=NOAA), size=.50) +
  geom_line(data = ts_WLmonthly_all3, aes(x=Date, y=SeaLevel_mean_filled), size=.50) +
  scale_color_npg(name = 'NOAA Station') +
  scale_x_date(limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  #ggtitle('Mean Monlthy Sea Level') +
  geom_text(aes(x = as.Date("1984-06-01"), y = Inf, label="A) Mean Monlthy Sea Level"), hjust=0, vjust=1.5, size=3.5)+
  ylab('Sea Level (m NAVD88)') +
  xlab('') +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.box.margin=margin(0,0,0,-2, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm"))

p1=ggplot() +
  geom_line(data = ts_WLmonthly_all3, aes(x=Date, y=SeaLevel_mean, color=NOAA),  alpha=0.75) + #size=0.65,
  scale_color_npg(name = 'NOAA Station') +
  scale_y_continuous("Sea Level (m NAVD88)", expand = c(0, 0.1)) +
  
  scale_x_date("", limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  geom_text(aes(x = as.Date("1984-06-01"), y = Inf, label="A) Mean Monlthy Sea Level"), hjust=0, vjust=1.5, size=3.5)+
  theme_bw() +
  theme(axis.text.x=element_blank(),
        text = element_text(size = 10),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.box.margin=margin(0,0,0,-2, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm")) #top, right, bottom, left
p1

######
# CDIP wave data
input <- read.csv(file="../Time Series Sites.csv", head=TRUE, sep=",") #.. up one directory
Sites=input$Site
# Sites = levels(Sites)

ts_Waves_all=NULL
for (i in 1:length(Sites)){
  filename = paste("./CDIP/MOP Nearshore Pts/", Sites[i], "_monthly.csv",sep="")
  Site=Sites[i]
  CDIPregion = subset(input, Site==Sites[i], select=RSUsubregion)
  
  read.csv(file=filename, head=TRUE, sep=",")
  a=print(Site)
  b=print(read.csv(file=filename, head=TRUE, sep=","))
  c=print(CDIPregion)
  ts_Waves_all=rbind(ts_Waves_all, cbind.data.frame(Site,c,b))
}

ts_Waves_all2 = ts_Waves_all %>% 
  mutate(Date = ymd(paste(Date_ym, "-01", sep="")),
         CDIP_region2 = RSUsubregion)

pB=ggplot() +
  geom_line(data = ts_Waves_all2, aes(x=Date, y=waveHs_mean, color=CDIP_region2, linetype=Site),  alpha=0.75) +
  scale_color_discrete(name = "Wave Climate Subregions") +
  scale_x_date("", limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  scale_y_continuous("Wave Height (m NAVD88)", expand = c(0, 0.1)) +
  geom_text(aes(x = as.Date("1984-06-01"), y = Inf, label="B) Mean Monlthy Significant Wave Height"), hjust=0, vjust=1.5, size=3.5)+
  guides(linetype = FALSE) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        text = element_text(size = 10),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.box.margin=margin(0,0,0,-2, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm")) #top, right, bottom, left
pB

# USGS Stream Data
#Fix the Santa Barbara gap for graphing
ts_Streamflow_monthly_all2 = ts_Streamflow_monthly_all %>% 
  mutate(Date = ymd(paste(Date_ym, "-01", sep=""))) %>%
  group_by(Site_no) %>% 
  mutate(mx = max(Discharge_mean_cms)) %>% 
  arrange(desc(mx), desc(Discharge_mean_cms)) %>% 
  select(-mx) %>%
  filter(Site_no != "11047300") %>%
  filter(Site_no != "11046300")

# Define a unique palette
nb.cols <- 13
palette <- colorRampPalette(brewer.pal(12, "Greys"))(nb.cols) #"#F7FCF0", "#E7F6E2", 
palette = c("#D9F0D3", "#CCEBC5", "#B4E1BA", "#99D7BA", "#7BCCC4", "#5DBBCD", "#42A6CC", "#2B8CBE", "#1374B2", "#085A9D", "#084081") #, "#081D58","#000000")

p2=ggplot() +
  geom_col(data = ts_Streamflow_monthly_all2, aes(x=Date, y=Discharge_mean_cms, fill=as.factor(Site_no))) +
  #scale_color_npg(name = 'USGS Gauge') +
  # scale_fill_manual(name = 'USGS Gauge',
  #                   values=(palette)) +
  scale_fill_viridis(name = 'USGS Gauge', option="magma", discrete=TRUE) +
  guides(fill=guide_legend(ncol=2)) +
  scale_x_date("", limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  scale_y_continuous("Stream Flow (cms)", expand = c(0, 0), limits=c(0,13000)) +
  geom_text(aes(x = as.Date("1984-06-01"), y = Inf, label="C) Mean Monlthy Stream Flow"), hjust=0, vjust=1.5, size=3.5)+
  theme_bw()  +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text.x=element_blank(),
        text = element_text(size = 10),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.box.margin=margin(0,0,0,-1, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm")) #top, right, bottom, left
p2

# PRISM Temp and Precip
# Create regional means
ts_TempPrecip_all=NULL
for (i in 1:length(Sites)){
  filename = paste("./PRISM Data/", Sites[i], "_monthly.csv",sep="")
  Site=Sites[i]
  
  read.csv(file=filename, head=TRUE, sep=",")
  a=print(Site)
  b=print(read.csv(file=filename, head=TRUE, sep=","))
  
  ts_TempPrecip_all=rbind(ts_TempPrecip_all, cbind.data.frame(Site,b))
}

ts_TempPrecip_all2 = ts_TempPrecip_all %>%
  group_by(Date_ym) %>%
  summarise(ppt_mean = mean(pptmm),
            tmindegreesC_mean = mean(tmindegreesC, na.rm=TRUE),
            tmeandegreesC_mean = mean(tmeandegreesC, na.rm=TRUE),
            tmaxdegreesC_mean = mean(tmaxdegreesC, na.rm=TRUE),
            tdmeandegreesC_mean = mean(tdmeandegreesC, na.rm=TRUE),
            vpdminhPa_mean = mean(vpdminhPa, na.rm=TRUE),
            vpdmaxhPa_mean = mean(vpdmaxhPa, na.rm=TRUE),
            PDSI_mean = mean(PDSI, na.rm=TRUE),
            SPI_mean = mean(SPI, na.rm=TRUE),
            SPEI_mean = mean(SPEI, na.rm=TRUE)) %>%
  mutate(Date = ymd(paste(Date_ym, "-01", sep="")))

p3=ggplot() +
  data = ts_TempPrecip_all2, mapping = aes(x = Date, y = ppt_mean, group = 1)) +
  
  geom_line(data = ts_TempPrecip_all2, mapping = aes(x = Date, y = tmeandegreesC_mean*10), color="black", size=0.5) + # Scale data to match desired scale
  geom_bar(data = ts_TempPrecip_all2, aes(x = Date, y = ppt_mean), stat = "identity", color="blue", fill="blue", width = 0.5) + 
  scale_y_continuous("Precipitation (mm)", 
                     expand = c(0, 0),
                     sec.axis = sec_axis(~ . *0.1, name = "Temperature (?C)")) + # Reverse transformation to match data
  scale_x_date(limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0.1), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  geom_text(aes(x = as.Date("1984-06-01"), y = 360, label="D) Regional Mean Monthly Temperature and Precipitation"), hjust=0, vjust=1.5, size=3.5)+
  xlab('') +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        text = element_text(size = 10),
        plot.margin = margin(0, 0.5, 0, 0, "cm"))
p3

# Drought Indices
ts_TempPrecip_all3 = ts_TempPrecip_all2 %>%
  rename(PDSI=PDSI_mean,
         SPI=SPI_mean,
         SPEI=SPEI_mean) %>%
  mutate(SPEI= na_if(SPEI, -Inf)) %>%
  gather(key = Index, value = value, 
         c("PDSI", "SPI", "SPEI"))

p4=ggplot() + 
  geom_line(data = ts_TempPrecip_all3, aes(x=Date, y=value, group=Index, color=Index, linetype=Index,size=Index)) +
  # geom_line(data = ts_monthly_drought, aes(x=Date, y=PDO), color='blue') +
  geom_hline(yintercept = 0, color="grey33", size=0.5) +
  scale_size_manual(values = c(1,0.5,0.5)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_color_manual(values = c("black", "red", "black")) +
  scale_x_date(limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  geom_text(aes(x = as.Date("1984-06-01"), y = 7, label="E) Regional Monthly Drought Indices"), hjust=0, vjust=1.5, size=3.5)+
  ylab('Drought Index') +
  xlab('') +
  theme_bw() + 
  theme(axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.box.margin=margin(0,0,0,-3, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm")) #top, right, bottom, left
p4
# Climate Oscillations
ts_ONI = read.csv("./ONI/Monthly Oceanic Nino Index.csv", head=TRUE, sep=",")
ts_ONI = ts_ONI %>%
  mutate(Date_ym = as.factor(sprintf("%d-%02d", YEAR, MON.MMM))) %>%
  select(Date_ym, ONI, PHASE)

ts_PDO = read.csv("./PDO/PDO.csv", head=TRUE, sep=",")
ts_PDO = ts_PDO %>%
  filter(Date >= 198401) %>%
  mutate(Date_ym = as.factor(paste(substr(Date, 1, 4), substr(Date, 5, 6), sep = '-'))) %>%
  rename(PDO = Value) %>%
  select(Date_ym, PDO)

# MEI
ts_MEI = read.csv("./MEI/MEI.csv", head=TRUE, sep=",")
ts_MEI = ts_MEI %>%
  filter(YEAR >= 1984,
         YEAR < 2020) %>%
  mutate(Mon = rep(1:12, 36)) %>%
  mutate(Date_ym = as.factor(sprintf("%d-%02d", YEAR, Mon)))%>%
  select(Date_ym, MEI)

# Merge to NDVI TS by Date_ym
ts_monthly_drought = NULL
ts_monthly_drought = ts_ONI %>%
  left_join(ts_PDO, by='Date_ym') %>%
  left_join(ts_MEI, by='Date_ym') %>%
  mutate(Date = ymd(paste(Date_ym, "-01", sep=""))) %>%
  filter(Date > "1984-04-01", Date < "2020-01-01") %>%
  gather(key = Index, value = value, 
         c("ONI", "PDO", "MEI"))

p5=ggplot() + 
  geom_line(data = ts_monthly_drought, aes(x=Date, y=value, group=Index, color=Index, linetype=Index,size=Index)) +
  geom_hline(yintercept = 0, color="grey33", size=0.5) +
  scale_size_manual(values = c(1,0.5,0.5)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed")) +
  scale_color_manual(values = c("black", "blue", "black")) +
  scale_x_date(limits = c(as.Date("1984-01-01"),as.Date("2019-12-31")), expand = c(0, 0), date_minor_breaks = "1 years", breaks = seq(as.Date("1985-01-01"), as.Date("2015-01-01"), by="5 years"), date_labels = "%Y") +
  geom_text(aes(x = as.Date("1984-06-01"), y = 3.5, label="F) Regional Monthly Climate Indices"), hjust=0, vjust=1.5, size=3.5)+
  ylab('Climate Index') +
  xlab('') +
  theme_bw() + 
  theme(#axis.text.x=element_blank(),
        legend.title = element_blank(),
        legend.box.margin=margin(0,0,0,-3, "cm"),
        plot.margin = margin(0, 0, 0, 0, "cm")) #top, right, bottom, left

p5

p1/pB/p2/p3/p4/p5

ggsave(filename = "Environmental Time Series.png",
       path = "../Figures",
       bg = "transparent",
       scale = 1, width = 200, 
       height = 68*4, units = "mm", 
       dpi = 600, limitsize = TRUE)


#######################################################
#Calculate Stats and Trends in Regional data?

# Sea Level
ts_WLmonthly_all2

# Stream Discharge
ts_Streamflow_monthly_all2
  
# Temp and Precip and drought
ts_TempPrecip_all3b = ts_TempPrecip_all2 %>%
  rename(PDSI=PDSI_mean,
         SPI=SPI_mean,
         SPEI=SPEI_mean) %>%
  mutate(SPEI= na_if(SPEI, -Inf))

#Climate INdices
ts_monthly_droughtb = ts_ONI %>%
  left_join(ts_PDO, by='Date_ym') %>%
  left_join(ts_MEI, by='Date_ym') %>%
  mutate(Date = ymd(paste(Date_ym, "-01", sep=""))) %>%
  filter(Date > "1984-04-01", Date < "2020-01-01")
 
#Monthly
ts_monthly_enviro_REGION = ts_WLmonthly_all2 %>%
  left_join(ts_Waves_all2, by='Date_ym') %>%
  left_join(ts_Streamflow_monthly_all2, by='Date_ym') %>%
  left_join(ts_TempPrecip_all3b, by='Date_ym') %>%
  left_join(ts_monthly_droughtb, by='Date_ym') %>%
  mutate(Year = year(Date.x)) %>% 
  mutate(Comp = ifelse(Year <= 1989, 1986, ifelse(Year >= 2014, 2016, 999))) 
# %>%
#   select(-NOAA, -Site, -RSUsubregion,-CDIPregion,-CDIPregion2,-SeaLevel_mean_RegAvg_filled, -SeaLevel_mean_filled, -Site_no, -Date.x, -Date.y, -PHASE, -Date, -Date_ym)


# Summarize beginning and end values
ts_StartEnd_summary = ts_monthly_enviro_REGION %>%
  select(-RSUsubregion) %>%
  filter(Comp !=999) %>%
  select(-Year) %>%
  group_by(Comp) %>%
  summarise_all(list(mean, sd), na.rm=FALSE)

ts_monthly_enviro_REGION

#CDIP not in beginning...
test = ts_Waves_all2 %>%
  filter(Date <= as.Date("2005-12-31"))

mean(test$waveHs_mean)
sd(test$waveHs_mean)

# Summarize by year then get trend
ts_enviroTRENDS = ts_monthly_enviro_REGION %>%
    group_by(Year) %>%
    summarise(SeaLevel_mean = mean(SeaLevel_mean, na.rm=TRUE),
              #SeaLevel_mean_filled = mean(SeaLevel_mean_filled, na.rm=TRUE),
              Discharge_mean_cms_sum = sum(Discharge_mean_cms, na.rm=FALSE),
              ppt_sum = sum(ppt_mean, na.rm=TRUE),
              waveHs_mean = mean(waveHs_mean, na.rm=TRUE),
              tmindegreesC_mean = mean(tmindegreesC_mean, na.rm=TRUE),
              tmeandegreesC_mean = mean(tmeandegreesC_mean, na.rm=TRUE),
              tmaxdegreesC_mean = mean(tmaxdegreesC_mean, na.rm=TRUE),
              tdmeandegreesC_mean = mean(tdmeandegreesC_mean, na.rm=TRUE),
              vpdminhPa_mean = mean(vpdminhPa_mean, na.rm=TRUE),
              vpdmaxhPa_mean = mean(vpdmaxhPa_mean, na.rm=TRUE),
              PDSI_mean = mean(PDSI, na.rm=TRUE),
              SPI_mean = mean(SPI, na.rm=TRUE),
              SPEI_mean = mean(SPEI, na.rm=TRUE),
              ONI_mean = mean(ONI, na.rm=TRUE),
              PDO_mean = mean(PDO, na.rm=TRUE),
              MEI_mean = mean(MEI, na.rm=TRUE))

ts_enviroTRENDS = subset(ts_enviroTRENDS, Year!="NA")


#Sen's slope
library(trend)
list[[1]]
list=c("SeaLevel_mean","waveHs_mean","Discharge_mean_cms_sum","ppt_sum",
       "tmindegreesC_mean", "tmeandegreesC_mean","tmaxdegreesC_mean",
       "tdmeandegreesC_mean","vpdminhPa_mean","vpdmaxhPa_mean",
       "PDSI_mean","SPI_mean","SPEI_mean","ONI_mean","PDO_mean","MEI_mean")

sensTest = sens.slope(ts_enviroTRENDS$waveHs_mean,
                      conf.level = 0.95)
sensTest$estimates
sensTest$p.value


sensTest = sens.slope(ts_enviroTRENDS$ppt_sum,
                      conf.level = 0.95)
sensTest$estimates
sensTest$p.value


dat = ts_enviroTRENDS$Discharge_mean_cms_sum[!is.na(ts_enviroTRENDS$Discharge_mean_cms_sum)]
sensTest = sens.slope(dat,
                      conf.level = 0.95)
sensTest$estimates
sensTest$p.value


dat2 = ts_enviroTRENDS$ppt_sum[!is.na(ts_enviroTRENDS$ppt_sum)]
sensTest = sens.slope(dat2,
                      conf.level = 0.95)
sensTest$estimates
sensTest$p.value