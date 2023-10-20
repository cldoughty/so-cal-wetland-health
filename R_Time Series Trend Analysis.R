##Time series trend analysis and visualization
setwd("files")

#Packages
library(dplyr) 
library(tidyr) 
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggforce)
library(rgdal)
library(mapproj)
library(trend)
library(patchwork)
library(broom)

#Import and organize data
#Full list of sites for looping
input <- read.csv(file="./Time Series Sites.csv", head=TRUE, sep=",")
Sites=input$Site
# Sites = levels(Sites)

##################################
# TRENDS
##################################

#Create dataframe with annual NDVI time series per site
df_Annual=NULL
for (i in 1:length(Sites)){
  filename = paste("./Time Series w Environmental Data/", Sites[i], "_4annual.csv",sep="")
  read.csv(file=filename, head=TRUE, sep=",")
  a=print(read.csv(file=filename, head=TRUE, sep=","))
  df_Annual=rbind(df_Annual, a)
}

#####
# Calculate trends 
# all sites NDVI
df_Annualtrend=NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  fit <- lm(NDVI_mean ~ Year, data = df_site)
  
  Site=Sites[i]
  lmSlope=fit$coefficients[2]
  lmIntercept=fit$coefficients[1]
  lmR2=summary(fit)$r.squared
  lmPval=summary(fit)$coefficients[2,4]

  sensTest=sens.slope(df_site$NDVI_mean, conf.level = 0.95)
  sensSlope=sensTest$estimates
  sensPval=sensTest$p.value

  NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
  
  df_Annualtrend=rbind(df_Annualtrend, cbind.data.frame(Site,lmSlope,lmIntercept,lmR2,lmPval,sensSlope,sensPval,NDVI_sd_mean))
  rownames(df_Annualtrend) <- NULL
}

#all sites land Area
df_Annualtrend_land=NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  fit <- lm(landArea_mean ~ Year, data = df_site)
  
  Site=Sites[i]
  lmSlope=fit$coefficients[2]
  lmIntercept=fit$coefficients[1]
  lmR2=summary(fit)$r.squared
  lmPval=summary(fit)$coefficients[2,4]
  
  sensTest=sens.slope(df_site$landArea_mean, conf.level = 0.95)
  sensSlope=sensTest$estimates
  sensPval=sensTest$p.value
  
  NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
  
  df_Annualtrend_land=rbind(df_Annualtrend_land, cbind.data.frame(Site,lmSlope,lmIntercept,lmR2,lmPval,sensSlope,sensPval,NDVI_sd_mean))
  rownames(df_Annualtrend_land) <- NULL
}

#all sites water Area
df_Annualtrend_water=NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  fit <- lm(waterArea_mean ~ Year, data = df_site)
  
  Site=Sites[i]
  lmSlope=fit$coefficients[2]
  lmIntercept=fit$coefficients[1]
  lmR2=summary(fit)$r.squared
  lmPval=summary(fit)$coefficients[2,4]
  
  sensTest=sens.slope(df_site$waterArea_mean, conf.level = 0.95)
  sensSlope=sensTest$estimates
  sensPval=sensTest$p.value
  
  NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
  
  df_Annualtrend_water=rbind(df_Annualtrend_water, cbind.data.frame(Site,lmSlope,lmIntercept,lmR2,lmPval,sensSlope,sensPval,NDVI_sd_mean))
  rownames(df_Annualtrend_water) <- NULL
}

#all sites perc Water Area
df_Annualtrend_waterPerc=NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  fit <- lm(percWaterArea ~ Year, data = df_site)
  
  Site=Sites[i]
  lmSlope=fit$coefficients[2]
  lmIntercept=fit$coefficients[1]
  lmR2=summary(fit)$r.squared
  lmPval=summary(fit)$coefficients[2,4]
  
  sensTest=sens.slope(df_site$percWaterArea, conf.level = 0.95)
  sensSlope=sensTest$estimates
  sensPval=sensTest$p.value
  
  NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
  
  df_Annualtrend_waterPerc=rbind(df_Annualtrend_waterPerc, cbind.data.frame(Site,lmSlope,lmIntercept,lmR2,lmPval,sensSlope,sensPval,NDVI_sd_mean))
  rownames(df_Annualtrend_waterPerc) <- NULL
}

#all sites percWaterArea
df_Annualtrend_landPerc=NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  fit <- lm(percLandArea ~ Year, data = df_site)
  
  Site=Sites[i]
  lmSlope=fit$coefficients[2]
  lmIntercept=fit$coefficients[1]
  lmR2=summary(fit)$r.squared
  lmPval=summary(fit)$coefficients[2,4]
  
  sensTest=sens.slope(df_site$percLandArea, conf.level = 0.95)
  sensSlope=sensTest$estimates
  sensPval=sensTest$p.value
  
  NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
  
  df_Annualtrend_landPerc=rbind(df_Annualtrend_landPerc, cbind.data.frame(Site,lmSlope,lmIntercept,lmR2,lmPval,sensSlope,sensPval,NDVI_sd_mean))
  rownames(df_Annualtrend_landPerc) <- NULL
}

#All sites NDVI Trends by Pre Post Restoration
df_Annualtrend_rest=NULL
df_temp = NULL
for (i in 1:length(Sites)){
  df_site=subset(df_Annual, Site==Sites[i])
  Site=Sites[i]
  df_site_pre = subset(df_site, restStage=="Pre")
  df_site_post = subset(df_site, restStage=="Post")
  
  # If both pre and post have enough data....
  if ((dim(df_site_pre)[1] > 2) & (dim(df_site_post)[1] > 2)) {
    sensTest_pre=sens.slope(df_site_pre$NDVI_mean, conf.level = 0.95)
    sensSlope_pre=sensTest_pre$estimates
    sensPval_pre=sensTest_pre$p.value
    
    sensTestLAND_pre=sens.slope(df_site_pre$landArea_mean, conf.level = 0.95)
    sensSlopeLAND_pre=sensTestLAND_pre$estimates
    sensPvalLAND_pre=sensTestLAND_pre$p.value
    
    sensTest_post=sens.slope(df_site_post$NDVI_mean, conf.level = 0.95)
    sensSlope_post=sensTest_post$estimates
    sensPval_post=sensTest_post$p.value
    
    sensTestLAND_post=sens.slope(df_site_post$landArea_mean, conf.level = 0.95)
    sensSlopeLAND_post=sensTestLAND_post$estimates
    sensPvalLAND_post=sensTestLAND_post$p.value
    
    NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
    
    df_temp=cbind.data.frame(sensSlope_pre,sensPval_pre,sensSlope_post,sensPval_post,
                             sensSlopeLAND_pre,sensPvalLAND_pre,sensSlopeLAND_post,sensPvalLAND_post,
                             NDVI_sd_mean)
  } else {
    # if there is no restoration data
    if ((dim(df_site_pre)[1] == 0) & (dim(df_site_post)[1] == 0)) {
      df_temp=cbind.data.frame(sensSlope_pre="NA",sensPval_pre="NA",sensSlope_post="NA",sensPval_post="NA",
                               sensSlopeLAND_pre="NA",sensPvalLAND_pre="NA",sensSlopeLAND_post="NA",sensPvalLAND_post="NA",
                               NDVI_sd_mean="NA")
      
    } else {
      # if no post restoration
      if ((dim(df_site_pre)[1] > 2) & (dim(df_site_post)[1] == 0)) {
        sensTest_pre=sens.slope(df_site_pre$NDVI_mean, conf.level = 0.95)
        sensSlope_pre=sensTest_pre$estimates
        sensPval_pre=sensTest_pre$p.value
        
        sensTestLAND_pre=sens.slope(df_site_pre$landArea_mean, conf.level = 0.95)
        sensSlopeLAND_pre=sensTestLAND_pre$estimates
        sensPvalLAND_pre=sensTestLAND_pre$p.value
        
        NDVI_sd_mean = mean(df_site$NDVI_sd, na.rm=TRUE)
        
        df_temp=cbind.data.frame(sensSlope_pre,sensPval_pre,sensSlope_post="NA",sensPval_post="NA",
                                 sensSlopeLAND_pre,sensPvalLAND_pre,sensSlopeLAND_post="NA",sensPvalLAND_post="NA",
                                 NDVI_sd_mean="NA")
  }}} 
  df_Annualtrend_rest=rbind(df_Annualtrend_rest, cbind.data.frame(Site, df_temp))
  rownames(df_Annualtrend_rest) <- NULL
}


######################
#join results to site list
df_Annualtrend2 = left_join(input, df_Annualtrend, by = "Site") %>%
  mutate(abs_sensSlope=abs(sensSlope))

df_Annualtrend2 = df_Annualtrend2 %>%
  left_join(df_Annualtrend_land[,c("Site", "sensSlope","sensPval")], by = "Site", suffix = c("", "_land")) %>%
  left_join(df_Annualtrend_landPerc[,c("Site", "sensSlope","sensPval")], by = "Site", suffix = c("", "_landPerc")) %>%
  left_join(df_Annualtrend_water[,c("Site", "sensSlope","sensPval")], by = "Site", suffix = c("", "_water")) %>%
  left_join(df_Annualtrend_waterPerc[,c("Site", "sensSlope","sensPval")], by = "Site", suffix = c("", "_waterPerc")) %>%
  left_join(df_Annualtrend_rest, by = "Site", suffix = c("", ""))

# order factors for plotting
df_Annualtrend2$Archetype_2020Update <- factor(df_Annualtrend2$Archetype_2020Update, levels = c("Small creeks and lagoons","Intermediate estuary","Large lagoon","Large river valley estuary","Fragmented river valley estuary"))
df_Annualtrend2$RSUsubregion <- factor(df_Annualtrend2$RSUsubregion, levels = c("Gaviota Coast","Ventura Coast","Santa Monica Bay","San Pedro Bay","San Diego Coast"))
df_Annualtrend2$restorationActivities <- factor(df_Annualtrend2$restorationActivities, levels = c("None","Small","Large"))
df_Annualtrend2$CPAD_access <- factor(df_Annualtrend2$CPAD_access, levels = c("Open Access","Restricted Access","Closed/No Public Access"))
df_Annualtrend2$estuaryEngineering <- factor(df_Annualtrend2$estuaryEngineering, levels = c("None","Jettied Mouth","Channelized","Both"))
df_Annualtrend2$closureBin <- factor(df_Annualtrend2$closureBin, levels = c("predominantly open","intermittently open/closed","predominantly closed"))

#NDVI Trend stats (ndvi)
min(df_Annualtrend2$sensSlope)
max(df_Annualtrend2$sensSlope)
sigPos = subset(df_Annualtrend2, sensPval<=0.05 & sensSlope>0)
sigNeg = subset(df_Annualtrend2, sensPval<=0.05 & sensSlope<0)
Unsig = subset(df_Annualtrend2, sensPval>=0.05)

#Land trends
sigPos = subset(df_Annualtrend2, sensPval_land<=0.05 & sensSlope_land>0)
sigNeg = subset(df_Annualtrend2, sensPval_land<=0.05 & sensSlope_land<0)
Unsig = subset(df_Annualtrend2, sensPval_land>=0.05)

min(sigNeg$sensSlope_land*(0.0001))
max(sigNeg$sensSlope_land*(0.0001))
mean(sigNeg$sensSlope_land*(0.0001))
min(sigPos$sensSlope_land*(0.0001))
max(sigPos$sensSlope_land*(0.0001))

#Responses
GreenLandLoss = subset(df_Annualtrend2, sensSlope>0 & sensSlope_land<0)
GreenLandGain = subset(df_Annualtrend2, sensSlope>0 & sensSlope_land>0)
BrownLandLoss = subset(df_Annualtrend2, sensSlope<0 & sensSlope_land<0)
BrownLandGain = subset(df_Annualtrend2, sensSlope<0 & sensSlope_land>0)
NoChange = subset(df_Annualtrend2, sensSlope==0 & sensSlope_land==0)

df_Annualtrend2 = df_Annualtrend2 %>% 
  mutate(ResponseClass = ifelse(sensSlope>0 & sensSlope_land<0, "GreenLandLoss",
                            ifelse(sensSlope>0 & sensSlope_land>0, "GreenLandGain",
                                   ifelse(sensSlope<0 & sensSlope_land<0, "BrownLandLoss", "BrownLandGain")))) %>% 
  mutate(LandClass = ifelse(sensSlope_land<0, "Loss", "Gain")) %>%
  mutate(NDVIsig = ifelse(sensPval>=0.05, "Nonsig", "Sig"))

df_Annualtrend2$ResponseClass <- factor(df_Annualtrend2$ResponseClass, 
                                        levels = c("BrownLandLoss","GreenLandLoss","GreenLandGain"),
                                        labels = c("Browning + Loss", "Greening + Loss","Greening + Gain" ))

#####
#####
#Map trends by site
NDVI_palette2=c(
  # "#422112ff",
  "#9f512aff",
  "#cda914ff",
  "#fdea01",
  # "#fdfe01ff", #pretty yellow
  "#d0df03ff", #yellowgreen
  "#b9cf00ff",
  "#a2c000ff",
  "#8cb004ff",
  "#759d00ff",
  # "#5c8e03ff",
  "#458000ff",
  "#2d7000ff"
  # "#176100ff"
  # "#005200ff"
  )

#inset
world <- map_data("world")
rect=  data.frame(xmin=-120.5, xmax=-117, ymin=32.5, ymax=34.5, id="1")
inset = ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "gray60", color = "gray50") + 
  coord_fixed(xlim = c(-129, -64),  ylim = c(22, 58), ratio = 1.3)+
  geom_rect(data=rect, aes(xmin=xmin , xmax=xmax, ymin=ymin, ymax=ymax ), color="black", fill="transparent", size=1) + 
  theme_bw()+ 
  theme(axis.ticks=element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "gray97"),
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,-1,-1),"mm"))

#CA counties shapefile
shapefile <- readOGR("./GIS Data/CA shapefiles", layer="SoCal_counties") #Should be in WGS84

df_pie <- data.frame(group = c("Greening", "Browning", "No Change"),
                     value = c(18, 1, 13))
df_pie = df_pie %>% 
  mutate(end = 2 * pi * cumsum(value)/sum(value),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

df_pie2 <- data.frame(group = c("Loss", "Gain", "No Change"),
                      value = c(15, 5, 12))
df_pie2 = df_pie2 %>% 
  mutate(end = 2 * pi * cumsum(value)/sum(value),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

df_pie3 <- data.frame(group = c("Greening\n  + Gain", "Greening + Loss", "Browning + Loss"),
                      value = c(10, 16, 6))
df_pie3 = df_pie3 %>% 
  mutate(end = 2 * pi * cumsum(value)/sum(value),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

pie1 =  ggplot(df_pie) + 
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, start = start, end = end, fill = group)) +
    geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = group, hjust = hjust, vjust = vjust), size=3) +
    coord_fixed() +
    scale_fill_manual(values=c("#9f512aff", "#5c8e03ff", "gray"), guide=FALSE) +
    ggtitle("NDVI") +
  scale_x_continuous(limits = c(-2.3, 2.3), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.1, 1.1), name = "", breaks = NULL, labels = NULL) +
    theme_void() +
    theme(plot.title=element_text(size=9, face="bold",hjust = 0.5, vjust=-1.5))

pie2 =  ggplot(df_pie2) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, start = start, end = end, fill = group)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = group, hjust = hjust, vjust = vjust), size=3) +
  coord_fixed() +
  scale_fill_manual(values=c("#5c8e03ff", "#9f512aff", "gray"), guide=FALSE) +
  ggtitle("Wetland Area") +
  scale_x_continuous(limits = c(-2.3, 2.3), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.1, 1.1), name = "", breaks = NULL, labels = NULL) +
  theme_void() +
  theme(plot.title=element_text(size=9, face="bold",hjust = 0.5, vjust=-1.5))

pie3 =
  ggplot(df_pie3) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, start = start, end = end, fill = group)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = group, hjust = hjust, vjust = vjust), size=3) +
  coord_fixed() +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  ggtitle("Response") +
  scale_x_continuous(limits = c(-2.3, 2.3), name = "", breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(-1.1, 1.1), name = "", breaks = NULL, labels = NULL) +
  theme_void() +
  theme(plot.title=element_text(size=9, face="bold",hjust = 0.5, vjust=-1.5))

main=
  ggplot() + 
  geom_polygon(data = shapefile, mapping = aes(x = long, y = lat, group=group), fill = "gray60", color = "gray50")+ #"black")+
  geom_point(data = df_Annualtrend2, aes(x = long, y = lat,  color=sensSlope, shape=LandClass), size=2.5, alpha=1,  stroke = 2)+
  geom_point(data = subset(df_Annualtrend2, sensPval>=0.05), aes(x = long, y = lat, shape = LandClass), fill="gray", size = 3,  alpha=1, show.legend = FALSE)+ # shape=1, stroke = 2,
  # Add site display codes
  geom_text(data = subset(df_Annualtrend2, DisplayCodePosition==1), aes(x = long, y = lat, label = DisplayCode), size=2.7, fontface='bold', 
            angle = 45,
            hjust = 0, nudge_x = 0.04,nudge_y = 0.04,
            ) +
  geom_text(data = subset(df_Annualtrend2, DisplayCodePosition==2), aes(x = long, y = lat, label = DisplayCode), size=2.7, fontface='bold',
            angle = 45,
            hjust = 1, nudge_x = -0.04,nudge_y = -0.04,
  ) +
  
  # Add some things to the legend
  geom_point(data = data.frame(lat = 32.575, long = -118.67), aes(x = long, y = lat), color="black", size = 3, shape = 2, show.legend = FALSE)+
  geom_text(aes(x = -118.6, y = 32.575, label="Nonsignificant"), hjust=0, vjust=0.5, size=3)+
  geom_text(aes(x = -120.6, y = 34.3, label="Regional Trends"), hjust=-0.5, vjust=2, fontface='bold')+
  geom_text(aes(x = -119, y = 33.5, label="Site Trends"), hjust=-0.5, vjust=1, fontface='bold')+
  
  coord_fixed(xlim = c(-120.5, -117),  ylim = c(32.5, 34.55), ratio = 1.3)+
  scale_colour_gradientn(name = "NDVI Trend", colours = NDVI_palette2, limits=c(-0.002, 0.005)) + #,guide = "legend",limits=c(-0.002, 0.004), breaks=seq(-0.002, 0.004, by=0.001)) +
  scale_shape_manual(name = "Area Trend", values=c(2,6)) +
  guides(shape = guide_legend(order = 2)) +
  theme_linedraw(base_size = 10)+
  theme(axis.ticks.length=unit(-0.15, "cm"),
        axis.text.x = element_text(margin=margin(t = 10, r = 10, b = 10, l = 10, unit="pt")),
        axis.text.y = element_text(margin=margin(t = 10, r = 10, b = 10, l = 10, unit="pt")),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "gray97"),
        panel.grid = element_blank(),
        panel.border = element_rect(size=1),
        legend.direction = "vertical", 
        legend.box = "vertical",
        legend.position = c(0.5, 0.1),
        legend.justification=c(0, 0),
        legend.background = element_blank(),
        legend.key.size = unit(.4, "cm"))
  
g.inset = ggplotGrob(inset)
g.pie1 = ggplotGrob(pie1)
g.pie2 = ggplotGrob(pie2)
g.pie3 = ggplotGrob(pie3)
main + annotation_custom(grob = g.inset, xmin = -117.7, xmax = -116.9, ymin = 34, ymax = 34.65) +
  annotation_custom(grob = g.pie1, xmin = -120.6, xmax = -119.35, ymin = 33.65, ymax = 34.2) + #best height = 0.55
  annotation_custom(grob = g.pie2, xmin = -120.6, xmax = -119.35, ymin = 33.1, ymax = 33.65) +
  annotation_custom(grob = g.pie3, xmin = -120.6, xmax = -119.35, ymin = 32.55, ymax = 33.1)

ggsave(filename = "./Figures/Site_NDVItrends_051321.png",
       bg = "transparent", scale = 1, 
       width = 20, height = 20, units = "cm",
       dpi = 600)

#####
#####
# Compare trends to data
trend = df_Annualtrend2$sensSlope #NDVI
        # df_Annualtrend2$sensSlope_land*0.0001 #convert to Ha
        # df_Annualtrend2$sensSlope_landPerc
        # df_Annualtrend2$sensSlope_water
        # df_Annualtrend2$sensSlope_waterPerc
        # df_Annualtrend2$sensSlope_pre
        # df_Annualtrend2$sensSlope_post

#Latitude
lm_mod = lm(trend ~ lat, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod)

pLat=ggplot(df_Annualtrend2, aes(x=lat, y=trend)) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Latitude") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pLat

#Size
lm_mod = lm(trend ~ Area_m_updated, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pSize=ggplot(df_Annualtrend2, aes(x=Area_m_updated*0.0001, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,3))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Size") +
  xlab("Site Area (ha)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  # ylab("Percent Area Trend "~"(% yr"^"-1"*")")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pSize

#Elevation
lm_mod = lm(trend ~ Elev_mean, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pElev=ggplot(df_Annualtrend2, aes(x=Elev_mean, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Elevation") +
  xlab("Mean Elevation (m NAVD88)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pElev

#Archetype
#ANOVA
lm_mod = lm(trend ~ factor(ArchetypeCode_2020Update), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ Archetype_2020Update, data = df_Annualtrend2)
summary(anova)

pArch=ggplot(df_Annualtrend2, aes(x=Archetype_2020Update, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Archetype") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("SCL", "IE", "LL", "LRVE", "FRVE"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pArch

#Percent vegetated
lm_mod = lm(trend ~ Perc_Vegetated, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pHabComp=ggplot(df_Annualtrend2, aes(x=Perc_Vegetated, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Habitat Composition") +
  xlab("Percent Vegetated (%)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pHabComp

#RSU Wave Subregion
#ANOVA
lm_mod = lm(trend ~ factor(RSUsubregion), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ RSUsubregion, data = df_Annualtrend2)
summary(anova)

pWaveReg=ggplot(df_Annualtrend2, aes(x=RSUsubregion, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Wave Climate Subregion") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("Gaviota\nCoast", "Ventura\nCoast", "SM\nBay", "SP\nBay", "SD\nCoast"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pWaveReg

# Mouth Closure
#ANOVA
lm_mod = lm(trend ~ factor(closureBin), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ closureBin, data = df_Annualtrend2)
summary(anova)

pMouthClos=ggplot(df_Annualtrend2, aes(x=closureBin, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Mouth Closure") +
  xlab("Percent Closure Category")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("Open (<40%)", "Intermittent\n(40-60%)", "Closed (>60%)"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pMouthClos

#Percent Change in Adjacent Development ( DevelArea_PercChange   Devel2016_percentArea)
lm_mod = lm(trend ~ Devel2016_percentArea, data = df_Annualtrend2) 
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pDevel=ggplot(df_Annualtrend2, aes(x=Devel2016_percentArea, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Watershed Development") +
  xlab("Percent Developed Area (%)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pDevel

#Pop D Change in watershed
lm_mod = lm(trend ~ PopDensChange, data = df_Annualtrend2) 
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pPop=ggplot(df_Annualtrend2, aes(x=PopDensChange, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  ggtitle("Population Density") +
  xlab("Change in Population Density 2000-2020")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pPop

#Restoration Project Activities
lm_mod = lm(trend ~ factor(restorationActivities), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ restorationActivities, data = df_Annualtrend2)
summary(anova)

pRestSize=ggplot(df_Annualtrend2, aes(x=restorationActivities, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Restoration Activities") +
  xlab("Restoration Activities")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  #scale_x_discrete(labels = c("SCL", "IE", "LL", "LRVE", "FRVE"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pRestSize

#CPAD
#ANOVA
lm_mod = lm(trend ~ factor(CPAD_access), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ CPAD_access, data = df_Annualtrend2)
summary(anova)

pCPAD=ggplot(df_Annualtrend2, aes(x=CPAD_access, y=trend)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Protected Area Access") +
  xlab("Public Access Level")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("Open", "Restricted", "Closed"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pCPAD


#Mouth/Channel Engineering
#ANOVA
lm_mod = lm(trend ~ factor(estuaryEngineering), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ estuaryEngineering, data = df_Annualtrend2)
summary(anova)

pEngin=ggplot(df_Annualtrend2, aes(x=estuaryEngineering, y=trend)) +
  geom_boxplot() +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Mouth/Channel Engineering") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  # ylab("Area Trend "~"(ha yr"^"-1"*")")+
  # scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("None", "Jettied\nMouth", "Chanelized", "Both"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pEngin


#Compare to pre and post restoration trends NDVI
df_Annualtrend2_rest = df_Annualtrend2 %>%
  select(Site, sensSlope_pre, sensSlope_post, ResponseClass) %>%
  gather(restType, sensSlope, c(sensSlope_pre, sensSlope_post)) %>%
  filter(sensSlope!="NA") %>%
  mutate(sensSlope=as.numeric(sensSlope)) 

df_Annualtrend2_rest$restType <- factor(df_Annualtrend2_rest$restType, levels = c("sensSlope_pre", "sensSlope_post"))

anova=aov(sensSlope ~ restType, data = df_Annualtrend2_rest)
summary(anova)

#paired t-test
p <- df_Annualtrend2_rest %>%
  filter(duplicated(Site) | duplicated(Site, fromLast=TRUE)) %>%
  arrange(restType, Site)

ttest <- with(p, t.test(sensSlope ~ restType, paired=TRUE))
ttest
ttest_stats=glance(ttest)
ttest_stats

pPPrestNDVI=ggplot(df_Annualtrend2_rest, aes(x=restType, y=sensSlope)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_line(aes(group=Site), colour="grey50", linetype="11") +
  # geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = ttest_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Pre- and Post-Restoration Trends") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  scale_x_discrete(labels = c("Pre-Restoration", "Post-Restoration"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pPPrestNDVI

# some #s for the written results
wide = df_Annualtrend2_rest %>% 
  spread(restType, sensSlope) %>%
  mutate(trendChange = sensSlope_post-sensSlope_pre)

mean(wide$sensSlope_pre, na.rm=TRUE)
mean(wide$sensSlope_post, na.rm=TRUE)
mean(wide$trendChange, na.rm=TRUE)

mean(wide$sensSlope_post, na.rm=TRUE)-

mean(wide$trendChange, na.rm=TRUE)/mean(wide$sensSlope_post, na.rm=TRUE)

#Compare to pre and post restoration trends LAND
df_Annualtrend2_restLAND = df_Annualtrend2 %>%
  select(Site, sensSlopeLAND_pre, sensSlopeLAND_post, ResponseClass) %>%
  gather(restType, sensSlope, c(sensSlopeLAND_pre, sensSlopeLAND_post)) %>%
  filter(sensSlope!="NA") %>%
  mutate(sensSlope=as.numeric(sensSlope)*0.0001)

df_Annualtrend2_restLAND$restType <- factor(df_Annualtrend2_restLAND$restType, levels = c("sensSlopeLAND_pre", "sensSlopeLAND_post"))

anova=aov(sensSlope ~ restType, data = df_Annualtrend2_restLAND)
summary(anova)

#paired t-test
p <- df_Annualtrend2_restLAND %>%
  filter(duplicated(Site) | duplicated(Site, fromLast=TRUE)) %>%
  arrange(restType, Site)

ttest <- with(p, t.test(sensSlope ~ restType, paired=TRUE))
ttest
ttest_stats=glance(ttest)

pPPrestLAND=ggplot(df_Annualtrend2_restLAND, aes(x=restType, y=sensSlope)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_line(aes(group=Site), colour="grey50", linetype="11") +
  # geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = ttest_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  ggtitle("Pre- and Post-Restoration Trends") +
  xlab("")+
  ylab("Area Trend "~"(ha yr"^"-1"*")")+
  scale_y_continuous(limits=c(-10,2),breaks = seq(-10, 2, by=1)) +
  scale_x_discrete(labels = c("Pre-Restoration", "Post-Restoration"))+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_text(size=10))
pPPrestLAND

# some #s for the written results
wideLAND = df_Annualtrend2_restLAND %>% 
  spread(restType, sensSlope) %>%
  mutate(trendChange = sensSlopeLAND_post-sensSlopeLAND_pre)

mean(wide$sensSlope_pre, na.rm=TRUE)
mean(wide$sensSlope_post, na.rm=TRUE)
mean(wide$trendChange, na.rm=TRUE)

mean(wide$sensSlope_post, na.rm=TRUE)-
  
  mean(wide$trendChange, na.rm=TRUE)/mean(wide$sensSlope_post, na.rm=TRUE)

# Join
join = wide %>%
  left_join(wideLAND, by = "Site", suffix = c("", "_land"))

join2 = join %>%
  mutate(ResponseClass = ifelse(trendChange>0 & trendChange_land<0, "GreenLandLoss",
                                ifelse(trendChange>0 & trendChange_land>0, "GreenLandGain",
                                       ifelse(trendChange<0 & trendChange_land<0, "BrownLandLoss", "BrownLandGain")))) 


#############
# Bring Plots together (for Area Trend Supp Plot)
(pLat + pElev + pSize)/(pArch + pHabComp + pWaveReg)/(pMouthClos + pDevel + pCPAD)/(pEngin + pRestSize + pPPrestLAND)

ggsave(filename = "./Figures/Trends_SiteChars_Areatrend_Supp.png",
       bg = "transparent", scale = 1, 
       width = 28, height = 32, units = "cm",
       dpi = 600)


# Bring Plots together (for NDVI Trend Supp Plot)
(pLat + pElev + pSize)/(pArch + pHabComp + pWaveReg)/(pMouthClos + pDevel + pCPAD)/(pEngin + pRestSize + pPPrestNDVI)

ggsave(filename = "./Figures/Trends_SiteChars_NDVItrend_Supp.png",
       bg = "transparent", scale = 1, 
       width = 28, height = 32, units = "cm",
       dpi = 600)


### Final Figure 4
#Elevation NDVI
lm_mod = lm(sensSlope ~ Elev_mean, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pElev_NDVI=ggplot(df_Annualtrend2, aes(x=Elev_mean, y=sensSlope)) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  geom_text(aes(x = -Inf, y = Inf, label="A)"), hjust=-0.3, vjust=1.5, size=4.5)+
  ggtitle("Elevation") +
  xlab("")+#Mean Elevation (m NAVD88)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())
pElev_NDVI

#Elevation LAND
lm_mod = lm(sensSlope_land*0.0001 ~ Elev_mean, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pElev_LAND=ggplot(df_Annualtrend2, aes(x=Elev_mean, y=sensSlope_land*0.0001)) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  # ggtitle("Elevation") +
  geom_text(aes(x = -Inf, y = Inf, label="D)"), hjust=-0.3, vjust=1.5, size=4.5)+
  xlab("Mean Elevation (m NAVD88)")+
  ylab("Area Trend "~"(ha yr"^"-1"*")")+
  scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(panel.grid = element_blank())
pElev_LAND

#Size NDVI
lm_mod = lm(sensSlope ~ Area_m_updated, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pSize_NDVI=ggplot(df_Annualtrend2, aes(x=Area_m_updated*0.0001, y=sensSlope)) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,3))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  geom_text(aes(x = -Inf, y = Inf, label="B)"), hjust=-0.3, vjust=1.5, size=4.5)+
  ggtitle("Size") +
  xlab("")+#Site Area (ha)")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())
pSize_NDVI

#Size LAND
lm_mod = lm(sensSlope_land*0.0001 ~ Area_m_updated, data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

pSize_LAND=ggplot(df_Annualtrend2, aes(x=Area_m_updated*0.0001, y=sensSlope_land*0.0001)) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  geom_smooth(method = "lm", se = FALSE, color="black")+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,3))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,3))), hjust=1.2, vjust=2.6, parse = TRUE, size=4)+
  # ggtitle("Size") +
  geom_text(aes(x = -Inf, y = Inf, label="E)"), hjust=-0.3, vjust=1.5, size=4.5)+
  xlab("Site Area (ha)")+
  ylab("Area Trend "~"(ha yr"^"-1"*")")+
  scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())
pSize_LAND

#Archetype NDVI
#ANOVA
lm_mod = lm(sensSlope ~ factor(ArchetypeCode_2020Update), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ Archetype_2020Update, data = df_Annualtrend2)
summary(anova)

pArch_NDVI=ggplot(df_Annualtrend2, aes(x=Archetype_2020Update, y=sensSlope)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75,position=position_jitter(h=0.0001, w=0)) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(aes(x = -Inf, y = Inf, label="C)"), hjust=-0.3, vjust=1.5, size=4.5)+
  ggtitle("Archetype") +
  xlab("")+
  ylab("Trend "~"(NDVI yr"^"-1"*")")+
  scale_x_discrete(labels = c("SCL", "IE", "LL", "LRVE", "FRVE"))+
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())
pArch_NDVI

#Archetype LAND
#ANOVA
lm_mod = lm(sensSlope_land*0.0001 ~ factor(ArchetypeCode_2020Update), data = df_Annualtrend2)
summary(lm_mod)
lm_stats=glance(lm_mod) #lm stats

anova=aov(trend ~ Archetype_2020Update, data = df_Annualtrend2)
summary(anova)

pArch_LAND=ggplot(df_Annualtrend2, aes(x=Archetype_2020Update, y=sensSlope_land*0.0001)) +
  # geom_hline(yintercept = 0, color="grey") +
  geom_boxplot() +
  # geom_point(colour="black", alpha=0.75) +
  geom_point(aes(fill=ResponseClass), shape=21, size=2, alpha=0.75,position=position_jitter(h=0.1, w=0)) +
  scale_fill_manual(values=c("#9f512aff", "#d0df03ff", "#5c8e03ff"), guide=FALSE) +
  #geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("r^2 ==",round(r.squared,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  geom_text(data = lm_stats, aes(x = Inf, y = Inf, label=paste("p ==", round(p.value,2))), hjust=1.2, vjust=1.2, parse = TRUE, size=4)+
  # ggtitle("Archetype") +
  geom_text(aes(x = -Inf, y = Inf, label="F)"), hjust=-0.3, vjust=1.5, size=4.5)+
  
  xlab("Archetype")+
  ylab("Area Trend "~"(ha yr"^"-1"*")")+
  scale_y_continuous(limits=c(-4,2),breaks = seq(-4, 2, by=1)) +
  scale_x_discrete(labels = c("SCL", "IE", "LL", "LRVE", "FRVE"))+
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())
pArch_LAND

(pElev_NDVI + pSize_NDVI + pArch_NDVI)/(pElev_LAND + pSize_LAND + pArch_LAND)

ggsave(filename = "./Figures/Trends_SiteChars_trends_fig4.png",
       bg = "transparent", scale = 1, 
       width = 24, height = 16, units = "cm",
       dpi = 600)




#########################
#Site time series NDVI
#########################
df_Annual2 = left_join(input, df_Annual, by = "Site") %>%
  mutate(DisplayNamewCode = paste(DisplayName, " (", DisplayCode, ")", sep="")) 
# %>%
#   mutate(DisplayNamewCode = fct_reorder(DisplayNamewCode, Dist_NS_km))

ggplot() +
  geom_line(data = df_Annual2, aes(x = Year, y = NDVI_mean), color = "black", show.legend=FALSE) +
  geom_point(data = df_Annual2, aes(x = Year, y = NDVI_mean, color=NDVI_mean), show.legend=TRUE) +
  scale_colour_gradientn(name = "NDVI", colours = NDVI_palette2)+
  facet_wrap(~DisplayNamewCode, ncol = 4)+
  labs(x="", y="NDVI") +
  theme_bw()+
  theme(#strip.background =element_rect(fill="white"),
        strip.background = element_blank(), strip.placement = "outside",
        strip.text = element_text(size=7.5),
        legend.position="bottom"
        )


####################
# In Depth TS Plots
#####################
# Convert to long to graph as stacked bar
df_Annual2_long = df_Annual %>% 
  # mutate(percLandArea = (landArea_mean/(landArea_mean+waterArea_mean))*100,
  #        percWaterArea = (waterArea_mean/(landArea_mean+waterArea_mean))*100 ) %>%
  select(Site, Year, percLandArea, percWaterArea) %>%
  gather(AreaType, percArea, c(percLandArea, percWaterArea)) %>%
  mutate(AreaType = fct_rev(AreaType))


###################
#new figures - one site
df_Restoration = read.csv(file="./Restoration/RestorationHistory.csv", head=TRUE, sep=",", fileEncoding="UTF-8-BOM")

df_Restoration = df_Restoration %>%
  mutate(end = replace(end, end == 2020, 2019))


# TS with NDVI as lines and area as bars
for (i in 1:length(Sites)){
  
  siteName=Sites[i]
  
  df_Site_AnnualNDVIenviro = df_Annual2 %>%
    filter(Site==siteName)
  
  df_Site_Restoration = df_Restoration %>%
    filter(Include==1) %>%
    filter(site==siteName)
  
  df_Annual2_long_site = df_Annual2_long %>%
    filter(Site==siteName)
  
  if(dim(df_Site_Restoration)[1] == 0) {
    A = 
      ggplot()+
      geom_ribbon(data = df_Site_AnnualNDVIenviro, aes(x = Year, ymin = NDVI_mean-NDVI_sd, ymax = NDVI_mean+NDVI_sd), alpha = 0.15)+
      geom_line(data=df_Site_AnnualNDVIenviro, aes(x=Year, y=NDVI_mean), color="black", alpha=0.5)+
      geom_point(data=df_Site_AnnualNDVIenviro, aes(x=Year, y=NDVI_mean, color=NDVI_mean), shape=16)+
      geom_smooth(data = df_Site_AnnualNDVIenviro, aes(x = Year, y = NDVI_mean), method = "lm", formula='y ~ x', size=0.3, se=FALSE, color="black", linetype="dashed") +
      # geom_text(aes(x = -Inf, y = Inf, label=as.character(paste("", df_Site_AnnualNDVIenviro$Site, sep=""))), hjust=-0.2, vjust=1.5, size=5)+
      ggtitle(paste("", df_Site_AnnualNDVIenviro$DisplayNamewCode, sep="")) +
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      scale_y_continuous(name="NDVI", limits = c(0.1, 0.65)) +
      # scale_color_gradient(low="#9f855e", high= "#029a36", guide=FALSE)+
      scale_colour_gradientn(name = "NDVI", colours = NDVI_palette2,limits = c(0.14,0.53), guide = FALSE)+
      theme_bw(base_size = 8)+
      theme(#legend.position = c(0.9, 0.8),
        # axis.line.x.bottom = element_line(color = 'white'),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = margin(0.5, 0, 0, 0, "cm")) #top, right, bottom, left
    
    B =
      ggplot()+
      geom_bar(data= df_Annual2_long_site, aes(x= Year, y=percArea, fill=AreaType), alpha=0.7,
               stat="identity", position = "stack", width = 1, show.legend=FALSE) +
      scale_fill_manual(values=c("#3e8bd4","#6b9431")) + #,"#3e8bd4" 906d4b
      geom_smooth(data = subset(df_Annual2_long_site, AreaType=="percLandArea"), 
                  aes(x = Year, y = percArea), method = "lm", formula='y ~ x', size=0.3, se=FALSE, color="black", linetype="dashed") +
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      scale_y_continuous(name="Area (%)")+
      scale_color_gradient(low="#9f855e", high= "#029a36")+
      theme_bw(base_size = 8)+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            plot.margin = margin(-0.1, 0, 0, 0, "cm")) #top, right, bottom, left
    
    C =
      ggplot()+
      geom_col(data = df_Site_AnnualNDVIenviro, aes(x = Year, y = scene_count), width=0.5, show.legend=FALSE) +
      scale_y_continuous(name="Scene\nCount")+
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      theme_bw(base_size = 8) +
      theme(plot.margin = margin(-5, 0, 0, 0, "cm")) #top, right, bottom, left

    
    A/B/C + 
      plot_layout(heights = c(0.5, 0.5, 0.25))
  }
  else{
    A = 
      ggplot()+
      geom_rect(data = df_Site_Restoration, aes(xmin = start, xmax = end+1, ymin = -Inf, ymax = Inf),
                alpha = 0.15, fill = "black")+
      geom_ribbon(data = df_Site_AnnualNDVIenviro, aes(x = Year, ymin = NDVI_mean-NDVI_sd, ymax = NDVI_mean+NDVI_sd), alpha = 0.15)+
      geom_line(data=df_Site_AnnualNDVIenviro, aes(x=Year, y=NDVI_mean), color="black", alpha=0.5)+
      geom_point(data=df_Site_AnnualNDVIenviro, aes(x=Year, y=NDVI_mean, color=NDVI_mean), shape=16)+
      
      geom_smooth(data = subset(df_Site_AnnualNDVIenviro, restStage=="Pre"), aes(x = Year, y = NDVI_mean), method = "lm", formula='y ~ x', color="red", size=0.3, se=FALSE,  linetype="dashed") +
      geom_smooth(data = subset(df_Site_AnnualNDVIenviro, restStage=="Post"), aes(x = Year, y = NDVI_mean), method = "lm", formula='y ~ x', color="red", size=0.3, se=FALSE,  linetype="dashed") +
      
      geom_smooth(data = df_Site_AnnualNDVIenviro, aes(x = Year, y = NDVI_mean), method = "lm", formula='y ~ x', size=0.3, se=FALSE, color="black", linetype="dashed") +

      ggtitle(paste("", df_Site_AnnualNDVIenviro$DisplayNamewCode, sep="")) +
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      scale_y_continuous(name="NDVI", limits = c(0.1, 0.65)) +
      # scale_color_gradient(low="#9f855e", high= "#029a36", guide=FALSE)+
      scale_colour_gradientn(name = "NDVI", colours = NDVI_palette2,limits = c(0.14,0.53), guide = FALSE)+
      theme_bw(base_size = 8)+
      theme(#legend.position = c(0.9, 0.8),
        # axis.line.x.bottom = element_line(color = 'white'),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = margin(0.5, 0, 0, 0, "cm")) #top, right, bottom, left
    
    B =
      ggplot()+
      geom_rect(data = df_Site_Restoration, aes(xmin = start, xmax = end+1, ymin = -Inf, ymax = Inf),
                alpha = 0.15,  fill = "black")+
      geom_bar(data= df_Annual2_long_site, aes(x= Year, y=percArea, fill=AreaType), alpha=0.7,
               stat="identity", position = "stack", width = 1, show.legend=FALSE) +
      scale_fill_manual(values=c("#3e8bd4","#6b9431")) + #,"#3e8bd4" 906d4b
      
      # geom_smooth(data = subset(df_Annual2_long_site, restStage=="Pre"), aes(x = Year, y = percArea), method = "lm", formula='y ~ x', color="red", size=0.3, se=FALSE,  linetype="dashed") +
      # geom_smooth(data = subset(df_Annual2_long_site, restStage=="Post"), aes(x = Year, y = percArea), method = "lm", formula='y ~ x', color="red", size=0.3, se=FALSE,  linetype="dashed") +
      # 
      geom_smooth(data = subset(df_Annual2_long_site, AreaType=="percLandArea"), 
                  aes(x = Year, y = percArea), method = "lm", formula='y ~ x', size=0.3, se=FALSE, color="black", linetype="dashed") +
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      scale_y_continuous(name="Area (%)")+
      scale_color_gradient(low="#9f855e", high= "#029a36")+
      theme_bw(base_size = 8)+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x=element_blank(),
            plot.margin = margin(-0.1, 0, 0, 0, "cm")) #top, right, bottom, left
    
    C =
      ggplot()+
      geom_col(data = df_Site_AnnualNDVIenviro, aes(x = Year, y = scene_count), width=0.5, show.legend=FALSE) +
      scale_y_continuous(name="Scene\nCount", breaks = seq(0, 2015, by=5))+
      scale_x_continuous(name="", limits = c(1983, 2020), expand = c(0, 0), breaks = seq(1985, 2015, by=5), minor_breaks = seq(1984, 2019, by=1)) +
      theme_bw(base_size = 8)+
      theme(#legend.position = c(0.9, 0.8),
        plot.margin = margin(-1, 0, 0, 0.0, "cm")) #top, right, bottom, left
  }
  
  A/B/C + 
    plot_layout(heights = c(0.5, 0.5, 0.25))
  
  ggsave(filename = paste("./Figures/Site Time Series/", siteName, ".png", sep=""),
         bg = "transparent", scale = 1, 
         width = 9, height = 6, units = "cm",
         dpi = 600)
}