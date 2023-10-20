##Time series driver analysis

setwd("files")

#Packages
library(dplyr) 
library(tidyr) 
library(tidyverse)
library(ggplot2)
library(mgcv)
library(gratia)
library(tidyext)
library(visibly)


#Import and organize data
#Full list of sites for looping
input <- read.csv(file="./Time Series Sites.csv", head=TRUE, sep=",")
Sites = input$Site
input[1:7,1:7]

#####
# Bring in annual timeseries for regression and join to site input data
df_temp = NULL
df = NULL

for (i in 1:length(Sites)){
  #read in site enviro data
  filename = paste("./Time Series w Environmental Data/", Sites[i], "_4annual.csv",sep="")
  temp = read.csv(file=filename, head=TRUE, sep=",")
  
  df_temp = temp %>%
    left_join(input, by = 'Site') #%>%

  df = rbind(df, df_temp)
}

# Categorical vars as unordered factor
df <- transform(df, Site_uo=factor(Site, ordered=FALSE))
df <- transform(df, ArchetypeCode_2020Update=factor(ArchetypeCode_2020Update, ordered=FALSE))
df <- transform(df, subregion_uo=factor(RSUsubregion, ordered=FALSE))
df <- transform(df, restStage_uo=factor(restStage, ordered=FALSE))
df <- transform(df, restStage_uo=as.numeric(factor(restStage_uo)))


df = df %>% select(Site:restStage, Site_uo, ArchetypeCode_2020Update, subregion_uo, restStage_uo, NDVI_sd:MEI_mean)
colnames(df)

plot(df$SeaLevel_mean, df$NDVI_mean)

#########
# Inspect data distributions and descriptive stats
#########
# response vars
df_resp_long = df %>%
  gather(key = NDVIVar, value = value, 
         c("NDVI_sd":"waterArea_mean")) 

t_resp = df %>%
  select(c("NDVI_sd":"waterArea_mean")) %>%
  describe_all()
t_resp

ggplot(data = df_resp_long, aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_wrap(NDVIVar~., ncol=2, scales = 'free') +
  theme_bw()
shapiro.test(df$NDVI_max)

# explanatory vars
df_enviro_long = df %>%
  gather(key = EnviroVar, value = value, 
         c(SeaLevel_mean:MEI_mean))

df_enviro_long$EnviroVar <- factor(
  df_enviro_long$EnviroVar,
  levels = c(
    "SeaLevel_mean",
    "SeaLevel_mean_filled",
    "waveHs_max",
    "waveHs_sd",
    "waveHs_mean",
    "waveTp_mean",
    "waveTa_mean",
    "Discharge_mean_cms_sum",
    "Discharge_mean_cms_sum_WaterYr",
    "ppt_sum",
    "ppt_sum_WaterYr",
    "tmindegreesC_mean",
    "tmeandegreesC_mean",
    "tmaxdegreesC_mean",
    "tdmeandegreesC_mean",
    "vpdminhPa_mean",
    "vpdmaxhPa_mean",
    "PDSI_mean",
    "SPI_mean",
    "SPEI_mean",
    "ONI_mean",
    "PDO_mean",
    "MEI_mean"
  )
)

t_enviro = df %>%
  select(SeaLevel_mean:MEI_mean) %>%
  describe_all()
t_enviro

ggplot(data = df_enviro_long, aes(x = value)) + 
  geom_density(fill = "blue") +
  facet_wrap(EnviroVar~., ncol=3, scales = 'free') +
  theme_bw()


# Multicollinearity of Environmental Vars (Correlation matrix)
df_enviro = df %>%
  select(SeaLevel_mean:MEI_mean, -SeaLevel_mean_filled) %>%
  rename(
    "Sea Level"="SeaLevel_mean",
    "Max Wave Height" = "waveHs_max",
    "SD Wave Height" = "waveHs_sd",
    "Mean Wave Height" = "waveHs_mean",
    "Wave Period" = "waveTp_mean",
    "Peak Wave Direction" = "waveTa_mean",
    "Stream Discharge"= "Discharge_mean_cms_sum",
    "Stream Discharge (Water Yr)"= "Discharge_mean_cms_sum_WaterYr",
    "Precip"= "ppt_sum",
    "Precip (Water Yr)"= "ppt_sum_WaterYr",
    "Temp Min"= "tmindegreesC_mean",
    "Temp Mean"= "tmeandegreesC_mean",
    "Temp Max"= "tmaxdegreesC_mean",
    "Dew Point Temp"= "tdmeandegreesC_mean",
    "VPD Min"= "vpdminhPa_mean",
    "VPD Max"= "vpdmaxhPa_mean",
    "PDSI"= "PDSI_mean",
    "SPI"= "SPI_mean",
    "SPEI"= "SPEI_mean",
    "ONI"= "ONI_mean",
    "PDO"= "PDO_mean",
    "MEI"= "MEI_mean"
  ) 

# Compute a correlation matrix
library(rstatix)
cor.mat <- df_enviro %>%
  cor_mat(method = "spearman")

# Compute a matrix of correlation p-values
cor.p = cor.mat %>% cor_get_pval() 

library(ggcorrplot)
ggcorrplot(cor.mat,  type = "lower", insig = "blank",
           lab = TRUE, lab_size = 3,
           p.mat = as.matrix(corrP),
           outline.col = "white",
           show.legend = FALSE,
           tl.cex=10,
           ggtheme = ggplot2::theme_classic,
           colors = c("#6D9EC1", "white", "#E46726"))



#########
# Regression models
#########

#####
# linear model
mod_lm = lm(NDVI_mean ~ landArea_mean + SeaLevel_mean + ppt_sum + tmindegreesC_mean, data=df)
summary(mod_lm)
AIC(mod_lm)

plot_coefficients(lm(NDVI_mean ~ landArea_mean + SeaLevel_mean + ppt_sum + tmindegreesC_mean, data=df),
                  #palette = 'oslo',
                  #order = 'decreasing',
                  sd_multi = 1,
                  keep_intercept = TRUE,
                  ref_line = c(-1:1))

#####
#  GAM model development: model variable testing using penalization

mod_gam1 = gam(NDVI_mean ~ s(SeaLevel_mean) + s(ppt_sum) + s(tmindegreesC_mean), 
               data=df, family=gaussian(link=log), method="REML", select = TRUE, na.action = na.exclude)
summary(mod_gam1)
AIC(mod_gam1)
plot_gam(mod_gam1) 
gam.check(mod_gam1)
plot.gam(mod_gam1, seWithMean = TRUE)
draw(mod_gam1)

mod_summary = summary(mod_gam1)
mod_summary$cov.scaled #?
mod_summary$p.table
mod_summary$s.table
mod_summary$chi.sq

coef(mod_gam1)


library(caret)
varImp(mod_gam1)

# all variables
mod_gam5 = gam(NDVI_mean ~ s(SeaLevel_mean) + 
                 #s(SeaLevel_mean_filled) + 
                 s(waveHs_mean) + 
                 #s(waveTp_mean) + 
                 #s(waveTa_mean) + 
                 #s(waveHs_max) +
                 s(ppt_sum) + 
                 # s(ppt_sum_WaterYr) +
                 s(Discharge_mean_cms_sum) + 
                 # s(Discharge_mean_cms_sum_WaterYr) +
                 s(vpdmaxhPa_mean) + 
                 #s(vpdminhPa_mean) + 
                 s(tmeandegreesC_mean) + 
                 # s(tmindegreesC_mean) +
                 s(tmaxdegreesC_mean) +
                 # s(tdmeandegreesC_mean) +
                 # s(PDSI_mean) +
                 # s(SPI_mean) +
                 # s(SPEI_mean) +
                 # s(ONI_mean) +
                 # s(PDO_mean) +
                 # s(MEI_mean)
               # s(Year) #+ 
                s(Site_uo, bs="re")
               ,  
               data=df, family=gaussian(link=log), method="REML", select = TRUE)

#percLandArea*0.01 landArea_mean*0.0001
mod_gam5 = gam(percLandArea*0.01 ~ s(SeaLevel_mean) + 
                 #s(SeaLevel_mean_filled) + 
                 s(waveHs_mean) + 
                 #s(waveTp_mean) + 
                 #s(waveTa_mean) + 
                 #s(waveHs_max) +
                 s(ppt_sum) + 
                 # s(ppt_sum_WaterYr) +
                 s(Discharge_mean_cms_sum) + 
                 # s(Discharge_mean_cms_sum_WaterYr) +
                 s(vpdmaxhPa_mean) + 
                 #s(vpdminhPa_mean) + 
                 s(tmeandegreesC_mean) + 
                 s(tmindegreesC_mean) +
                 s(tmaxdegreesC_mean) +
                 s(tdmeandegreesC_mean) +
                 s(PDSI_mean) +
                 s(SPI_mean) +
                 s(SPEI_mean) +
                 s(ONI_mean) +
                 s(PDO_mean) +
                 s(MEI_mean)
                 # s(Year) #+ 
                 # s(Site_uo, bs="re")
               ,  
               data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(mod_gam5)
AIC(mod_gam5)
plot_gam(mod_gam5)
plot_gam_check(mod_gam5)
gam.check(mod_gam5, k.rep=1000)
concurvity(mod_gam5) 
draw(mod_gam5)
viz.gam(mod_gam5)
AIC(mod_gam1, mod_gam5)
anova(mod_gam1, mod_gam5, test="Chisq")


###################
# Hierarchial GAMs (following Pederson tutorial)
###################

#####
# Global Model (G) EXAMPLE
modG <- gam(NDVI_mean ~ s(SeaLevel_mean, # the term to be smoothed
                               k=10,          # the maximum number of basis functions; defarult=10; number of observations... per year of ts? 35
                               bs="tp") +     # type of smoother; tp=TPRS smoother; ts=thin-plate regression spline
                   s(Site_uo,                 # the term to be smoothed
                     k=32,                    # 32 sites; random effect smoother always has a k value equal to the number of levels in the grouping variable 
                     bs="re"),                # type of smoother; re=random effects
                 data=df, 
                 method="REML",               # the smoothing parameter estimation method; REML=Restricted Maximum Likelihood
                 family=gaussian(link=log),   # family object specifying the distribution and link to use in fitting; See family.mgcv
                 select=TRUE,                # add extra penalty that can remove terms from model
                 na.action = na.exclude)
summary(modG)
AIC(modG)
draw(modG)                               


#################################################
######## NDVI Results begin
# Global model - super simple
NDVI_mod1 = gam(NDVI_mean ~ 
                   s(SeaLevel_mean) + 
                   s(ppt_sum) + 
                   s(tmeandegreesC_mean)
                 ,  
                 data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(NDVI_mod1)
AIC(NDVI_mod1)
plot_gam(NDVI_mod1) 
plot_gam_check(NDVI_mod1)
gam.check(NDVI_mod1, k.rep=1000) 
concurvity(NDVI_mod1) 
draw(NDVI_mod1)
viz.gam(NDVI_mod1)

varImp(NDVI_mod1)

gam.vcomp(NDVI_mod1)
NDVI_mod1$converged

# Global model - simple
NDVI_mod2 = gam(NDVI_mean ~ 
                  s(SeaLevel_mean) + 
                  s(tmeandegreesC_mean) +
                  s(ppt_sum) + 
                  s(waveHs_mean) + 
                  s(Discharge_mean_cms_sum) +  
                  s(vpdmaxhPa_mean)
                ,  
                data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(NDVI_mod2)
AIC(NDVI_mod2)
plot_gam(NDVI_mod2)
plot_gam_check(NDVI_mod2)
gam.check(NDVI_mod2, k.rep=1000)
concurvity(NDVI_mod2) 
draw(NDVI_mod2, residuals = TRUE)

AIC(NDVI_mod1, NDVI_mod2)
anova(NDVI_mod1, NDVI_mod2, test="F")

varImp(NDVI_mod2, scale=FALSE, useModel = TRUE, nonpara = TRUE)


# Global model - simple with site term
NDVI_modG = gam(NDVI_mean ~ 
                   s(SeaLevel_mean) + 
                   s(tmeandegreesC_mean) + 
                   s(ppt_sum) + 
                   s(waveHs_mean) + 
                   s(Discharge_mean_cms_sum) +  
                   s(vpdmaxhPa_mean) +
                   s(Site_uo,                 # the term to be smoothed
                     k=32,                    # 32 sites; random effect smoother always has a k value equal to the number of levels in the grouping variable 
                     bs="re"),  
                 data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(NDVI_modG)
AIC(NDVI_modG)
plot_gam(NDVI_modG)
plot_gam_check(NDVI_modG)
gam.check(NDVI_modG, k.rep=1000)
concurvity(NDVI_modG) 
draw(NDVI_modG)
viz.gam(NDVI_modG)

AIC(NDVI_mod1, NDVI_mod2, NDVI_modG)

coef(NDVI_modG)
vcov(NDVI_modG)
sqrt(diag(vcov(NDVI_modG)))

intervals(NDVI_modG)
gam.vcomp(NDVI_modG)
anova(NDVI_modG)
# plot(NDVI_modG)

varImp(NDVI_modG, scale=TRUE)


# Global model - simple with archetype term
NDVI_modG_arch = gam(NDVI_mean ~ 
                  s(SeaLevel_mean) + 
                  s(tmeandegreesC_mean) +
                  s(ppt_sum) + 
                  s(waveHs_mean) + 
                  s(Discharge_mean_cms_sum) +  
                  s(vpdmaxhPa_mean) +
                  s(ArchetypeCode_2020Update,                 # the term to be smoothed
                    k=6,                    # 32 sites; random effect smoother always has a k value equal to the number of levels in the grouping variable 
                    bs="re"),  
                data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(NDVI_modG_arch) # p should be well below 0.05 to assume effects
AIC(NDVI_modG_arch)
plot_gam(NDVI_modG_arch) #, main_var = SeaLevel_mean)
plot_gam_check(NDVI_modG_arch)
gam.check(NDVI_modG_arch, k.rep=1000) #k looks good
concurvity(NDVI_modG_arch) 
draw(NDVI_modG_arch)
viz.gam(NDVI_modG_arch)

# a null model
NDVI_modNULL = gam(NDVI_mean ~ 1,  
                     data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(NDVI_modNULL)
NDVI_modNULL$converged
AIC(NDVI_modNULL)
plot_gam(NDVI_modNULL) 
plot_gam_check(NDVI_modNULL)
gam.check(NDVI_modNULL, k.rep=1000) 
concurvity(NDVI_modNULL) 
draw(NDVI_modNULL)
viz.gam(NDVI_modNULL)

#comparing models
anova(NDVI_modNULL,NDVI_mod2,test="F")
AIC(NDVI_mod1, NDVI_mod2, NDVI_modG, NDVI_modNULL)


######## LAND Results begin
# Global model - super simple
LAND_mod1 = gam(percLandArea*0.01 ~ 
                  s(SeaLevel_mean) + 
                  s(ppt_sum) + 
                  s(tmeandegreesC_mean)
                ,  
                data=df, family=gaussian(link=identity), method="REML", select = TRUE)
summary(LAND_mod1)
AIC(LAND_mod1)
plot_gam(LAND_mod1)
plot_gam_check(LAND_mod1)
gam.check(LAND_mod1, k.rep=1000)
concurvity(LAND_mod1) 
draw(LAND_mod1)
viz.gam(LAND_mod1)

# Global model - simple
LAND_mod2 = gam(percLandArea*0.01 ~ 
                  s(SeaLevel_mean) + 
                  s(tmeandegreesC_mean) +
                  s(ppt_sum) + 
                  s(waveHs_mean) + 
                  s(Discharge_mean_cms_sum) +
                  s(vpdmaxhPa_mean)
                ,  
                data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(LAND_mod2) 
AIC(LAND_mod2)
plot_gam(LAND_mod2)
plot_gam_check(LAND_mod2)
gam.check(LAND_mod2, k.rep=1000)
concurvity(LAND_mod2) 
draw(LAND_mod2, residuals = FALSE)

AIC(LAND_mod1, LAND_mod2)


# Global model - simple with site term
LAND_modG = gam(percLandArea*0.01 ~ 
                  s(SeaLevel_mean) + 
                  s(tmeandegreesC_mean) + #, by=Site_uo) +
                  s(ppt_sum) + 
                  s(waveHs_mean) + 
                  s(Discharge_mean_cms_sum) +  
                  s(vpdmaxhPa_mean) +
                  s(Site_uo,                 # the term to be smoothed
                    k=32,                    # 32 sites; random effect smoother always has a k value equal to the number of levels in the grouping variable 
                    bs="re"),  
                data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(LAND_modG) 
AIC(LAND_modG)
plot_gam(LAND_modG) 
plot_gam_check(LAND_modG)
gam.check(LAND_modG, k.rep=1000)
concurvity(LAND_modG) 
draw(LAND_modG)
viz.gam(LAND_modG)

AIC(LAND_mod1, LAND_mod2, LAND_modG)

coef(LAND_modG)
vcov(LAND_modG)
sqrt(diag(vcov(LAND_modG)))

intervals(LAND_modG)
gam.vcomp(LAND_modG)
anova(LAND_modG)
plot(LAND_modG)

# Global model - simple with archetype term
LAND_modG_arch = gam(percLandArea*0.01 ~ 
                       s(SeaLevel_mean) + 
                       s(tmeandegreesC_mean) + #, by=Site_uo) +
                       s(ppt_sum) + 
                       s(waveHs_mean) + 
                       s(Discharge_mean_cms_sum) +  
                       s(vpdmaxhPa_mean) +
                       s(ArchetypeCode_2020Update,                 # the term to be smoothed
                         k=6,                    # 32 sites; random effect smoother always has a k value equal to the number of levels in the grouping variable 
                         bs="re"),  
                     data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(LAND_modG_arch) 
AIC(LAND_modG_arch)
plot_gam(LAND_modG_arch)
plot_gam_check(LAND_modG_arch)
gam.check(LAND_modG_arch, k.rep=1000)
concurvity(LAND_modG_arch) 
draw(LAND_modG_arch)
viz.gam(LAND_modG_arch)

# a null model
LAND_modNULL = gam(percLandArea*0.01 ~ 1,  
                   data=df, family=gaussian(link=log), method="REML", select = TRUE)
summary(LAND_modNULL)
LAND_modNULL$converged
AIC(LAND_modNULL)
plot_gam(LAND_modNULL)
plot_gam_check(LAND_modNULL)
gam.check(LAND_modNULL, k.rep=1000)
concurvity(LAND_modNULL)
draw(LAND_modNULL)
viz.gam(LAND_modNULL)

#comparing models
anova(LAND_modNULL,LAND_mod2,test="F")
AIC(LAND_mod1, LAND_mod2, LAND_modG, LAND_modNULL)
