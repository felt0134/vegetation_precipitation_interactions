library(reshape2)
library(tidyverse)

#make lists to put looped fles into
list.coefficients.final<-list()
list.variograms<-list()
list.residual.rasters<-list()
list.residuals.full <- list()
list.models <- list()

#reduce columns in dataset to use

head(rangeland_npp_covariates_deviations_1)
rangeland_npp_covariates_deviations_reduced <-subset(rangeland_npp_covariates_deviations_1,select=c('x','y','year','npp.x',
                                                                                                  'mm.y','mm.dev','region.x'))
head(rangeland_npp_covariates_deviations_reduced)

#inspect autocorrelation
for(i in 1:1000)
  {
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
 
stratified_final<-merge(test.strat, rangeland_npp_covariates_deviations_reduced,by=c('x','y'))

stratified_final_lm<-lm(npp.x~mm.dev*region.x*mm.y
                        ,stratified_final)

list.models[[i]] <- stratified_final_lm
newcoef1 <- stratified_final_lm$coefficients 
df<-data.frame(newcoef1)
df$id = i
list.coefficients.final[[i]] <- data.frame(df)

#look all residuals
stratified_final$resids <-residuals(stratified_final_lm)
list.residuals.full[[i]] <- stratified_final

#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)

#make rasters
residual.raster<-rasterFromXYZ(mean.resids)
#residual.plot<-plot(residual.raster)
list.residual.rasters[[i]] <- data.frame(mean.resids)

#variogram of mean
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)
variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
list.variograms[[i]] <- variogram.plot

}

#inspect variograms of mean residuals for each run
list.variograms[1:50]

#look at coefficients
summary(stratified_final_lm)
df.coefficients <- do.call("rbind", list.coefficients.final)
head(df.coefficients)
df.coefficients.2 <- cbind(rownames(df.coefficients), data.frame(df.coefficients, row.names=NULL))

colnames(df.coefficients.2)  <- c("predictor","coefficient","run.id")

df.coefficients.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub(':', '_', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub('-', '_', df.coefficients.2$predictor)
df2<-reshape(df.coefficients.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(df2)
summary(df2)

#spatial slopes

#california
df2$california_slope <- df2$coefficient.mm.y
#cold deserts
df2$cold_deserts_slope <- df2$coefficient.mm.y + df2$coefficient.region.xcold_deserts_mm.y
#hot_deserts
df2$hot_deserts_slope <-  df2$coefficient.mm.y + df2$coefficient.region.xhot_deserts_mm.y
#northern mixed
df2$northern_mixed_slope <- df2$coefficient.mm.y  + df2$coefficient.region.xnorthern_mixed_prairies_mm.y
#sgs
df2$sgs_slope <- df2$coefficient.mm.y + df2$coefficient.region.xsemi_arid_steppe_mm.y 

spatial_slopes<-subset(df2,select=c('sgs_slope','northern_mixed_slope','hot_deserts_slope','cold_deserts_slope',
                                         'california_slope','run.id'))
head(spatial_slopes)
data_long_spatial <- gather(spatial_slopes, site, slope,-run.id, factor_key=TRUE)
head(data_long_spatial)
data_long_spatial$model<-'Spatial'
summary(data_long)
?gather
#temporal slopes

#california annuals
df2$california_slope <- df2$coefficient.mm.dev
#cold deserts
df2$cold_deserts_slope <- df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xcold_deserts 
#hot_deserts
df2$hot_deserts_slope <-  df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xhot_deserts 
#northern mixed
df2$northern_mixed_slope <- df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xnorthern_mixed_prairies
#sgs
df2$sgs_slope <- df2$coefficient.mm.dev  + df2$coefficient.mm.dev_region.xsemi_arid_steppe

temporal_slopes<-subset(df2,select=c('sgs_slope','northern_mixed_slope','hot_deserts_slope','cold_deserts_slope',
                                     'california_slope','run.id'))
head(temporal_slopes)
data_long_temporal <- gather(temporal_slopes, site, slope,-run.id, factor_key=TRUE)
data_long_temporal$model<-'Temporal'
head(data_long_temporal)
summary(data_long_temporal)

#merge the spatial and temporal coefficient dataframes
rbind_spatial_temporal<-rbind(data_long_spatial,data_long_temporal)
head(rbind_spatial_temporal)
summary(rbind_spatial_temporal)

#temporal*spatial interaction 
#california annuals
df2$california_slope<- df2$coefficient.mm.dev_mm.y
#cold deserts
df2$cold_deserts_slope <- df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xcold_deserts_mm.y
#hot_deserts
df2$hot_deserts_slope <-  df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xhot_deserts_mm.y
#northern mixed
df2$northern_mixed_slope <- df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xnorthern_mixed_prairies_mm.y
#sgs
df2$sgs_slope <- df2$coefficient.mm.dev_mm.y  + df2$coefficient.mm.dev_region.xsemi_arid_steppe_mm.y

temporal_spatial_slopes<-subset(df2,select=c('hot_deserts_slope','cold_deserts_slope',
                                    'california_slope','sgs_slope','northern_mixed_slope','run.id'))
head(temporal_spatial_slopes)
data_long_temporal_spatial <- gather(temporal_spatial_slopes, site, slope,-run.id, factor_key=TRUE)
data_long_temporal_spatial$model<-'Spatiotemporal'
head(data_long_temporal_spatial)
summary(data_long_temporal_spatial)

#intercepts by veg types
colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
head(df2)
#california annuals
df2$california_intercept <- df2$intercept
#cold deserts
df2$cold_deserts_intercept <- df2$intercept + df2$coefficient.region.xcold_deserts
#hot_deserts
df2$hot_deserts_intercept <-  df2$intercept  + df2$coefficient.region.xhot_deserts
#northern mixed
df2$northern_mixed_intercept <- df2$intercept  + df2$coefficient.region.xnorthern_mixed_prairies
#sgs
df2$sgs_intercept<- df2$intercept + df2$coefficient.region.xsemi_arid_steppe

vegetation_intercepts<-subset(df2,select=c('hot_deserts_intercept','cold_deserts_intercept',
                                             'california_intercept','sgs_intercept','northern_mixed_intercept','run.id'))
head(vegetation_intercepts)
data_long_vegetation_intercepts <- gather(vegetation_intercepts, site, slope,-run.id, factor_key=TRUE)
data_long_vegetation_intercepts$model<-'Intercept'
head(data_long_vegetation_intercepts)
summary(data_long_vegetation_intercepts)

#########look at residuals#############

df.residuals <- do.call("rbind", list.residuals.full)
head(df.residuals)


###scrap code###########

list.results<-list()

#inspect plots of residuals
list.residual.rasters[1:50]
colnames <- c("x","y","resids") 
extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)
conus_split_3<-lapply(list.residual.rasters, setNames, colnames)
conus_split_4<-lapply(conus_split_3, rasterFromXYZ) #change each year into its own raster
conus_split_5<-lapply(conus_split_4, resample)
conus_npp_procossed_stack <-stack(conus_split_4,quick=TRUE) #stack each raster
plot(conus_npp_procossed_stack)
extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)

for(i in 1:length(conus_split_4)) {
  #extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)
  r <-conus_split_4[[i]] # raster(files[i])
  rc <- crop(r, extent.all)
  list.results[[i]] <- rc
  env_data<- stack(list.results)}
if(sum(as.matrix(extent(rc))!=as.matrix(extent.all)) == 0){ # edited
  # You can't mask with extent, only with a Raster layer, RStack or RBrick
}

list.results[[i]] <- rc
env_data<- stack(list.results)

}

plot(list.residual.rasters[10])
plot(rc)
plot(residual.raster)
?rasterFromXYZ
plot(list.residual.rasters[5]$x,list.residual.rasters[5]$y,list.residual.rasters[5]$resids)

list.variograms[1:50]
filenames_plan <- names(list.variograms)


for (i in 1:length(list.variograms)){
  #outname <- paste("C:/Users/A02296270/Desktop/variogram_test/", list.variograms[i], "_.jpeg",sep='')
  jpeg(list.variograms[[i]], filename = 'outname.jpeg')
}

pred<-predict(stratified_final_lm[3],stratified_final)
data.frame(pred)
summary(list.models[1])
summary(data_long)
#ggplot
library(ggplot2)
library(ggridges)
devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)
ggpredict(stratified_final_lm)

#spatial coefficients plot
ggplot(data_long,aes(x=slope,y=site,fill=site)) +
  #geom_point(alpha=.1) +
  #geom_histogram(binwidth = .01) +
  #geom_histogram(color='black',size=.5,alpha=.7) +
  geom_density_ridges(size=1,alpha=1) +
  xlab('Slope of spatial model') +
  #geom_point(size=.1,pch=19,alpha=.1) +
  scale_y_discrete(limits=c('hot_deserts_spatial_slope','cold_deserts_spatial_slope',
                            'sgs_spatial_slope','coefficient.mm.y','northern_mixed_spatial_slope'),
  #scale_y_continuous(expand=c(0,0),limits=c(0,1)) +
  #scale_colour_manual(values=c('coefficient.mm.y'='black','northern_mixed_spatial_slope'='orange',
                            # 'hot_deserts_spatial_slope' = 'red','cold_deserts_spatial_slope'='blue',
                             #'sgs_spatial_slope'='darkgreen'),name="Vegetaton type",
                    
  labels=c('coefficient.mm.y'='California annuals','northern_mixed_spatial_slope'='Mixed prairies',
                             'hot_deserts_spatial_slope' = 'Hot deserts','cold_deserts_spatial_slope'='Cold deserts',
                             'sgs_spatial_slope'='Semi-arid steppe')) +
  

xlab(bquote('Spatial sensitivity ('*g/m^2/mm*')')) +
  
  ylab("") +
  
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  
  theme(
    
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=20),
    
    axis.title = element_text(color='black',size=23),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    legend.text = element_text(size=12),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))

#temporalcoefficients plot
summary(data_long_temporal)
ggplot(data_long_temporal,aes(x=slope,y=site,fill=site)) +
  geom_density_ridges(size=1,alpha=1) +
  #xlab('Slope of spatial model') +
  scale_y_discrete(limits=c('hot_deserts_temporal_slope','cold_deserts_temporal_slope',
                            'sgs_temporal_slope','coefficient.mm.y_mm.x','northern_mixed_temporal_slope'),
                   labels=c('coefficient.mm.y_mm.x'='California annuals','northern_mixed_temporal_slope'='Mixed prairies',
                            'hot_deserts_temporal_slope' = 'Hot deserts','cold_deserts_temporal_slope'='Cold deserts',
                            'sgs_temporal_slope'='Semi-arid steppe')) +
  
  
  xlab(bquote('change in sensitivity per mm of MAP ('*g/m^2/'%mm change'*')')) +
  xlab('change in sensitivity per mm of MAP') +
  ylab("") +
  geom_vline(xintercept=0,color='black',size=2) +
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  
  theme(
    
    axis.text.x = element_text(color='black',size=18), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=20),
    
    axis.title = element_text(color='black',size=18),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    legend.text = element_text(size=12),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))
