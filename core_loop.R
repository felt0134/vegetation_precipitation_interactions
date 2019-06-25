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
summary(rangeland_npp_covariates_deviations_reduced)

#run the loop
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

#get coefficients into a usable dataframe 
summary(stratified_final_lm)
df.coefficients <- do.call("rbind", list.coefficients.final)
head(df.coefficients)
df.coefficients.2 <- cbind(rownames(df.coefficients), data.frame(df.coefficients, row.names=NULL))

colnames(df.coefficients.2)  <- c("predictor","coefficient","run.id")

df.coefficients.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub(':', '_', df.coefficients.2$predictor)
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


