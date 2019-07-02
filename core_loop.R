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

