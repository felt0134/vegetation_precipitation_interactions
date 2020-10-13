#variogram of OLS residuals prior to stratified sampling

###first assess spatial range of autocorrelation####

#combine dataframes
stratified_df_find_range<-rbind(northern_mixed_prairies_above_below, semiarid_steppe_above_below, california_annuals_above_below, 
                                cold_deserts_above_below, hot_deserts_above_below)

stratified_merged_find_range<-merge(stratified_df_find_range, rangeland_npp_covariates_deviations_1,by=c('x','y'))

stratified_lm_find_range<-lm(npp.x~mm.y*region.x*mm.dev
                             ,stratified_merged_find_range)

summary(stratified_lm_find_range)

#establish autocorrelation in residuals exists
lm.morantest(lm_full, listw = sparse_neighbor_weights_xy_stratified, zero.policy=NULL, alternative = "greater",
             spChk=NULL, resfun=weighted.residuals, naSubset=TRUE)

#variograms
stratified_merged_find_range$resids<-residuals(stratified_lm_find_range)

subset_1996<-subset<-subset(stratified_merged_find_range,year=='1996')
coordinates(subset_1996)= ~ x+y
TheVariogram_1996.2=variogram(resids~1, data=subset_1996)
plot(TheVariogram_1996.2,main='1996') #5 

subset_1986<-subset<-subset(stratified_merged_find_range,year=='1986')
coordinates(subset_1986)= ~ x+y
TheVariogram_1986.2=variogram(resids~1, data=subset_1986)
plot(TheVariogram_1986.2,main='1986')

subset_2006<-subset<-subset(stratified_merged_find_range,year=='2006')
coordinates(subset_2006)= ~ x+y
TheVariogram_2006.2=variogram(resids~1, data=subset_2006)
plot(TheVariogram_2006.2,main='2006')

subset_2015<-subset<-subset(stratified_merged_find_range,year=='2015')
coordinates(subset_2015)= ~ x+y
TheVariogram_2015.2=variogram(resids~1, data=subset_2015)
plot(TheVariogram_2015.2,main='2015')

subset_2000<-subset<-subset(stratified_merged_find_range,year=='2000')
coordinates(subset_2000)= ~ x+y
TheVariogram_2000.2=variogram(resids~1, data=subset_2000)
plot(TheVariogram_2000.2,main='2000')


#in stratified dataframe
stratified_final_lm_find_range<-lm(npp.x~mm.y*region.x*mm.dev
                             ,stratified_final)

summary(stratified_final_lm_find_range)

#establish autocorrelation in residuals exists
lm.morantest(stratified_final_lm_find_range, listw = sparse_neighbor_weights_xy_stratified, zero.policy=NULL, alternative = "greater",
             spChk=NULL, resfun=weighted.residuals, naSubset=TRUE)

#variograms
stratified_final$resids<-residuals(stratified_lm_find_range)

subset_1996<-subset<-subset(stratified_final,year=='1996')
coordinates(subset_1996)= ~ x+y
TheVariogram_1996.2=variogram(resids~1, data=subset_1996)
plot(TheVariogram_1996.2,main='1996') #5 

subset_1986<-subset<-subset(stratified_final,year=='1986')
coordinates(subset_1986)= ~ x+y
TheVariogram_1986.2=variogram(resids~1, data=subset_1986)
plot(TheVariogram_1986.2,main='1986')

subset_2006<-subset<-subset(stratified_final,year=='2006')
coordinates(subset_2006)= ~ x+y
TheVariogram_2006.2=variogram(resids~1, data=subset_2006)
plot(TheVariogram_2006.2,main='2006')

subset_2015<-subset<-subset(stratified_final,year=='2015')
coordinates(subset_2015)= ~ x+y
TheVariogram_2015.2=variogram(resids~1, data=subset_2015)
plot(TheVariogram_2015.2,main='2015')

subset_2000<-subset<-subset(stratified_final,year=='2000')
coordinates(subset_2000)= ~ x+y
TheVariogram_2000.2=variogram(resids~1, data=subset_2000)
plot(TheVariogram_2000.2,main='2000')

#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
head(mean.resids)
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)

#plot it: mean
mean.residual.raster<-rasterFromXYZ(mean.resids)
#plot(mean.residual.raster)
plot(mean.residual.raster,main='Mean residuals')

#plot it: for every year
head(stratified_final)
test<-function(x) { 
  subset(x,
  select=c('x','y','resids'))}
startified_final_residuals<-subset(stratified_final,select=c('x','y','resids','year')) #remove columns not needed
startified_final_residuals_split<-split(startified_final_residuals,startified_final_residuals$year) #split each year into its own df

startified_final_residuals_2<-lapply(startified_final_residuals_split,test) #get rid of year column
colnames <- c("x","y","z") 
startified_final_residuals_3<-lapply(startified_final_residuals_2, setNames, colnames)
startified_final_residuals_4<-lapply(startified_final_residuals_3, rasterFromXYZ) #change each year into its own raster
startified_final_residuals_stack <-stack(startified_final_residuals_4) #stack each raster
plot(startified_final_residuals_stack)
