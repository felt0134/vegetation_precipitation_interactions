rm(list=ls())
setwd("C:/Users/A02296270/Desktop/CONUS_AFRI/CONUS")
rangeland_npp_covariates<- read.csv("/Users/A02296270/Desktop/CONUS_AFRI/CONUS/npp_climate_rangelands.csv")
head(rangeland_npp_covariates)

mean_mm_site<-aggregate(mm~x+y+region,mean,data=rangeland_npp_covariates)
head(mean_mm_site)
summary(mean_mm_site)

#load packages
library(dplyr)
library(spdep)
library(splitstackshape)

#make MAP groupings for each vegetation type
#hotdeserts
hot_deserts_1 <-subset(mean_mm_site,region=="hot_deserts")
summary(hot_deserts_1)
hot_deserts_below <-hot_deserts_1 %>% dplyr::filter(mm < 253.84)
hot_deserts_below$map <- 'below'
hot_deserts_above  <-hot_deserts_1 %>%  dplyr::filter(mm > 253.84)
hot_deserts_above$map <- 'above'
hot_deserts_above_below <- rbind(hot_deserts_above,hot_deserts_below)
hot_deserts_above_below_2<-hot_deserts_above_below[-3]
#head(hot_deserts_above_below_2)
#summary(hot_deserts_above_below)

#cold_deserts
cold_deserts_1 <-subset(mean_mm_site,region=="cold_deserts")
summary(cold_deserts_1)
cold_deserts_below <-cold_deserts_1 %>% dplyr::filter(mm < 285.99)
cold_deserts_below$map <- 'below'
cold_deserts_above  <-cold_deserts_1 %>%  dplyr::filter(mm > 285.99)
cold_deserts_above$map <- 'above'
cold_deserts_above_below <- rbind(cold_deserts_above,cold_deserts_below)

#california_annuals
california_annuals_1 <-subset(mean_mm_site,region=="california_annuals")
summary(california_annuals_1)
california_annuals_below <-california_annuals_1 %>% dplyr::filter(mm < 403.5)
california_annuals_below$map <- 'below'
california_annuals_above  <-california_annuals_1 %>%  dplyr::filter(mm > 403.5)
california_annuals_above$map <- 'above'
california_annuals_above_below <- rbind(california_annuals_above,california_annuals_below)

#shortgrass steppe
semiarid_steppe_1 <-subset(mean_mm_site,region=="semi-arid_steppe")
summary(semiarid_steppe_1)
semiarid_steppe_below <-semiarid_steppe_1 %>% dplyr::filter(mm < 417.1)
semiarid_steppe_below$map <- 'below'
semiarid_steppe_above  <-semiarid_steppe_1 %>%  dplyr::filter(mm > 417.1)
semiarid_steppe_above$map <- 'above'
semiarid_steppe_above_below <- rbind(semiarid_steppe_above,semiarid_steppe_below)

#northern mixed
northern_mixed_prairies_1 <-subset(mean_mm_site,region=="northern_mixed_prairies")
summary(northern_mixed_prairies_1)
northern_mixed_prairies_below <-northern_mixed_prairies_1 %>% dplyr::filter(mm < 404.3)
northern_mixed_prairies_below$map <- 'below'
northern_mixed_prairies_above  <-northern_mixed_prairies_1 %>%  dplyr::filter(mm > 403.3)
northern_mixed_prairies_above$map <- 'above'
northern_mixed_prairies_above_below <- rbind(northern_mixed_prairies_above,northern_mixed_prairies_below)

stratified_df<-rbind(northern_mixed_prairies_above_below, semiarid_steppe_above_below, california_annuals_above_below, 
                     cold_deserts_above_below, hot_deserts_above_below)

#summary(stratified_df)

#making colomn for per-pixel mean precip and npp
rangeland_mean_npp<-aggregate(npp ~ x + y + region,mean,data=rangeland_npp_covariates)
rangeland_mean_mm<-aggregate(mm ~ x + y + region,mean,data=rangeland_npp_covariates)
mm_production_mean<-merge(rangeland_mean_npp,rangeland_mean_mm,by=c('x','y','region'))
#head(mm_production_mean)

rangeland_npp_covariates_deviations_1<-merge(rangeland_npp_covariates,mm_production_mean,by=c('x','y'))
#head(rangeland_npp_covariates_deviations_1)

#add percent mm and percent npp deviations
rangeland_npp_covariates_deviations_1$npp.dev<-((rangeland_npp_covariates_deviations_1$npp.x - rangeland_npp_covariates_deviations_1$npp.y)/ rangeland_npp_covariates_deviations_1$npp.y)
rangeland_npp_covariates_deviations_1$mm.dev<-((rangeland_npp_covariates_deviations_1$mm.x - rangeland_npp_covariates_deviations_1$mm.y)/rangeland_npp_covariates_deviations_1$mm.y)
#head(rangeland_npp_covariates_deviations_1)

###to assess spatial range of autocorrelation####
library(gstat)
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
