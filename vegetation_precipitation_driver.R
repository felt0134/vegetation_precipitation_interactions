
#Import and set up dataframe

#set directory for loading NPP/cover files

#go back up the directory tree
setwd("./../../..")

#load file
rangeland_npp_covariates<-readRDS("./Processing NPP Data/NPP Data processing/Dryland_NPP.rds") #loads file and name it annualSWA_OctDec I guess
#head(rangeland_npp_covariates)
#unique(rangeland_npp_covariates$region)

#take a look
#summary(rangeland_npp_covariates)
#head(rangeland_npp_covariates)
#str(rangeland_npp_covariates)

#get mean annual precip for each pixel
mean_mm_site<-aggregate(mm~x+y+region,mean,data=rangeland_npp_covariates)
#head(mean_mm_site)
#summary(mean_mm_site)

#hot deserts
hot_deserts_1 <-subset(mean_mm_site,region=="hot_deserts")
#summary(hot_deserts_1) #regional mean annual precipitation is 286.1
hot_deserts_above_below <- above_below_map(hot_deserts_1)
rm(hot_deserts_1)

#cold_deserts
cold_deserts_1 <-subset(mean_mm_site,region=="cold_deserts")
#summary(cold_deserts_1) #regional mean annual precipitation is 288.1
cold_deserts_above_below <- above_below_map(cold_deserts_1)
rm(cold_deserts_1)

#california_annuals
california_annuals_1 <-subset(mean_mm_site,region=="california_annuals")
#summary(california_annuals_1) #regional mean annual precipitation is 403.9
california_annuals_above_below <- above_below_map(california_annuals_1)
rm(california_annuals_1)

#shortgrass steppe
semiarid_steppe_1 <-subset(mean_mm_site,region=="shortgrass_steppe")
#summary(semiarid_steppe_1) #regional mean annual precip is 417.1
semiarid_steppe_above_below <- above_below_map(semiarid_steppe_1)
rm(semiarid_steppe_1)

#northern mixed prairies
northern_mixed_prairies_1 <-subset(mean_mm_site,region=="northern_mixed_prairies")
#summary(northern_mixed_prairies_1) #regional mean annual precip is 404.3
northern_mixed_prairies_above_below <- above_below_map(northern_mixed_prairies_1)
rm(northern_mixed_prairies_1)

#making colomnn for per-pixel mean npp
rangeland_mean_npp<-aggregate(npp ~ x + y + region,mean,data=rangeland_npp_covariates)

#merge with mean precip
mm_production_mean<-merge(rangeland_mean_npp,mean_mm_site,by=c('x','y','region'))
#head(mm_production_mean)
#summary(mm_production_mean)

rm(rangeland_mean_npp,mean_mm_site)

#merge initial dataframe with means
rangeland_npp_covariates<-merge(rangeland_npp_covariates,mm_production_mean,by=c('x','y','region'))

#take a look
#head(rangeland_npp_covariates_deviations_1)
#summary(rangeland_npp_covariates_deviations_1)
rm(mm_production_mean)

#add mm and npp deviations from the mean
rangeland_npp_covariates$npp.dev<-rangeland_npp_covariates$npp.x - rangeland_npp_covariates$npp.y
rangeland_npp_covariates$mm.dev<-rangeland_npp_covariates$mm.x - rangeland_npp_covariates$mm.y

#take a look
#summary(rangeland_npp_covariates)
#head(rangeland_npp_covariates)

#
#

#cnan etither use fractional NPP, or fractional % cover data, for herbaceous vegetation covariate

#import fractional npp dataframe#######
# fractional_npp<-readRDS(('/Volumes/GoogleDrive/My Drive/range-resilience/Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/Fractional_NPP_Western_US.rds'))
# #head(fractional_npp)
# 
# #get % herbaceous NPP
# fractional_npp$perc_herb<-round((fractional_npp$annual_grass_forb + fractional_npp$perennial_grass_forb)/
#   (fractional_npp$annual_grass_forb + fractional_npp$perennial_grass_forb +
#   fractional_npp$shrub + fractional_npp$tree)*100,1)
# 
# fractional_npp<-fractional_npp[-c(4,5,6,7)]
# #head(fractional_npp)
# perc_herb_mean <-aggregate(perc_herb~x+y,mean,data=fractional_npp)
# perc_herb_mean$perc_herb_mean<-round(perc_herb_mean$perc_herb,1)
# perc_herb_mean<-perc_herb_mean[-c(3)]
# #head(perc_herb_mean)
# fractional_npp<-merge(fractional_npp,perc_herb_mean,by=c('x','y'))
# #head(fractional_npp)
# rm(perc_herb_mean)
# 
# #merge with dryland npp dataset
# rangeland_npp_covariates<-merge(rangeland_npp_covariates,fractional_npp,
#                                              by=c('x','y','year'))
# #head(rangeland_npp_covariates)
# rm(fractional_npp)
##########

#####import fractional cover dataframe######

fractional_cover<-readRDS(('Processing NPP Data/Fractional Cover Processing/fractional_cover_processing/Fractional_cover_Western_US.rds'))
#head(fractional_cover)
fractional_cover <-aggregate(herb_cover~x+y,mean,data=fractional_cover)
fractional_cover$perc_herb_mean <- fractional_cover$herb_cover
fractional_cover$perc_herb_mean <- round(fractional_cover$perc_herb_mean,1)
fractional_cover <- fractional_cover[-c(3)]

#merge
rangeland_npp_covariates<-merge(rangeland_npp_covariates,fractional_cover,
                                by=c('x','y'))
head(rangeland_npp_covariates)
#Done. 