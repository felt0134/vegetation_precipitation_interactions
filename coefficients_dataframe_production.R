
####get coefficients into a usable dataframe #####
list.coefficients.herb.region.no.threeway[1]
#to look at model structure of output
# #summary(stratified_final_lm_region) 
# summary(stratified_final_lm_herb_map)
# summary(stratified_final_lm_herb_region)

#model 1
map_coefficients<-list_to_df(list.coefficients.noveg,region=F)
#head(map_coefficients)

#model 2
map_herb_coefficients<-list_to_df(list.coefficients.herb.map,region=F) #need to look into this
#head(map_herb_coefficients)

#model 3
map_region_coefficients <-list_to_df(list.coefficients.ecoregion.null,region=T)
#head(map_region_coefficients)

#temporal slope at MAP for each ecoregion
map_region_coefficients$temporal_sensitivity_map <-
  map_region_coefficients$Temporal + map_region_coefficients$Spatiotemporal*map_region_coefficients$map
#head(map_region_coefficients)

#model 4
herb_region_coefficients <-list_to_df_with_ecoregion(list.coefficients.herb.region,veg = T,full=F)
#head(herb_region_coefficients)

#model 5
herb_region_coefficients_no.threeway <-list_to_df(list.coefficients.herb.region.no.threeway,region=T)
#head(herb_region_coefficients_no.threeway)

#model 6
# #veg-mm.dev-ecoregion spatiotemporal interaction
# herb_region_coefficients_full_veg <-list_to_df_with_ecoregion(list.coefficients.full,veg=T,full=F)
# herb_region_coefficients_full_veg$st<-'veg.mm.dev'
# #head(herb_region_coefficients_full_veg)
# #map-mm.dev-ecoregion spatiotemporal interaction
# herb_region_coefficients_full_veg_map <-list_to_df_with_ecoregion(list.coefficients.full,veg=F,full=T)
# herb_region_coefficients_full_veg_map$st<-'map.mm.dev'
# #head(herb_region_coefficients_full_veg_map)
# herb_region_coefficients_full_merged<-rbind(herb_region_coefficients_full_veg_map,herb_region_coefficients_full_veg)
# #merge them
# 
# st_mean<-aggregate(Spatiotemporal ~ region + st,mean,data=herb_region_coefficients_full_merged)
# 
# head(herb_region_coefficients_full_merged)

#get mean herb cover for each ecoregion
region_mean_herb<-aggregate(perc_herb_mean~region,mean,data=rangeland_npp_covariates) 
region_mean_herb$herb<-round(region_mean_herb$perc_herb,1)
region_mean_herb<-region_mean_herb[-c(2)]

herb_region_coefficients<-merge(herb_region_coefficients,region_mean_herb,by=c('region'))
#########

# summary statistics: model coefficicents #########

# map-no ecoregion model #

# mean
map_coefficient_means <-map_coefficients %>%
  summarise_all(mean)
map_coefficient_means<-data.frame(map_coefficient_means)
map_coefficient_means$stat <- 'mean'

# 95% confidence interval
map_coefficient_95ci <- map_coefficients %>%
  summarise_all(error.95)
map_coefficient_95ci <-data.frame(map_coefficient_95ci)
map_coefficient_95ci$stat <- '95_ci'

map_coef_summary_merged<-rbind(map_coefficient_95ci,map_coefficient_means)
write.csv(map_coef_summary_merged,
          file = '/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/tables/map_coef.csv')


# map-ecoregion model #

# mean
map_region_coefficient_means <-map_region_coefficients %>%
  group_by(region) %>%
  summarise_all(mean)
map_region_coefficient_means<-data.frame(map_region_coefficient_means)
map_region_coefficient_means$stat <- 'mean'

# 95% confidence interval
map_region_coefficient_95ci <- map_region_coefficients %>%
  group_by(region) %>%
  summarise_all(error.95)
map_region_coefficient_95ci <-data.frame(map_region_coefficient_95ci)
map_region_coefficient_95ci$stat <- '95_ci'

map_region_coef_summary_merged<-rbind(map_region_coefficient_95ci,map_region_coefficient_means)
write.csv(map_region_coef_summary_merged,
          file = '/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/tables/map_region_coef.csv')

# herb-no ecoregion model #

map_herb_coefficient_means <-map_herb_coefficients %>%
  summarise_all(mean)
map_herb_coefficients_means<-data.frame(map_herb_coefficient_means)
map_herb_coefficient_means$stat <- 'mean'

#95% confidence interval
map_herb_coefficient_95ci <- map_herb_coefficients %>%
  summarise_all(error.95)
map_herb_coefficient_95ci <-data.frame(map_herb_coefficient_95ci)
map_herb_coefficient_95ci$stat <- '95_ci'

map_herb_coef_summary_merged<-rbind(map_herb_coefficient_95ci,map_herb_coefficient_means)
write.csv(map_herb_coef_summary_merged,
          file = '/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/tables/map_herb_coef.csv')
                                   

# herb-region model #

#mean
herb_region_coefficient_means <- herb_region_coefficients %>%
  group_by(region) %>%
  summarise_all(mean)
herb_region_coefficient_means<-data.frame(herb_region_coefficient_means)
herb_region_coefficient_means$stat <- 'mean'

#95% confidence interval
herb_region_coefficient_95ci <- herb_region_coefficients %>%
  group_by(region) %>%
  summarise_all(error.95)
herb_region_coefficient_95ci <-data.frame(herb_region_coefficient_95ci)
herb_region_coefficient_95ci$stat <- '95_ci'

herb_region_coef_summary_merged<-rbind(herb_region_coefficient_95ci,herb_region_coefficient_means)
write.csv(herb_region_coef_summary_merged,
          file = '/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/tables/herb_region_coef.csv')

#
#


######
###### summary statstics: MAP #######
map_means_by_ecoregion<-aggregate(mm.y~x+y+region,mean,data=rangeland_npp_covariates)
map_ci_by_ecoregion<-aggregate(mm.y~region,error.95,data=map_means_by_ecoregion)
map_means_by_ecoregion<-aggregate(mm.y~region,mean,data=rangeland_npp_covariates)

# AIC dataframe production #######

#model 4
aic.region.binded<-bind.aic(list.aic.region)
aic.region.binded$model<-'ecoregion'

#model 2
aic.herb.map.binded<-bind.aic(list.aic.herb.map)
aic.herb.map.binded$model<-'herb.map'

#model 3
aic.herb.region.binded<-bind.aic(list.aic.herb.region)
aic.herb.region.binded$model<-'herb.region'

#model 1
aic.noveg.binded<-bind.aic(list.aic.noveg)
aic.noveg.binded$model<-'no.veg'

#model 5
aic.herb.region.binded.no.threeway<-bind.aic(list.aic.herb.region.no.threeway)
aic.herb.region.binded.no.threeway$model<-'no.threeway'

#model 6
aic.full<-bind.aic(list.aic.full)
aic.full$model<-'full'

aic.binded<-do.call("rbind", list(aic.herb.map.binded, aic.region.binded, aic.herb.region.binded,
                                  aic.noveg.binded,aic.herb.region.binded.no.threeway))

#head(aic.binded)
#summary(aic.model.1234)
aic_means<-aggregate(aic~model,mean,data=aic.binded)

#get % reduction in aic from best (full) to next 'best' model...
((subset(aic_means,model=='no.threeway')$aic - 
    subset(aic_means,model=='herb.region')$aic)/subset(aic_means,model=='herb.region')$aic)*100


#compare s-t interactions with map, herb, and ecoregion as interaction with mm.dev
#ecoregion null model
aic.ecoregion.null <- bind.aic(list.aic.ecoregion.null)
aic.ecoregion.null$model <- 'ecoregion.null'

aic.map.null<-bind.aic(list.aic.noveg)
aic.map.null$model <- 'map.null'

aic.herb.map.null <- bind.aic(list.aic.herb.map)
aic.herb.map.null$model <- 'herb.null'

nulls.binded.aic<-rbind(aic.herb.map.null,aic.map.null,aic.ecoregion.null)
nulls.binded.aic<-aggregate(aic~model,mean,data=nulls.binded.aic)

#
#

#####
# BIC dataframe production #######

#model 4
bic.region.binded<-bind.bic(list.bic.region)
bic.region.binded$model<-'ecoregion'

#model 2
bic.herb.map.binded<-bind.bic(list.bic.herb.map)
bic.herb.map.binded$model<-'herb.map'

#model 3
bic.herb.region.binded<-bind.bic(list.bic.herb.region)
bic.herb.region.binded$model<-'herb.region'

#model 1
bic.noveg.binded<-bind.bic(list.bic.noveg)
bic.noveg.binded$model<-'no.veg'

#model 5
bic.herb.region.binded.no.threeway<-bind.bic(list.bic.herb.region.no.threeway)
bic.herb.region.binded.no.threeway$model<-'no.threeway'

#model 6
bic.full<-bind.bic(list.bic.full)
bic.full$model<-'full'

bic.binded<-do.call("rbind", list(bic.herb.map.binded, bic.region.binded, bic.herb.region.binded,
                                  bic.noveg.binded,bic.herb.region.binded.no.threeway,bic.full))

#head(bic.binded)
#summary(bic.model.1234)
bic_means<-aggregate(bic~model,mean,data=bic.binded)

#get % reduction in aic from best (full) to next 'best' model...
((subset(bic_means,model=='herb.region')$bic - 
    subset(bic_means,model=='full')$bic)/subset(bic_means,model=='herb.region')$bic)*100

########
# R-squared dataframe production ########

#model 4
r.squared.region.binded<-bind.r.squared(list.r.squared.region)
r.squared.region.binded$model <- 'ecoregion'

#model 2
r.squared.herb.map.binded<-bind.r.squared(list.r.squared.herb.map)
r.squared.herb.map.binded$model <- 'herb.map'

r.squared.model.1.2<-rbind(r.squared.region.binded,r.squared.herb.map.binded)

#model 3
r.squared.region.binded<-bind.r.squared(list.r.squared.herb.region)
r.squared.region.binded$model<-'herb.region'

#model 1
r.squared.noveg.binded<-bind.r.squared(list.r.squared.noveg)
r.squared.noveg.binded$model<-'no.veg'

r.squared.model.3.4<-rbind(r.squared.region.binded,r.squared.noveg.binded)

r.squared.model.1234<-rbind(r.squared.model.1.2,r.squared.model.3.4)

#head(r.squared.model.1234)
r.squared.model.1234.mean<-aggregate(r.squared~model,mean,data=r.squared.model.1234)

#0.57 + 0.55

#model 6
r.squared.full<-bind.r.squared(list.r.squared.full)
r.squared.full$model <-'full'

#model 7
r.squared.no.int<-bind.r.squared(list.r.squared.no.int)
r.squared.no.int$model <-'no.interaction'

r.squared.model.6.7<-rbind(r.squared.no.int,r.squared.full)

r.squared.model.123467<-rbind(r.squared.model.6.7,r.squared.model.1234)

r.squared_means<-aggregate(r.squared~model,mean,data=r.squared.model.123467)

#model 8
r.squared.full.no.eco<-bind.r.squared(list.r.squared.full.noecoregion)
summary(r.squared.full.no.eco)

#compare s-t interactions with map, herb, and ecoregion as interaction with mm.dev
#ecoregion null model
r.squared.ecoregion.null <- bind.r.squared(list.r.squared.ecoregion.null)
r.squared.ecoregion.null$model <- 'ecoregion.null'

r.squared.map.null<-bind.r.squared(list.r.squared.noveg)
r.squared.map.null$model <- 'map.null'

r.squared.herb.map.null <- bind.r.squared(list.r.squared.herb.map)
r.squared.herb.map.null$model <- 'herb.null'

nulls.binded<-rbind(r.squared.herb.map.null,r.squared.map.null,r.squared.ecoregion.null)
nulls.binded.r.square<-aggregate(r.squared~model,mean,data=nulls.binded)

#done