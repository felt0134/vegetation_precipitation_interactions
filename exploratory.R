#additional exploration


#look at sensitivity maps of the three 'null' models############
#you need some ofthese dataframes for comparing r-squared values

#first produce the dataframes of the coefficients

#model 1: map
map_coefficients<-list_to_df(list.coefficients.noveg,region=F)
#head(map_coefficients)
map_data<-aggregate(mm.y~x+y,mean,data=rangeland_npp_covariates)
head(map_data)
map_data$coef<-mean(map_coefficients$coefficient.temporal) +
  + mean(map_coefficients$coefficient.spatial_temporal)*map_data$mm.y

head(map_data)

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/map_null.pdf',
    width=6,height=6)
#this function only works if you run code in figure_1_updated figure_3_updated where per-pixel empirical slopes
#are calculated, as the function draws the color scale from that. 
#It was meant originally just for the figure script.

plot_sensitivity(rasterFromXYZ(map_data[c(1,2,4)]))
dev.off()

#just basic plot
plot(rasterFromXYZ(map_data[c(1,2,4)]))

#model 2: herb
map_herb_coefficients<-list_to_df(list.coefficients.herb.map,region=F) 
head(map_herb_coefficients)
map_herb_data<-aggregate(perc_herb_mean~x+y,mean,data=rangeland_npp_covariates)
head(map_herb_data)
map_herb_data$coef<-mean(map_herb_coefficients$coefficient.temporal) +
+ mean(map_herb_coefficients$coefficient.temporal_herb_mean)*map_herb_data$perc_herb_mean

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/herb_null_map.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(map_herb_data[c(1,2,4)]))
dev.off()

#basic plot
plot(rasterFromXYZ(map_herb_data[c(1,2,4)]))
     
#head(map_herb_coefficients)
#list.coefficients.herb.map[1]

#model 3: ecoregion
ecoregion_coefficients<-list_to_df_initial(list.coefficients.ecoregion.null)
ecoregion_coefficient_2<-ecoregion_coefficients[c(1,2,3)]
list.coefficients.ecoregion.null[1]

#have to do this part manually...

#temporal slopes

#california annuals
ecoregion_coefficients$california_annuals <- ecoregion_coefficients$coefficient.temporal
#cold deserts
ecoregion_coefficients$cold_deserts <- ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regioncold_deserts 
#hot_deserts
ecoregion_coefficients$hot_deserts <-  ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regionhot_deserts 
#northern mixed
ecoregion_coefficients$northern_mixed_prairies <- ecoregion_coefficients$coefficient.temporal + ecoregion_coefficients$coefficient.temporal_regionnorthern_mixed_prairies
#sgs
ecoregion_coefficients$shortgrass_steppe <- ecoregion_coefficients$coefficient.temporal  + ecoregion_coefficients$coefficient.temporal_regionshortgrass_steppe

temporal_slopes<-select_columns(ecoregion_coefficients)

#change to long format

data_long_temporal <- gather(temporal_slopes, region, coefficient,-run.id, factor_key=TRUE)
data_long_temporal$model<-'temporal'
coefficients_wide<- spread(data_long_temporal, model, coefficient)

#merge the spatial and temporal coefficient dataframes
rbind_spatial_temporal<-merge(coefficients_wide,ecoregion_coefficient_2,by=c('run.id'))

ecoregion_null_model<-rbind_spatial_temporal %>%
  group_by(region) %>%
  summarise(temporal=mean(temporal))
ecoregion_null_model<-data.frame(ecoregion_null_model)

mean_for_merge<-aggregate(npp.x~x+y+region,mean,data=rangeland_npp_covariates)
head(mean_for_merge)
mean_for_merge<-merge(mean_for_merge[c(1,2,3)],ecoregion_null_model,by=c('region'))
mean_for_merge$coef<-round(mean_for_merge$temporal,2)
mean_for_merge<-mean_for_merge[c(2,3,5)]

#save plot for model 3
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/ecoregion_null_map.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(mean_for_merge))
dev.off()

#normal plot
plot(rasterFromXYZ(mean_for_merge))

###########

#compare observed versus predicted NPP for the three models#######

#Decompose to spatial, temporal, and total variation


#map null model

#head(map_data) #from line 12

map_coefficients_test<-map_coefficients[c(2,100),] #compare small subset to eachother

#they differ..

#now do everything...
#make lists to store correlations
list_cor_total_map<-list()
list_cor_spatial_map<-list()
list_cor_temporal_map_test<-list()

for(i in 1:nrow(map_coefficients_test)){

map_npp_yearly<-rangeland_npp_covariates[c(1,2,4,5,8,10)]

beta_i <- (map_coefficients$intercept[i])
beta_s <- (map_coefficients$coefficient.spatial[i])
beta_t <- (map_coefficients$coefficient.temporal[i])
beta_sxt <- (map_coefficients$coefficient.spatial_temporal[i])

map_npp_yearly$npp.pred <- (beta_i + beta_s*map_npp_yearly$mm.y) + #intercept and spatial
 (beta_t + beta_sxt*map_npp_yearly$mm.y)*map_npp_yearly$mm.dev #temporal and s-t interaction

#total r-square
cor_total_map<-round(cor(map_npp_yearly$npp.pred,map_npp_yearly$npp.x)^2,2)
list_cor_total_map[[i]]<-cor_total_map



#just spatial
map_null_npp_means<-map_npp_yearly[c(1,2,4,7)] %>%
  group_by(x,y) %>%
  summarise_all(mean)
map_null_npp_means<-data.frame(map_null_npp_means)

#spatial means
cor_spatial_map<-round(cor(map_null_npp_means$npp.pred,map_null_npp_means$npp.x,method="pearson")^2,2)
list_cor_spatial_map[[i]]<-cor_spatial_map

#take a shot at temporal componenet
map_null_npp_temporal_cor<-map_npp_yearly[c(1,2,4,7)] %>%
  group_by(x,y) %>%
  summarise(cor(npp.pred,npp.x,method = "pearson"))
colnames(map_null_npp_temporal_cor)<-c('x','y','cor')
#head(map_null_npp_temporal_cor)
map_null_npp_temporal_cor$r.square<-(map_null_npp_temporal_cor$cor)^2
cor_temporal_map<-round(mean(map_null_npp_temporal_cor$r.square),10)
list_cor_temporal_map_test[[i]] <-cor_temporal_map

rm(map_npp_yearly)

}

#this is producing the same correlation for each set of coefficients...even though the coefficients
#differ. Perhaps the variation in the boostrapped cofficients is not strong enough to really change
#the correlation of expected versus predicted NPP, given the tons of data points produced?

#hist(map_null_npp_temporal_cor$r.square)  

#now look at herb model
head(map_herb_data)
herb_npp_yearly<-rangeland_npp_covariates[c(1,2,4,5,8,10,11)]
head(herb_npp_yearly)

#compare different iterations...correlations don't change, but NPP predictions do, slightly
map_herb_coefficients<-list_to_df(list.coefficients.herb.map,region=F) 
map_herb_coefficients<-map_herb_coefficients[1,]

beta_i <- mean(map_herb_coefficients$intercept)
beta_s <- mean(map_herb_coefficients$coefficient.spatial)
beta_t <- mean(map_herb_coefficients$coefficient.temporal)
beta_sxt <- mean(map_herb_coefficients$coefficient.herb_temporal)

herb_npp_yearly$npp.pred <- (beta_i + beta_s*herb_npp_yearly$mm.y) +
  (beta_t + beta_sxt*herb_npp_yearly$perc_herb)*herb_npp_yearly$mm.dev

#summary(herb_npp_yearly)
#total r-square
cor_total_herb<-round(cor(herb_npp_yearly$npp.pred,herb_npp_yearly$npp.x,method="pearson")^2,10)

#just spatial
herb_null_npp_means<-herb_npp_yearly[c(1,2,4,8)] %>%
  group_by(x,y) %>%
  summarise_all(mean)
herb_null_npp_means<-data.frame(herb_null_npp_means)
head(herb_null_npp_means)

#spatial means
cor_spatial_herb<-round(cor(herb_null_npp_means$npp.pred,herb_null_npp_means$npp.x)^2,2)

#take a shot at temporal componenet
herb_null_npp_temporal_cor<-herb_npp_yearly[c(1,2,4,8)] %>%
  group_by(x,y) %>%
  summarise(cor(npp.pred,npp.x,method='pearson'))

colnames(herb_null_npp_temporal_cor)<-c('x','y','cor')
head(herb_null_npp_temporal_cor)
herb_null_npp_temporal_cor$r.square<-(herb_null_npp_temporal_cor$cor)^2
cor_temporal_herb<-round(mean(herb_null_npp_temporal_cor$r.square),2)
hist(herb_null_npp_temporal_cor$r.square)  

#thoughts - spatial effect of MAP consistnt across the two models, as you would expect
#given they use the same covariates. The modifying/indirect effect of either MAP or %herb on temporal var,
#on the response of NPP to PPT, is quite small relative to the direct effect of PPT, it seems,
#yes, spatial variation in these factors leads to large variation in temporal sens, but at the site level
#the modifying effect of these facots on NPP, relative to PPT amount in any given year, is small and gets drowned out when
#we consider the whole west. I expect to see similar NPP prediction in the ecoregion model because
#it has what appears to be the more controlling factor on NPP in any given year: PPT.

#STOPPED HERE
#ecoregion null model#

region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')
npp.pred<-list()

for(iRegion in 1:length(region.list)){
  
  ecoregion <- region.list[iRegion]
  
  #map model
  empirical.1<-subset(rangeland_npp_covariates,region==ecoregion)
  model.coef<-subset(rbind_spatial_temporal,region==ecoregion)
  
  beta_i <- mean(model.coef$intercept)
  beta_s <- mean(model.coef$coefficient.spatial)
  beta_t <- mean(model.coef$temporal)
  #beta_sxt <- mean(map_coefficients$coefficient.spatial_temporal)
  
  empirical.1$npp.pred <- (beta_i + beta_s*empirical.1$mm.y) + #intercept and spatial
    (beta_t)*empirical.1$mm.dev #temporal and s-t interaction
  
  npp.pred[[iRegion]] <- empirical.1
  
}

ecoregion_npp_yearly<- do.call("rbind", npp.pred)
head(ecoregion_npp_yearly)


#total r-square
cor_total_ecoregion<-round(cor(ecoregion_npp_yearly$npp.pred,ecoregion_npp_yearly$npp.x,method="pearson")^2,2)

#just spatial
ecoregion_null_npp_means<-ecoregion_npp_yearly[c(1,2,5,12)] %>%
  group_by(x,y) %>%
  summarise_all(mean)
ecoregion_null_npp_means<-data.frame(ecoregion_null_npp_means)
head(ecoregion_null_npp_means)

#spatial means
cor_spatial_ecoregion<-round(cor(ecoregion_null_npp_means$npp.pred,ecoregion_null_npp_means$npp.x)^2,2)

#take a shot at temporal componenet
ecoregion_null_npp_temporal_cor<-ecoregion_npp_yearly[c(1,2,5,12)] %>%
  group_by(x,y) %>%
  summarise(cor(npp.pred,npp.x,method='pearson'))

colnames(ecoregion_null_npp_temporal_cor)<-c('x','y','cor')
head(ecoregion_null_npp_temporal_cor)
ecoregion_null_npp_temporal_cor$r.square<-(ecoregion_null_npp_temporal_cor$cor)^2
cor_temporal_ecoregion<-round(mean(ecoregion_null_npp_temporal_cor$r.square),2)
hist(ecoregion_null_npp_temporal_cor$r.square)  

#follow up to see how predictions vary

#DF of MAP predictions
map_pred_yearly<-map_npp_yearly[c(1,2,4,7)]
colnames(map_pred_yearly)<-c('x','y','npp.x','npp.pred.map')
head(map_pred_yearly)

#DF of herb predictions
herb_pred_yearly<-herb_npp_yearly[c(1,2,4,8)]
colnames(herb_pred_yearly)<-c('x','y','npp.x','npp.pred.herb')
head(herb_pred_yearly)

#DF of ecoregion predictions
ecoregion_pred_yearly<-ecoregion_npp_yearly[c(1,2,5,12)]
colnames(ecoregion_pred_yearly)<-c('x','y','npp.x','npp.pred.ecoregion')
head(ecoregion_pred_yearly)

#calculate differences in predictions
merge_herb_map<-merge(herb_pred_yearly,map_pred_yearly,by=c('x','y','npp.x'))
head(merge_herb_map)
merge_herb_map$map_minus_herb<-merge_herb_map$npp.pred.map - merge_herb_map$npp.pred.herb
summary(merge_herb_map)

pdf('/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/comparing NPP predictions/mapminusherb.pdf')
boxplot(merge_herb_map$map_minus_herb,main='Difference in map and herb npp predictions',
        ylab='MAP - Herb predicted NPP')
dev.off()

#calculate differences in predictions for ecoregion model
merge_herb_map<-merge(merge_herb_map,ecoregion_pred_yearly,by=c('x','y','npp.x'))
head(merge_herb_map)
merge_herb_map$map_minus_ecoregion<-merge_herb_map$npp.pred.map - merge_herb_map$npp.pred.ecoregion
summary(merge_herb_map)

pdf('/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/comparing NPP predictions/mapminusecoregion.pdf')
boxplot(merge_herb_map$map_minus_ecoregion,main='Difference in map and ecoregion npp predictions',
        ylab='MAP - Ecoregion predicted NPP')
dev.off()

#map out average difference
head(merge_herb_map)
#difference between map and herb
map_minus_herb_mean<-aggregate(map_minus_herb~x + y,mean,data=merge_herb_map)
pdf('/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/comparing NPP predictions/mapminusherb.mapped.pdf')
plot(rasterFromXYZ(map_minus_herb_mean),main='MAP-Herb model predicted NPP (average difference)')
dev.off()

#difference between map and ecoregion
map_minus_ecoregion_mean<-aggregate(map_minus_ecoregion~x + y,mean,data=merge_herb_map)
pdf('/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/comparing NPP predictions/mapminusecoregion.mapped.pdf')
plot(rasterFromXYZ(map_minus_ecoregion_mean),main='MAP-ecoregion model predicted NPP (average difference)')
dev.off()

merge_herb_map$obs.minus.map<-merge_herb_map$npp.x-merge_herb_map$npp.pred.map
merge_herb_map$obs.minus.herb<-merge_herb_map$npp.x-merge_herb_map$npp.pred.herb
merge_herb_map$obs.minus.ecoregion<-merge_herb_map$npp.x-merge_herb_map$npp.pred.ecoregion

#compare averge differences in obs vs. predicted in all three models
comp.differences<-merge_herb_map[c(1,2,9,10,11)]
comp.differences.mean<-comp.differences %>%
  group_by(x,y) %>%
  summarise_all(mean)
comp.differences.mean<-data.frame(comp.differences.mean)

pdf('/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/comparing NPP predictions/obs.minus.pred.mapped.pdf')
plot(rasterFromXYZ(comp.differences.mean),
     main=c('obs - pred NPP MAP model','obs - pred NPP herb model','obs - pred NPP ecoregion model'))
dev.off()
head(comp.differences.mean)

#stopped here




###look at the within-loop results######

#total r-squared#

#map null
r.squared.map.null<-bind.r.squared(list.r.squared.noveg)
r.squared.map.null$model<-'map'
summary(r.squared.map.null)
mean(r.squared.map.null$r.squared)

#herb null
r.squared.herb.null<-bind.r.squared(list.r.squared.herb.map)
r.squared.herb.null$model<-'herb'
summary(r.squared.herb.null)
mean(r.squared.herb.null$r.squared)

#ecoregion null
r.squared.ecoregion.null<-bind.r.squared(list.r.squared.ecoregion.null)
r.squared.ecoregion.null$model<-'ecoregion'
summary(r.squared.ecoregion.null)
mean(r.squared.ecoregion.null$r.squared)

#combine them
merge_total_same_main<-rbind(r.squared.ecoregion.null,r.squared.herb.null,r.squared.map.null)
head(merge_total_same_main)
summary(merge_total_same_main)
unique(merge_total_same_main$model)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/R_squared/total_same_main.pdf',
    width=6,height=6)
ggplot(merge_total_same_main,aes(x=r.squared,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(alpha=0.6,aes(y=..scaled..)) +
  ggtitle('same main effects') +
  #ggtitle('model-specific main effects') +
  xlab('Total R-squared') +
  ylab('Density')
dev.off()

#temporal r-squared#

#map null
r.squared.map.temporal.df<-data.frame(do.call("rbind", list.temporal.r.squared.map.null))
colnames(r.squared.map.temporal.df)  <- c("r.squared")
r.squared.map.temporal.df$model<-'map'
head(r.squared.map.temporal.df)

#herb null
r.squared.herb.temporal.df<-data.frame(do.call("rbind", list.temporal.r.squared.veg.null))
colnames(r.squared.herb.temporal.df)  <- c("r.squared")
r.squared.herb.temporal.df$model<-'herb'
head(r.squared.herb.temporal.df)

#ecoregion null
r.squared.ecoregion.temporal.df<-data.frame(do.call("rbind", list.temporal.r.squared.ecoregion.null))
colnames(r.squared.ecoregion.temporal.df)  <- c("r.squared")
r.squared.ecoregion.temporal.df$model<-'ecoregion'
head(r.squared.ecoregion.temporal.df)

merge_temporal_same_main<-rbind(r.squared.map.temporal.df,
                                 r.squared.herb.temporal.df,
                                 r.squared.ecoregion.temporal.df)

summary(merge_temporal_same_main)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/R_squared/temporal_same_main.pdf',
    width=6,height=6)
ggplot(merge_temporal_same_main,aes(x=model,y=r.squared)) +
  #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  #geom_density(alpha=0.6,aes(y=..scaled..)) +
  stat_summary(geom='bar',fun.y='mean') +
  #geom_errorbar()
  ggtitle('same main effects') +
  #ggtitle('model-specific main effects') +
  ylab('Temporal R-squared') +
  xlab('')
  dev.off()
  
#spatial r-squared#
  #map null
  r.squared.map.spatial.df<-data.frame(do.call("rbind", list.spatial.r.squared.map.null))
  colnames(r.squared.map.spatial.df)  <- c("r.squared")
  r.squared.map.spatial.df$model<-'map'
  head(r.squared.map.spatial.df)
  
  #herb null
  r.squared.herb.spatial.df<-data.frame(do.call("rbind", list.spatial.r.squared.veg.null))
  colnames(r.squared.herb.spatial.df)  <- c("r.squared")
  r.squared.herb.spatial.df$model<-'herb'
  head(r.squared.herb.spatial.df)
  
  #ecoregion null
  r.squared.ecoregion.spatial.df<-data.frame(do.call("rbind", list.spatial.r.squared.ecoregion.null))
  colnames(r.squared.ecoregion.spatial.df)  <- c("r.squared")
  r.squared.ecoregion.spatial.df$model<-'ecoregion'
  head(r.squared.ecoregion.spatial.df)
  
  #combine them
  merge_spatial_same_main<-rbind(r.squared.map.spatial.df,
                                  r.squared.herb.spatial.df,
                                  r.squared.ecoregion.spatial.df)
  
  #plot it
  pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/R_squared/spatial_same_main.pdf',
      width=6,height=6)
  ggplot(merge_spatial_same_main,aes(x=model,y=r.squared)) +
    #scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
    #geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
    #geom_density(alpha=0.6,aes(y=..scaled..)) +
    stat_summary(geom='bar',fun.y='mean') +
    ggtitle('same main effects') +
    #ggtitle('model-specific main effect') +
    xlab('') +
    ylab('Spatial R Squared')
  dev.off()
#check the function temporal function
#   test_temporal_cor<-function(x){
#   
#     stratified_final$predict<-predict(x,data=stratified_final)
#   npp_temporal<-stratified_final %>%
#     group_by(x,y) %>%
#     summarise(cor(predict,npp.x,method = "pearson"))
#   colnames(npp_temporal)<-c('x','y','cor')
#   npp_temporal$cor<-(npp_temporal$cor)^2
#   return(data.frame(npp_temporal))
# }
#   #
# npp_temporal_herb<-test_temporal_cor(stratified_final_lm_herb_map)
# head(npp_temporal_herb)
# npp_temporal_map<-test_temporal_cor(stratified_final_lm_noveg)
# head(npp_temporal_map)

#look at within-site temporal predictions 
#this function applies to the last data frame and set of coefficients
#produced in the loop
get_annual_npp<-function(x){
  stratified_final$predict<-predict(x,data=stratified_final)
  return(stratified_final)
  
}

#herb
herb<-get_annual_npp(stratified_final_lm_herb_map)
# herb_mean<-aggregate(predict~x+y,mean,data=herb)
# head(herb_mean)
# colnames(herb_mean) <-c('x','y','mean_predict')
# herb<-merge(herb,herb_mean,by=c('x','y'))
# herb$dev<-herb$predict - herb$mean_predict
# head(herb)
# herb$model <-'herb'
# herb<-herb %>% dplyr::select(c('x','y','year','model','dev'))

#map
map<-get_annual_npp(stratified_final_lm_noveg)
# map_mean<-aggregate(predict~x+y,mean,data=map)
# head(map_mean)
# colnames(map_mean) <-c('x','y','mean_predict')
# map<-merge(map,map_mean,by=c('x','y'))
# map$dev<-map$predict - map$mean_predict
# head(map)
# map$model<-'map'
# map<-map %>% dplyr::select(c('x','y','year','model','dev'))
# 
# map_herb<-merge(map,herb,by=c('x','y','year'))
# head(map_herb)
# map_herb$diff<-map_herb$dev.x - map_herb$dev.y
# 
# herb_map_temporal_cor<-map_herb %>%
#   group_by(x,y) %>%
#   summarise(cor(dev.x,dev.y,method = "pearson"))
# colnames(npp_temporal)<-c('x','y','cor')
# npp_temporal$cor<-(npp_temporal$cor)^2

coef(lm(predict~mm.x,data=map[c(1:30),]))[2]

#ecoregion
ecoregion<-get_annual_npp(stratified_final_ecoregion.null)
# ecoregion_mean<-aggregate(predict~x+y,mean,data=ecoregion)
# head(ecoregion_mean)
# colnames(ecoregion_mean) <-c('x','y','mean_predict')
# ecoregion<-merge(ecoregion,ecoregion_mean,by=c('x','y'))
# ecoregion$dev<-ecoregion$predict - ecoregion$mean_predict
# head(ecoregion)
# ecoregion$model<-'ecoregion'
# ecoregion<-ecoregion %>% dplyr::select(c('x','y','year','model','dev'))

#merge with original data

#herb
# stratified_final_2<- stratified_final %>% dplyr::select(c('x','y','year','npp.dev'))
# head(stratified_final_2)
# stratified_final_2_herb<-merge(stratified_final_2,herb,by=c('x','y','year'))
# head(stratified_final_2_herb)

# stratified_final_2_herb_rsquare<-stratified_final_2_herb %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=summary(model)$adj.r.squared)
# 
# median(stratified_final_2_herb_rsquare$coef)
# 
# #map
# stratified_final_2_map<-merge(stratified_final_2,map,by=c('x','y','year'))
# head(stratified_final_2_map)
# 
# stratified_final_2_map_rsquare<-stratified_final_2_map %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=summary(model)$adj.r.squared)
# 
# median(stratified_final_2_map_rsquare$coef)
# 
# summary(stratified_final_2_herb)
# summary(stratified_final_2_map)
# 
# #ecoregion
# stratified_final_2_ecoregion<-merge(stratified_final_2,ecoregion,by=c('x','y','year'))
# head(stratified_final_2_ecoregion)
# 
# stratified_final_2_ecoregion_rsquare<-stratified_final_2_ecoregion %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=summary(model)$adj.r.squared)
# 
# #herb
# stratified_final_2_herb_slope<-stratified_final_2_herb %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=coef(model)[2])
# 
# #map
# stratified_final_2_map_slope<-stratified_final_2_map %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=coef(model)[2])
# 
# #the same r-squared values do not mean they will have the same slopes. Take a look at this...
# stratified_final_2_ecoregion_slope<-stratified_final_2_ecoregion %>%
#   group_by(x,y) %>%
#   dplyr::do(model = lm(npp.dev~dev, data = .)) %>%
#   dplyr::mutate(coef=coef(model)[2])
# head(stratified_final_2_ecoregion_slope)

#show example of within-site temporal relationship
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Exploratory/R_squared/within_site_example_same_main.pdf',
    width=8,height=6)
par(mfrow=c(1,3))
plot(predict~npp.x,herb[c(1:30),],main='Herb model',
     xlab='Observed NPP',ylab='Predicted NPP',ylim=c(120,220))
text(400,220,paste("model sensitivity =",signif(coef(lm(predict~mm.x,data=herb[c(1:30),]))[2],digits=2)))
text(400,215,paste("P-O r-squared =",signif(summary(lm(predict~npp.x,data=herb[c(1:30),]))$r.squared,digits=2)))
text(400,210,"California annuals site")

plot(predict~npp.x,data=map[c(1:30),],main='MAP model',
     xlab='Observed NPP',ylab='Predicted NPP',ylim=c(120,220))
text(400,220,paste("model sensitivity =",signif(coef(lm(predict~mm.x,data=map[c(1:30),]))[2],digits=2)))
text(400,215,paste("P-O r-squared =",signif(summary(lm(predict~npp.x,data=map[c(1:30),]))$r.squared,digits=2)))

plot(predict~npp.x,data=ecoregion[c(1:30),],main='Ecoregion model',
     xlab='Observed NPP',ylab='Predicted NPP',ylim=c(120,220))
text(400,220,paste("model sensitivity =",signif(coef(lm(predict~mm.x,data=ecoregion[c(1:30),]))[2],digits=2)))
text(400,215,paste("P-O r-squared =",signif(summary(lm(predict~npp.x,data=ecoregion[c(1:30),]))$r.squared,digits=2)))

dev.off()

#main effects have strong impact on NPP intercept, especially for ecoregion model

#next: plot for a site with strong correlation...the residual variation doesn't change, just the X axes,
#which impacts the temporal slope, but seems to have larger implcations for spatial relationships in 
#terms of predictive performance....


#look at AIC across models

herb.aic.null<-mean(bind.aic(list.aic.herb.map)$aic)
map.aic.null<-mean(bind.aic(list.aic.noveg)$aic)
ecoregion.aic.null<-mean(bind.aic(list.aic.ecoregion.null)$aic)

#same main effects

#herb: 187353.7
#map: 187423.2
#ecoregion: 187261.6


#model-specific main effects

##herb: 182891.6
#map: 187444.6
#ecoregion: 172698.1

# VARIACE PARITIONING ########

# LOOK AT RMSE

#ecoregion model
ecoregion.rmse.binded <- do.call("rbind", compare.rmse.region.list)
ecoregion.rmse.binded<-data.frame(ecoregion.rmse.binded)
ecoregion.rmse.binded$model<-'ecoregion'
head(ecoregion.rmse.binded)

#herb model
herb.rmse.binded <- do.call("rbind", compare.rmse.herb.list)
herb.rmse.binded<-data.frame(herb.rmse.binded)
herb.rmse.binded$model<-'herb'
head(herb.rmse.binded)

#map model
map.rmse.binded <- do.call("rbind", ompare.rmse.map.list)
map.rmse.binded<-data.frame(map.rmse.binded)
map.rmse.binded$model<-'map'
head(map.rmse.binded)

#combine all three
rmse.comparisons<-rbind(ecoregion.rmse.binded,herb.rmse.binded,map.rmse.binded)
head(rmse.comparisons)

#partition reduction in RMSE
rmse.comparisons$temporal<-rmse.comparisons$S - rmse.comparisons$S.T
rmse.comparisons$interaction<-rmse.comparisons$S.T - rmse.comparisons$intX
rmse.comparisons$spatial<-rmse.comparisons$S
head(rmse.comparisons)
rmse.comparisons_2<-rmse.comparisons[c(4:7)] #isolate colums before codnensing
head(rmse.comparisons_2)

rmse.comparisons_long <- gather(rmse.comparisons_2, term, value, temporal:spatial, factor_key=TRUE)
head(rmse.comparisons_long)
# summary(rmse.comparisons_long)
# unique(rmse.comparisons$model)
unique(rmse.comparisons$term)

#re-order factors on x axis for plotting
rmse.comparisons_long$model <- factor(rmse.comparisons_long$model,
                                         levels = c("map", "herb","ecoregion"))

#plot it
pdf('./../../../../manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/variance_paritioning/rmse.partitioning.pdf',
    width=6,height=4)
#plot it
ggplot(rmse.comparisons_long,aes(x=model,y=value,fill=term)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,77)) + #for bar plot
  scale_fill_manual(values=c('spatial'='grey','temporal'='blue','interaction'='red'),
                    labels=c('spatial'='Spatial','temporal'='Temporal',
                             'interaction'='Spatiotemporal')) +
  stat_summary(geom='bar',position="stack",fun.y='mean',color='black',size=0.2)+
  scale_x_discrete(breaks=c("ecoregion","herb","map"),
                   labels=c('Ecoregion', "Herbaceous", "MAP")) +
  ylab('Root mean square error') +
  xlab('Model') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=15),
    legend.position = "top",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

# done

# LOOK AT R-SQUARED #

#ecoregion model
ecoregion.rsquare.binded <- do.call("rbind", compare.r.squared.region.list)
ecoregion.rsquare.binded<-data.frame(ecoregion.rsquare.binded )
ecoregion.rsquare.binded$model<-'ecoregion'
head(ecoregion.rsquare.binded)

#herb model
herb.rsquare.binded <- do.call("rbind", compare.r.squared.herb.list)
herb.rsquare.binded<-data.frame(herb.rsquare.binded)
herb.rsquare.binded$model<-'herb'
head(herb.rsquare.binded)

#map model
map.rsquare.binded <- do.call("rbind", ompare.r.squared.map.list)
map.rsquare.binded<-data.frame(map.rsquare.binded)
map.rsquare.binded$model<-'map'
head(map.rsquare.binded)

#combine all three
rsquare.comparisons<-rbind(ecoregion.rsquare.binded,herb.rsquare.binded,map.rsquare.binded)

#partition
rsquare.comparisons$temporal<-rsquare.comparisons$S.T - rsquare.comparisons$S
rsquare.comparisons$spatiotemporal<-rsquare.comparisons$intX - rsquare.comparisons$S.T
rsquare.comparisons$spatial<- rsquare.comparisons$S
rsquare.comparisons_2<-rsquare.comparisons[c(4:7)] #isolate colums before codnensing
head(rsquare.comparisons_2)

#change to long format
rsquare.comparisons_long <- gather(rsquare.comparisons_2, term, value, temporal:spatial, factor_key=TRUE)
head(rsquare.comparisons_long)
rsquare.comparisons_long$variance<-rsquare.comparisons_long$value*100
rsquare.comparisons_mean<-aggregate(variance~model + term,mean,data=rsquare.comparisons_long)
?geom_bar


#re-order factors on x axis for plotting
rsquare.comparisons_long$model <- factor(rsquare.comparisons_long$model,
                                         levels = c("map", "herb","ecoregion"))

#plotting this out: r-squared
pdf('./../../../../manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/variance_paritioning/r.square.partitioning.pdf',
    width=6,height=4)
#plot it
ggplot(rsquare.comparisons_long,aes(x=model,y=variance,fill=term)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,55)) + #for bar plot
  scale_fill_manual(values=c('spatial'='grey','temporal'='blue',spatiotemporal='red'),
                    labels=c('spatial'='Spatial','temporal'='Temporal',
                             'spatiotemporal'='Spatiotemporal')) +
  stat_summary(geom='bar',position="stack",fun.y='mean',color='black',size=0.2)+
  scale_x_discrete(breaks=c("ecoregion","herb","map"),
                   labels=c('Ecoregion', "Herbaceous", "MAP")) +
  ylab('% Variation in NPP explained') +
  xlab('Model') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=15),
    legend.position = "top",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

dev.off()

#done

#########


#follow up to incorporate main effect into compare fit function#######
data$pred_s <- coef(stratified_final_lm_herb_map)[1] + coef(stratified_final_lm_herb_map)[2]*data$mm.y

herb_lm_test<-lm(npp.x~mm.y + mm.dev  + perc_herb_mean + mm.dev:perc_herb_mean
                                 ,stratified_final)
coef(stratified_final_ecoregion.null)[10]
#test with new ifelse statement for models with different main effects

compare_fit_2 <-function(model,data,metric,main,model){
  
  
  # # metric = "R2" or "RMSE"

  # Get predictions from full model (with space*time interaction)
  data$pred_intX <- predict(model)
  
  if(main=='diff'){
    
    
    #just spatial
    data$pred_s <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[4]*data$perc_herb_mean
    
    #spatial and temporal
    data$pred_st <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[3]*data$mm.dev +
      coef(model)[4]*data$perc_herb_mean
        
      }
    
    else{
    
    #just spatial
    data$pred_s <- coef(model)[1] + coef(model)[2]*data$mm.y
    
    #spatial and temporal   
    data$pred_st <- coef(model)[1] + coef(model)[2]*data$mm.y + coef(model)[3]*data$mm.dev
  }
  
  attach(data,warn.conflicts = FALSE)
  if(metric=="RMSE"){
    out <- c(sqrt(mean((npp.x-pred_intX)^2)),
             sqrt(mean((npp.x-pred_st)^2)),
             sqrt(mean((npp.x-pred_s)^2)))
  }else{
    out <- c(cor(npp.x,pred_intX,method="pearson")^2,
             cor(npp.x,pred_st,method="pearson")^2,
             cor(npp.x,pred_s,method="pearson")^2)
  }
  detach(data)
  
  names(out) <- c("intX","S&T","S")
  
  return(out)
  
}
            
compare_fit_2(stratified_final_lm_herb_map,stratified_final,metric = "R2",main='diff')
compare_fit_2(stratified_final_lm_herb_map,stratified_final,metric = 'R2',main='diff')
#stopped here, might need to add an ifelse statement withint he exisiting ifelse statementhe
######


#look at how theherb interaction term changes with the main effect...
map_herb_coefficients<-list_to_df(list.coefficients.herb.map,region=F)
summary(map_herb_coefficients)

# interaction term with main effect of per_herb_mean
# 0.002380
#same value without the per_herb_mean main effect in the model...

# COMPARE PREDICTED VERSUS FITTED TEMPORAL SLOPES ACROSS THE THREE MODELS ########

# FIRST GET PER-PIXEL SLOPES #

sensitivity_conus <- rangeland_npp_covariates %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.dev, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

#head(sensitivity_conus)
sensitivity_conus_coef_only<- data.frame(sensitivity_conus[-c(3)])
head(sensitivity_conus_coef_only)

# NOW GET ECOREGION MODEL SLOPES#

#temporal slopes

df2<-list_to_df_initial(list.coefficients.ecoregion.null) 

#california annuals
df2$california_annuals <- df2$coefficient.temporal
#cold deserts
df2$cold_deserts <- df2$coefficient.temporal + df2$coefficient.temporal_regioncold_deserts 
#hot_deserts
df2$hot_deserts <-  df2$coefficient.temporal + df2$coefficient.temporal_regionhot_deserts 
#northern mixed
df2$northern_mixed_prairies <- df2$coefficient.temporal + df2$coefficient.temporal_regionnorthern_mixed_prairies
#sgs
df2$shortgrass_steppe <- df2$coefficient.temporal  + df2$coefficient.temporal_regionshortgrass_steppe

temporal_slopes<-select_columns(df2)

#change to long format

ecoregion_model <- gather(temporal_slopes, region, coefficient,-run.id, factor_key=TRUE)
#head(ecoregion_model)

#model with ecoregion-specific interaction term...
region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')
coef.list.map<-list()
#coef.list.veg<-list()

for(iRegion in 1:length(region.list)){
  
  ecoregion <- region.list[iRegion]
  
  #map model
  empirical.1<-subset(rangeland_npp_covariates,region==ecoregion)
  model.map<-subset(ecoregion_model,region==ecoregion)
  empirical.1$coef<-mean(model.map$coefficient) 
  slopes.map.st.model.region<-aggregate(coef~x+y,mean,data=empirical.1)
  coef.list.map[[iRegion]] <-slopes.map.st.model.region
  
  
}

ecoregion_model_2<-do.call("rbind",coef.list.map)
#head(ecoregion_model_2)

#combine with empirical dataset
ecoregion_empirical<-merge(sensitivity_conus_coef_only,ecoregion_model_2,by=c('x','y'))
head(ecoregion_empirical)

#merge with ecorergion ID
head(rangeland_npp_covariates)
#just get site means so we have one value per site to merge with
rangeland_npp_covariates_mean<-aggregate(mm.y~x+y+region,mean,data=rangeland_npp_covariates)
head(rangeland_npp_covariates_mean)
ecoregion_empirical <- merge(ecoregion_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))
head(ecoregion_empirical)

#

# MODEL WITH JUST MAP INTERACTION TERM #

map_coefficients <-list_to_df(list.coefficients.noveg,region=F)
#head(map_coefficients)
#head(rangeland_npp_covariates)
map.model<-aggregate(mm.y~x+y,mean,data=rangeland_npp_covariates)
#head(map.model)
#

#temporal slope at MAP for each ecoregion
map.model$temporal_sensitivity <-
  mean(map_coefficients$coefficient.temporal) + 
  mean(map_coefficients$coefficient.spatial_temporal)*map.model$mm.y
#head(map.model)
#

#merge with empirical dataset
map_empirical<-merge(sensitivity_conus_coef_only,map.model,by=c('x','y'))
map_empirical <- merge(map_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))
head(map_empirical)
#

# MODEL WITH JUST %HERB INTERACTION TERM #

herb_coefficients <-list_to_df(list.coefficients.herb.map,region=F)
#head(herb_coefficients)
#head(rangeland_npp_covariates)
herb.model<-aggregate(perc_herb_mean~x+y,mean,data=rangeland_npp_covariates)
#head(herb.model)

#temporal slope at MAP for each ecoregion
herb.model$temporal_sensitivity <-
  mean(herb_coefficients$coefficient.temporal) + 
  mean(herb_coefficients$coefficient.temporal_herb_mean)*herb.model$perc_herb_mean
#head(herb.model)

#merge with empirical dataset
herb_empirical<-merge(sensitivity_conus_coef_only,herb.model,by=c('x','y'))
herb_empirical <- merge(herb_empirical,rangeland_npp_covariates_mean[c(1,2,3)],by=c('x','y'))
head(herb_empirical)


#
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

#plot all this out and save. Margins are being a pain with multi-panel plot...


# S
# specify the color scheme for each ecoregion (same as in the maps) #

sgs.cols <- sample(c("green4"),100,TRUE)
nmp.cols <- sample(c("steelblue2"),100,TRUE)
hd.cols <- sample(c("firebrick3"),100,TRUE)
cd.cols <- sample(c("gold"),100,TRUE)
ca.cols <- sample(c("grey"),100,TRUE)

#if you want the three panel made through R....
# mar.default <- c(5,4,4,2) + 0.1
# # par(mar = mar.default + c(2, 2, 0, 0),mfrow=c(1,3))
# par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1),mfrow=c(1,3),pty="s")
# par(mar.default,mfrow=c(1,3),pty="s")

par("mar")
par(mar=c(1,1,1,1))

mar.default <- c(5,4,2,2) + 0.1
par(mar = mar.default + c(1, 1, 0, 0), mgp=c(3,1,0))

#plot 1: map model predicted versus observed
#library(scales)

sgs.map<-subset(map_empirical,region=='shortgrass_steppe')
nmp.map<-subset(map_empirical,region=='northern_mixed_prairies')
hd.map<-subset(map_empirical,region=='hot_deserts')
cd.map<-subset(map_empirical,region=='cold_deserts')
ca.map<-subset(map_empirical,region=='california_annuals')

pdf('./../../../../manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/predicted_observed/map.model.pdf',
    width=5,height=5.5)

plot(temporal_sensitivity~coef,data=nmp.map,cex=1.2,
     ylab='Predicted temporal sensitivity',xlab='Observed temporal sensitivity',
     main='MAP Model',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)

#cd
points(temporal_sensitivity~coef,data=cd.map,cex=1.2,col=addTrans(cd.cols,15))

#sgs
points(temporal_sensitivity~coef,data=sgs.map,cex=1.2,col=addTrans(sgs.cols,15))

#hd
points(temporal_sensitivity~coef,data=hd.map,cex=1.2,col=addTrans(hd.cols,5))

#ca
points(temporal_sensitivity~coef,data=ca.map,cex=1.2,col=addTrans(ca.cols,60))

abline(a=0,b=1,col='black',lwd=4)
text(0-.02,-0.1,'1:1 line')

legend(-0.15, 0.55, legend=c("Hot deserts", "Cold deserts","California annuals",
                            "Northern mixed prairies","Shortgrass steppe"),
       col=c("firebrick3", "gold","grey","steelblue2","green4"), lty=1,lwd=2,cex=1,box.lty=0)
dev.off()

#plot 2: herb model predicted versus observed

sgs.herb<-subset(herb_empirical,region=='shortgrass_steppe')
nmp.herb<-subset(herb_empirical,region=='northern_mixed_prairies')
hd.herb<-subset(herb_empirical,region=='hot_deserts')
cd.herb<-subset(herb_empirical,region=='cold_deserts')
ca.herb<-subset(herb_empirical,region=='california_annuals')

pdf('./../../../../manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/predicted_observed/herb.model.pdf',
    width=5,height=5.5)

plot(temporal_sensitivity~coef,data=nmp.herb,cex=1.2,
     ylab='Predicted temporal sensitivity',xlab='Observed temporal sensitivity',
     main='Herbaceous Model',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)

#cd
points(temporal_sensitivity~coef,data=cd.herb,cex=1.2,col=addTrans(cd.cols,15))

#sgs
points(temporal_sensitivity~coef,data=sgs.herb,cex=1.2,col=addTrans(sgs.cols,15))

#hd
points(temporal_sensitivity~coef,data=hd.herb,cex=1.2,col=addTrans(hd.cols,5))

#ca
points(temporal_sensitivity~coef,data=ca.herb,cex=1.2,col=addTrans(ca.cols,60))
abline(a=0,b=1,col='black',lwd=4) 

dev.off()

#plot 3
unique(ecoregion_empirical$region)
sgs.ecoregion<-subset(ecoregion_empirical,region=='shortgrass_steppe')
nmp.ecoregion<-subset(ecoregion_empirical,region=='northern_mixed_prairies')
hd.ecoregion<-subset(ecoregion_empirical,region=='hot_deserts')
cd.ecoregion<-subset(ecoregion_empirical,region=='cold_deserts')
ca.ecoregion<-subset(ecoregion_empirical,region=='california_annuals')

pdf('./../../../../manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/predicted_observed/ecoregion.model.pdf',
    width=5,height=5.5)

plot(coef.y~coef.x,data=nmp.ecoregion,cex=1.2,
     ylab='Predicted temporal sensitivity',xlab='Observed temporal sensitivity',
     main='Ecoregion Model',ylim=c(-.13,0.55),xlim=c(-.13,0.55),col=addTrans(nmp.cols,15),cex.lab=1.25)

#cd
points(coef.y~coef.x,data=cd.ecoregion,cex=1.4,col=addTrans(cd.cols,15))

#sgs
points(coef.y~coef.x,data=sgs.ecoregion,cex=1.2,col=addTrans(sgs.cols,15))

#hd
points(coef.y~coef.x,data=hd.ecoregion,cex=1.2,col=addTrans(hd.cols,5))

#ca
points(coef.y~coef.x,data=ca.ecoregion,cex=1,col=addTrans(ca.cols,60))
abline(a=0,b=1,col='black',lwd=4) 

dev.off()

#done.

