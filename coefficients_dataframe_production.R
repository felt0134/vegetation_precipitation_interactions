
####get coefficients into a usable dataframe #####
summary(stratified_final_lm)
df.coefficients <- do.call("rbind", list.coefficients.final)
head(df.coefficients)
df.coefficients.2 <- cbind(rownames(df.coefficients), data.frame(df.coefficients, row.names=NULL))

colnames(df.coefficients.2)  <- c("predictor","coefficient","run.id")

df.coefficients.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub(':', '_', df.coefficients.2$predictor)
df2<-reshape(df.coefficients.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
head(df2)
summary(df2)

####spatial slopes#####

#california
df2$california_annuals <- df2$coefficient.mm.y
#cold deserts
df2$cold_deserts <- df2$coefficient.mm.y + df2$coefficient.region.xcold_deserts_mm.y
#hot_deserts
df2$hot_deserts<-  df2$coefficient.mm.y + df2$coefficient.region.xhot_deserts_mm.y
#northern mixed
df2$northern_mixed_prairies <- df2$coefficient.mm.y  + df2$coefficient.region.xnorthern_mixed_prairies_mm.y
#sgs
df2$semi_arid_steppe <- df2$coefficient.mm.y + df2$coefficient.region.xsemi_arid_steppe_mm.y 

spatial_slopes<-subset(df2,select=c('hot_deserts','cold_deserts','california_annuals','northern_mixed_prairies',
                                    'semi_arid_steppe','run.id'))
head(spatial_slopes)
data_long_spatial <- gather(spatial_slopes, site, coefficient,-run.id, factor_key=TRUE)
head(data_long_spatial)
data_long_spatial$model<-'Spatial'


#####temporal slopes#######

#california annuals
df2$california_annuals <- df2$coefficient.mm.dev
#cold deserts
df2$cold_deserts <- df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xcold_deserts 
#hot_deserts
df2$hot_deserts <-  df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xhot_deserts 
#northern mixed
df2$northern_mixed_prairies <- df2$coefficient.mm.dev + df2$coefficient.mm.dev_region.xnorthern_mixed_prairies
#sgs
df2$semi_arid_steppe <- df2$coefficient.mm.dev  + df2$coefficient.mm.dev_region.xsemi_arid_steppe

temporal_slopes<-subset(df2,select=c('hot_deserts','cold_deserts','california_annuals','northern_mixed_prairies',
                                     'semi_arid_steppe','run.id'))
head(temporal_slopes)
data_long_temporal <- gather(temporal_slopes, site, coefficient,-run.id, factor_key=TRUE)
data_long_temporal$model<-'Temporal'
head(data_long_temporal)
summary(data_long_temporal)

#merge the spatial and temporal coefficient dataframes
rbind_spatial_temporal<-rbind(data_long_spatial,data_long_temporal)
head(rbind_spatial_temporal)
summary(rbind_spatial_temporal)

#####temporal*spatial interaction ########
#california annuals
df2$california_annuals<- df2$coefficient.mm.dev_mm.y
#cold deserts
df2$cold_deserts <- df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xcold_deserts_mm.y
#hot_deserts
df2$hot_deserts<-  df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xhot_deserts_mm.y
#northern mixed
df2$northern_mixed_prairies<- df2$coefficient.mm.dev_mm.y + df2$coefficient.mm.dev_region.xnorthern_mixed_prairies_mm.y
#sgs
df2$semi_arid_steppe <- df2$coefficient.mm.dev_mm.y  + df2$coefficient.mm.dev_region.xsemi_arid_steppe_mm.y

temporal_spatial_slopes<-subset(df2,select=c('hot_deserts','cold_deserts','california_annuals','northern_mixed_prairies',
                                             'semi_arid_steppe','run.id'))
head(temporal_spatial_slopes)
data_long_temporal_spatial <- gather(temporal_spatial_slopes, site, coefficient,-run.id, factor_key=TRUE)
data_long_temporal_spatial$model<-'Spatiotemporal'
head(data_long_temporal_spatial)
summary(data_long_temporal_spatial)

#merge the spatial and temporal coefficient dataframes
rbind_spatial_temporal_spatiotemporal<-rbind(data_long_temporal_spatial,rbind_spatial_temporal)
head(rbind_spatial_temporal_spatiotemporal)
summary(rbind_spatial_temporal)

#####intercepts by veg types######

#california annuals
df2$california_annuals <- df2$intercept
#cold deserts
df2$cold_deserts <- df2$intercept + df2$coefficient.region.xcold_deserts
#hot_deserts
df2$hot_deserts <-  df2$intercept  + df2$coefficient.region.xhot_deserts
#northern mixed
df2$northern_mixed_prairies <- df2$intercept  + df2$coefficient.region.xnorthern_mixed_prairies
#sgs
df2$semi_arid_steppe<- df2$intercept + df2$coefficient.region.xsemi_arid_steppe

vegetation_intercepts<-subset(df2,select=c('hot_deserts','cold_deserts','california_annuals','northern_mixed_prairies',
                                           'semi_arid_steppe','run.id'))
head(vegetation_intercepts)
data_long_vegetation_intercepts <- gather(vegetation_intercepts, site, coefficient,-run.id, factor_key=TRUE)
data_long_vegetation_intercepts$model<-'Intercept'
head(data_long_vegetation_intercepts)
summary(data_long_vegetation_intercepts)

#all coefficient for each veg type
coefficients_full<-rbind(data_long_vegetation_intercepts,rbind_spatial_temporal_spatiotemporal)
head(coefficients_full)

#for wide format
coefficients_wide<- spread(coefficients_full, model, coefficient)
head(coefficients_wide)


site<-c('semi_arid_steppe','northern_mixed_prairies','california_annuals','cold_deserts','hot_deserts')
map<-c(417.1,404.3,403.9,288.1,286.14)
site_map<-data.frame(site,map)

coefficients_wide_map<-merge(coefficients_wide,site_map,by=c('site'))
head(coefficients_wide_map)

#temporal slope at MAP for each vegetation type
coefficients_wide_map$temporal_sensitivity <-
  coefficients_wide_map$Temporal + coefficients_wide_map$Spatiotemporal*coefficients_wide_map$map

#check 95% CI
error.95 <-function(x) {
  n = length(x)
  std.error = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se(x)
  return(error)
}

ci.site<-aggregate(slope~site,error.95,data=data_long_temporal_spatial)
mean.site<-aggregate(slope~site,mean,data=data_long_temporal_spatial)
mean.ci.site.temporal.spatial.slope<-merge(ci.site,mean.site,by='site')