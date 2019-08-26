# 'global' model for each vegetation type 

######For california############

cali_overview <- subset(rangeland_npp_covariates_deviations_reduced,region.x=='california_annuals')
summary(cali_overview)
hist(cali_overview$mm.dev)
california_coefficients<-subset(coefficients_wide_map,site=='california_annuals')
cali_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(200,900,50)))

#get means of the coefficients

beta_i_cali <- mean(california_coefficients$Intercept)
beta_s_cali <- mean(california_coefficients$Spatial)
beta_t_cali <- mean(california_coefficients$Temporal)
beta_sxt_cali <- mean(california_coefficients$Spatiotemporal)
head(california_coefficients)

cali_new<-colMeans(california_coefficients[sapply(california_coefficients, is.numeric)])
data.frame(cali_new)

# slopes and NPP predictions from global model
  
  cali_fit$temporal_slope <- beta_t_cali + 
    beta_sxt_cali*cali_fit$map

  cali_fit$NPP= (beta_i_cali + beta_s_cali*cali_fit$map) + (beta_t_cali + beta_sxt_cali*cali_fit$map)*cali_fit$mm.dev

#prep for plotting
mean.slope<-aggregate(cali_temporal_slope~map,mean,data=merge.cali.predict.2)
plot(temporal_slope~map,data=cali_fit)
cali_slope_model<-lm(temporal_slope~map,data=cali_fit)
f <- range(cali_fit$map)
xNew <- seq(f[1],f[2])
yNew <- predict(cali_slope_model,list(map = xNew))
predict.cali.slope<-data.frame(xNew,yNew)

#

#######hot deserts#########

hot_deserts_coefficients<-subset(coefficients_wide_map,site=='hot_deserts')
hot_deserts_overview <- subset(rangeland_npp_covariates_deviations_reduced,region.x=='hot_deserts')
summary(hot_deserts_overview)
hist(hot_deserts_overview$mm.dev)
hot_deserts_fit<-expand.grid(list(mm.dev=seq(-100,200,25),map=seq(100,600,50)))
hot_deserts_fit$ID <- seq.int(nrow(hot_deserts_fit))

beta_i_hot_deserts <- mean(hot_deserts_coefficients$Intercept)
beta_s_hot_deserts <- mean(hot_deserts_coefficients$Spatial)
beta_t_hot_deserts <- mean(hot_deserts_coefficients$Temporal)
beta_sxt_hot_deserts <- mean(hot_deserts_coefficients$Spatiotemporal)

hot_deserts_fit$temporal_slope <- beta_t_hot_deserts + 
  beta_sxt_hot_deserts*hot_deserts_fit$map

hot_deserts_fit$NPP= (beta_i_hot_deserts + beta_s_hot_deserts*hot_deserts_fit$map) + 
  (beta_t_hot_deserts + beta_sxt_hot_deserts*hot_deserts_fit$map)*hot_deserts_fit$mm.dev

#prep for plotting
plot(temporal_slope~map,data=hot_deserts_fit)
hot_deserts_slope_model<-lm(temporal_slope~map,data=hot_deserts_fit)
f <- range(hot_deserts_fit$map)
xNew <- seq(f[1],f[2])
yNew <- predict(hot_deserts_slope_model,list(map = xNew))
predict.hot_deserts.slope<-data.frame(xNew,yNew)

######cold deserts#######
cold_deserts_coefficients<-subset(coefficients_wide_map,site=='cold_deserts')
cold_deserts_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='cold_deserts')
summary(cold_deserts_subset)
hist(cold_deserts_subset$mm.dev)
cold_deserts_fit<-expand.grid(list(mm.dev=seq(-100,200,25),map=seq(100,900,50)))
cold_deserts_fit$ID <- seq.int(nrow(cold_deserts_fit))

beta_i_cold_deserts <- mean(cold_deserts_coefficients$Intercept)
beta_s_cold_deserts <- mean(cold_deserts_coefficients$Spatial)
beta_t_cold_deserts <- mean(cold_deserts_coefficients$Temporal)
beta_sxt_cold_deserts <- mean(cold_deserts_coefficients$Spatiotemporal)

cold_deserts_fit$temporal_slope <- beta_t_cold_deserts + 
  beta_sxt_cold_deserts*cold_deserts_fit$map

cold_deserts_fit$NPP= (beta_i_cold_deserts + beta_s_cold_deserts*cold_deserts_fit$map) + 
  (beta_t_cold_deserts + beta_sxt_cold_deserts*cold_deserts_fit$map)*cold_deserts_fit$mm.dev


#prep for plotting
plot(temporal_slope~map,data=cold_deserts_fit)
cold_deserts_slope_model<-lm(temporal_slope~map,data=cold_deserts_fit)
f <- range(cold_deserts_fit$map)
xNew <- seq(f[1],f[2])
yNew <- predict(cold_deserts_slope_model,list(map = xNew))
predict.cold_deserts.slope<-data.frame(xNew,yNew)

#######shortgrass steppe######
sgs_coefficients<-subset(coefficients_wide_map,site=='semi_arid_steppe')
sgs_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='semi_arid_steppe')
summary(sgs_subset)
hist(sgs_subset$mm.dev)
sgs_fit<-expand.grid(list(mm.dev=seq(-200,200,50),map=seq(275,675,50)))
sgs_fit$ID <- seq.int(nrow(sgs_fit))

beta_i_sgs <- mean(sgs_coefficients$Intercept)
beta_s_sgs <- mean(sgs_coefficients$Spatial)
beta_t_sgs <- mean(sgs_coefficients$Temporal)
beta_sxt_sgs <- mean(sgs_coefficients$Spatiotemporal)

sgs_fit$temporal_slope <- beta_t_sgs + 
  beta_sxt_sgs*sgs_fit$map

sgs_fit$NPP= (beta_i_sgs + beta_s_sgs*sgs_fit$map) + 
  (beta_t_sgs + beta_sxt_sgs*sgs_fit$map)*sgs_fit$mm.dev

#prep for plotting

plot(temporal_slope~map,data=sgs_fit)
sgs_slope_model<-lm(temporal_slope~map,data=sgs_fit)
f <- range(sgs_fit$map)
xNew <- seq(f[1],f[2])
yNew <- predict(sgs_slope_model,list(map = xNew))
predict.sgs.slope<-data.frame(xNew,yNew)

#northern mixed prairies#######
northern_mixed_coefficients<-subset(coefficients_wide_map,site=='northern_mixed_prairies')
northern_mixed_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='northern_mixed_prairies')
summary(northern_mixed_subset)
hist(northern_mixed_subset$mm.y)
northern_mixed_fit<-expand.grid(list(mm.dev=seq(-200,200,50),map=seq(150,900,50)))
northern_mixed_fit$ID <- seq.int(nrow(northern_mixed_fit))

beta_i_northern_mixed <- mean(northern_mixed_coefficients$Intercept)
beta_s_northern_mixed <- mean(northern_mixed_coefficients$Spatial)
beta_t_northern_mixed <- mean(northern_mixed_coefficients$Temporal)
beta_sxt_northern_mixed <- mean(northern_mixed_coefficients$Spatiotemporal)

northern_mixed_fit$temporal_slope <- beta_t_northern_mixed + 
  beta_sxt_northern_mixed*northern_mixed_fit$map

northern_mixed_fit$NPP= (beta_i_northern_mixed + beta_s_northern_mixed*northern_mixed_fit$map) + 
  (beta_t_northern_mixed + beta_sxt_northern_mixed*northern_mixed_fit$map)*northern_mixed_fit$mm.dev

#prep for plotting
plot(temporal_slope~map,data=northern_mixed_fit)
northern_mixed_slope_model<-lm(temporal_slope~map,data=northern_mixed_fit)
f <- range(northern_mixed_fit$map)
xNew <- seq(f[1],f[2])
yNew <- predict(northern_mixed_slope_model,list(map = xNew))
predict.northern_mixed.slope<-data.frame(xNew,yNew)