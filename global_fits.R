#Extracting coeffcieints from the boostrapped, 'global' model

######For california############
head(df2)
colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
California.model.terms<-subset(df2,select = c('intercept','coefficient.mm.dev','coefficient.mm.y','coefficient.mm.dev_mm.y'))
head(California.model.terms)
#intercept
beta_i_cali <- mean(California.model.terms$intercept)
beta_s_cali <- mean(California.model.terms$coefficient.mm.y)
beta_t_cali <- mean(California.model.terms$coefficient.mm.dev)
beta_sxt_cali <- mean(California.model.terms$coefficient.mm.dev_mm.y)

cali_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(200,800,50)))
cali_fit$ID <- seq.int(nrow(cali_fit))

npp.cali.list<-list()
slope.cali.list<-list()
for(i in 1:nrow(cali_fit)){
  
  cali_temporal_slope<-beta_t_cali + beta_sxt_cali*cali_fit$map[i]

  NPP.cali = (beta_i_cali + beta_s_cali*cali_fit$map[i]) + (beta_t_cali + beta_sxt_cali*cali_fit$map[i])*cali_fit$mm.dev[i]
  
  npp.cali.list[[i]] <- data.frame(NPP.cali)
  slope.cali.list[[i]] <- data.frame(cali_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.cali.df<-do.call(rbind.data.frame, npp.cali.list)
npp.cali.df$ID <- seq.int(nrow(npp.cali.df))
merge.cali.predict<-merge(cali_fit,npp.cali.df,by=c('ID'))

#merge slope data with the merged data
slope.cali.df<-do.call(rbind.data.frame, slope.cali.list)
slope.cali.df$ID <- seq.int(nrow(slope.cali.df))
merge.cali.predict.2<-merge(merge.cali.predict,slope.cali.df,by=c('ID'))

#prep for plotting
mean.slope<-aggregate(cali_temporal_slope~map,mean,data=merge.cali.predict.2)
plot(cali_temporal_slope~map,data=mean.slope)
cali_slope_model<-lm(cali_temporal_slope~map,data=mean.slope)
f <- range(mean.slope$map)
xNew <- seq(f[1],f[2])
yNew <- predict(cali_slope_model,list(map = xNew))
predict.cali.slope<-data.frame(xNew,yNew)


#######hot deserts#########
beta_i_hot_deserts <- mean(vegetation_intercepts$hot_deserts_intercept)
beta_s_hot_deserts<- mean(spatial_slopes$hot_deserts_slope)
beta_t_hot_deserts <- mean(temporal_slopes$hot_deserts_slope)
beta_sxt_hot_deserts <- mean(temporal_spatial_slopes$hot_deserts_slope)

hot_deserts_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(100,500,50)))
hot_deserts_fit$ID <- seq.int(nrow(hot_deserts_fit))

npp.hot_deserts.list<-list()
slope.hot_deserts.list<-list()

for(i in 1:nrow(hot_deserts_fit)){
  
  hot_deserts_temporal_slope<-beta_t_hot_deserts + beta_sxt_hot_deserts*hot_deserts_fit$map[i]
  
  NPP.hot_deserts = (beta_i_hot_deserts + beta_s_hot_deserts*hot_deserts_fit$map[i]) + (beta_t_hot_deserts + beta_sxt_hot_deserts*hot_deserts_fit$map[i])*hot_deserts_fit$mm.dev[i]
  
  npp.hot_deserts.list[[i]] <- data.frame(NPP.hot_deserts)
  slope.hot_deserts.list[[i]] <- data.frame(hot_deserts_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.hot_deserts.df<-do.call(rbind.data.frame, npp.hot_deserts.list)
npp.hot_deserts.df$ID <- seq.int(nrow(npp.hot_deserts.df))
merge.hot_deserts.predict<-merge(hot_deserts_fit,npp.hot_deserts.df,by=c('ID'))

#merge slope data with the merged data
slope.hot_deserts.df<-do.call(rbind.data.frame, slope.hot_deserts.list)
slope.hot_deserts.df$ID <- seq.int(nrow(slope.hot_deserts.df))
merge.hot_deserts.predict.2<-merge(merge.hot_deserts.predict,slope.hot_deserts.df,by=c('ID'))

#prep for plotting
mean.slope<-aggregate(hot_deserts_temporal_slope~map,mean,data=merge.hot_deserts.predict.2)
plot(hot_deserts_temporal_slope~map,data=mean.slope)
hot_deserts_slope_model<-lm(hot_deserts_temporal_slope~map,data=mean.slope)
f <- range(mean.slope$map)
xNew <- seq(f[1],f[2])
yNew <- predict(hot_deserts_slope_model,list(map = xNew))
predict.hot_deserts.slope<-data.frame(xNew,yNew)

######cold deserts#######

beta_i_cold_deserts <- mean(vegetation_intercepts$cold_deserts_intercept)
beta_s_cold_deserts<- mean(spatial_slopes$cold_deserts_slope)
beta_t_cold_deserts <- mean(temporal_slopes$cold_deserts_slope)
beta_sxt_cold_deserts <- mean(temporal_spatial_slopes$cold_deserts_slope)

cold_deserts_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='cold_deserts')
summary(cold_deserts_subset)
cold_deserts_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(100,800,50)))
cold_deserts_fit$ID <- seq.int(nrow(cold_deserts_fit))

npp.cold_deserts.list<-list()
slope.cold_deserts.list<-list()

for(i in 1:nrow(cold_deserts_fit)){
  
  cold_deserts_temporal_slope<-beta_t_cold_deserts + beta_sxt_cold_deserts*cold_deserts_fit$map[i]
  
  NPP.cold_deserts = (beta_i_cold_deserts + beta_s_cold_deserts*cold_deserts_fit$map[i]) + (beta_t_cold_deserts + beta_sxt_cold_deserts*cold_deserts_fit$map[i])*cold_deserts_fit$mm.dev[i]
  
  npp.cold_deserts.list[[i]] <- data.frame(NPP.cold_deserts)
  slope.cold_deserts.list[[i]] <- data.frame(cold_deserts_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.cold_deserts.df<-do.call(rbind.data.frame, npp.cold_deserts.list)
npp.cold_deserts.df$ID <- seq.int(nrow(npp.cold_deserts.df))
merge.cold_deserts.predict<-merge(cold_deserts_fit,npp.cold_deserts.df,by=c('ID'))

#merge slope data with the merged data
slope.cold_deserts.df<-do.call(rbind.data.frame, slope.cold_deserts.list)
slope.cold_deserts.df$ID <- seq.int(nrow(slope.cold_deserts.df))
merge.cold_deserts.predict.2<-merge(merge.cold_deserts.predict,slope.cold_deserts.df,by=c('ID'))

#prep for plotting
mean.slope<-aggregate(cold_deserts_temporal_slope~map,mean,data=merge.cold_deserts.predict.2)
plot(cold_deserts_temporal_slope~map,data=mean.slope)
cold_deserts_slope_model<-lm(cold_deserts_temporal_slope~map,data=mean.slope)
f <- range(mean.slope$map)
xNew <- seq(f[1],f[2])
yNew <- predict(cold_deserts_slope_model,list(map = xNew))
predict.cold_deserts.slope<-data.frame(xNew,yNew)

#######shortgrass steppe######

beta_i_sgs <- mean(vegetation_intercepts$sgs_intercept)
beta_s_sgs<- mean(spatial_slopes$sgs_slope)
beta_t_sgs <- mean(temporal_slopes$sgs_slope)
beta_sxt_sgs <- mean(temporal_spatial_slopes$sgs_slope)

sgs_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='semi-arid_steppe')
summary(sgs_subset)
sgs_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(250,700,50)))
sgs_fit$ID <- seq.int(nrow(sgs_fit))

npp.sgs.list<-list()
slope.sgs.list<-list()

for(i in 1:nrow(sgs_fit)){
  
  sgs_temporal_slope<-beta_t_sgs + beta_sxt_sgs*sgs_fit$map[i]
  
  NPP.sgs = (beta_i_sgs + beta_s_sgs*sgs_fit$map[i]) + (beta_t_sgs + beta_sxt_sgs*sgs_fit$map[i])*sgs_fit$mm.dev[i]
  
  npp.sgs.list[[i]] <- data.frame(NPP.sgs)
  slope.sgs.list[[i]] <- data.frame(sgs_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.sgs.df<-do.call(rbind.data.frame, npp.sgs.list)
npp.sgs.df$ID <- seq.int(nrow(npp.sgs.df))
merge.sgs.predict<-merge(sgs_fit,npp.sgs.df,by=c('ID'))

#merge slope data with the merged data
slope.sgs.df<-do.call(rbind.data.frame, slope.sgs.list)
slope.sgs.df$ID <- seq.int(nrow(slope.sgs.df))
merge.sgs.predict.2<-merge(merge.sgs.predict,slope.sgs.df,by=c('ID'))

#prep for plotting
mean.slope<-aggregate(sgs_temporal_slope~map,mean,data=merge.sgs.predict.2)
plot(sgs_temporal_slope~map,data=mean.slope)
sgs_slope_model<-lm(sgs_temporal_slope~map,data=mean.slope)
f <- range(mean.slope$map)
xNew <- seq(f[1],f[2])
yNew <- predict(sgs_slope_model,list(map = xNew))
predict.sgs.slope<-data.frame(xNew,yNew)

#northern mixed prairies#######

beta_i_northern_mixed <- mean(vegetation_intercepts$northern_mixed_intercept)
beta_s_northern_mixed<- mean(spatial_slopes$northern_mixed_slope)
beta_t_northern_mixed <- mean(temporal_slopes$northern_mixed_slope)
beta_sxt_northern_mixed <- mean(temporal_spatial_slopes$northern_mixed_slope)

northern_mixed_subset<-subset(rangeland_npp_covariates_deviations_reduced,region.x=='northern_mixed_prairies')
summary(northern_mixed_subset)
northern_mixed_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(150,900,50)))
northern_mixed_fit$ID <- seq.int(nrow(northern_mixed_fit))

npp.northern_mixed.list<-list()
slope.northern_mixed.list<-list()

for(i in 1:nrow(northern_mixed_fit)){
  
  northern_mixed_temporal_slope<-beta_t_northern_mixed + beta_sxt_northern_mixed*northern_mixed_fit$map[i]
  
  NPP.northern_mixed = (beta_i_northern_mixed + beta_s_northern_mixed*northern_mixed_fit$map[i]) + (beta_t_northern_mixed + beta_sxt_northern_mixed*northern_mixed_fit$map[i])*northern_mixed_fit$mm.dev[i]
  
  npp.northern_mixed.list[[i]] <- data.frame(NPP.northern_mixed)
  slope.northern_mixed.list[[i]] <- data.frame(northern_mixed_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.northern_mixed.df<-do.call(rbind.data.frame, npp.northern_mixed.list)
npp.northern_mixed.df$ID <- seq.int(nrow(npp.northern_mixed.df))
merge.northern_mixed.predict<-merge(northern_mixed_fit,npp.northern_mixed.df,by=c('ID'))

#merge slope data with the merged data
slope.northern_mixed.df<-do.call(rbind.data.frame, slope.northern_mixed.list)
slope.northern_mixed.df$ID <- seq.int(nrow(slope.northern_mixed.df))
merge.northern_mixed.predict.2<-merge(merge.northern_mixed.predict,slope.northern_mixed.df,by=c('ID'))

#prep for plotting
mean.slope<-aggregate(northern_mixed_temporal_slope~map,mean,data=merge.northern_mixed.predict.2)
plot(northern_mixed_temporal_slope~map,data=mean.slope)
northern_mixed_slope_model<-lm(northern_mixed_temporal_slope~map,data=mean.slope)
f <- range(mean.slope$map)
xNew <- seq(f[1],f[2])
yNew <- predict(northern_mixed_slope_model,list(map = xNew))
predict.northern_mixed.slope<-data.frame(xNew,yNew)