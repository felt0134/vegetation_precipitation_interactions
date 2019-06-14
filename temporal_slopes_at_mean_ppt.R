#xtracting temporal coefficient at a common MAP across veg types

colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
California.model.terms<-subset(df2,select = c('intercept','coefficient.mm.dev','coefficient.mm.y','coefficient.mm.dev_mm.y'))
head(California.model.terms)

#intercept
beta_i_cali <- California.model.terms$intercept
beta_s_cali <- California.model.terms$coefficient.mm.y
beta_t_cali <- California.model.terms$coefficient.mm.dev
beta_sxt_cali <- California.model.terms$coefficient.mm.dev_mm.y

cali_fit<-expand.grid(list(mm.dev=seq(-200,400,50),map=seq(403.9,403.9)))
cali_fit$ID <- seq.int(nrow(cali_fit))

npp.cali.list.temporal<-list()
slope.cali.list.temporal<-list()

for(i in 1:nrow(California.model.terms)){
  
  cali_temporal_slope<-beta_t_cali[i] + beta_sxt_cali[i]*403.9
  
  slope.cali.list.temporal[[i]] <- data.frame(cali_temporal_slope)
}

npp.cali.temporal.df<-do.call(rbind.data.frame, slope.cali.list.temporal)
npp.cali.temporal.df$ID <- seq.int(nrow(npp.cali.temporal.df))
summary(npp.cali.temporal.df)
hist(npp.cali.temporal.df$cali_temporal_slope)

#hot deserts
hot_deserts_intercept <- subset(vegetation_intercepts,select=c('hot_deserts_intercept','run.id'))
hot_deserts_spatial_slope <- subset(spatial_slopes,select=c('hot_deserts_slope','run.id'))
hot_deserts_temporal_slopes <- subset(temporal_slopes,select=c('hot_deserts_slope','run.id'))
hot_deserts_spatial_temporal <- subset(temporal_spatial_slopes,select=c('hot_deserts_slope','run.id'))
hot_deserts_model<-merge(hot_deserts_intercept ,hot_deserts_spatial_slope,by='run.id')
hot_deserts_model_2<-merge(hot_deserts_temporal_slopes,hot_deserts_model,by='run.id')
hot_deserts_model_3<-merge(hot_deserts_spatial_temporal,hot_deserts_model_2,by='run.id')
head(hot_deserts_model_3)

#make list
slope.hot_deserts.temporal.list<-list()

beta_s_hot_deserts<-hot_deserts_model_3$hot_deserts_slope.x
beta_sxt_hot_deserts<-hot_deserts_model_3$hot_deserts_slope

for(i in 1:nrow(hot_deserts_model_3)){
  
  hot_deserts_temporal_slope<-beta_s_hot_deserts[i] + beta_sxt_hot_deserts[i]*286.14
  
  slope.hot_deserts.temporal.list[[i]] <- data.frame(hot_deserts_temporal_slope)
}

#merge npp data with map and ppt deviation data
npp.hot_deserts.temporal.df<-do.call(rbind.data.frame, slope.hot_deserts.temporal.list)
summary(npp.hot_deserts.temporal.df)
npp.hot_deserts.temporal.df$ID <- seq.int(nrow(npp.hot_deserts.temporal.df))
hist(npp.hot_deserts.temporal.df$hot_deserts_temporal_slope)