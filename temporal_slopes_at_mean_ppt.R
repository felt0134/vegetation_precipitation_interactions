#xtracting temporal coefficient at a common MAP across veg types

####california######
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
  
  slope<-beta_t_cali[i] + beta_sxt_cali[i]*403.9
  
  slope.cali.list.temporal[[i]] <- data.frame(slope)
}

npp.cali.temporal.df<-do.call(rbind.data.frame, slope.cali.list.temporal)
npp.cali.temporal.df$ID <- seq.int(nrow(npp.cali.temporal.df))
npp.cali.temporal.df$site <- 'California annuals'
head(npp.cali.temporal.df)
summary(npp.cali.temporal.df)
hist(npp.cali.temporal.df$slope)

#######hot deserts###########
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
  
  slope<-beta_s_hot_deserts[i] + beta_sxt_hot_deserts[i]*286.14
  
  slope.hot_deserts.temporal.list[[i]] <- data.frame(slope)
}

#merge npp data with map and ppt deviation data
npp.hot_deserts.temporal.df<-do.call(rbind.data.frame, slope.hot_deserts.temporal.list)
npp.hot_deserts.temporal.df$site<-'Hot deserts'
head(npp.hot_deserts.temporal.df)
summary(npp.hot_deserts.temporal.df)
npp.hot_deserts.temporal.df$ID <- seq.int(nrow(npp.hot_deserts.temporal.df))
hist(npp.hot_deserts.temporal.df$slope)

########cold deserts##########
cold_deserts_intercept <- subset(vegetation_intercepts,select=c('cold_deserts_intercept','run.id'))
cold_deserts_spatial_slope <- subset(spatial_slopes,select=c('cold_deserts_slope','run.id'))
cold_deserts_temporal_slopes <- subset(temporal_slopes,select=c('cold_deserts_slope','run.id'))
cold_deserts_spatial_temporal <- subset(temporal_spatial_slopes,select=c('cold_deserts_slope','run.id'))
cold_deserts_model<-merge(cold_deserts_intercept ,cold_deserts_spatial_slope,by='run.id')
cold_deserts_model_2<-merge(cold_deserts_temporal_slopes,cold_deserts_model,by='run.id')
cold_deserts_model_3<-merge(cold_deserts_spatial_temporal,cold_deserts_model_2,by='run.id')
head(cold_deserts_model_3)

#make list
slope.cold_deserts.temporal.list<-list()

beta_s_cold_deserts<-cold_deserts_model_3$cold_deserts_slope.x
beta_sxt_cold_deserts<-cold_deserts_model_3$cold_deserts_slope

for(i in 1:nrow(cold_deserts_model_3)){
  
  slope<-beta_s_cold_deserts[i] + beta_sxt_cold_deserts[i]*288.1
  
  slope.cold_deserts.temporal.list[[i]] <- data.frame(slope)
}

#merge npp data with map and ppt deviation data
npp.cold_deserts.temporal.df<-do.call(rbind.data.frame, slope.cold_deserts.temporal.list)
npp.cold_deserts.temporal.df$site<-'Cold deserts'
head(npp.cold_deserts.temporal.df)
summary(npp.cold_deserts.temporal.df)
npp.cold_deserts.temporal.df$ID <- seq.int(nrow(npp.cold_deserts.temporal.df))
hist(npp.cold_deserts.temporal.df$slope)

########northern mixed prairies##########
northern_mixed_intercept <- subset(vegetation_intercepts,select=c('northern_mixed_intercept','run.id'))
northern_mixed_spatial_slope <- subset(spatial_slopes,select=c('northern_mixed_slope','run.id'))
northern_mixed_temporal_slopes <- subset(temporal_slopes,select=c('northern_mixed_slope','run.id'))
northern_mixed_spatial_temporal <- subset(temporal_spatial_slopes,select=c('northern_mixed_slope','run.id'))
northern_mixed_model<-merge(northern_mixed_intercept ,northern_mixed_spatial_slope,by='run.id')
northern_mixed_model_2<-merge(northern_mixed_temporal_slopes,northern_mixed_model,by='run.id')
northern_mixed_model_3<-merge(northern_mixed_spatial_temporal,northern_mixed_model_2,by='run.id')
head(northern_mixed_model_3)

#make list
slope.northern_mixed.temporal.list<-list()

beta_s_northern_mixed<-northern_mixed_model_3$northern_mixed_slope.x
beta_sxt_northern_mixed<-northern_mixed_model_3$northern_mixed_slope

for(i in 1:nrow(northern_mixed_model_3)){
  
  slope<-beta_s_northern_mixed[i] + beta_sxt_northern_mixed[i]*403.3
  
  slope.northern_mixed.temporal.list[[i]] <- data.frame(slope)
}

#merge npp data with map and ppt deviation data
npp.northern_mixed.temporal.df<-do.call(rbind.data.frame, slope.northern_mixed.temporal.list)
npp.northern_mixed.temporal.df$site<-'Northern mixed prairies'
head(npp.northern_mixed.temporal.df)
summary(npp.northern_mixed.temporal.df)
npp.northern_mixed.temporal.df$ID <- seq.int(nrow(npp.northern_mixed.temporal.df))
hist(npp.northern_mixed.temporal.df$slope)

########shortgrass steppe##########
sgs_intercept <- subset(vegetation_intercepts,select=c('sgs_intercept','run.id'))
sgs_spatial_slope <- subset(spatial_slopes,select=c('sgs_slope','run.id'))
sgs_temporal_slopes <- subset(temporal_slopes,select=c('sgs_slope','run.id'))
sgs_spatial_temporal <- subset(temporal_spatial_slopes,select=c('sgs_slope','run.id'))
sgs_model<-merge(sgs_intercept ,sgs_spatial_slope,by='run.id')
sgs_model_2<-merge(sgs_temporal_slopes,sgs_model,by='run.id')
sgs_model_3<-merge(sgs_spatial_temporal,sgs_model_2,by='run.id')
head(sgs_model_3)

#make list
slope.sgs.temporal.list<-list()

beta_s_sgs<-sgs_model_3$sgs_slope.x
beta_sxt_sgs<-sgs_model_3$sgs_slope

for(i in 1:nrow(sgs_model_3)){
  
  slope<-beta_s_sgs[i] + beta_sxt_sgs[i]*417.1
  
  slope.sgs.temporal.list[[i]] <- data.frame(slope[i])
}

#merge npp data with map and ppt deviation data
npp.sgs.temporal.df<-do.call(rbind.data.frame, slope.sgs.temporal.list)
npp.sgs.temporal.df$site <-'Shortgrass steppe'
head(npp.sgs.temporal.df)
summary(npp.sgs.temporal.df)
npp.sgs.temporal.df$ID <- seq.int(nrow(npp.sgs.temporal.df))
hist(npp.sgs.temporal.df$slope)

######merge all into one dataframe#######
temporal_1<-rbind(npp.cali.temporal.df,npp.hot_deserts.temporal.df)
head(temporal_1)
summary(temporal_1)
temporal_2<-rbind(temporal_1,npp.cold_deserts.temporal.df)
temporal_3<-rbind(temporal_2,npp.northern_mixed.temporal.df)
temporal_4<-rbind(temporal_3,npp.sgs.temporal.df)
temporal_4$model<-'Temporal'
colnames(temporal_4)[colnames(temporal_4)=="ID"] <- "run.id"
head(temporal_4)

######merge with spatial slopes dataset

#first get in the desired order
data_long_spatial$site <- factor(data_long_spatial$site, levels = c("sgs_slope","northern_mixed_slope","california_slope",
                                                                    "cold_deserts_slope", "hot_deserts_slope"))
                                                                              

rename_sites<- c(sgs_slope="Shortgrass steppe",northern_mixed_slope="Northern mixed prairies",california_slope="California annuals", 
                 cold_deserts_slope="Cold deserts",hot_deserts_slope="Hot deserts")

data_long_spatial$site <- as.character(rename_sites[data_long_spatial$site])

data_long_spatial$site <- factor(data_long_spatial$site, levels = c("Shortgrass steppe","Northern mixed prairies","California annuals",
                                                                    "Cold deserts","Hot deserts"))

head(data_long_spatial)

temporal_spatial_slopes_rbind<-rbind(data_long_spatial,temporal_4)
head(temporal_spatial_slopes_rbind)
