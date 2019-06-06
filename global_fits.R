#Extracting coeffcieints from the boostrapped, 'global' model
The intercept term: Beta_I

The spatial slope: Beta_S

The temporal slope: Beta_T

The interaction term: Beta_SxT

NPP = Beta_I + Beta_S*MAP_at_X + Beta_T*Dev + Beta_SxT*MAP_at_X*Devs

#For california
head(df2)
colnames(df2)[colnames(df2)=="coefficient.(Intercept)"] <- "intercept"
California.model.terms<-subset(df2,select = c('intercept','coefficient.mm.dev','coefficient.mm.y','coefficient.mm.dev_mm.y'))
head(California.model.terms)
#intercept
beta_i_cali <- mean(California.model.terms$intercept)
beta_s_cali <- mean(California.model.terms$coefficient.mm.y)
beta_t_cali <- mean(California.model.terms$coefficient.mm.dev)
beta_sxt_cali <- mean(California.model.terms$coefficient.mm.dev_mm.y)

cali_fit<-expand.grid(list(mm.dev=seq(-100,200,50),map=seq(100,500,50)))
cali_fit$ID <- seq.int(nrow(cali_fit))

beta_t_cali + beta_sxt_cali*100
         
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

