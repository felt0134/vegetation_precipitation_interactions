# figure 3

#it is assuemd some figure 1 prep code has already been run. If not, re-run the prep code for figure 1

######## 3A: empirical map of sensitivity ##########

sensitivity_conus <- rangeland_npp_covariates %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.dev, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

#head(sensitivity_conus)
sensitivity_conus_coef_only<- data.frame(sensitivity_conus[ -c(3) ]) #isolate coefficient so only slope is graphed
#head(sensitivity_conus_coef_only)

#make raster
sensitivity_raster <- rasterFromXYZ(sensitivity_conus_coef_only)

#make colors
bks<- quantile(sensitivity_conus_coef_only$coef, probs=seq(0, 1, by=0.05), na.rm = TRUE)
sensitivity=c("purple",'cyan3','green','yellow','orange','red')
bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
r.range.sens <- round(c(minValue(sensitivity_raster), maxValue(sensitivity_raster)),digits=2)

#update projection
proj4string(sensitivity_raster) <- CRS("+proj=longlat")
sensitivity_raster_2<-projectRaster(sensitivity_raster, crs=aea.proj)

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_3/empirical_sens_map_2.pdf',
    width=6,height=6)
plot(sensitivity_raster_2,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    cex.axis=1.75),
     legend.args=list(text='', side=4, font=10, line=2.5, cex.axis=15))
plot(states_all_sites,add=TRUE,lwd = 1)
dev.off()
######

###### prep for producing modeled maps of sensitivity ######
#look at temporal sensitivities in map and veg models with ecoregion
head(rangeland_npp_covariates)
unique(rangeland_npp_covariates$region)

region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')
coef.list.map<-list()
coef.list.veg<-list()

for(iRegion in 1:length(region.list)){
  
  ecoregion <- region.list[iRegion]
  
  #map model
  empirical.1<-subset(rangeland_npp_covariates,region==ecoregion)
  model.map<-subset(map_region_coefficients,region==ecoregion)
  empirical.1$coef<-mean(model.map$Temporal) + mean(model.map$Spatiotemporal)*empirical.1$mm.y
  slopes.map.st.model.region<-aggregate(coef~x+y,mean,data=empirical.1)
  coef.list.map[[iRegion]] <-slopes.map.st.model.region
  
  #veg model
  empirical.2<-subset(rangeland_npp_covariates,region==ecoregion)
  model.veg<-subset(herb_region_coefficients,region==ecoregion)
  empirical.2$coef<-mean(model.veg$Temporal) + mean(model.veg$Spatiotemporal)*empirical.2$perc_herb_mean
  slopes.veg.st.model.region<-aggregate(coef~x+y,mean,data=empirical.2)
  coef.list.veg[[iRegion]] <- slopes.veg.st.model.region
  
  #no ecoregion model:map
  
}
head(empirical.2)

#turn in to function: 
plot_sensitivity<-function(x){
  
  #use color gradient/range from empirical data for all maps so all are ons ame scale
  bks<- quantile(sensitivity_conus_coef_only$coef, probs=seq(0, 1, by=0.05), na.rm = TRUE)
  sensitivity=c("purple",'cyan3','green','yellow','orange','red')
  bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
  r.range.sens <- round(c(minValue(sensitivity_raster), maxValue(sensitivity_raster)),digits=2)
  
  #update projection
  proj4string(x) <- CRS("+proj=longlat")
  sensitivity_raster_2<-projectRaster(x, crs=aea.proj)
  
  plot(sensitivity_raster_2,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
       legend.width=1, legend.shrink=.75,
       axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                      labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                      cex.axis=1.75),
       legend.args=list(text='', side=4, font=10, line=2.5, cex.axis=15,cex.lab=1.5))
  plot(states_all_sites,add=TRUE,lwd = 1)
  
}

#########

####### 3B: map model ##########
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_3/map_sens_proj.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(do.call('rbind',coef.list.map)))
dev.off()

####### 3C: veg model #######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_3/herb_sens_proj.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(do.call('rbind',coef.list.veg)))
dev.off()