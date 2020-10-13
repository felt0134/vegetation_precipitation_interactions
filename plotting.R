#load libraries needed

library(ggplot2)
library(lattice)
library(grid)
library(colorspace)
library(rgdal)
library(latticeExtra)
library(sp)
library(mapproj)

#####extra stuff for plotting#####

veg_names <- c(
  `hot_deserts` = "Hot deserts",
  `cold_deserts` = "Cold deserts",
  'northern_mixed_prairies' = "Northern mixed prairies",
  `semi_arid_steppe` = "Shortgrass steppe",
  `california_annuals` = "California annuals"
)

neworder <- c("semi_arid_steppe","northern_mixed_prairies","california_annuals","cold_deserts","hot_deserts")

coefficients_wide_map_ordered <- arrange(mutate(coefficients_wide_map,
                           site=factor(site,levels=neworder)),site)
#for reference
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_production_raster<-rasterFromXYZ(mean_production)
plot(mean_production_raster)

#needed projection
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

###### Figure 1a: shapefiles#######

#mojave sonoran
mojave.sonoran.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/MojaveSonoran",layer="MojaveSonoran")
plot(mojave.sonoran.shape)
mojave.sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(mojave.sonoran.shape)

#step 2:
mojave.sonoran.shape.2 <- sp::spTransform(mojave.sonoran.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(mojave.sonoran.shape.2)
writeOGR(mojave.sonoran.shape, ".", "test", driver="ESRI Shapefile")
mojave.sonoran.shape.3 <- spTransform(mojave.sonoran.shape.2, CRS(aea.proj))
plot(mojave.sonoran.shape.3)

#chihuahan
ChihuahuanDesert.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/ChihuahuanDesert",layer="ChihuahuanDesert")
plot(ChihuahuanDesert.shape)
ChihuahuanDesert.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(ChihuahuanDesert.shape)
#step 2:
ChihuahuanDesert.shape.2 <- sp::spTransform(ChihuahuanDesert.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ChihuahuanDesert.shape.2)
ChihuahuanDesert.shape.3 <- spTransform(ChihuahuanDesert.shape.2, CRS(aea.proj))
plot(ChihuahuanDesert.shape.3)

#grama galleta steppe
Grama.Galleta.Steppe.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/GramaGalletaSteppe",layer="GramaGalletaSteppe")
plot(Grama.Galleta.Steppe.shape)
Grama.Galleta.Steppe.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(Grama.Galleta.Steppe.shape)
#step 2:
Grama.Galleta.Steppe.shape.2 <- sp::spTransform(Grama.Galleta.Steppe.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(Grama.Galleta.Steppe.shape.2)
Grama.Galleta.Steppe.shape.3 <- spTransform(Grama.Galleta.Steppe.shape.2, CRS(aea.proj))
plot(Grama.Galleta.Steppe.shape.3)

#california annuals
CaliforniaAnnual.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/CaliforniaAnnual",layer="CaliforniaAnnual")
plot(CaliforniaAnnual.shape)
CaliforniaAnnual.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(CaliforniaAnnual.shape)
#step 2:
CaliforniaAnnual.shape.2 <- sp::spTransform(CaliforniaAnnual.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(CaliforniaAnnual.shape.2)
CaliforniaAnnual.shape.3 <- spTransform(CaliforniaAnnual.shape.2, CRS(aea.proj))
plot(CaliforniaAnnual.shape.3)

#cold deserts
ColdDeserts.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/ColdDeserts",layer="ColdDeserts")
plot(CaliforniaAnnual.shape)
ColdDeserts.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(ColdDeserts.shape)
#step 2:
ColdDeserts.shape.2 <- sp::spTransform(ColdDeserts.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ColdDeserts.shape.2,col='blue')
ColdDeserts.shape.3 <- spTransform(ColdDeserts.shape.2, CRS(aea.proj))
plot(ColdDeserts.shape.3)

#shortgrass steppe
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(SGS.shape)
#step 2:
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(SGS.shape.2)
SGS.shape.3 <- spTransform(SGS.shape.2, CRS(aea.proj))
plot(SGS.shape.3)

#northern mixed
NorthernMixedSubset.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
plot(NorthernMixedSubset.shape)
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(NorthernMixedSubset.shape)
#step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(NorthernMixedSubset.shape.2)
crop(NorthernMixedSubset.shape.2,mean_production_raster)
NorthernMixedSubset.shape.3 <- spTransform(NorthernMixedSubset.shape.2, CRS(aea.proj))
plot(NorthernMixedSubset.shape.3)

###### maps prep ########

#shapefile referecne for state outlines. This will results in a sp file being downloaded...
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]
#projections
#aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
#aea.proj.2<- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#orcounty.shp.proj <- spTransform(us , CRS(aea.proj))
#plot(orcounty.shp.proj)
#crs(states_all_sites)
#crs(sensitivity_raster)

#plot shapefiles: figure 1A
plot(states_all_sites)
plot(NorthernMixedSubset.shape.3,col='steelblue2', lwd = .1,add=TRUE)
plot(SGS.shape.3, lwd = 0.1,col='green4', lwd = .1,add=TRUE)
plot(CaliforniaAnnual.shape.3,col='grey', lwd = .1,add=TRUE)
plot(ChihuahuanDesert.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(mojave.sonoran.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(Grama.Galleta.Steppe.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(ColdDeserts.shape.3, col='gold', lwd = .1,add=TRUE)

###### Figure 1c: mean npp graph#######

aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
npp= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bkcols.npp <- colorRampPalette(npp)(length(bks_npp)-1)
bks_npp<- quantile(mean_production$npp.x, probs=seq(0.0, 1, by=0.05), na.rm = TRUE)
proj4string(mean_production_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

r.range.npp <- round(c(minValue(mean_production_raster), maxValue(mean_production_raster)))

#update projection
proj4string(mean_production_raster) <- CRS("+proj=longlat")
mean_production_raster_2<-projectRaster(mean_production_raster, crs=aea.proj)
plot(mean_production_raster_2)
mean_production_raster_3<-crop(mean_production_raster_2,extent(crop_extent))

#plot it
plot(mean_production_raster_2,breaks = bks_npp,box=F,axes=F,col = bkcols.npp,
     legend.width=1,legend.shrink=0.75,
     axis.args=list(at=seq(r.range.npp[1], r.range.npp[2], 100),
                    labels=seq(r.range.npp[1], r.range.npp[2], 100),
                    cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE) 

###### Figure 1b: mean precipitation map##########
head(rangeland_npp_covariates_deviations_1)
mean_mm<-aggregate(mm.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_mm_raster<-rasterFromXYZ(mean_mm)
plot(mean_mm_raster)
precip= c("red", "orange", "yellow",'green','cyan3','purple')
bks_map<- quantile(mean_mm$mm.x, probs=seq(0, 1, by=0.05), na.rm = TRUE)
bkcols.precip <- colorRampPalette(precip)(length(bks_map)-1)
proj4string(mean_mm_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")
r.range <- round(c(minValue(mean_mm_raster), maxValue(mean_mm_raster)))

#update projection
proj4string(mean_mm_raster) <- CRS("+proj=longlat")
mean_mm_raster_2<-projectRaster(mean_mm_raster, crs=aea.proj)
plot(mean_mm_raster_2)

#crop_test_2
plot(mean_mm_raster_2,breaks = bks_map,axes=F,box=F,col = bkcols.precip,
     legend.width=1,legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 100),
                    labels=seq(r.range[1], r.range[2], 100),
                    cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE)

####### Figure 1d: NPP sensitivity to precipitation map########

sensitivity_conus <- rangeland_npp_covariates_deviations_1 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.x, data = .)) %>%
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

us.cropped<-crop(states_all_sites,extent(sensitivity_raster))
plot(us.cropped)
rast_new = projectExtent(sensitivity_raster)
rast_new = projectRaster(sensitivity_raster)
values(rast_new) = values(rast1)
map("state",proj="conic",par=39.83,add=TRUE)

#update projection
proj4string(sensitivity_raster) <- CRS("+proj=longlat")
sensitivity_raster_2<-projectRaster(sensitivity_raster, crs=aea.proj)
plot(mean_production_raster_2)
#sensitivity_raster_3<-crop(sensitivity_raster_2,extent(crop_extent)) #may want more specified cropping...

plot(sensitivity_raster_2,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    cex.axis=1.75),
     legend.args=list(text='', side=4, font=10, line=2.5, cex.axis=15))
plot(states_all_sites,add=TRUE,lwd = 1)

#######change in sensitivity per mm of map all points###########
head(rangeland_npp_covariates_deviations_1)

#create a mean annual precip subset for each veg type and each coordinate
mean_mm_veg<-aggregate(mm.x~ x + y + region.x,mean,data=rangeland_npp_covariates_deviations_1)
head(mean_mm_veg)

#merge this with the sensitivity dataset created from 'sensitivity to precipitation map' section
merge_mm_sensitivity<-merge(mean_mm_veg,sensitivity_conus_coef_only,by=c('x','y'))
head(merge_mm_sensitivity)
summary(merge_mm_sensitivity)
#dimenson: 600 by 500
#subset by site
cali_annuals_sensitivity<-subset(merge_mm_sensitivity,region.x=='california_annuals')
hot_deserts_sensitivity<-subset(merge_mm_sensitivity,region.x=='hot_deserts')
cold_deserts_sensitivity<-subset(merge_mm_sensitivity,region.x=='cold_deserts')
sgs_sensitivity<-subset(merge_mm_sensitivity,region.x=='semi_arid_steppe')
northern_mixed_sensitivity<-subset(merge_mm_sensitivity,region.x=='northern_mixed_prairies')
summary(cali_annuals_sensitivity)

#califonia
sensitivty_inset_cali<-ggplot(cali_annuals_sensitivity,aes(mm.x,coef)) +
  #geom_point(size=1,pch=1) +
  #scale_x_continuous(limit=c(50,1100)) +
  #geom_line(data=predict.cali.slope,aes(xNew,yNew),color='red',size=1.5) +
  geom_point(size=1.25,pch=21,fill='white',color='black',alpha=0.75) +
  geom_line(data=cali_fit,aes(map,temporal_slope),size=2,color='red') +
  xlab('MAP') +
  #ylab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Sensitivity') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#hot deserts
sensitivty_inset_hot_deserts<-ggplot(hot_deserts_sensitivity,aes(mm.x,coef)) +
  geom_point(size=1.25,pch=21,fill='white',color='black',alpha=0.75) +
  geom_line(data=hot_deserts_fit,aes(map,temporal_slope),size=2,color='red') +
  #scale_x_continuous(limit=c(50,1100)) +
  #geom_line(data=predict.hot_deserts.slope,aes(xNew,yNew),color='red',size=1.5) +
  xlab('MAP') +
  #ylab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Sensitivity') +
  #xlab("Mean annual precipitation (mm)") +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#cold_deserts
sensitivty_inset_cold_deserts<-ggplot(cold_deserts_sensitivity,aes(mm.x,coef)) +
  geom_point(size=1.25,pch=21,fill='white',color='black',alpha=0.75) +
  geom_line(data=cold_deserts_fit,aes(map,temporal_slope),size=2,color='red') +
  #geom_point(size=1,pch=1) +
  #scale_x_continuous(limit=c(50,1100)) +
  #geom_line(data=predict.cold_deserts.slope,aes(xNew,yNew),color='red',size=1.5) +
  xlab('MAP') +
  #ylab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Sensitivity') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#shortgrass steppe

sensitivity_inset_sgs<-ggplot(sgs_sensitivity,aes(mm.x,coef)) +
  geom_point(size=1.25,pch=21,fill='white',color='black',alpha=0.75) +
  geom_line(data=sgs_fit,aes(map,temporal_slope),size=2,color='red') +
  #geom_point(size=1,pch=1) +
  #scale_x_continuous(limit=c(50,1100)) +
  #geom_line(data=predict.sgs.slope,aes(xNew,yNew),color='red',size=1.5) +
  xlab('MAP') +
  #ylab(bquote('MAP ('*mm yr^{-1}*')')) +
  ylab('Sensitivity') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#northern mixed
sensitivity_inset_northern_mixed<-ggplot(northern_mixed_sensitivity,aes(mm.x,coef)) +
  geom_point(size=1.25,pch=21,fill='white',color='black',alpha=0.75) +
  geom_line(data=northern_mixed_fit,aes(map,temporal_slope),size=2,color='red') +
  #geom_point(size=1,pch=1) +
  #scale_x_continuous(limit=c(50,1100)) +
  #geom_line(data=predict.northern_mixed.slope,aes(xNew,yNew),color='red',size=1.5) +
  xlab('MAP') +
  #ylab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Sensitivity') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#####variance explained figure 2D: veg versus no veg figure 2d##########
ggplot(bind.veg.noveg ,aes(x=var,fill=model)) +
  geom_histogram(binwidth = 0.5,color='black') +
  scale_fill_manual(name = 'Spatiotemporal model',values=c('veg'='red','no.veg'='lightblue'),
                    labels=c('veg'='With ecoregion','no.veg'='Without ecoregion')) +
  xlab('Variation in NPP explained (%)') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), 
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=17),
    legend.position = c(0.22,0.75),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#####bivariate versions of 3d plot figure 3a######

#color schemes
#colfunc <- colorRampPalette(c("red", "blue"))
#colfunc(10)
#plot(rep(1,10),col=colfunc(10),pch=19,cex=3)

#hot deserts

#dynamics of the spatial model

#minus 100 slope and intercept
hot_deserts_fit_wet_dry_minus_100<-hot_deserts_fit %>% filter(mm.dev %in% c('-100'))

#average year: zero deviation
hot_deserts_fit_wet_dry_minus_0<-hot_deserts_fit %>% filter(mm.dev %in% c('0'))

#plues 200 mm slope and intercept
hot_deserts_fit_wet_dry_plus_200<-hot_deserts_fit %>% filter(mm.dev %in% c('200'))

#subset to hot deserts
hot_deserts_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="hot_deserts")

main_hot_deserts<-ggplot(hot_deserts_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=2,alpha=0.5,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)',breaks=c(-200,0,200,500)) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_100,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-100','myline2'= '0','myline3'='200')) + 
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    legend.text = element_text(size=20),
    legend.title = element_text(size=25),
    panel.background = element_rect(fill=NA),
    legend.key.size = unit(.4, "cm"),
    legend.key.width = unit(0.7,"cm"), 
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")) +
    guides(col = guide_legend(order = 1))


#inset plot
vp <- viewport(width = 0.3, height = 0.27, x = 0.28,y=0.77)

#executing the inset, you create a function the utlizes all the previous code


full <- function() {
  print(main_hot_deserts)
  print(sensitivty_inset_hot_deserts, vp = vp)
}
full()


#dynamics of the temporal model
hot_deserts_fit_wet_dry_2<-hot_deserts_fit %>% filter(map %in% c('100','300','500'))

#select specific columns
hot_deserts_dry_map <- hot_deserts_test %>% dplyr::filter(mm.y > 95, mm.y < 105) 
summary(hot_deserts_dry_map)

hot_deserts_mean_map <- hot_deserts_test %>% dplyr::filter(mm.y > 299,mm.y <301) 
summary(hot_deserts_dry_map)

hot_deserts_wet_map <- hot_deserts_test %>% dplyr::filter(mm.y > 475, mm.y < 525) 
summary(hot_deserts_dry_map)

ggplot(hot_deserts_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=hot_deserts_wet_map,aes(x=mm.dev,y=npp.x),color='blue',fill='white',pch=21,size=3,alpha=1) +
  geom_point(data=hot_deserts_mean_map,aes(x=mm.dev,y=npp.x),color='grey39',fill='white',pch=21,size=3,alpha=1) +
  geom_point(data=hot_deserts_dry_map,aes(x=mm.dev,y=npp.x),fill='white',color='red',pch=21,size=3,alpha=1) +
  scale_colour_manual(values=c('100'='red','300' = 'black','500'='blue'),name='MAP (mm)',
                      labels=c('100'='100','300'= '300','500'='500')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(-101,201) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#cold deserts
summary(cold_deserts_fit)

#minus 100 slope and intercept
cold_deserts_fit_wet_dry_minus_100<-cold_deserts_fit %>% filter(mm.dev %in% c('-100'))

#average year: zero deviation
cold_deserts_fit_wet_dry_minus_0<-cold_deserts_fit %>% filter(mm.dev %in% c('0'))

#plus 200 mm slope and intercept
cold_deserts_fit_wet_dry_plus_200<-cold_deserts_fit %>% filter(mm.dev %in% c('200'))

cold_deserts_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="cold_deserts")

#plot spatial relationship dynamics
main_cold_deserts<-ggplot(cold_deserts_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=1.25,alpha=0.75,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)') +
  geom_smooth(data = cold_deserts_fit_wet_dry_minus_100,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = cold_deserts_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = cold_deserts_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-200','myline2'= '0','myline3'='200')) + 
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    #legend.box = 'Horizontal', 
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    legend.text = element_text(size=11),
    legend.title = element_text(size=13),
    panel.background = element_rect(fill=NA),
    legend.key.size = unit(.4, "cm"),
    legend.key.width = unit(0.7,"cm"), 
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#inset plot
vp <- viewport(width = 0.3, height = 0.27, x = 0.28,y=0.77)

#executing the inset, you create a function the utlizes all the previous code


full <- function() {
  print(main_cold_deserts)
  print(sensitivty_inset_cold_deserts, vp = vp)
}
full()


#dynamics of the temporal model
cold_deserts_fit_wet_dry_2<-cold_deserts_fit %>% filter(map %in% c('100','300','700'))

#select specific columns
cold_deserts_dry_map <- cold_deserts_test %>% dplyr::filter(mm.y > 90, mm.y < 110) 
summary(cold_deserts_dry_map)


cold_deserts_mean_map <- cold_deserts_test %>% dplyr::filter(mm.y > 299, mm.y < 301) 
summary(cold_deserts_mean_map)

cold_deserts_wet_map <- cold_deserts_test %>% dplyr::filter(mm.y > 690, mm.y < 710) 
summary(cold_deserts_dry_map)

#temporal dynamics
ggplot(cold_deserts_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=cold_deserts_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',pch=21,size=3,alpha=1,fill='white') +
  geom_point(data=cold_deserts_wet_map,aes(x=mm.dev,y=npp.x),color='blue',pch=21,size=3,alpha=1,fill='white') +
  geom_point(data=cold_deserts_dry_map,aes(x=mm.dev,y=npp.x),color='red',pch=21,size=3.25,alpha=1,fill='white') +
  scale_colour_manual(values=c('100'='red','300' = 'black','700'='blue'),name='MAP (mm)',
                      labels=c('100'='100','300'= '300','700'='900')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(-99,201) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#california annuals
summary(cali_fit)

#minus 200 slope and intercept
cali_fit_wet_dry_minus_200<-cali_fit %>% filter(mm.dev %in% c('-200'))

#average year: zero deviation
cali_fit_wet_dry_minus_0<-cali_fit %>% filter(mm.dev %in% c('0'))

#plus 400 mm slope and intercept
cali_fit_wet_dry_plus_400<-cali_fit %>% filter(mm.dev %in% c('400'))

cali_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="california_annuals")

#plot spatial relationship dynamics
main_cali<-ggplot(cali_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=1.25,alpha=0.75,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)') +
  geom_smooth(data = cali_fit_wet_dry_minus_200,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = cali_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = cali_fit_wet_dry_plus_400,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  #geom_smooth(data = cali_fit_wet_dry_3,aes(map,NPP,color=deviation),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-200','myline2'= '0','myline3'='400')) + 
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    #legend.box = 'Horizontal', 
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    legend.text = element_text(size=11),
    legend.title = element_text(size=13),
    panel.background = element_rect(fill=NA),
    legend.key.size = unit(.4, "cm"),
    legend.key.width = unit(0.7,"cm"), 
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#inset plot
vp <- viewport(width = 0.3, height = 0.27, x = 0.28,y=0.77)

#executing the inset, you create a function the utlizes all the previous code

full <- function() {
  print(main_cali)
  print(sensitivty_inset_cali, vp = vp)
}
full()

#temporal dynamics
cali_fit_wet_dry_2<-cali_fit %>% filter(map %in% c('200','400','800'))

#select specific columns

cali_dry_map <- cali_test %>% dplyr::filter(mm.y > 189, mm.y < 211) 
summary(cali_dry_map)


cali_mean_map <- cali_test %>% dplyr::filter(mm.y > 389, mm.y < 411) 
summary(cali_mean_map)

cali_wet_map <- cali_test %>% dplyr::filter(mm.y > 775, mm.y < 825) 
summary(cali_wet_map)

#subset and get slopes and intercepts
ggplot(cali_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=cali_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=3,pch=21,fill='white',alpha=1) +
  geom_point(data=cali_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=3.25,pch=21,fill='white',alpha=1) +
  geom_point(data=cali_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=3,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('200'='red','400' = 'black','800'='blue'),name='MAP (mm)',
  labels=c('200'='200','400'= '400','800'='800')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(-199,401) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#northern mixed
summary(northern_mixed_fit)
northern_mixed_fit_wet_dry_2<-northern_mixed_fit %>% filter(map %in% c('150','400','900'))

#spatial dynamics

#minus 100 slope and intercept
northern_mixed_fit_wet_dry_minus_200<-northern_mixed_fit %>% filter(mm.dev %in% c('-200'))

#average year: zero deviation
northern_mixed_fit_wet_dry_minus_0<-northern_mixed_fit %>% filter(mm.dev %in% c('0'))

#plus 200 mm slope and intercept
northern_mixed_fit_wet_dry_plus_200<-northern_mixed_fit %>% filter(mm.dev %in% c('200'))

#subset to
northern_mixed_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="northern_mixed_prairies")


#plot spatial relationship dynamics
main_northern_mixed<-ggplot(northern_mixed_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=1.25,alpha=0.75,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)') +
  geom_smooth(data = northern_mixed_fit_wet_dry_minus_200,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = northern_mixed_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = northern_mixed_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-200','myline2'= '0','myline3'='200')) + 
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    #legend.box = 'Horizontal', 
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    legend.text = element_text(size=11),
    legend.title = element_text(size=13),
    panel.background = element_rect(fill=NA),
    legend.key.size = unit(.4, "cm"),
    legend.key.width = unit(0.7,"cm"), 
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#inset
vp <- viewport(width = 0.3, height = 0.27, x = 0.28,y=0.77)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(main_northern_mixed)
  print(sensitivity_inset_northern_mixed, vp = vp)
}
full()

#dynamics of the temporal model
northern_mixed_fit_wet_dry_2<-northern_mixed_fit %>% filter(map %in% c('200','400','650'))
#select specific columns
northern_mixed_test_dry_map <- northern_mixed_test %>% dplyr::filter(mm.y > 195, mm.y < 205) 
summary(northern_mixed_test_dry_map)


northern_mixed_test_mean_map <- northern_mixed_test %>% dplyr::filter(mm.y > 399,mm.y < 401) 
summary(northern_mixed_test_dry_map)

northern_mixed_test_wet_map <- northern_mixed_test %>% dplyr::filter(mm.y > 649, mm.y < 651) 
summary(northern_mixed_test_wet_map)

ggplot(northern_mixed_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=northern_mixed_test_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=3,pch=21,fill='white',alpha=1) +
  geom_point(data=northern_mixed_test_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=3.5,pch=21,fill='white',alpha=1) +
  geom_point(data=northern_mixed_test_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=3.5,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('200'='red','400' = 'black','650'='blue'),name='MAP (mm)',
                      labels=c('200'='200','400'= '400','650'='650')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(-201,201) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#shortgrass steppe
summary(sgs_fit)

#spatial dynamics

#minus 100 slope and intercept
sgs_fit_wet_dry_minus_200<-sgs_fit %>% filter(mm.dev %in% c('-200'))

#average year: zero deviation
sgs_fit_wet_dry_minus_0<-sgs_fit %>% filter(mm.dev %in% c('0'))

#plus 200 mm slope and intercept
sgs_fit_wet_dry_plus_200<-sgs_fit %>% filter(mm.dev %in% c('200'))

sgs_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="semi_arid_steppe")

#plot spatial relationship dynamics
main_sgs<-ggplot(sgs_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=1.25,alpha=0.75,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)') +
  geom_smooth(data = sgs_fit_wet_dry_minus_200,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = sgs_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = sgs_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-200','myline2'= '0','myline3'='200')) + 
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    #legend.box = 'Horizontal', 
    legend.direction  = 'horizontal', 
    #legend.margin = margin(r = 125, l = 125),
    legend.position = 'top',
    legend.text = element_text(size=25),
    legend.title = element_text(size=20),
    panel.background = element_rect(fill=NA),
    legend.key.size = unit(.4, "cm"),
    legend.key.width = unit(0.7,"cm"), 
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

vp <- viewport(width = 0.3, height = 0.27, x = 0.28,y=0.77)

#executing the inset, you create a function the utlizes all the previous code
full <- function() {
  print(main_sgs)
  print(sensitivity_inset_sgs, vp = vp)
}
full()

#temporal dynamics
sgs_fit_wet_dry_2<-sgs_fit %>% filter(map %in% c('275','425','675'))

#select specific columns
sgs_test_dry_map <- northern_mixed_test %>% dplyr::filter(mm.y > 274, mm.y < 276) 
summary(sgs_test_dry_map)


sgs_test_mean_map <- northern_mixed_test %>% dplyr::filter(mm.y > 424, mm.y < 426) 
summary(sgs_test_dry_map)

sgs_test_wet_map <- northern_mixed_test %>% dplyr::filter(mm.y > 670, mm.y < 680) 
summary(northern_mixed_test_wet_map)

ggplot(sgs_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=sgs_test_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=3,pch=21,fill='white',alpha=1) +
  geom_point(data=sgs_test_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=3.25,pch=21,fill='white',alpha=1) +
  geom_point(data=sgs_test_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=3,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('275'='red','425' = 'black','675'='blue'),name='MAP (mm)',
                      labels=c('275'='275','425'= '425','675'='675')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(-201,201) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

citation()

###########temporal variation in vegetation cover among veg types #########


#hot deserts
head(hot_deserts_cover)
hot_deserts_cover_2<-hot_deserts_cover[-c(6,7,8,9)]
head(hot_deserts_cover_2)
hot_deserts_cover_3<- gather(hot_deserts_cover_2, group, Cover,-Year, factor_key=TRUE)
head(hot_deserts_cover_3)

ggplot(hot_deserts_cover_3,aes(Year,Cover,color=as.factor(group))) +
  scale_colour_manual(values=c('Perennial.forb...grass.cover'='darkgreen','Bare.ground.cover' = 'black','Shrub.cover'='blue',
                               'Annual.forb...grass.cover'='red'),name='Cover type',
                      labels=c('Perennial.forb...grass.cover'='Perrenial grass and forb','Bare.ground.cover'= 'Bare ground',
                               'Shrub.cover'='Shrub','Annual.forb...grass.cover'='Annual grass and forb')) +
  stat_summary(geom='line',fun.y='mean',size=1) +
  scale_y_continuous(expand = c(0,0),limits=c(0,65)) +
  #ggtitle('Hot deserts') +
  ylab('Average % Cover') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.position = c(0.2,0.85),
    #legend.title = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=15),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#cold deserts
head(cold_deserts_cover)
cold_deserts_cover_2<-cold_deserts_cover[-c(6,7,8,9)]
head(cold_deserts_cover_2)
cold_deserts_cover_3<- gather(cold_deserts_cover_2, group, Cover,-Year, factor_key=TRUE)
head(cold_deserts_cover_3)

ggplot(cold_deserts_cover_3,aes(Year,Cover,color=as.factor(group))) +
  scale_colour_manual(values=c('Perennial.forb...grass.cover'='darkgreen','Bare.ground.cover' = 'black','Shrub.cover'='blue',
                               'Annual.forb...grass.cover'='red'),name='Cover type',
                      labels=c('Perennial.forb...grass.cover'='Perrenial grass and forb','Bare.ground.cover'= 'Bare ground',
                               'Shrub.cover'='Shrub','Annual.forb...grass.cover'='Annual grass and forb')) +
  stat_summary(geom='line',fun.y='mean',size=1) +
  scale_y_continuous(expand = c(0,0),limits=c(0,65)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=15),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#california annuals
head(cali_cover)
cali_cover_2<-cali_cover[-c(6,7,8,9)]
head(cali_cover_2)
cali_cover_3<- gather(cali_cover_2, group, Cover,-Year, factor_key=TRUE)
head(cali_cover_3)

ggplot(cali_cover_3,aes(Year,Cover,color=as.factor(group))) +
  scale_colour_manual(values=c('Perennial.forb...grass.cover'='darkgreen','Bare.ground.cover' = 'black','Shrub.cover'='blue',
                               'Annual.forb...grass.cover'='red'),name='Cover type',
                      labels=c('Perennial.forb...grass.cover'='Perrenial grass and forb','Bare.ground.cover'= 'Bare ground',
                               'Shrub.cover'='Shrub','Annual.forb...grass.cover'='Annual grass and forb')) +
  stat_summary(geom='line',fun.y='mean',size=1) +
  scale_y_continuous(expand = c(0,0),limits=c(0,65)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=15),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#northern mixed prairies
head(northern_mixed_cover)
northern_mixed_cover_2<-northern_mixed_cover[-c(6,7,8,9)]
head(northern_mixed_cover_2)
northern_mixed_cover_3<- gather(northern_mixed_cover_2, group, Cover,-Year, factor_key=TRUE)
head(northern_mixed_cover_3)

ggplot(northern_mixed_cover_3,aes(Year,Cover,color=as.factor(group))) +
  scale_colour_manual(values=c('Perennial.forb...grass.cover'='darkgreen','Bare.ground.cover' = 'black','Shrub.cover'='blue',
                               'Annual.forb...grass.cover'='red'),name='Cover type',
                      labels=c('Perennial.forb...grass.cover'='Perrenial grass and forb','Bare.ground.cover'= 'Bare ground',
                               'Shrub.cover'='Shrub','Annual.forb...grass.cover'='Annual grass and forb')) +
  stat_summary(geom='line',fun.y='mean',size=1) +
  scale_y_continuous(expand = c(0,0),limits=c(0,65)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=15),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#shortgrass steppe
sgs_cover_2<-sgs_cover[-c(6,7,8,9)]
head(sgs_cover_2)
sgs_cover_3<- gather(sgs_cover_2, group, Cover,-Year, factor_key=TRUE)
head(sgs_cover_3)

ggplot(sgs_cover_3,aes(Year,Cover,color=as.factor(group))) +
  scale_colour_manual(values=c('Perennial.forb...grass.cover'='darkgreen','Bare.ground.cover' = 'black','Shrub.cover'='blue',
                               'Annual.forb...grass.cover'='red'),name='Cover type',
                      labels=c('Perennial.forb...grass.cover'='Perrenial grass and forb','Bare.ground.cover'= 'Bare ground',
                               'Shrub.cover'='Shrub','Annual.forb...grass.cover'='Annual grass and forb')) +
  stat_summary(geom='line',fun.y='mean',size=1) +
  scale_y_continuous(expand = c(0,0),limits=c(0,65)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=17),
    legend.text = element_text(size=15),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#binned###########

#hot deserts spatial binned code.

hot_deserts_fit_wet_dry<-hot_deserts_fit %>% filter(mm.dev %in% c('-100','0','200'))

#select specific columns
hot_deserts_dry_year <- hot_deserts_test %>% dplyr::filter(mm.dev > -101, mm.dev < -99 ) 
#summary(hot_deserts_test)
#plot(npp.x~mm.y,data=hot_deserts_dry_year)

hot_deserts_mean_year <- hot_deserts_test %>% dplyr::filter(mm.dev > -1,mm.dev < 1) 
summary(hot_deserts_dry_map)

hot_deserts_wet_year <- hot_deserts_test %>% dplyr::filter(mm.dev > 199, mm.dev < 201) 
#summary(hot_deserts_dry_map) 

main_hot_deserts<-ggplot(hot_deserts_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=hot_deserts_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=3,alpha=1,fill='white',color='red') +
  geom_point(data=hot_deserts_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=hot_deserts_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=3,pch=21,alpha=1) +
  scale_colour_manual(values=c('-100'='red','0' = 'black','200'='blue'),name='Deviation (mm)',
                      labels=c('100'='-100','0'= '0','200'='+200')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(100,500) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#Cold deserts spatial binned code

cold_deserts_fit_wet_dry<-cold_deserts_fit %>% filter(mm.dev %in% c('-100','0','200'))

#select specific columns
cold_deserts_dry_year <- cold_deserts_test %>% dplyr::filter(mm.dev > -101, mm.dev < -99 ) 
#summary(cold_deserts_test)
#plot(npp.x~mm.y,data=cold_deserts_dry_year)

cold_deserts_mean_year <- cold_deserts_test %>% dplyr::filter(mm.dev > -1,mm.dev < 1) 
#summary(cold_deserts_dry_map)

cold_deserts_wet_year <- cold_deserts_test %>% dplyr::filter(mm.dev > 199, mm.dev < 201) 
#summary(cold_deserts_dry_map) 

main_cold_deserts<-ggplot(cold_deserts_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=cold_deserts_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=3.25,alpha=1,fill='white',color='red') +
  geom_point(data=cold_deserts_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=cold_deserts_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=3,pch=21,alpha=1) +
  scale_colour_manual(values=c('-100'='red','0' = 'black','200'='blue'),name='Deviation (mm)',
                      labels=c('-100'='-100','0'= '0','200'='200')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(100,700) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#california spatial binned

cali_fit_wet_dry<-cali_fit %>% filter(mm.dev %in% c('-200','0','400'))

#select specific columns
cali_dry_year <- cali_test %>% dplyr::filter(mm.dev > -205, mm.dev < -195 ) 
summary(cali_test)
plot(npp.x~mm.y,data=cali_dry_year)

cali_mean_year <- cali_test %>% dplyr::filter(mm.dev > -5,mm.dev < 5) 
summary(cali_dry_map)

cali_wet_year <- cali_test %>% dplyr::filter(mm.dev > 390, mm.dev < 410) 
summary(cali_dry_map) 


main_cali<-ggplot(cali_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=cali_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=cali_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=cali_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=3,alpha=1,fill='white',color='red') +
  scale_colour_manual(values=c('-200'='red','0' = 'black','400'='blue'),name='Deviation (mm)',
                      labels=c('-200'='-200','0'= '0','400'='+400')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(200,800) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#nothern mixed binned

northern_mixed_fit_wet_dry<-northern_mixed_fit %>% filter(mm.dev %in% c('-200','0','200'))

#select specific columns
northern_mixed_dry_year <- northern_mixed_test %>% dplyr::filter(mm.dev > -201, mm.dev < -199 ) 
summary(northern_mixed_test)
plot(npp.x~mm.y,data=northern_mixed_dry_year)

northern_mixed_mean_year <- northern_mixed_test %>% dplyr::filter(mm.dev > -1,mm.dev < 1) 
summary(northern_mixed_dry_map)

northern_mixed_wet_year <- northern_mixed_test %>% dplyr::filter(mm.dev > 199, mm.dev < 201) 
summary(northern_mixed_dry_map) 


main_northern_mixed<-ggplot(northern_mixed_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=northern_mixed_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=northern_mixed_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=northern_mixed_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=3,alpha=1,fill='white',color='red') +
  scale_colour_manual(values=c('-200'='red','0' = 'black','200'='blue'),name='Deviation (mm)',
                      labels=c('-200'='-200','0'= '0','200'='200')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(200,675) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#SGS binned

sgs_fit_wet_dry<-sgs_fit %>% filter(mm.dev %in% c('-200','0','200'))

#select specific columns
sgs_dry_year <- sgs_test %>% dplyr::filter(mm.dev > -201, mm.dev < -199 ) 
summary(sgs_test)
plot(npp.x~mm.y,data=sgs_dry_year)

sgs_mean_year <- sgs_test %>% dplyr::filter(mm.dev > -1,mm.dev < 1) 
summary(sgs_dry_map)

sgs_wet_year <- sgs_test %>% dplyr::filter(mm.dev > 199, mm.dev < 202) 
summary(sgs_dry_map) 


main_sgs<-ggplot(sgs_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=sgs_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=sgs_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=3,pch=21,alpha=1) +
  geom_point(data=sgs_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=3,alpha=1,fill='white',color='red') +
  scale_colour_manual(values=c('-200'='red','0' = 'black','200'='blue'),name='Deviation (mm)',
                      labels=c('-200'='-200','0'= '0','200'='200')) +
  geom_smooth(method='lm',se=FALSE,size=2,fullrange=TRUE) +
  xlim(275,675) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=25), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=25),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=25),
    legend.text = element_text(size=20),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))