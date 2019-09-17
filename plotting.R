library(ggplot2)
library(lattice)
library(grid)

#function to reference for facet plots
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

#### Figure 3a; surface plots #########

head(stratified_final)

#hot deserts

wireframe(NPP ~ mm.dev*map, data = hot_deserts_fit,
          xlab = list("Precipitation deviation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "Hot deserts",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(600),
          scales = list(z.ticks=1, arrows=FALSE),
          screen = list(z = -60, x = -60)
)

#california

wireframe(NPP ~ mm.dev*map, data = cali_fit,
          xlab = list("Precipitation deviation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "California annuals",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(800),
          scales = list(z.ticks=1, arrows=FALSE),
          screen = list(z = -60, x = -60)
)

#cold deserts

wireframe(NPP ~ mm.dev*map, data = cold_deserts_fit,
          xlab = list("Precipitation deviation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "Cold deserts",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(1100),
          scales = list(z.ticks=1, arrows=FALSE),
          screen = list(z = -60, x = -60)
)


#shortgrass steppe


wireframe(NPP ~ mm.dev*map, data = sgs_fit,
          xlab = list("Precipitation deviation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "shortgrass steppe",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(900),
          scales = list(z.ticks=1, arrows=FALSE),
          screen = list(z = -60, x = -60)
)

#northern mixed prairies

wireframe(NPP ~ mm.dev*map, data = northern_mixed_fit,
          xlab = list("Precipitation deviation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "northern mixed prairies",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(100),
          scales = list(z.ticks=1, arrows=FALSE),
          screen = list(z = -60, x = -60)
)


###### Figure 1a: shapefiles#######
library(rgdal)

#mojave sonoran
mojave.sonoran.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/MojaveSonoran",layer="MojaveSonoran")
plot(mojave.sonoran.shape)
mojave.sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(mojave.sonoran.shape)

#step 2:
mojave.sonoran.shape.2 <- sp::spTransform(mojave.sonoran.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(mojave.sonoran.shape.2)
writeOGR(mojave.sonoran.shape, ".", "test", driver="ESRI Shapefile") 

#chihuahan
ChihuahuanDesert.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/ChihuahuanDesert",layer="ChihuahuanDesert")
plot(ChihuahuanDesert.shape)
ChihuahuanDesert.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(ChihuahuanDesert.shape)
#step 2:
ChihuahuanDesert.shape.2 <- sp::spTransform(ChihuahuanDesert.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ChihuahuanDesert.shape.2)

#grama galleta steppe
Grama.Galleta.Steppe.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/GramaGalletaSteppe",layer="GramaGalletaSteppe")
plot(Grama.Galleta.Steppe.shape)
Grama.Galleta.Steppe.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(Grama.Galleta.Steppe.shape)
#step 2:
Grama.Galleta.Steppe.shape.2 <- sp::spTransform(Grama.Galleta.Steppe.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(Grama.Galleta.Steppe.shape.2)

#california annuals
CaliforniaAnnual.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/CaliforniaAnnual",layer="CaliforniaAnnual")
plot(CaliforniaAnnual.shape)
CaliforniaAnnual.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(CaliforniaAnnual.shape)
#step 2:
CaliforniaAnnual.shape.2 <- sp::spTransform(CaliforniaAnnual.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(CaliforniaAnnual.shape.2)

#cold deserts
ColdDeserts.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/ColdDeserts",layer="ColdDeserts")
plot(CaliforniaAnnual.shape)
ColdDeserts.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(ColdDeserts.shape)
#step 2:
ColdDeserts.shape.2 <- sp::spTransform(ColdDeserts.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ColdDeserts.shape.2,col='blue')

#shortgrass steppe
SGS.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(SGS.shape)
#step 2:
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(SGS.shape.2)

#northernmixed
NorthernMixedSubset.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
plot(NorthernMixedSubset.shape)
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(NorthernMixedSubset.shape)
#step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(NorthernMixedSubset.shape.2)
crop(NorthernMixedSubset.shape.2,mean_production_raster)
###### maps prep ########
library(colorspace)
library(latticeExtra)
library(sp)

#shapefile referecne for state outlines
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]
plot(states_all_sites)
extend(states_all_sites)
crop.test<-extend(mean_production_raster,extent(states_all_sites))
mean_raster_2<-extent(mean_production_raster)
spplot(crop.test,#scales = list(draw = TRUE),
       at=break_mean_npp,
       par.settings = list(axis.line = list(col = 'transparent')), 
       asp=0.6,
       colorkey=FALSE,
       col.regions = FALSE) +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.5)) +
  latticeExtra::layer(sp.polygons(NorthernMixedSubset.shape.2,fill='steelblue2', lwd = .1)) +
  latticeExtra::layer(sp.polygons(SGS.shape.2, lwd = 0.1,fill='green4')) +
  latticeExtra::layer(sp.polygons(CaliforniaAnnual.shape.2,fill='grey', lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ChihuahuanDesert.shape.2, fill='firebrick3', lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ColdDeserts.shape.2, fill='gold', lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(mojave.sonoran.shape.2,fill='firebrick3', lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(Grama.Galleta.Steppe.shape.2, fill='firebrick3', lwd = 0.1)) +

###### Figure 1c: mean npp graph#######
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_production_raster<-rasterFromXYZ(mean_production)
plot(mean_production_raster)
break_mean_npp<-quantile(mean_production$npp.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_mean_npp<-mean_production$npp.x
npp_mean_allsites
plot(us)

spplot(mean_production_raster,#scales = list(draw = TRUE),
       at=break_mean_npp,
       #par.settings = list(axis.line = list(col = 'transparent')),
       asp=0.01,
       col.regions = 
         rev(terrain.colors(length(break_mean_npp)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))


###### Figure 1b: mean precipitation map##########
head(rangeland_npp_covariates_deviations_1)
mean_mm<-aggregate(mm.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_mm_raster<-rasterFromXYZ(mean_mm)
plot(mean_mm_raster)
break_mean_mm<-quantile(mean_mm$mm.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_mean_mm<-mean_mm$mm.x
mm.cropped<-extend(mean_mm_raster,extent(states_all_sites))
spplot(mean_mm_raster,#scales = list(draw = TRUE),
       at=break_mean_mm,
       asp=.1,
       #par.settings = list(axis.line = list(col = 'transparent')),
       col.regions =
         rev(topo.colors(length(break_mean_mm)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 1))

####### Figure 1d: NPP sensitivity to precipitation map########
sensitivity_conus <- rangeland_npp_covariates_deviations_1 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~mm.x, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_conus)
sensitivity_conus_coef_only<- sensitivity_conus[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_conus_coef_only)


sensitivity_raster<-rasterFromXYZ(sensitivity_conus_coef_only)
plot(sensitivity_raster)
break_sensitivity<-quantile(sensitivity_conus_coef_only$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

spplot(sensitivity_raster,#scales = list(draw = TRUE),
       at=break_sensitivity,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

######Figure 2a: plotting spatial and temporal slopes histogram#######
head(coefficients_wide_map_ordered)
spatial_temporal<-coefficients_wide_map_ordered[-c(2,3,5,6,7)]
head(spatial_temporal)
data_long_spatial_temporal <- gather(spatial_temporal,model,coefficient,-site, factor_key=TRUE)
head(data_long_spatial_temporal)
data_spatial<-subset(data_long_spatial_temporal,model=='Spatial')

ggplot(data_long_spatial_temporal ,aes(x=coefficient,fill=model)) +
  geom_histogram(binwidth = .005,color='black') +
  facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_fill_manual(values=c('temporal_sensitivity'='red','Spatial'='lightblue'),
                    labels=c('temporal_sensitivity'='Temporal','Spatial'='Spatial')) +
  xlab(bquote('Sensitivity ('*g/m^2/mm*')')) +
  ylab('') +
  scale_x_continuous(expand = c(0,0),limits=c(0,0.85)) +
  scale_y_continuous(expand = c(0,0),limits=c(0,400)) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = c(0.82,0.95),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


######plotting spatiotemporal slopes historgram#######

ggplot(coefficients_wide_map_ordered,aes(x=Spatiotemporal)) +
  geom_histogram(binwidth = .000005,color='black',fill='white') +
  geom_vline(xintercept=0,size=1,color='red') +
  facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  xlab('Change in sensitivity per mm of MAP') +
  ylab("") +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


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

#####variance explained: veg versus no veg figure 2d##########
ggplot(bind.veg.noveg ,aes(x=var,fill=model)) +
  geom_histogram(binwidth = 0.5,color='black') +
  scale_fill_manual(name = 'Spatiotemporal model',values=c('veg'='red','no.veg'='lightblue'),
                    labels=c('veg'='With vegetation','no.veg'='Without vegetation')) +
  xlab('Variation in NPP explained (%)') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
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
#####other#########
#plot

#####bivariate versions of 3d plot figure 3a######

colfunc <- colorRampPalette(c("red", "blue"))
colfunc(10)
plot(rep(1,10),col=colfunc(10),pch=19,cex=3)

#hot deserts

#dynamics of the spatial model

#minus 100 slope and intercept
hot_deserts_fit_wet_dry_minus_100<-hot_deserts_fit %>% filter(mm.dev %in% c('-100'))

#average year: zero deviation
hot_deserts_fit_wet_dry_minus_0<-hot_deserts_fit %>% filter(mm.dev %in% c('0'))

#plues 400 mm slope and intercept
hot_deserts_fit_wet_dry_plus_200<-hot_deserts_fit %>% filter(mm.dev %in% c('200'))

#subset to hot deserts
hot_deserts_test<-subset(rangeland_npp_covariates_deviations_1,region.x=="hot_deserts")

main_hot_deserts<-ggplot(hot_deserts_test,aes(mm.y,npp.x,fill=mm.dev)) +
  geom_point(pch=21,size=1.25,alpha=0.75,color='white') +
  scale_fill_gradientn(colours = colfunc(10),name='Range (mm)',breaks=c(-200,0,200,500)) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_100,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-100','myline2'= '0','myline3'='200')) + 
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
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    legend.text = element_text(size=11),
    legend.title = element_text(size=11),
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
hot_deserts_fit_wet_dry_2<-hot_deserts_fit %>% filter(map %in% c('100','300','600'))


#select specific columns
hot_deserts_dry_map <- hot_deserts_test %>% dplyr::filter(mm.y > 95, mm.y < 105) 
summary(hot_deserts_dry_map)


hot_deserts_mean_map <- hot_deserts_test %>% dplyr::filter(mm.y > 299,mm.y <301) 
summary(hot_deserts_dry_map)

hot_deserts_wet_map <- hot_deserts_test %>% dplyr::filter(mm.y > 550, mm.y < 650) 
summary(hot_deserts_dry_map)

ggplot(hot_deserts_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=hot_deserts_dry_map,aes(x=mm.dev,y=npp.x),pch=21,size=2,alpha=0.75,fill='white',color='red') +
  geom_point(data=hot_deserts_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',fill='white',size=2,pch=21) +
  geom_point(data=hot_deserts_wet_map,aes(x=mm.dev,y=npp.x),color='blue',fill='white',size=2,pch=21) +
  scale_colour_manual(values=c('100'='red','300' = 'black','600'='blue'),name='MAP (mm)',
                      labels=c('100'='100','300'= '300','600'='600')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
  xlim(-101,201) +
  #ggtitle('hot deserts') +
  xlab('') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
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
cold_deserts_fit_wet_dry_2<-cold_deserts_fit %>% filter(map %in% c('100','300','900'))

#select specific columns
cold_deserts_dry_map <- cold_deserts_test %>% dplyr::filter(mm.y > 89, mm.y < 111) 
summary(cold_deserts_dry_map)


cold_deserts_mean_map <- cold_deserts_test %>% dplyr::filter(mm.y > 299, mm.y < 301) 
summary(cold_deserts_mean_map)

cold_deserts_wet_map <- cold_deserts_test %>% dplyr::filter(mm.y > 875, mm.y < 925) 
summary(cold_deserts_dry_map)

#temporal dynamics
ggplot(cold_deserts_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=cold_deserts_dry_map,aes(x=mm.dev,y=npp.x),color='red',pch=21,size=2,alpha=1,fill='white') +
  geom_point(data=cold_deserts_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',pch=21,size=2,alpha=1,fill='white') +
  geom_point(data=cold_deserts_wet_map,aes(x=mm.dev,y=npp.x),color='blue',pch=21,size=2,alpha=1,fill='white') +
  scale_colour_manual(values=c('100'='red','300' = 'black','900'='blue'),name='MAP (mm)',
                      labels=c('100'='100','300'= '300','900'='900')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
  xlim(-99,201) +
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
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
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
  geom_point(data=cali_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=cali_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=cali_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=2,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('200'='red','400' = 'black','800'='blue'),name='MAP (mm)',
  labels=c('200'='200','400'= '400','800'='800')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
  xlim(-199,401) +
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
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
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
northern_mixed_fit_wet_dry_2<-northern_mixed_fit %>% filter(map %in% c('150','400','900'))
#select specific columns
northern_mixed_test_dry_map <- northern_mixed_test %>% dplyr::filter(mm.y > 125, mm.y < 175) 
summary(northern_mixed_test_dry_map)


northern_mixed_test_mean_map <- northern_mixed_test %>% dplyr::filter(mm.y > 399,mm.y <401) 
summary(northern_mixed_test_dry_map)

northern_mixed_test_wet_map <- northern_mixed_test %>% dplyr::filter(mm.y > 850, mm.y < 950) 
summary(northern_mixed_test_wet_map)

ggplot(northern_mixed_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=northern_mixed_test_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=northern_mixed_test_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=northern_mixed_test_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=2,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('150'='red','400' = 'black','900'='blue'),name='MAP (mm)',
                      labels=c('100'='100','300'= '300','900'='900')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
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
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
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
    legend.text = element_text(size=11),
    legend.title = element_text(size=13),
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
sgs_test_dry_map <- northern_mixed_test %>% dplyr::filter(mm.y > 270, mm.y < 280) 
summary(sgs_test_dry_map)


sgs_test_mean_map <- northern_mixed_test %>% dplyr::filter(mm.y > 424,mm.y < 426) 
summary(sgs_test_dry_map)

sgs_test_wet_map <- northern_mixed_test %>% dplyr::filter(mm.y > 670, mm.y < 680) 
summary(northern_mixed_test_wet_map)

ggplot(sgs_fit_wet_dry_2,aes(mm.dev,NPP,color=as.factor(map))) +
  geom_point(data=sgs_test_dry_map,aes(x=mm.dev,y=npp.x),color='red',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=sgs_test_mean_map,aes(x=mm.dev,y=npp.x),color='gray47',size=2,pch=21,fill='white',alpha=1) +
  geom_point(data=sgs_test_wet_map,aes(x=mm.dev,y=npp.x),color='blue',size=2,pch=21,fill='white',alpha=1) +
  scale_colour_manual(values=c('275'='red','425' = 'black','675'='blue'),name='MAP (mm)',
                      labels=c('275'='275','425'= '425','675'='675')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
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
    legend.title = element_text(size=15),
    legend.text = element_text(size=14),
    legend.direction  = 'horizontal', 
    legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#### spatial and temporal slopes by veg types #########

#spatial slopes
ggplot(coefficients_wide_map_ordered,aes(x=Spatial,fill=site)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_fill_manual(values=c('semi_arid_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('semi_arid_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab(bquote('Spatial sensitivity ('*g/m^2/mm*')')) +
  ylab('') +
  #scale_x_continuous(expand = c(0,0),limits=c(0,0.85)) +
  #scale_y_continuous(expand = c(0,0),limits=c(0,400)) +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = 'top',
    #legend.direction  = 'horizontal', 
    #legend.position = c(0.95,0.85),
    #legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#temporal slopes
ggplot(coefficients_wide_map_ordered,aes(x=temporal_sensitivity,fill=site)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('semi_arid_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('semi_arid_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                            california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#spatiotemporal interaction
head(coefficients_wide_map_ordered)
ggplot(coefficients_wide_map_ordered,aes(x=Spatiotemporal,fill=site)) +
  geom_vline(xintercept=0,color='red',size=1.5) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('semi_arid_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('semi_arid_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  xlab('Change in sensitivity per mm of MAP') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=23),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


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


