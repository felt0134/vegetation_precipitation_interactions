####3d plotting#########

library(lattice)
head(stratified_final)
#hot deserts
hot_deserts_surface<-subset(stratified_final,region.x=='hot_deserts')
summary(hot_deserts_surface)
hot_deserts.loess<-lm(npp.x~mm.dev*mm.y,data=hot_deserts_surface)
summary(hot_deserts_surface)
hot_deserts_fit<-expand.grid(list(mm.dev=seq(-100,200,50),mm.y=seq(100,500,50)))
hot_deserts_fit[1:20,]
z = predict(hot_deserts.loess,hot_deserts_fit)
hot_deserts_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = hot_deserts_fit,
          xlab = list("Annual precipitation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "Hot deserts",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(900),
          screen = list(z = -60, x = -60)
)

#california
cali_annuals_surface<-subset(stratified_final,region.x=='california_annuals')
summary(cali_annuals_surface)
cali_annuals.loess<-lm(npp.x~mm.x*mm.y,data=cali_annuals_surface)
summary(cali_annuals_surface)
cali_annuals_fit<-expand.grid(list(mm.x=seq(25,1325,50),mm.y=seq(100,675,50)))
cali_annuals_fit[1:20,]
z = predict(cali_annuals.loess,cali_annuals_fit)
cali_annuals_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.x*mm.y, data = cali_annuals_fit,
          xlab = list("Annual precipitation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "California annuals",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(1200),
          screen = list(z = -60, x = -60)
)

#cold deserts
cold_deserts_surface<-subset(stratified_final,region.x=='cold_deserts')
summary(cold_deserts_surface)
cold_deserts.loess<-lm(npp.x~mm.x*mm.y,data=cold_deserts_surface)
summary(cold_deserts_surface)
cold_deserts_fit<-expand.grid(list(mm.x=seq(25,1175,50),mm.y=seq(100,800,50)))
cold_deserts_fit[1:20,]
z = predict(cold_deserts.loess,cold_deserts_fit)
cold_deserts_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.x*mm.y, data = cold_deserts_fit,
          xlab = list("Annual precipitation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "Cold deserts",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(1100),
          screen = list(z = -60, x = -60)
)


#shortgrass steppe
sgs_surface<-subset(stratified_final,region.x=='semi-arid_steppe')
summary(sgs_surface)
sgs.loess<-lm(npp.x~mm.x*mm.y,data=sgs_surface)
summary(sgs_surface)
sgs_fit<-expand.grid(list(mm.x=seq(25,900,50),mm.y=seq(275,650,50)))
sgs_fit[1:20,]
z = predict(sgs.loess,sgs_fit)
sgs_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.x*mm.y, data = sgs_fit,
          xlab = list("Annual precipitation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "shortgrass steppe",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(900),
          screen = list(z = -60, x = -60)
)

#northern mixed prairies
northern_mixed_surface<-subset(stratified_final,region.x=='northern_mixed_prairies')
summary(northern_mixed_surface)
northern_mixed.loess<-lm(npp.x~mm.x*mm.y,data=northern_mixed_surface)
summary(northern_mixed_surface)
northern_mixed_fit<-expand.grid(list(mm.x=seq(100,1025,50),mm.y=seq(175,700,50)))
northern_mixed_fit[1:20,]
z = predict(northern_mixed.loess,northern_mixed_fit)
northern_mixed_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.x*mm.y, data = northern_mixed_fit,
          xlab = list("Annual precipitation",rot=-50,cex=1.4), zlab = list("Net primary productivity",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "northern mixed prairies",
          drape = TRUE,
          colorkey = FALSE,
          par.settings = list(axis.line = list(col = 'transparent')),
          col.regions = colorRampPalette(c("red","orange", "green"))(100),
          screen = list(z = -60, x = -60)
)


######shapefiles#######
library(rgdal)

#mojave sonoran
mojave.sonoran.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/MojaveSonoran",layer="MojaveSonoran")
plot(mojave.sonoran.shape)
mojave.sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(mojave.sonoran.shape)
#step 2:
mojave.sonoran.shape.2 <- sp::spTransform(mojave.sonoran.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(mojave.sonoran.shape.2)

#chihuahan
ChihuahuanDesert.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/ChihuahuanDesert",layer="ChihuahuanDesert")
plot(ChihuahuanDesert.shape)
ChihuahuanDesert.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(ChihuahuanDesert.shape)
#step 2:
ChihuahuanDesert.shape.2 <- sp::spTransform(ChihuahuanDesert.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(ChihuahuanDesert.shape.2)

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
plot(ColdDeserts.shape.2)

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
######maps########
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
######mean npp graph#######
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_production_raster<-rasterFromXYZ(mean_production)
plot(mean_production_raster)
break_mean_npp<-quantile(mean_production$npp.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_mean_npp<-mean_production$npp.x
npp_mean_allsites
plot(us)

spplot(mean_production_raster,#scales = list(draw = TRUE),
       at=break_mean_npp,
       asp=0.001,
       col.regions = 
         rev(terrain.colors(length(break_mean_npp)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(NorthernMixedSubset.shape.2, lwd = 1)) +
  latticeExtra::layer(sp.polygons(SGS.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(CaliforniaAnnual.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ChihuahuanDesert.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ColdDeserts.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(mojave.sonoran.shape.2, lwd = 0.1)) +

######mean precipitation graph code##########
head(rangeland_npp_covariates_deviations_1)
mean_mm<-aggregate(mm.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
mean_mm_raster<-rasterFromXYZ(mean_mm)
plot(mean_mm_raster)
break_mean_mm<-quantile(mean_mm$mm.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_mean_mm<-mean_mm$mm.x

spplot(mean_mm_raster,#scales = list(draw = TRUE),
       at=break_mean_mm,
       asp=.1,
       col.regions =
         rev(topo.colors(length(break_mean_mm)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(NorthernMixedSubset.shape.2, lwd = 1)) +
  latticeExtra::layer(sp.polygons(SGS.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(CaliforniaAnnual.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ChihuahuanDesert.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(ColdDeserts.shape.2, lwd = 0.1)) +
  latticeExtra::layer(sp.polygons(mojave.sonoran.shape.2, lwd = 0.1)) +

#######sensitivity to precipitation map########
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

######plotting temporal-map slopes and 95% CIs from model runs#######

error.95 <-function(x) {
  n = length(x)
  std.error = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se(x)
  return(error)
}

ci.site<-aggregate(slope~site,error.95,data=data_long_temporal_spatial)
mean.site<-aggregate(slope~site,mean,data=data_long_temporal_spatial)
mean.ci.site.temporal.spatial.slope<-merge(ci.site,mean.site,by='site')
library(ggplot2)
ggplot(data_long_temporal_spatial,aes(x=slope)) +
  geom_histogram(binwidth = .000005,fill='white',color='black') +
facet_wrap(~site,nrow=5) +
geom_vline(xintercept = 0,color='red')
  geom_errorbar(aes(ymin=slope.y-slope.x,ymax=slope.y+slope.x),width=.2,size=1) +
  stat_summary(geom='point',fun.y=mean,pch=21,fill=
                 'white',size=5) +
  scale_x_discrete(limits=c('hot_deserts_temporal_spatial','cold_deserts_temporal_spatial',
                            'coefficient.mm.y_mm.x','sgs_temporal_spatial','northern_mixed_temporal_spatial'),
                   labels=c('coefficient.mm.y_mm.x'='California annuals','northern_mixed_temporal_spatial'='Northern mixed prairies',
                            'hot_deserts_temporal_spatial' = 'Hot deserts','cold_deserts_temporal_spatial'='Cold deserts',
                            'sgs_temporal_spatial'='Shortgrass steppe')) +
  
  #coord_flip() +        
  #ylab(bquote('Change in sensitivity per mm of MAP ('*g/m^2/mm/MAP*')')) +
  ylab('Change in sensitivity per mm of MAP') +
  
  xlab("") +
    geom_hline(yintercept=0) +
  
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  
  theme(
    
    axis.text.x = element_text(color='black',size=15, angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=11),
    
    axis.title = element_text(color='black',size=18),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    legend.text = element_text(size=12),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))

#########temporal slope with histograms#########
#rename the vegetation types
rename_sites<- c(hot_deserts_temporal_slope="Hot deserts", cold_deserts_temporal_slope="Cold deserts",
                 sgs_temporal_slope="Shortgrass steppe", coefficient.mm.dev="California annuals", 
                 northern_mixed_temporal_slope="Northern mixed prairies")

data_long_temporal$site <- as.character(rename_sites[data_long_temporal$site])

#change the order
data_long_temporal$site <- factor(data_long_temporal$site, levels = c("Hot deserts", "Cold deserts", "California annuals",
                                                    "Shortgrass steppe", "Northern mixed prairies"))

data_long_temporal$site <- factor(data_long_temporal$site, levels = c("hot_deserts_temporal_slope", "cold_deserts_temporal_slope", 
                                                    "california_temporal_slope","sgs_temporal_slope", 
                                                    "northern_mixed_temporal_slope"))
ggplot(data_long_temporal,aes(x=slope)) +
  geom_histogram(binwidth = .01,fill='white',color='black') +
  #geom_density() +
  facet_wrap(~site,nrow=5,scales='free_y') +
  #geom_vline(xintercept=0,color='red') +
  #geom_histogram(color='black',size=.5,alpha=.7) +
  #geom_density_ridges(size=1,alpha=0.5,color='black',calc_ecdf = TRUE) +
  #scale_fill_viridis(name = "Tail probability", direction = -1)
  #geom_density_ridges_gradient(stat = "binline", binwidth = 0.01,color='black',fill='white')
  xlab('Slope of spatial model') +
  #geom_point(size=.1,pch=19,alpha=.1) +
  #scale_y_discrete(limits=c('hot_deserts_spatial_slope','cold_deserts_spatial_slope',
  #   'sgs_spatial_slope','coefficient.mm','northern_mixed_spatial_slope'),
  #labels=c('coefficient.mm'='California annuals','northern_mixed_spatial_slope'='Mixed prairies',
  #   'hot_deserts_spatial_slope' = 'Hot deserts','cold_deserts_spatial_slope'='Cold deserts',
  #  'sgs_spatial_slope'='Semi-arid steppe')) +
  
  #scale_fill_manual(values=c('coefficient.mm'='gray28','northern_mixed_spatial_slope'='dodgerblue',
  #'hot_deserts_spatial_slope' = 'tomato3','cold_deserts_spatial_slope'='gold',
  #'sgs_spatial_slope'='green3')) +
  

xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  
  ylab("") +
  
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  
  theme(
    
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=12),
    
    axis.title = element_text(color='black',size=23),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))

#######change in sensitivity per mm of map all points###########
head(rangeland_npp_covariates_deviations_1)
mean_mm_veg<-aggregate(mm.x~ x + y + region.x,mean,data=rangeland_npp_covariates_deviations_1)
head(mean_mm_veg)
merge_mm_sensitivity<-merge(mean_mm_veg,sensitivity_conus_coef_only,by=c('x','y'))
head(merge_mm_sensitivity)
summary(merge_mm_sensitivity)

rename_sites<- c(hot_deserts="Hot deserts", cold_deserts="Cold deserts",
                 semi-arid_steppe ="Shortgrass steppe", california_annuals="California annuals", 
                 northern_mixed_prairies="Northern mixed prairies")

merge_mm_sensitivity$region.x <- as.character(rename_sites[merge_mm_sensitivity$region.x])

#change the order
merge_mm_sensitivity$region.x <- factor(merge_mm_sensitivity$region.x, levels = c("Hot deserts", "Cold deserts", "California annuals",
                                                                      "Shortgrass steppe", "Northern mixed prairies"))

merge_mm_sensitivity$region.x <- factor(merge_mm_sensitivity$region.x, levels = c("hot_deserts", "cold_deserts", 
                                                                      "california_annuals","semi-arid_steppe", 
                                                                      "northern_mixed_prairies"))
#
cali_annuals_sensitivity<-subset(merge_mm_sensitivity,region.x=='california_annuals')
hot_deserts_sensitivity<-subset(merge_mm_sensitivity,region.x=='hot_deserts')
cold_deserts_sensitivity<-subset(merge_mm_sensitivity,region.x=='cold_deserts')
sgs_sensitivity<-subset(merge_mm_sensitivity,region.x=='semi-arid_steppe')
northern_mixed_sensitivity<-subset(merge_mm_sensitivity,region.x=='northern_mixed_prairies')

ggplot(cali_annuals_sensitivity,aes(mm.x,coef)) +
  geom_point(size=0.5,pch=1) +
  #facet_wrap(~region.x,nrow=1,scale='free') +
  #stat_smooth(method='lm',size=1,color='red') +
  geom_line(data=predict.cali.slope,aes(xNew,yNew),color='red',size=1) +
  xlab('') +
  ylab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  
  xlab("Mean annual precipitation (mm)") +
  #ylab('') +
  
  theme(
    
    axis.text.x = element_text(color='black',size=15), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=15),
    
    axis.title = element_text(color='black',size=20),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))

#####other#########
#plot
library(plotly)
packageVersion('plotly')
devtools::install_github("r-lib/later")
head(volcano)
summary(data)
data <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/3d-line1.csv')
data$color <- as.factor(data$color)
p <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 6, color = ~color, reverscale = FALSE))
#test
head(rangeland_npp_covariates_deviations_1)
summary(rangeland_npp_covariates_deviations_1)
str(data)
rangeland_npp_covariates_deviations_1$region.x <-as.factor(rangeland_npp_covariates_deviations_1$region.x)
sd(rangeland_npp_covariates_deviations_1$npp.dev)

p_test <- plot_ly(rangeland_npp_covariates_deviations_1, x = ~mm.dev, y = ~mm.y,z  = ~npp.dev, 
                  type = 'scatter3d',mode='lines',
                  opacity = 1, line = list(width = 6, color= ~region.x, reverscale = FALSE))
plot(p_test)

effect_plot(cali_model,pred=mm.dev*mm.y)
#ggplot
spatial.hot.deserts<-subset(rangeland_npp_covariates_deviations_1 ,region.x=='hot_deserts')
head(spatial.hot.deserts)
plot.lm<-lm(npp.x~mm.y,spatial.hot.deserts)
summary(plot.lm)
plot.lm<-lm(npp.x~mm.dev,spatial.cali)
head(spatial.cali)
spatial.cali<-subset(rangeland_npp_covariates_deviations_1 ,region.x=='california_annuals')
summary(spatial.cali)
cali_model<-lm(npp.x~mm.dev*mm.y,data=spatial.cali)
summary(cali_model)
library(interplot)
interplot(m = cali_model, var1 = "mm.y", var2 = "mm.dev:mm.y")
#mean
spatial_mean_cali<-aggregate(npp.x~mm.y + x + y,mean,data=spatial.cali)
lm_cal_spatial_mean<-lm(npp.x~mm.y,data=spatial_mean_cali)
summary(lm_cal_spatial_mean)
f <- range(spatial_mean_cali$mm.y)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_cal_spatial_mean,list(mm.y = xNew))
predict.cali_spatial<-data.frame(xNew,yNew)
lines(xNew,yNew)

#driest
lm_cal_spatial_min<-lm(npp.x~mm.x,data=spatial_cali_min_final)
summary(lm_cal_spatial_min)
f <- range(spatial_mean_cali$mm.y)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_cal_spatial_min,list(mm.x = xNew))
predict.cali_spatial_min<-data.frame(xNew,yNew)
lines(xNew,yNew)

#wettest
lm_cal_spatial_max<-lm(npp.x~mm.x,data=spatial_cali_max_final)
summary(lm_cal_spatial_max)
f <- range(spatial_mean_cali$mm.y)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_cal_spatial_max,list(mm.x = xNew))
predict.cali_spatial_max<-data.frame(xNew,yNew)
lines(xNew,yNew)

library(ggplot2)
ggplot(spatial.cali,aes(mm.x,npp.x)) +
  #geom_point(alpha=.1,size=.2,fill='grey',pch=21) +
  #geom_line(data=predict.cali_spatial,aes(xNew,yNew),size=1,color='black') +
  #geom_line(data=predict.cali_spatial_min,aes(xNew,yNew),size=1,,color='red') +
  #geom_line(data=predict.cali_spatial_max,aes(xNew,yNew),size=1,color='blue') +
  stat_smooth(method='lm',se=TRUE,size=.1) +
  facet_wrap(~region.x)
  theme(
    axis.text.x = element_text(color='black',size=12),#angle=45,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    #panel.border = element_rect(fill=NA,colour = "black"),
    #legend.title = element_text(size=15),
    #legend.text = element_text(size=12),
    #legend.position = c(.7,.7),
    #legend.position="none",
    #legend.background=element_blank(),
    #legend.key=element_blank(),
    #legend.position = c(.8,.8),
    panel.background = element_rect(fill=NA,colour = 'black'),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

install.packages("multcomp")
install.packages("sjPlot")
library(sjPlot)
interaction.plot()
summary(stratified_final_lm)
plot_model(stratified_final_lm,type='int')
plot_model(stratified_final_lm,type='int',terms = c('mm.dev'))
interaction.plot()
library(jtools)
interact_plot(stratified_final_lm, pred = "mm.y", modx = "region.x",x.label = 'Mean annual precipitation',y.label = "Mean net primary production")

library(gmodels)
library(snakecase)
library(dplyr)
head(spatial.cali)

rangeland_npp_covariates_min_max <-subset(rangeland_npp_covariates_deviations_1,select=c('x','y','year','npp.x',
                                                                                                    'mm.y','mm.x','region.x'))

spatial.cali<-subset(rangeland_npp_covariates_min_max,region.x=='california_annuals')
spatial.cali$ID <- seq.int(nrow(spatial.cali))
head(spatial.cali)


#identifying the direst and wettest year per pixel

#wettest year per pixel
mm_max<- rangeland_npp_covariates_deviations_1 %>%
  group_by(x,y) %>%
  dplyr::summarise(mm.x = max(mm.x))

mm_max_final<-merge(mm_max,rangeland_npp_covariates_deviations_1,by=c('mm.x','y','x'),no.dups=TRUE)
mm_max_final_reduced <-subset(mm_max_final,select=c('x','y','npp.x','mm.x','region.x'))
mm_max_final_npp_raster<-rasterFromXYZ(mm_max_final_reduced)
plot(mm_max_final_npp_raster)                                                                                               
lm.dry<-lm(npp.x~mm.x,data=spatial_cali_min_final)
plot(npp.x~mm.x,data=mm_max_final)

#driest year per pixel
mm_min<- rangeland_npp_covariates_deviations_1  %>%
  group_by(x,y) %>%
  dplyr::summarise(mm.x = min(mm.x)) 
  
mm_min_final<-merge(mm_min,rangeland_npp_covariates_deviations_1,by=c('mm.x','y','x'),no.dups=TRUE)
head(mm_min_final)
plot(npp.x~mm.x,data=mm_min_final)
mm_min_final_reduced <-subset(mm_min_final,select=c('x','y','npp.x','mm.x','region.x'))
mm_min_final_npp_raster<-rasterFromXYZ(mm_min_final_reduced)
plot(mm_min_final_npp_raster)                                                                                               
lm.dry<-lm(npp.x~mm.x,data=spatial_cali_min_final)
summary(lm.dry)

#mean npp-precip
spatial_npp_means<-aggregate(npp.x~ x + y + region.x,mean,data=rangeland_npp_covariates_deviations_1)
spatial_mm_means<-aggregate(mm.x~x + y,mean,data=rangeland_npp_covariates_deviations_1)
merge_npp_mm_means<-merge(spatial_npp_means,spatial_mm_means,by=c('x','y'))
head(merge_npp_mm_means)

summary(mm_min_final_reduced)
#california
california_mean<-subset(merge_npp_mm_means,region.x=='california_annuals')
california_wet<-subset(mm_max_final_reduced,region.x=='california_annuals')

#produce fit for wettest year per pixel
lm_cal_spatial_max<-lm(npp.x~mm.x,data=california_wet)
summary(lm_cal_spatial_max)
f <- range(california_mean$mm.x)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_cal_spatial_max,list(mm.x = xNew))
predict.cali_spatial_max<-data.frame(xNew,yNew)

#produce fit for driest year per pixel
california_dry<-subset(mm_min_final_reduced,region.x=='california_annuals')
lm_cal_spatial_min<-lm(npp.x~mm.x,data=california_dry)
summary(lm_cal_spatial_min)
f <- range(california_mean$mm.x)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_cal_spatial_min,list(mm.x = xNew))
predict.cali_spatial_min<-data.frame(xNew,yNew)

ggplot(california_mean,aes(mm.x,npp.x)) +
  geom_line(data=predict.cali_spatial_min,aes(xNew,yNew),size=1,,color='red',linetype='dashed') +
  geom_line(data=predict.cali_spatial_max,aes(xNew,yNew),size=1,color='blue',linetype='dashed') +
  stat_smooth(method='lm',size=2,se=FALSE,color='black') +
  ylab(bquote('NPP ('*g/m^2/mm*')')) +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=23),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(.10,.1),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#hot deserts

hot_deserts_mean<-subset(merge_npp_mm_means,region.x=='hot_deserts')
hot_deserts_wet<-subset(mm_max_final_reduced,region.x=='hot_deserts')
hot_deserts_dry<-subset(mm_min_final_reduced,region.x=='hot_deserts')
#produce fit for wettest year per pixel
lm_hot_deserts_spatial_max<-lm(npp.x~mm.x,data=hot_deserts_wet)
summary(lm_hot_deserts_spatial_max)
f <- range(hot_deserts_wet$mm.x)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_hot_deserts_spatial_max,list(mm.x = xNew))
predict.hot_deserts_spatial_max<-data.frame(xNew,yNew)

#produce fit for driest year per pixel
lm_hot_deserts_spatial_min<-lm(npp.x~mm.x,data=hot_deserts_dry)
summary(lm_hot_deserts_spatial_min)
f <- range(hot_deserts_dry$mm.x)
xNew <- seq(f[1],f[2])
yNew <- predict(lm_hot_deserts_spatial_min,list(mm.x = xNew))
predict.hot_deserts_spatial_min<-data.frame(xNew,yNew)

ggplot(hot_deserts_mean,aes(mm.x,npp.x)) +
  geom_line(data=predict.hot_deserts_spatial_min,aes(xNew,yNew),size=1,,color='red',linetype='dashed') +
  geom_line(data=predict.hot_deserts_spatial_max,aes(xNew,yNew),size=1,color='blue',linetype='dashed') +
  stat_smooth(method='lm',size=2,se=FALSE,color='black') +
  ylab(bquote('NPP ('*g/m^2/mm*')')) +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=23),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    #legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.position = c(.10,.1),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
