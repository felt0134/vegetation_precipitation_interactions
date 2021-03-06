main_hot_deserts<-ggplot(hot_deserts_fit_wet_dry,aes(map,NPP,color=as.factor(mm.dev))) +
  geom_point(data=hot_deserts_dry_year,aes(x=mm.y,y=npp.x),pch=21,size=1,alpha=0.75,fill='white',color='red') +
  geom_point(data=hot_deserts_mean_year,aes(x=mm.y,y=npp.x),color='gray47',fill='white',size=1,pch=21,alpha=0.75) +
  geom_point(data=hot_deserts_wet_year,aes(x=mm.y,y=npp.x),color='blue',fill='white',size=1,pch=21,alpha=0.75) +
  scale_colour_manual(values=c('-100'='red','0' = 'black','200'='blue'),name='Deviation (mm)',
                      labels=c('100'='-100','0'= '0','200'='200')) +
  geom_smooth(method='lm',se=FALSE,size=3,fullrange=TRUE) +
  xlim(100,500) +
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

hot_deserts_fit_wet_dry<-hot_deserts_fit %>% filter(mm.dev %in% c('-100','0','200'))

#select specific columns
hot_deserts_dry_year <- hot_deserts_test %>% dplyr::filter(mm.dev > -105, mm.dev < -95 ) 
summary(hot_deserts_test)
plot(npp.x~mm.y,data=hot_deserts_dry_year)

hot_deserts_mean_year <- hot_deserts_test %>% dplyr::filter(mm.dev > -5,mm.dev < 5) 
summary(hot_deserts_dry_map)

hot_deserts_wet_year <- hot_deserts_test %>% dplyr::filter(mm.dev > 195, mm.dev < 205) 
summary(hot_deserts_dry_map) 


###john's code###
library(raster)
library(sp)
library(rgdal)
#change projection
projInfo(type = "proj")
?projInfo
test<-projectExtent(crop_mm_raster, target_CRS)
proj4string(crop_mm_raster) <- CRS("+proj=longlat")
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
raster_AES <- projectRaster(crop_mm_raster, crs=aea.proj) 
shapefile_aes<-spTransform(states_all_sites,aea.proj)
plot(raster_AES)
plot(shapefile_aes)
crop_extent<-zoom()
crop_test_2<-crop(raster_AES,extent(crop_extent))
plot(crop_test_2)
target_CRS <- "+proj=utm +zone=12 +north +ellps=WGS84 +units=m"        
test_raster_2<- projectRaster(sensitivity_raster, test, crs=target_CRS,method="bilinear", 
                                     alignOnly=FALSE, over=FALSE)  
proj4string(sensitivity_raster_2)
state_all_sites_2 <- spTransform(states_all_sites , CRS(target_CRS))
proj4string(state_all_sites_2)
plot(state_all_sites_2)
plot(sensitivity_raster_2)

#Create a matrix with random data & use image()
xy <- matrix(rnorm(400),20,20)
image(xy)

# Turn the matrix into a raster
rast <- raster(xy)
# Give it lat/lon coords for 36-37°E, 3-2°S
extent(rast) <- c(-122, -97.4375, 29.1875, 49)

# ... and assign a projection
projection(crop_mm_raster) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(crop_mm_raster)
plot(state_all_sites_2),add=TRUE)
library(tmap)
library(tmaptools)

set_projection(crop_mm_raster, projection = aea.proj.2)
plot(crop_mm_raster)
crs(sensitivity_raster_2) <- CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs')
test_crop<-crop(state_all_sites_2,extent(crop_mm_raster))
#For colors, there are a lot of ways to do it.  Here's what I'm currently doing:
#  1) Identify vectors of colornames that will define how a palette progresses. Here are a few that I've generated:
plot(test_crop)
  brown2green <- c("saddlebrown", "tan", "darkolivegreen1", "darkolivegreen")
red2pink <- c("gray20", "saddlebrown", "tan1", "red3", "pink")
yopr <- c( "navy", "lightskyblue1", "gray20",    "saddlebrown" ,"red", "pink","orange","yellow")
blue2darkblue= c( "lightskyblue1", "navy", "plum3", "purple", "darkmagenta")


#2) For a given plot, determine the breaks that you want to use to divide up the values for coloring...this example just breaks the values into quantiles from 0 to 1, by .05 (21 breaks, which will correspond to 20 colors)

#(dat is your dataset with one row per site; var is the variable you want to plot)

bks<- quantile(sensitivity_conus_coef_only$coef, probs=seq(0, 1, by=0.05), na.rm = TRUE) #the rounding just makes the breaks more appealing for plotting in the legend
bks<- round(quantile(sensitivity_conus_coef_only[, coef], probs=seq(0, 1, by=0.05), na.rm = TRUE), 1)
#3) Using those breaks and one of the color vectors created in step 1,  create a color palette of colors for all intervals between the breaks 

bkcols.blues <- colorRampPalette(blue2darkblue)(length(bks_map)-1)
plot(bkcols.blues)
#You can also use this to create palettes with different colors for values above or below zero (useful for variable depicting the change in something):

delbkcols <- c( colorRampPalette(red2pink)(sum(bks<0) ),colorRampPalette(blue2darkblue)(sum(bks>0)))

#4) Use those breaks and colors to plot a raster:


#mean annual precip
precip= c("red", "orange", "yellow",'green','cyan3','purple')
bkcols.precip <- colorRampPalette(precip)(length(bks_map)-1)
bks_map<- quantile(mean_mm$mm.x, probs=seq(0, 1, by=0.05), na.rm = TRUE)
proj4string(mean_mm_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")
 
r.range <- round(c(minValue(mean_mm_raster), maxValue(mean_mm_raster)))

plot(crop_test_2,breaks = bks_map,axes=F,box=F,col = bkcols.precip,
     legend.width=1,legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 100),
                                       labels=seq(r.range[1], r.range[2], 100),
                                   cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(shapefile_aes, lwd = 1,add=TRUE)

test_crop<-crop(shapefile_aes,extent(raster_AES))

#mean primary production
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
plot(out, lwd = 1,add=TRUE) 

par(mfrow=c(1,1))
#sensitivity to precip
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
sensitivity_raster_3<-crop(sensitivity_raster_2,extent(crop_extent))

plot(sensitivity_raster_3,breaks = bks,axes=F,box=F,col = bkcols.sensitivity,legend=TRUE,
     legend.width=1, legend.shrink=.75,
     axis.args=list(at=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    labels=seq(r.range.sens[1], r.range.sens[2], 0.1),
                    cex.axis=1.75),
     legend.args=list(text='', side=4, font=10, line=2.5, cex.axis=15))
plot(shapefile_aes,add=TRUE,lwd = 1)

help(plot, package="raster")
#For the isolines, I don't have a ready example, but I think this function might be what you need: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/kde2d.html



latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.5)) +
  plot(NorthernMixedSubset.shape.2,border='steelblue2', lwd = 2.5,add=TRUE) +
  plot(SGS.shape.2, lwd = 2.5,border='green4',add=TRUE) +
plot(CaliforniaAnnual.shape.2,border='grey', lwd = 1.5,add=TRUE) +
plot(ChihuahuanDesert.shape.2, col='firebrick3', lwd = 0.1) +
latticeExtra::layer(sp.polygons(ColdDeserts.shape.2, fill='gold', lwd = 0.1)) +
latticeExtra::layer(sp.polygons(mojave.sonoran.shape.2,fill='firebrick3', lwd = 0.1)) +
latticeExtra::layer(sp.polygons(Grama.Galleta.Steppe.shape.2, fill='firebrick3', lwd = 0.1))


#compare herbaceous and ecoregion model AIC May 28 2020######

average<-ggplot(aic.model.1234,aes(x=aic ,color=model)) +
  geom_density(alpha=1,size=1,aes(y=..scaled..)) +
  #geom_histogram(binwidth = 0.5,color='black') +
  scale_colour_manual(name = 'Model (R syntax)',
                    values=c('ecoregion' = 'black',
                             'herb.map'='blue',
                             'herb.region'='darkgreen',
                             'no.veg' = 'brown'
                             ),
                    labels=c('ecoregion'='~mm.dev*ecoregion*mm.mean',
                             'herb.map'='~mm.dev*perc_herb*mm.mean',
                             'herb.region'='~mm.dev*ecoregion*perc_herb',
                             'no.veg' = '~mm.dev*mm.mean')) +
  xlab('AIC') +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=15), 
    axis.text.y = element_text(color='black',size=15),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=12),
    legend.text = element_text(size=8),
    #legend.position = c(0.30,0.8),
    #legend.position = 'top',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# %fraction of herbaceous npp across ecoregions

ggplot(rangeland_npp_covariates,aes(x=perc_herb_mean,color=region)) +
  #facet_wrap(~region) +
  geom_density(alpha=1,aes(y=..scaled..),size=1) +
  scale_colour_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab('% Herbaceous NPP') +
  #xlab('Change in sensitivity per mm of MAP') +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=18), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=18),
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