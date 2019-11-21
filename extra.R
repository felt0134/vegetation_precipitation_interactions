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

proj4string(sensitivity_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
spTransform(states_all_sites)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(sensitivity_raster)
For colors, there are a lot of ways to do it.  Here's what I'm currently doing:
#  1) Identify vectors of colornames that will define how a palette progresses. Here are a few that I've generated:

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
proj4string(mean_mm_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
 
r.range <- round(c(minValue(mean_mm_raster), maxValue(mean_mm_raster)))

plot(crop_mm_raster,breaks = bks_map,axes=F,col = bkcols.precip,
     legend.width=2,
     axis.args=list(at=seq(r.range[1], r.range[2], 100),
                                       labels=seq(r.range[1], r.range[2], 100),
                                   cex.axis=0.6),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE) 

#mean primary production
npp= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bkcols.npp <- colorRampPalette(npp)(length(bks_npp)-1)
bks_npp<- quantile(mean_production$npp.x, probs=seq(0.0, 1, by=0.05), na.rm = TRUE)
proj4string(mean_production_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

r.range.npp <- round(c(minValue(mean_production_raster), maxValue(mean_production_raster)))

plot(mean_production_raster,breaks = bks_npp,axes=F,col = bkcols.npp,
     legend.width=2,
     axis.args=list(at=seq(r.range.npp[1], r.range.npp[2], 100),
                    labels=seq(r.range.npp[1], r.range.npp[2], 100),
                    cex.axis=0.6),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE) 

plot(mean_production_raster)
#sensitivity to precip
sensitivity=c("purple",'cyan3','green','yellow','orange','red')
bkcols.sensitivity <- colorRampPalette(sensitivity)(length(bks)-1)
r.range <- round(c(minValue(sensitivity_raster), maxValue(sensitivity_raster),0.001))

plot(sensitivity_raster,breaks = bks,axes=F,box=T,col = bkcols.sensitivity,
     legend.width=2,
     axis.args=list(at=seq(r.range[1], r.range[2], 0.05),
                    labels=seq(r.range[1], r.range[2], 0.05),
                    cex.axis=0.6),
     legend.args=list(text='', side=4, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE)

 
states_all_sites
par(mar=c(1,1,1,1))
 
 plot(mean_mm_raster)
 
#


#For the isolines, I don't have a ready example, but I think this function might be what you need: https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/kde2d.html



latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.5)) +
  plot(NorthernMixedSubset.shape.2,border='steelblue2', lwd = 2.5,add=TRUE) +
  plot(SGS.shape.2, lwd = 2.5,border='green4',add=TRUE) +
plot(CaliforniaAnnual.shape.2,border='grey', lwd = 1.5,add=TRUE) +
plot(ChihuahuanDesert.shape.2, col='firebrick3', lwd = 0.1) +
latticeExtra::layer(sp.polygons(ColdDeserts.shape.2, fill='gold', lwd = 0.1)) +
latticeExtra::layer(sp.polygons(mojave.sonoran.shape.2,fill='firebrick3', lwd = 0.1)) +
latticeExtra::layer(sp.polygons(Grama.Galleta.Steppe.shape.2, fill='firebrick3', lwd = 0.1))


###2 dimensial kernal density

library(MASS)
head(hot_deserts_test)
myPal <- colorRampPalette(c("white","blue","gold", "orange", "red"))
dens <- kde2d(hot_deserts_test$mm.y, hot_deserts_test$npp.x)
image(dens, col=transp(myPal(300),.7), add=TRUE) 
contour(dens, add=T) 
filled.contour(dens,color.palette=colorRampPalette(c('white','blue','yellow','red','darkred')))

ggplot(data=hot_deserts_test,aes(mm.y,npp.x)) + 
  #geom_point(size=.01) +
  #stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  stat_density_2d(aes(fill = stat(nlevel)),geom = "polygon",size=.1,colour='black') +
  scale_fill_viridis_c() +
  #scale_fill_continuous(low="green",high="red") +
  filled.contour(dens,color.palette=colorRampPalette(c('white','blue','yellow','red','darkred'))) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_100,aes(map,NPP,color='myline1'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_minus_0,aes(map,NPP, color='myline2'),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  geom_smooth(data = hot_deserts_fit_wet_dry_plus_200,aes(map,NPP,color='myline3' ),method='lm',se=FALSE,size=3,fullrange=TRUE) +
  #geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  scale_colour_manual(name='Deviation (mm)',values=c(myline1 ="red", myline2 ="grey8", myline3 ="blue"),
                      labels=c('myline1'='-100','myline2'= '0','myline3'='200')) + 
  ylab('NPP') +
  xlab('MAP') +
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
