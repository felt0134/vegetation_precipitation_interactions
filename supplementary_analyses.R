#supplmentary analyses and graphs
library(rgeos)
library(maptools)
#####see how sd of PPT and MAP varies according to vegetation type#########
sd_map_veg<-aggregate(mm.x~ x)

sd_map_veg <- rangeland_npp_covariates_deviations_1 %>% group_by(x, y) %>%
  dplyr::summarize(st.dev = sd(mm.x)) %>%
  
head(sd_map_veg)
data.frame(sd_map_veg)

merge_sd_map<-merge(mm_production_mean,sd_map_veg,by=c('x','y'))
head(merge_sd_map)

ggplot(merge_sd_map,aes(x=mm,y=st.dev)) +
  geom_point(color='black',pch=1) +
  stat_smooth(method = 'lm',color='red') +
  facet_wrap(~region,nrow=5,scales='free_y') +
  ylab('Standard devation of precipitation') +
  xlab('Mean annual precipitation') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
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

####look at differences in cover/bare ground from kuchlar shapefile#######

rawHTML <- paste(readLines("C:/Users/A02296270/Desktop/rangeland_cover/vegetation-cover-v1-1988.tif"), collapse="\n")
summary(rawHTML)
raster(rawHTML)
summary(rangeland_npp_covariates)
library(raster)
library(rgdal)
data.1986<- "C:/Users/A02296270/Desktop/rangeland_cover/vegetation-cover-v1-1988.tif" 
raster_1986<-raster(data.1986,band=2,quick=FALSE)
test<-as.data.frame(raster_1986)
data.frame(raster_1986,band=2)
plot(raster_1986)
head(raster_1986)
print(raster_1986)
library(tiff)
read_file<-readTIFF(data.1986) 
summary(raster_1986)
use warnings()


url <- 'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v1/vegetation-cover-v1-1993.tif'

geotiff_file <- tempfile(fileext='.tif')
httr::GET(url,httr::write_disk(path=geotiff_file))
my_raster <- raster(geotiff_file)
my_raster

library(data.table)
test<-fread("http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v1/vegetation-cover-v1-1993.tif")



#need to re-save certain shapefiles so compatiable with range-cover application 
writeOGR(mojave.sonoran.shape, ".", "mojave_sonoran", driver="ESRI Shapefile") 

#combing separate desert files into one shapefile
summary(Grama.Galleta.Steppe.shape.2)
summary(mojave.sonoran.shape)
test.2<-bind(mojave.sonoran.shape.2,ChihuahuanDesert.shape.2)
plot(test.2)
test.3<-bind(test.2,Grama.Galleta.Steppe.shape.2)
plot(test.3)

#full hot deserts shapefile
writeOGR(test.3, ".", "hot_deserts", driver="ESRI Shapefile") 

#make directory for cover files extracted from online range-cover application
wd_cover_means<-"G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/vegetation_precipitation_interactions/Cover_Means"

#Cali annuals
cali_cover<-read.csv(file.path(wd_cover_means, "california_annuals_cover_estimates.csv"))
cali_cover$region<-'cali_annuals'

#cold deserts
northern_mixed_cover<-read.csv(file.path(wd_cover_means, "northern_mixed_cover.csv"))
northern_mixed_cover$region <- 'northern_mixed'

#Northern mixed prairies
northern_mixed_cover<-read.csv(file.path(wd_cover_means, "northern_mixed_cover.csv"))
northern_mixed_cover$region <- 'northern_mixed'
head(northern_mixed_cover)

#shortgrass steppe
sgs_cover<-read.csv(file.path(wd_cover_means, "sgs_cover.csv"))
sgs_cover$region <- 'sgs'
head(sgs_cover)

#hot deserts
hot_deserts_cover<-read.csv(file.path(wd_cover_means, "hot_deserts_combined_cover.csv"))
hot_deserts_cover$region <- 'hot_deserts'
head(hot_deserts_cover)

#merging
merge_1<- rbind(cali_cover,cold_deserts_cover)
merge_2<- rbind(merge_1,northern_mixed_cover)
merge_3<- rbind(merge_2,sgs_cover)
merge_4<- rbind(merge_3,hot_deserts_cover)
summary(merge_4)

#aggregate to get cover means
annuals_mean<-aggregate(Annual.forb...grass.cover~region,mean,data=merge_4)
perrenials_mean<-aggregate(Perennial.forb...grass.cover~region,mean,data=merge_4)
shrubs_mean<-aggregate(Shrub.cover~region,mean,data=merge_4)
bare_ground_mean<-aggregate(Bare.ground.cover~region,mean,data=merge_4)
precip_mean<-aggregate(Annual.precip~region,mean,data=merge_4)
 fro kuchlar shapefile

#splitting hot deserts ecoregion by specific deserts

#cropping sonoran out of mojave-sonoran shapefile

sonoran.shape<-readOGR(dsn="G:/My Drive/range-resilience/scope/study-area-shapefiles/SonoranDesert",layer="SonoranDesert")
plot(sonoran.shape)
sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(sonoran.shape)
#step 2:
sonoran.shape.2 <- sp::spTransform(sonoran.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(sonoran.shape.2)

Clipped_polys = subset(mojave.sonoran.shape.2,sonoran.shape.2)
plot(Clipped_polys)
?gIntersection
str(mojave.sonoran.shape.2)

#sonoran Desert
sonoran_cover<-read.csv(file.path(wd_cover_means, "Sonoran_Cover.csv"))
sonoran_cover$region <- 'sonoran'
summary(sonoran_cover)

#chihuahuan desert
chi_cover<-read.csv(file.path(wd_cover_means, "Chihuahuan_Cover.csv"))
chi_cover$region <- 'chihuahuan'
summary(chi_cover)

rbind_chihuahuan_sonoran <-rbind(chi_cover,sonoran_cover)
perrenial_grass_deserts<-

#grama gallette desert
grama_gallette_cover<-read.csv(file.path(wd_cover_means, "grama_gallette_Cover.csv"))
grama_gallette_cover$region <- 'grama_gallette'


### create shapefile of NPP regions from cleaned raster product###########

library(maptools)
library(rgdal)
library(sp)
library(rgeos)

#
mean_mm_veg<-aggregate(mm.x~ x + y + region.x,mean,data=rangeland_npp_covariates_deviations_1)
head(mean_mm_veg)

#california shapefile production
cali_shape<-subset(mean_mm_veg,region.x=='california_annuals')
head(cali_shape)
cali_shape_2<-cali_shape[-3]
cali_shape_raster<-rasterFromXYZ(cali_shape_2)
plot(cali_shape_raster)
r <- cali_shape_raster> -Inf

cali_shp<-rasterToPolygons(r, dissolve=TRUE)
plot(cali_shp)
crs(cali_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(cali_shp, dsn=getwd(),layer="cali_shp",driver="ESRI Shapefile")

#hot deserts
hot_deserts_shape<-subset(mean_mm_veg,region.x=='hot_deserts')
head(hot_deserts_shape)
hot_deserts_shape_2<-hot_deserts_shape[-3]
hot_deserts_shape_raster<-rasterFromXYZ(hot_deserts_shape_2)
plot(hot_deserts_shape_raster)
h <- hot_deserts_shape_raster> -Inf

hot_deserts_shp<-rasterToPolygons(h, dissolve=TRUE)
plot(hot_deserts_shp)
crs(hot_deserts_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hot_deserts_shp, dsn=getwd(),layer="hot_deserts_shp",driver="ESRI Shapefile")

#cold deserts
cold_deserts_shape<-subset(mean_mm_veg,region.x=='cold_deserts')
head(cold_deserts_shape)
cold_deserts_shape_2<-cold_deserts_shape[-3]
cold_deserts_shape_raster<-rasterFromXYZ(cold_deserts_shape_2)
plot(cold_deserts_shape_raster)
c <- cold_deserts_shape_raster> -Inf

cold_deserts_shp<-rasterToPolygons(c, dissolve=TRUE)
plot(cold_deserts_shp)
crs(cold_deserts_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(cold_deserts_shp, dsn=getwd(),layer="cold_deserts_shp",driver="ESRI Shapefile")

#northern mixed prairies
northern_mixed_shape<-subset(mean_mm_veg,region.x=='northern_mixed_prairies')
head(northern_mixed_shape)
northern_mixed_shape_2<-northern_mixed_shape[-3]
northern_mixed_shape_raster<-rasterFromXYZ(northern_mixed_shape_2)
plot(northern_mixed_shape_raster)
n <- northern_mixed_shape_raster> -Inf

northern_mixed_shp<-rasterToPolygons(n, dissolve=TRUE)
plot(northern_mixed_shp)
crs(northern_mixed_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(northern_mixed_shp, dsn=getwd(),layer="northern_mixed_shp",driver="ESRI Shapefile")


#shortgrass steppe
summary(mean_mm_veg)
sgs_shape<-subset(mean_mm_veg,region.x=='semi_arid_steppe')
head(sgs_shape)
sgs_shape_2<-sgs_shape[-3]
sgs_shape_raster<-rasterFromXYZ(sgs_shape_2)
plot(sgs_shape_raster)
s <- sgs_shape_raster> -Inf

sgs_shp<-rasterToPolygons(s, dissolve=TRUE)
plot(sgs_shp)
crs(sgs_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(sgs_shp, dsn=getwd(),layer="sgs_shp",driver="ESRI Shapefile")


#hot deserts region above and below MAP

hot_deserts_shape_below <-hot_deserts_shape_2 %>% dplyr::filter(mm.x < 286.14)



hot_deserts_shape_below_raster<-rasterFromXYZ(hot_deserts_shape_below)
plot(hot_deserts_shape_below_raster)
hb <- hot_deserts_shape_below_raster > -Inf

hot_deserts_shape_below_shp<-rasterToPolygons(hb, dissolve=TRUE)
plot(hot_deserts_shape_below_shp)
crs(hot_deserts_shape_below_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hot_deserts_shape_below_shp, dsn=getwd(),layer="hot_deserts_shape_below_shp",driver="ESRI Shapefile")


#above regional average
hot_deserts_shape_above  <-hot_deserts_shape_2 %>%  dplyr::filter(mm.x > 286.14)
hot_deserts_shape_above_raster<-rasterFromXYZ(hot_deserts_shape_above)
plot(hot_deserts_shape_above_raster)
ha <- hot_deserts_shape_above_raster > -Inf

hot_deserts_shape_above_shp<-rasterToPolygons(ha, dissolve=TRUE)
plot(hot_deserts_shape_above_shp)
crs(hot_deserts_shape_above_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hot_deserts_shape_above_shp, dsn=getwd(),layer="hot_deserts_shape_above_shp",driver="ESRI Shapefile")

#compare cover
#make directory for cover files extracted from online range-cover application
wd_cover_means_clean<-"G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/vegetation_precipitation_interactions/Cover_Means/From Cleaned NPP Raster to Shapefiles"

#Cali annuals
cali_cover<-read.csv(file.path(wd_cover_means_clean, "California_Annuals_Cover_cleaned.csv"))
cali_cover$region<-'cali_annuals'

#cold deserts
cold_deserts_cover<-read.csv(file.path(wd_cover_means_clean, "cold_deserts_cover_cleaned.csv"))
cold_deserts_cover$region <- 'cold_deserts'

#Northern mixed prairies
northern_mixed_cover<-read.csv(file.path(wd_cover_means_clean, "northern_mixed_cover_clean.csv"))
northern_mixed_cover$region <- 'northern_mixed'
head(northern_mixed_cover)

#shortgrass steppe
sgs_cover<-read.csv(file.path(wd_cover_means_clean, "sgs_cover_clean.csv"))
sgs_cover$region <- 'sgs'
head(sgs_cover)

#hot deserts
hot_deserts_cover<-read.csv(file.path(wd_cover_means_clean, "hot_deserts_cover_cleaned.csv"))
hot_deserts_cover$region <- 'hot_deserts'
head(hot_deserts_cover)

#merging
merge_1<- rbind(cali_cover,cold_deserts_cover)
merge_2<- rbind(merge_1,northern_mixed_cover)
merge_3<- rbind(merge_2,sgs_cover)
merge_4<- rbind(merge_3,hot_deserts_cover)
summary(merge_4)

#aggregate to get cover means
annuals_mean<-aggregate(Annual.forb...grass.cover~region,mean,data=merge_4)
perrenials_mean<-aggregate(Perennial.forb...grass.cover~region,mean,data=merge_4)
shrubs_mean<-aggregate(Shrub.cover~region,mean,data=merge_4)
bare_ground_mean<-aggregate(Bare.ground.cover~region,mean,data=merge_4)

#below regional map
hd_below_map<-read.csv(file.path(wd_cover_means, "hot_deserts_below_map_cover_cleaned.csv"))
hd_below_map$map<-'below'
summary(hd_below_map)
#above regional map
hd_above_map<-read.csv(file.path(wd_cover_means, "hot_deserts_above_map_.csv"))
hd_above_map$map<-'above'
summary(hd_above_map)

hot_deserts_cover_a_b<-rbind(hd_below_map,hd_above_map)
mean_bare_ground<-aggregate(Bare.ground.cover~map,mean,data=hot_deserts_cover_a_b)
mean_perrenial<-aggregate(Perennial.forb...grass.cover~map,mean,data=hot_deserts_cover_a_b)

#plotting
library(ggplot2)

#figure SX

#california

#get rid of unneeded columns
cali_cover_2 <- cali_cover[-c(6,7,8,9)]

#melt to single column
data_long_cali_cover <- gather(cali_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_cali_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                             'Shrub.cover'='blue','Bare.ground.cover'='orange'),
                    labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                             'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground')) +
  scale_y_continuous(expand = c(0,0),limits=c(0,70)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=17), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=17),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    #legend.position = c(0.82,0.95),
    #legend.position = c(0.82,0.85),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##cold deserts
cold_deserts_cover_2 <- cold_deserts_cover[-c(6,7,8,9)]

#melt to single column
data_long_cold_deserts_cover <- gather(cold_deserts_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_cold_deserts_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground')) +
  scale_y_continuous(expand = c(0,0),limits=c(0,70)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=17), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=17),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    #legend.position = c(0.82,0.95),
    #legend.position = c(0.82,0.85),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##northern mixed prairies
northern_mixed_cover_2 <- northern_mixed_cover[-c(6,7,8,9)]

#melt to single column
data_long_northern_mixed_cover <- gather(northern_mixed_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_northern_mixed_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground')) +
  scale_y_continuous(expand = c(0,0),limits=c(0,70)) +
  #xlab('Change in sensitivity per mm of MAP') +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=17), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=17),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    #legend.position = c(0.82,0.95),
    #legend.position = c(0.82,0.85),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##shortgrass steppe
sgs_cover_2 <- sgs_cover[-c(6,7,8,9)]

#melt to single column
data_long_sgs_cover <- gather(sgs_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_sgs_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground')) +
  #xlab('Change in sensitivity per mm of MAP') +
  scale_y_continuous(expand = c(0,0),limits=c(0,70)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=17), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=17),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    #legend.position = c(0.82,0.95),
    #legend.position = c(0.82,0.85),
    legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##hot deserts
hot_deserts_cover_2 <- hot_deserts_cover[-c(6,7,8,9)]

#melt to single column
data_long_hot_deserts_cover <- gather(hot_deserts_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_hot_deserts_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perennial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground')) +
  #xlab('Change in sensitivity per mm of MAP') +
  scale_y_continuous(expand = c(0,0),limits=c(0,70)) +
  ylab('') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=17), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=17),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    legend.position = c(0.6,0.75),
    #legend.position = c(0.82,0.85),
    #legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# hot deserts wet versus dry sites
hot_deserts_cover_a_b_2 <- hot_deserts_cover_a_b[-c(6,7,8)]
hot_deserts_cover_a_b_3 <- gather(hot_deserts_cover_a_b_2,Vegetation, cover,-c(Year,map), factor_key=TRUE)
hot_deserts_cover_a_b_4<-aggregate(cover~ Vegetation + map,mean,data=hot_deserts_cover_a_b_3)

#plot it
library(ggplot2)
ggplot(hot_deserts_cover_a_b_4,aes(Vegetation,cover,fill=map)) +
  stat_summary(fun.y='mean',geom='point',pch=21,size=8) +
  scale_fill_manual(values=c('below'='red','above'='blue'),name='Mean annual precipitation') +
  scale_x_discrete(labels=c("Annual.forb...grass.cover"="Annual grass and forb","Perennial.forb...grass.cover"="Perennial grass and forb",
                            "Shrub.cover"="Shrub","Bare.ground.cover"="Bare ground")) +
  ylab('% Cover') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=14,angle=30,hjust=1),
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=35),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=20),
    legend.text = element_text(size=17),
    legend.position = c(0.3,0.8),
    #legend.position = c(0.82,0.85),
    #legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#map of mean annual precipitation in hot deserts

#taken from the shapefile production code
break_mean_ppt_deserts<-quantile(hot_deserts_shape_2$mm.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
break_mean_npp<-mean_production$npp.x
npp_mean_allsites
plot(us)

spplot(hot_deserts_shape_raster,#scales = list(draw = TRUE),
       at=break_mean_ppt_deserts,
       #par.settings = list(axis.line = list(col = 'transparent')),
       asp=0.01,
       col.regions = 
         rev(topo.colors(length(break_mean_ppt_deserts)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(dat.poly, lwd = 1))
