#supplmentary analyses and graphs
library(rgeos)
library(maptools)
library(raster)
library(rgdal)
library(tiff)

#link  to rangeland analysis platform where produced shapefiles are put into and vegetation
#cover data is then derviced from
#https://rangelands.app/rap/?ll=39.0000,-103.0000&z=5&lc=pfgc&year=2018

####look at differences in cover/bare ground from kuchlar shapefile#######


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
cali_cover$region<-'hot_deserts'

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

#aggregate to get cover means for each ecoregion
annuals_mean<-aggregate(Annual.forb...grass.cover~region,mean,data=merge_4)
perrenials_mean<-aggregate(Perennial.forb...grass.cover~region,mean,data=merge_4)
shrubs_mean<-aggregate(Shrub.cover~region,mean,data=merge_4)
bare_ground_mean<-aggregate(Bare.ground.cover~region,mean,data=merge_4)
precip_mean<-aggregate(Annual.precip~region,mean,data=merge_4)

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


### create shapefile of NPP regions from cleaned raster product (vegetation_precipitation_driver)###########

#mean annual precip per pixel
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

#california annuals ecoregion above and below MAP to test hypothesis that
#dry and wet sites differ in tree cover

#subset to below ecoregion-wide mean annual precipitation
cali_shape_below <-cali_shape_2 %>% dplyr::filter(mm.x < 403.9)

cali_shape_below_raster<-rasterFromXYZ(cali_shape_below)
plot(cali_shape_below_raster)
hb <- cali_shape_below_raster > -Inf

cali_shape_below_shp<-rasterToPolygons(hb, dissolve=TRUE)
plot(cali_shape_below_shp)
crs(cali_shape_below_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#save shapefle that will be input to the rangeland analysis platform
writeOGR(cali_shape_below_shp, dsn=getwd(),layer="cali_shape_below_shp",driver="ESRI Shapefile")

#subset to above regional average PPT
cali_shape_above  <-cali_shape_2 %>%  dplyr::filter(mm.x > 403.9)
cali_shape_above_raster<-rasterFromXYZ(cali_shape_above)
plot(cali_shape_above_raster)
ha <- cali_shape_above_raster > -Inf

cali_shape_above_shp<-rasterToPolygons(ha, dissolve=TRUE)
plot(cali_shape_above_shp)
crs(cali_shape_above_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(cali_shape_above_shp, dsn=getwd(),layer="cali_shape_above_shp",driver="ESRI Shapefile")

#hot deserts shapefile production
hot_deserts_shape<-subset(mean_mm_veg,region.x=='hot_deserts')
head(hot_deserts_shape)
hot_deserts_shape_2<-hot_deserts_shape[-3]
hot_deserts_shape_raster<-rasterFromXYZ(hot_deserts_shape_2)
plot(hot_deserts_shape_raster)
h <- hot_deserts_shape_raster> -Inf

hot_deserts_shp<-rasterToPolygons(h, dissolve=TRUE)
plot(hot_deserts_shp)
crs(hot_deserts_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#save 'cleaned'
writeOGR(hot_deserts_shp, dsn=getwd(),layer="hot_deserts_shp",driver="ESRI Shapefile")

#cold deserts shapefile production
#subset to cold deserts
cold_deserts_shape<-subset(mean_mm_veg,region.x=='cold_deserts')
head(cold_deserts_shape)
cold_deserts_shape_2<-cold_deserts_shape[-3]
cold_deserts_shape_raster<-rasterFromXYZ(cold_deserts_shape_2)
plot(cold_deserts_shape_raster)
c <- cold_deserts_shape_raster> -Inf

cold_deserts_shp<-rasterToPolygons(c, dissolve=TRUE)
plot(cold_deserts_shp)
crs(cold_deserts_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#save 'cleaned' cold desers shapefile
writeOGR(cold_deserts_shp, dsn=getwd(),layer="cold_deserts_shp",driver="ESRI Shapefile")

#northern mixed prairies shapefile production
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


#shortgrass steppe shapefile production
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


#hot deserts region above and below MAP to test the hypothesis that
#wetter sites have increased perrenial vegetation cover and reduced bare ground

# subset hot deserts below ecoregion-wide mean annual precipitation
hot_deserts_shape_below <-hot_deserts_shape_2 %>% dplyr::filter(mm.x < 286.14)

hot_deserts_shape_below_raster<-rasterFromXYZ(hot_deserts_shape_below)
plot(hot_deserts_shape_below_raster)
hb <- hot_deserts_shape_below_raster > -Inf

hot_deserts_shape_below_shp<-rasterToPolygons(hb, dissolve=TRUE)
plot(hot_deserts_shape_below_shp)
crs(hot_deserts_shape_below_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#save shapefile to input into rangeland analysis platform
writeOGR(hot_deserts_shape_below_shp, dsn=getwd(),layer="hot_deserts_shape_below_shp",driver="ESRI Shapefile")

#subset to above regional mean annual precipitation
hot_deserts_shape_above  <-hot_deserts_shape_2 %>%  dplyr::filter(mm.x > 286.14)
hot_deserts_shape_above_raster<-rasterFromXYZ(hot_deserts_shape_above)
plot(hot_deserts_shape_above_raster)
ha <- hot_deserts_shape_above_raster > -Inf

hot_deserts_shape_above_shp<-rasterToPolygons(ha, dissolve=TRUE)
plot(hot_deserts_shape_above_shp)
crs(hot_deserts_shape_above_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#save shapefile to input into rangeland analysis platform
writeOGR(hot_deserts_shape_above_shp, dsn=getwd(),layer="hot_deserts_shape_above_shp",driver="ESRI Shapefile")

#import and compare cover dynamics across ecoregions
#make directory for cover files extracted from online rangeland analysis platform
wd_cover_means_clean<-"G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/vegetation_precipitation_interactions/Cover_Means/From Cleaned NPP Raster to Shapefiles"

#Cali annuals
cali_cover<-read.csv(file.path(wd_cover_means_clean, "California_Annuals_Cover_cleaned.csv"))
cali_cover$region<-'hot_deserts'

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

#merging all these datasets into one
merge_1<- rbind(cali_cover,cold_deserts_cover)
merge_2<- rbind(merge_1,northern_mixed_cover)
merge_3<- rbind(merge_2,sgs_cover)
merge_4<- rbind(merge_3,hot_deserts_cover)
summary(merge_4)

#aggregate to get cover means for table S1
annuals_mean<-aggregate(Annual.forb...grass.cover~region,mean,data=merge_4)
perrenials_mean<-aggregate(Perennial.forb...grass.cover~region,mean,data=merge_4)
shrubs_mean<-aggregate(Shrub.cover~region,mean,data=merge_4)
bare_ground_mean<-aggregate(Bare.ground.cover~region,mean,data=merge_4)
tree_mean<-aggregate(Tree.cover~region,mean,data=merge_4)

#plotting
library(ggplot2)

#figure S1

#california

#get rid of unneeded columns 
cali_cover_2 <- cali_cover[-c(7,8,9)]
head(cali_cover_2)
#melt to single column
data_long_cali_cover <- gather(cali_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_cali_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                             'Shrub.cover'='blue','Bare.ground.cover'='orange','Tree.cover'='black'),
                    labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                             'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground','Tree.cover'='Tree')) +
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

# cali wet versus dry sites for figure S3

# California annuals wet versus dry sites

#below regional map
cali_below_map<-read.csv(file.path(wd_cover_means_clean, "cali_below_map_cover.csv"))
cali_below_map$map<-'below'
summary(cali_below_map)

#above regional map
cali_above_map<-read.csv(file.path(wd_cover_means_clean, "cali_above_map_cover.csv"))
cali_above_map$map<-'above'
summary(cali_above_map)

#merge the two datasets into one
cali_cover_a_b<-rbind(cali_below_map,cali_above_map)
cali_cover_a_b_2 <- cali_cover_a_b[-c(7,8)]
cali_cover_a_b_3 <- gather(cali_cover_a_b_2,Vegetation, cover,-c(Year,map), factor_key=TRUE)
cali_cover_a_b_4<-aggregate(cover~ Vegetation + map,mean,data=cali_cover_a_b_3)

#plot it for figure S3
ggplot(cali_cover_a_b_4,aes(Vegetation,cover,fill=map)) +
  stat_summary(fun.y='mean',geom='point',pch=21,size=8) +
  scale_fill_manual(values=c('below'='red','above'='blue'),name='Mean annual precipitation') +
  scale_x_discrete(labels=c("Annual.forb...grass.cover"="Annual grass and forb","Perennial.forb...grass.cover"="Perennial grass and forb",
                            "Shrub.cover"="Shrub","Bare.ground.cover"="Bare ground","Tree.cover"='Tree')) +
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
    legend.position = c(0.7,0.8),
    #legend.position = c(0.82,0.85),
    #legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

##cold deserts for figure S1
cold_deserts_cover_2 <- cold_deserts_cover[-c(7,8,9)]

#melt to single column
data_long_cold_deserts_cover <- gather(cold_deserts_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_cold_deserts_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange','Tree.cover'='black'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground','Tree.cover'='Tree')) +
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

##northern mixed prairies for figure S1
northern_mixed_cover_2 <- northern_mixed_cover[-c(7,8,9)]

#melt to single column
data_long_northern_mixed_cover <- gather(northern_mixed_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_northern_mixed_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange','Tree.cover'='black'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground','Tree.cover'='Tree')) +
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

##shortgrass steppe for figure S1
sgs_cover_2 <- sgs_cover[-c(7,8,9)]

#melt to single column
data_long_sgs_cover <- gather(sgs_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_sgs_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange','Tree.cover'='black'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perrenial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground','Tree.cover'='Tree')) +
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

##hot deserts for figure S1
hot_deserts_cover_2 <- hot_deserts_cover[-c(7,8,9)]

#melt to single column
data_long_hot_deserts_cover <- gather(hot_deserts_cover_2,Vegetation, cover,-Year, factor_key=TRUE)

#plot temporal dynamics of veg cover
ggplot(data_long_hot_deserts_cover,aes(Year,cover,color=Vegetation)) +
  geom_line(size=1.5) +
  #facet_wrap(~site,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  scale_colour_manual(values=c('Annual.forb...grass.cover'='red','Perennial.forb...grass.cover'='darkgreen',
                               'Shrub.cover'='blue','Bare.ground.cover'='orange','Tree.cover'='black'),
                      labels=c('Annual.forb...grass.cover'='Annual grass and forb','Perennial.forb...grass.cover'='Perennial grass and forb',
                               'Shrub.cover'='Shrub','Bare.ground.cover'='Bare ground','Tree.cover'='Tree')) +
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
    legend.text = element_text(size=30),
    legend.position = c(0.65,0.80),
    #legend.position = c(0.82,0.85),
    #legend.position = "none",
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# hot deserts wet versus dry sites for figure S2

#below regional map
hd_below_map<-read.csv(file.path(wd_cover_means_clean, "hot_deserts_below_map_cover_cleaned.csv"))
hd_below_map$map<-'below'
summary(hd_below_map)

#above regional map
hd_above_map<-read.csv(file.path(wd_cover_means_clean, "hot_deserts_above_map_.csv"))
hd_above_map$map<-'above'
summary(hd_above_map)

#merge the two datasets into one
hot_deserts_cover_a_b<-rbind(hd_below_map,hd_above_map)
mean_bare_ground<-aggregate(Bare.ground.cover~map,mean,data=hot_deserts_cover_a_b)
mean_perrenial<-aggregate(Perennial.forb...grass.cover~map,mean,data=hot_deserts_cover_a_b)
hot_deserts_cover_a_b_2 <- hot_deserts_cover_a_b[-c(7,8)]
hot_deserts_cover_a_b_3 <- gather(hot_deserts_cover_a_b_2,Vegetation, cover,-c(Year,map), factor_key=TRUE)
hot_deserts_cover_a_b_4<-aggregate(cover~ Vegetation + map,mean,data=hot_deserts_cover_a_b_3)

#plot it
library(ggplot2)
ggplot(hot_deserts_cover_a_b_4,aes(Vegetation,cover,fill=map)) +
  stat_summary(fun.y='mean',geom='point',pch=21,size=8) +
  scale_fill_manual(values=c('below'='red','above'='blue'),name='Mean annual precipitation') +
  scale_x_discrete(labels=c("Annual.forb...grass.cover"="Annual grass and forb","Perennial.forb...grass.cover"="Perennial grass and forb",
                            "Shrub.cover"="Shrub","Bare.ground.cover"="Bare ground","Tree.cover"='Tree')) +
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

####create shapefiles of different sensitivity levels in hot deserst ecoregion####
#for figure S2B to see how perrenial cover correlates with temporal sensitivity

head(cali_annuals_sensitivity) #from the plotting script - temporal sensitivities across cali annuals ecoregion
summary(hot_deserts_sensitivity)
cali_annuals_sensitivity_2<-hot_deserts_sensitivity[-c(3,4)]
summary(cali_annuals_sensitivity_2)
0.5-(-.13)
cali_0.1<- cali_annuals_sensitivity_2 %>% filter(coef < 0.1)
head(cali_low)
cali_0.2<- cali_annuals_sensitivity_2 %>% filter(coef > 0.1, coef < 0.2)
summary(cali_0.2)
cali_0.3<- cali_annuals_sensitivity_2 %>% filter(coef > 0.2, coef < 0.3)
cali_0.4<- cali_annuals_sensitivity_2 %>% filter(coef > 0.3, coef < 0.4)
cali_0.5<- cali_annuals_sensitivity_2 %>% filter(coef > 0.4, coef < 0.51)

#turn into shapefiles...
#below 0.1 sensitivity
cali_0.1_raster<-rasterFromXYZ(cali_0.1)
plot(cali_0.1_raster)
ca.1 <- cali_0.1_raster> -Inf

ca.1_shp<-rasterToPolygons(ca.1, dissolve=TRUE)
plot(ca.1_shp)
crs(ca.1_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(ca.1_shp, dsn=getwd(),layer="california_annuals.1_shp",driver="ESRI Shapefile")

#below 0.2 sensitivity
cali_0.2_raster<-rasterFromXYZ(cali_0.2)
plot(cali_0.2_raster)
ca.2 <- cali_0.2_raster> -Inf

ca.2_shp<-rasterToPolygons(ca.2, dissolve=TRUE)
plot(ca.2_shp)
crs(ca.2_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(ca.2_shp, dsn=getwd(),layer="california_annuals.2_shp",driver="ESRI Shapefile")

#below 0.3 sensitivity
cali_0.3_raster<-rasterFromXYZ(cali_0.3)
plot(cali_0.3_raster)
ca.3 <- cali_0.3_raster> -Inf

ca.3_shp<-rasterToPolygons(ca.3, dissolve=TRUE)
plot(ca.3_shp)
crs(ca.3_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(ca.3_shp, dsn=getwd(),layer="california_annuals.3_shp",driver="ESRI Shapefile")

#below 0.4 sensitivity
cali_0.4_raster<-rasterFromXYZ(cali_0.4)
plot(cali_0.4_raster)
ca.4 <- cali_0.4_raster> -Inf

ca.4_shp<-rasterToPolygons(ca.4, dissolve=TRUE)
plot(ca.4_shp)
crs(ca.4_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(ca.4_shp, dsn=getwd(),layer="california_annuals.4_shp",driver="ESRI Shapefile")

#below 0.5 sensitivity
cali_0.5_raster<-rasterFromXYZ(cali_0.5)
plot(cali_0.5_raster)
ca.5 <- cali_0.5_raster> -Inf

ca.5_shp<-rasterToPolygons(ca.5, dissolve=TRUE)
plot(ca.5_shp)
crs(ca.5_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(ca.5_shp, dsn=getwd(),layer="california_annuals.5_shp",driver="ESRI Shapefile")

#read in cover datasets
library(plyr)
library(readr)
#below 0.4 sensitivity
setwd("~/Desktop")
mydir = "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Shapefiles_From_Cleaned_NPP_Data/California_sensitivity_levels/cal_sensitivity_cover_datasets"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
dat_csv = ldply(myfiles, read_csv)
as.data.frame(dat_csv)
str(dat_csv)
head(dat_csv)
mean(dat_csv$`Tree cover`)
plot(`Tree cover` ~ sensitivity,data=dat_csv)
summary(cali_0.5)

#upload insidivually to add mean sensitivities
#0.1
summary(cali_0.1)
cali.01.cover<-read.csv(myfiles[1])
cali.01.cover$sensitivity.2 <- '0.057'
#0.2
summary(cali_0.2)
cali.02.cover<-read.csv(myfiles[2])
cali.02.cover$sensitivity.2 <- '0.14'
#0.3
summary(cali_0.3)
cali.03.cover<-read.csv(myfiles[3])
cali.03.cover$sensitivity.2 <- '0.24'
#0.4
summary(cali_0.4)
cali.04.cover<-read.csv(myfiles[4])
cali.04.cover$sensitivity.2 <- '0.35'
#0.5
summary(cali_0.5)
cali.05.cover<-read.csv(myfiles[5])
cali.05.cover$sensitivity.2 <- '0.45'
str(cali.01.cover)
#merge all of them
cali_cover_2<-rbind(cali.01.cover,cali.02.cover)
head(cali_cover_2)
cali_cover_2<-rbind(cali_cover_2,cali.03.cover)
cali_cover_3<-rbind(cali_cover_2,cali.04.cover)
cali_cover_4<-rbind(cali_cover_3,cali.05.cover)

#plot it
library(ggplot2)
ggplot(cali_cover_4,aes(as.numeric(sensitivity.2),as.numeric(Tree.cover))) +
  #geom_point(size=1,pch=1) +
  geom_point(size=5,pch=21,fill='white',color='black') +
  geom_smooth(method = 'lm', formula = y ~ poly(x,2),size=1) +
  ylab('% Tree cover') +
  xlab(bquote('Temporal NPP sensitivity ('*g/m^2/mm*')')) +
  scale_x_continuous(limits = c(0, 0.5)) +
  #xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    #axis.title.x = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


####create shapefiles of different sensitivity levels in hot deserts####

head(hot_deserts_sensitivity) #from the plotting script - temporal sensitivities across hot deserts ecoregion
summary(hot_deserts_sensitivity)
hot_deserts_sensitivity_2<-hot_deserts_sensitivity[-c(3,4)]
head(hot_deserts_sensitivity_2)
0.5-(-.13)
hd_0.1<- hot_deserts_sensitivity_2 %>% filter(coef < 0.1)
head(hd_0.1)
summary(hd_0.1)
hd_0.2<- hot_deserts_sensitivity_2 %>% filter(coef > 0.1, coef < 0.2)
summary(hd_0.2)
hd_0.3<- hot_deserts_sensitivity_2 %>% filter(coef > 0.2, coef < 0.3)
summary(hd_0.3)
hd_0.4<- hot_deserts_sensitivity_2 %>% filter(coef > 0.3, coef < 0.4)
summary(hd_0.4)

#turn into shapefiles
#below 0.1 sensitivity
hd_0.1_raster<-rasterFromXYZ(hd_0.1)
plot(hd_0.1_raster)
hd.1 <- hd_0.1_raster> -Inf

hd.1_shp<-rasterToPolygons(hd.1, dissolve=TRUE)
plot(hd.1_shp)
crs(hd.1_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hd.1_shp, dsn=getwd(),layer="hot_deserts.1_shp",driver="ESRI Shapefile")

#below 0.2 sensitivity
hd_0.2_raster<-rasterFromXYZ(hd_0.2)
plot(hd_0.2_raster)
hd.2 <- hd_0.2_raster> -Inf

hd.2_shp<-rasterToPolygons(hd.2, dissolve=TRUE)
plot(hd.2_shp)
crs(hd.2_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hd.2_shp, dsn=getwd(),layer="hot_deserts.2_shp",driver="ESRI Shapefile")

#below 0.3 sensitivity
hd_0.3_raster<-rasterFromXYZ(hd_0.3)
plot(hd_0.3_raster)
hd.3 <- hd_0.3_raster> -Inf

hd.3_shp<-rasterToPolygons(hd.3, dissolve=TRUE)
plot(hd.3_shp)
crs(hd.3_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hd.3_shp, dsn=getwd(),layer="hot_deserts.3_shp",driver="ESRI Shapefile")

#below 0.4 sensitivity
hd_0.4_raster<-rasterFromXYZ(hd_0.4)
plot(hd_0.4_raster)
hd.4 <- hd_0.4_raster> -Inf

hd.4_shp<-rasterToPolygons(hd.4, dissolve=TRUE)
plot(hd.4_shp)
crs(hd.4_shp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeOGR(hd.4_shp, dsn=getwd(),layer="hot_deserts.4_shp",driver="ESRI Shapefile")


#read in cover datasets
library(plyr)
library(readr)
#below 0.4 sensitivity
setwd("~/Desktop")
mydir.hd = "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/Shapefiles_From_Cleaned_NPP_Data/hot_deserts_sensitivity_levels/hd_sensitivity_cover_datasets"
myfiles.hd = list.files(path=mydir.hd, pattern="*.csv", full.names=TRUE)
myfiles.hd
dat_csv.hd = ldply(myfiles.hd, read_csv)
as.data.frame(dat_csv.hd)
str(dat_csv)
head(dat_csv.hd)
plot(`Perennial forb & grass cover` ~ sensitivity,data=dat_csv.hd)
summary(cali_0.5)

#upload insidivually to add mean sensitivities
#0.1
summary(hd_0.1)
hd.01.cover<-read.csv(myfiles.hd[1])
hd.01.cover$sensitivity.2 <- '0.077'
#0.2
summary(hd_0.2)
hd.02.cover<-read.csv(myfiles.hd[2])
hd.02.cover$sensitivity.2 <- '0.15'
#0.3
summary(hd_0.3)
hd.03.cover<-read.csv(myfiles.hd[3])
hd.03.cover$sensitivity.2 <- '0.24'
#0.4
summary(hd_0.4)
hd.04.cover<-read.csv(myfiles.hd[4])
hd.04.cover$sensitivity.2 <- '0.33'

#merge all of them
hd_cover_2<-rbind(hd.01.cover,hd.02.cover)
head(hd_cover_2)
hd_cover_2<-rbind(hd_cover_2,hd.03.cover)
hd_cover_3<-rbind(hd_cover_2,hd.04.cover)

#plot it
library(ggplot2)
ggplot(hd_cover_3,aes(as.numeric(sensitivity.2),as.numeric(Perennial.forb...grass.cover))) +
  #geom_point(size=1,pch=1) +
  geom_point(size=5,pch=21,fill='white',color='black') +
  geom_smooth(method = 'lm', formula = y ~ poly(x,2),size=1) +
  ylab('% Perennial grass and forb cover') +
  scale_y_continuous(limits = c(8, 35)) +
  xlab(bquote('Temporal NPP sensitivity ('*g/m^2/mm*')')) +
  #xlab('') +
  #ylab('Sensitivity') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    #axis.title.x = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#look at ranges of mean and deviations of precip for each ecoregion
#sgs
sgs_summary<-subset(rangeland_npp_covariates_deviations_1,region.x=='semi_arid_steppe')
summary(sgs_summary)
hist(sgs_summary$mm.dev) #normally distributed
hist(sgs_summary$mm.y)
quantile(sgs_summary$mm.dev, c(.95))
quantile(sgs_summary$mm.dev, c(.05))

#northern mixed
northern_mixed_summary<-subset(rangeland_npp_covariates_deviations_1,region.x=='northern_mixed_prairies')
summary(northern_mixed_summary)
hist(northern_mixed_summary$mm.dev) #normally distributed
hist(northern_mixed_summary$mm.y)
quantile(northern_mixed_summary$mm.dev, c(.95))
quantile(northern_mixed_summary$mm.dev, c(.05))

#hot deserts
hot_deserts_summary<-subset(rangeland_npp_covariates_deviations_1,region.x=='hot_deserts')
summary(hot_deserts_summary)
hist(hot_deserts_summary$mm.dev) #normally distributed
hist(hot_deserts_summary$mm.y)
quantile(hot_deserts_summary$mm.dev, c(.95))
quantile(hot_deserts_summary$mm.dev, c(.05)) #more than -100, but can't go lower because some sites has MAP below 100...

#cold deserts
cold_deserts_summary<-subset(rangeland_npp_covariates_deviations_1,region.x=='cold_deserts')
summary(cold_deserts_summary)
hist(cold_deserts_summary$mm.dev) #normally distributed
hist(cold_deserts_summary$mm.y)
quantile(cold_deserts_summary$mm.dev, c(.95))
quantile(cold_deserts_summary$mm.dev, c(.05))

#california annuals
cali_annuals_summary<-subset(rangeland_npp_covariates_deviations_1,region.x=='california_annuals')
summary(cali_annuals_summary)
hist(cali_annuals_summary$mm.dev) #normally distributed
hist(cali_annuals_summary$mm.y)
quantile(cali_annuals_summary$mm.dev, c(.95))
quantile(cali_annuals_summary$mm.dev, c(.05))