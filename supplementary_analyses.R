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

####look at differences in cover/bare ground#######

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
cold_deserts_cover<-read.csv(file.path(wd_cover_means, "cold_deserts_cover.csv"))
cold_deserts_cover$region <- 'cold_deserts'

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


###attempting to create shapefile of NPP regions###########

library(maptools)
library(rgdal)
library(sp)

head(mean_production$geodeticDa)

plot(mean_production_raster)
dat.poly <- rasterToPolygons(mean_production_raster, dissolve=TRUE)
plot(dat.poly)
r <- SpatialPolygonsDataFrame(mean_production[,1:2],
                            mean_production)

crs(dat.poly) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs 
                 +ellps=WGS84 +towgs84=0,0,0" 

plot(r)

extent(r)

r.2<-SpatialPolygonsDataFrame(r)

writeOGR(r, getwd(),
         "r", driver="ESRI Shapefile")

WGScoor<-  mean_production#data to convert
coordinates(WGScoor)=~x+y #column names of the lat long cols
proj4string(WGScoor)<- CRS("++proj=longlat +datum=WGS84") # set coordinate system to WGS
WGScoor.df <- SpatialPointsDataFrame(WGScoor, data.frame(id=1:length(WGScoor)))
LLcoor<-spTransform(WGScoor.df,CRS("+proj=longlat"))
LLcoor.df=SpatialPointsDataFrame(LLcoor, data.frame(id=1:length(LLcoor)))
<-writeOGR(LLcoor.df, dsn=getwd(),layer="MyShapefile",driver="ESRI Shapefile")

plot(LLcoor)


WGScoor<-  mean_production
coordinates(WGScoor)=~x+y
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
raster::shapefile(LLcoor, "MyShapefile_2.shp")