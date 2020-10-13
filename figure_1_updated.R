# Figure 1

####prep####
#needed projection
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#shapefile referecne for state outlines. This will results in a sp file being downloaded...
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]

states_all_sites <- sp::spTransform(states_all_sites, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
states_all_sites <- spTransform(states_all_sites, CRS(aea.proj))
#plot(states_all_sites)
#####
##### 1A: ecoregion map ######

#mojave sonoran
mojave.sonoran.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/MojaveSonoran",layer="MojaveSonoran")
#plot(mojave.sonoran.shape)
mojave.sonoran.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(mojave.sonoran.shape)

#step 2:
mojave.sonoran.shape.2 <- sp::spTransform(mojave.sonoran.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(mojave.sonoran.shape.2)
writeOGR(mojave.sonoran.shape, ".", "test", driver="ESRI Shapefile")
mojave.sonoran.shape.3 <- spTransform(mojave.sonoran.shape.2, CRS(aea.proj))
#plot(mojave.sonoran.shape.3)

#chihuahan
ChihuahuanDesert.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/ChihuahuanDesert",layer="ChihuahuanDesert")
#plot(ChihuahuanDesert.shape)
ChihuahuanDesert.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(ChihuahuanDesert.shape)

#step 2:
ChihuahuanDesert.shape.2 <- sp::spTransform(ChihuahuanDesert.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(ChihuahuanDesert.shape.2)
ChihuahuanDesert.shape.3 <- spTransform(ChihuahuanDesert.shape.2, CRS(aea.proj))
#plot(ChihuahuanDesert.shape.3)

#grama galleta steppe
Grama.Galleta.Steppe.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/GramaGalletaSteppe",layer="GramaGalletaSteppe")
#plot(Grama.Galleta.Steppe.shape)
Grama.Galleta.Steppe.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(Grama.Galleta.Steppe.shape)

#step 2:
Grama.Galleta.Steppe.shape.2 <- sp::spTransform(Grama.Galleta.Steppe.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(Grama.Galleta.Steppe.shape.2)
Grama.Galleta.Steppe.shape.3 <- spTransform(Grama.Galleta.Steppe.shape.2, CRS(aea.proj))
#plot(Grama.Galleta.Steppe.shape.3)

#california annuals
CaliforniaAnnual.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/CaliforniaAnnual",layer="CaliforniaAnnual")
#plot(CaliforniaAnnual.shape)
CaliforniaAnnual.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(CaliforniaAnnual.shape)

#step 2:
CaliforniaAnnual.shape.2 <- sp::spTransform(CaliforniaAnnual.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(CaliforniaAnnual.shape.2)
CaliforniaAnnual.shape.3 <- spTransform(CaliforniaAnnual.shape.2, CRS(aea.proj))
#plot(CaliforniaAnnual.shape.3)

#cold deserts
ColdDeserts.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/ColdDeserts",layer="ColdDeserts")
#plot(CaliforniaAnnual.shape)
ColdDeserts.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(ColdDeserts.shape)

#step 2:
ColdDeserts.shape.2 <- sp::spTransform(ColdDeserts.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(ColdDeserts.shape.2,col='blue')
ColdDeserts.shape.3 <- spTransform(ColdDeserts.shape.2, CRS(aea.proj))
#plot(ColdDeserts.shape.3)

#shortgrass steppe
SGS.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/SGS",layer="SGS")
#plot(SGS.shape)
SGS.shape@bbox <- as.matrix(extent(mean_production_raster))
#plot(SGS.shape)

#step 2:
SGS.shape.2 <- sp::spTransform(SGS.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(SGS.shape.2)
SGS.shape.3 <- spTransform(SGS.shape.2, CRS(aea.proj))
#plot(SGS.shape.3)

#northern mixed
NorthernMixedSubset.shape<-readOGR(dsn="/Volumes/GoogleDrive/My Drive/range-resilience/scope/study-area-shapefiles/NorthernMixedSubset",layer="NorthernMixedSubset")
#plot(NorthernMixedSubset.shape)
NorthernMixedSubset.shape@bbox <- as.matrix(extent(mean_production_raster))
plot(NorthernMixedSubset.shape)
#step 2:
NorthernMixedSubset.shape.2 <- sp::spTransform(NorthernMixedSubset.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#plot(NorthernMixedSubset.shape.2)
crop(NorthernMixedSubset.shape.2,mean_production_raster)
NorthernMixedSubset.shape.3 <- spTransform(NorthernMixedSubset.shape.2, CRS(aea.proj))
#plot(NorthernMixedSubset.shape.3)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_1/ecoregion_map.pdf',
    width=6,height=6)
plot(states_all_sites)
plot(NorthernMixedSubset.shape.3,col='steelblue2', lwd = .1,add=TRUE)
plot(SGS.shape.3, lwd = 0.1,col='green4', lwd = .1,add=TRUE)
plot(CaliforniaAnnual.shape.3,col='grey', lwd = .1,add=TRUE)
plot(ChihuahuanDesert.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(mojave.sonoran.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(Grama.Galleta.Steppe.shape.3, col='firebrick3', lwd = .1,add=TRUE)
plot(ColdDeserts.shape.3, col='gold', lwd = .1,add=TRUE)
dev.off()
##### 1B: mean production map ######

mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates)
mean_production_raster<-rasterFromXYZ(mean_production)
#plot(mean_production_raster)

npp= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bkcols.npp <- colorRampPalette(npp)(length(bks_npp)-1)
bks_npp<- quantile(mean_production$npp.x, probs=seq(0.0, 1, by=0.05), na.rm = TRUE)
proj4string(mean_production_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

r.range.npp <- round(c(minValue(mean_production_raster), maxValue(mean_production_raster)))

#update projection
proj4string(mean_production_raster) <- CRS("+proj=longlat")
mean_production_raster_2<-projectRaster(mean_production_raster, crs=aea.proj)
#plot(mean_production_raster_2)

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_1/mean_npp_map.pdf',
    width=6,height=6)
plot(mean_production_raster_2,breaks = bks_npp,box=F,axes=F,col = bkcols.npp,
     legend.width=1,legend.shrink=0.75,
     axis.args=list(at=seq(r.range.npp[1], r.range.npp[2], 100),
                    labels=seq(r.range.npp[1], r.range.npp[2], 100),
                    cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE)
dev.off()
#########
##### 1C: mean annual precipitation map ######

mean_mm<-aggregate(mm.x~ x + y,mean,data=rangeland_npp_covariates)
mean_mm_raster<-rasterFromXYZ(mean_mm)
#plot(mean_mm_raster)
precip= c("red", "orange", "yellow",'green','cyan3','purple')
bks_map<- quantile(mean_mm$mm.x, probs=seq(0, 1, by=0.05), na.rm = TRUE)
bkcols.precip <- colorRampPalette(precip)(length(bks_map)-1)
proj4string(mean_mm_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +datum=NAD83 +units=m +no_defs")
r.range <- round(c(minValue(mean_mm_raster), maxValue(mean_mm_raster)))

#update projection
proj4string(mean_mm_raster) <- CRS("+proj=longlat")
mean_mm_raster_2<-projectRaster(mean_mm_raster, crs=aea.proj)
#plot(mean_mm_raster_2)

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_1/precip_map.pdf',
    width=6,height=6)
plot(mean_mm_raster_2,breaks = bks_map,axes=F,box=F,col = bkcols.precip,
     legend.width=1,legend.shrink=0.75,
     axis.args=list(at=seq(r.range[1], r.range[2], 100),
                    labels=seq(r.range[1], r.range[2], 100),
                    cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE)
dev.off()
######
##### 1D: %Herbaceouns NPP map ######
mean_herb<-aggregate(perc_herb~ x + y,mean,data=rangeland_npp_covariates)
mean_herb_raster<-rasterFromXYZ(mean_herb)
plot(mean_herb_raster)

npp= c('wheat3','wheat', "orange", "yellow",'green','darkgreen')
bkcols.npp <- colorRampPalette(npp)(length(bks_npp)-1)
bks_npp<- quantile(mean_herb$perc_herb, probs=seq(0.0, 1, by=0.05), na.rm = TRUE)
proj4string(mean_herb_raster)<-CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

r.range.npp <- round(c(minValue(mean_herb_raster), maxValue(mean_herb_raster)))

#update projection
proj4string(mean_herb_raster) <- CRS("+proj=longlat")
mean_herb_raster_2<-projectRaster(mean_herb_raster, crs=aea.proj)
#plot(mean_herb_raster_2)

#plot it
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_1/herb_map.pdf',
    width=6,height=6)
plot(mean_herb_raster_2,breaks = bks_npp,box=F,axes=F,col = bkcols.npp,
     legend.width=1,legend.shrink=1,
     axis.args=list(at=seq(r.range.npp[1], r.range.npp[2], 10),
                    labels=seq(r.range.npp[1], r.range.npp[2], 10),
                    cex.axis=1.75),
     legend.args=list(text='', side=1, font=2, line=2.5, cex=0.8))
plot(states_all_sites, lwd = 1,add=TRUE)
#looks good!
dev.off()
######