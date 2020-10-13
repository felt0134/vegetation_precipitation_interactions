#experiments
#idea to replace figure 1d with average percent herb, then put current 1d to figure 
#1a

#percent herb plot#######

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
#change in sensitivity per mm map########
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_2/change_sens_map.pdf',
    width=8,height=6)
ggplot(map_region_coefficients,aes(x=Spatiotemporal,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  #xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  xlab('Change in sensitivity per mm MAP') +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=18), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()
######
#change in sensitivity % herbaceous NPP#######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_2/change_sens_herb.pdf',
    width=8,height=6)
ggplot(herb_region_coefficients,aes(x=Spatiotemporal,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.6,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  #xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  xlab('Change in sensitivity per % herbaceous NPP') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    #legend.position = c(0.15,0.7),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()
########

#temporal sensitivity scaled to MAP#######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_2/temporal_sens.pdf',
    width=8,height=6)
ggplot(map_region_coefficients,aes(x=temporal_sensitivity_map,fill=region)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  xlab(bquote('Temporal sensitivity ('*g/m^2/mm*')')) +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title.x = element_text(color='black',size=25),
    axis.title.y = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=12),
    legend.position = c(0.5,0.85),
    #legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()

#aic#########
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_3/aic.pdf',
    width=8,height=6)
ggplot(aic.binded,aes(x=aic ,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(alpha=1,size=1,aes(y=..scaled..)) +
  #geom_histogram(binwidth = 0.5,color='black') +
  scale_colour_manual(name = 'Spatiotemporal interaction',
                      values=c('ecoregion' = 'black',
                               'herb.map'='red',
                               'herb.region'='blue',
                               'no.veg' = 'darkgrey'
                      ),
                      labels=c('ecoregion'='MAP x Dev x Ecoregion',
                               'herb.map'='%Herb x Dev',
                               'herb.region'='%Herb x Dev x Ecoregion',
                               'no.veg' = 'MAP x Dev')) +
  xlab('AIC') +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=20), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=18),
    legend.text = element_text(size=14),
    legend.position = c(0.5,0.5),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()

########
#bic#########
unique(bic.binded$model)
bic.binded.2<-bic.binded %>%
  dplyr::filter(model==c("herb.map","ecoregion","herb.region","no.veg"))
unique(bic.binded.2$model)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/bic.pdf',
    width=8,height=6)
ggplot(bic.binded.2,aes(x=bic ,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  geom_density(alpha=1,size=1,aes(y=..scaled..)) +
  scale_colour_manual(name = 'Spatiotemporal interaction',
                      values=c('ecoregion' = 'black',
                               'herb.map'='red',
                               'herb.region'='blue',
                               'no.veg' = 'darkgrey'
                      ),
                      labels=c('ecoregion'='MAP x Dev x Ecoregion',
                               'herb.map'='%Herb x Dev',
                               'herb.region'='%Herb x Dev x Ecoregion',
                               'no.veg' = 'MAP x Dev')) +
  xlab('BIC') +
  ylab('Density') +
  theme(
    axis.text.x = element_text(color='black',size=20), 
    axis.text.y = element_text(color='black',size=18),
    axis.title = element_text(color='black',size=30),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=18),
    legend.text = element_text(size=14),
    legend.position = c(0.5,0.5),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()

#######
#####percent herb and MAP by ecoregion#######
mean_herb_map<-aggregate(perc_herb_mean~ mm.y + region,mean,data=rangeland_npp_covariates)
head(mean_herb_map)
neworder <- c("shortgrass_steppe","northern_mixed_prairies","california_annuals","cold_deserts","hot_deserts")
#change ordering for plotting
unique(mean_herb_map$region)
veg_names <- c(
  `hot_deserts` = "Hot deserts",
  `cold_deserts` = "Cold deserts",
  'northern_mixed_prairies' = "Northern mixed",
  `shortgrass_steppe` = "Shortgrass steppe",
  `california_annuals` = "California annuals"
)

neworder <- c("shortgrass_steppe","northern_mixed_prairies","california_annuals","cold_deserts","hot_deserts")
neworder.2 <- c("hot_deserts","cold_deserts","california_annuals","northern_mixed_prairies","shortgrass_steppe")
mean_herb_map_ordered <- arrange(mutate(mean_herb_map,
                                                region=factor(region,levels=neworder.2)),region)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/herb_map_correlations.pdf',
    width=8,height=6)
ggplot(mean_herb_map_ordered,aes(x=mm.y,y=perc_herb_mean)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  geom_point(size=0.75,pch=1,alpha=0.75) +
  #stat_smooth(method = 'lm') +
  # scale_colour_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
  #                              california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
  #                     labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
  #                              california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('% Herbaceous NPP') +
  #xlab('Change in sensitivity per mm of MAP') +
  xlab('Mean annual precipitation (mm)') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
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
dev.off()
#####
####change in sens per % herbaceous NPP not by ecoregion#######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/herb_dev_noecoregion.pdf',
    width=8,height=6)
mar.default <- c(5,4,2,2) + 0.1
par(mar = mar.default + c(1, 1, 0, 0))
hist(herb_region_coefficients_no.threeway$coefficient.temporal_herb,main='',
     xlab='Change in sensivity per % herbaceous NPP',cex.lab=1.5)
dev.off()
#######