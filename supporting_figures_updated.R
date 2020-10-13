# supporting figures

##### modeled maps of sensitivity without ecoregion ######

#A

#map no ecoregion
#head(rangeland_npp_covariates)
map.no.ecoregion<-rangeland_npp_covariates
map.no.ecoregion$coef<-mean(map_coefficients$coefficient.temporal) + 
  mean(map_coefficients$coefficient.spatial_temporal)*map.no.ecoregion$mm.y
map.no.ecoregion<-aggregate(coef~x+y,mean,data=map.no.ecoregion)
#head(map.no.ecoregion)

#plot it: supporting figure x
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/sensitivity_maps/map_sens_noecoregion.proj.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(map.no.ecoregion))
dev.off()

#B

#herb no ecoregion
herb.no.ecoregion<-rangeland_npp_covariates
herb.no.ecoregion$coef<-mean(map_herb_coefficients$coefficient.temporal) + 
  mean(map_herb_coefficients$coefficient.herb_temporal)*herb.no.ecoregion$perc_herb_mean
herb.no.ecoregion<-aggregate(coef~x+y,mean,data=herb.no.ecoregion)
#head(herb.no.ecoregion)

#plot it: supporting figure x
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/sensitivity_maps/herb_sens_noecoregion.proj.pdf',
    width=6,height=6)
plot_sensitivity(rasterFromXYZ(herb.no.ecoregion))
dev.off()

#######

#### MAP-%herbaceous correlations by ecoregion ########
mean_herb_map<-aggregate(perc_herb_mean~ mm.y + x + y + region,mean,data=rangeland_npp_covariates)
head(mean_herb_map)
neworder <- c("shortgrass_steppe","northern_mixed_prairies","california_annuals","cold_deserts","hot_deserts")
#change ordering for plotting
#unique(mean_herb_map$region)
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

herb_map_cors<-mean_herb_map %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(cor(mm.y, perc_herb_mean))

########
######## MAP-%woody by ecoregion #######

#get % woody
fractional_npp<-readRDS(('/Volumes/GoogleDrive/My Drive/range-resilience/Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/Fractional_NPP_Western_US.rds'))
head(fractional_npp)
fractional_npp$perc_woody<-round((fractional_npp$shrub + fractional_npp$tree)/
                                  (fractional_npp$annual_grass_forb + fractional_npp$perennial_grass_forb + 
                                     fractional_npp$shrub + fractional_npp$tree)*100,1)

fractional_npp<-fractional_npp[-c(4,5,6,7)]
#head(fractional_npp)
perc_woody_mean <-aggregate(perc_woody~x+y,mean,data=fractional_npp)
perc_woody_mean$perc_woody_mean<-round(perc_woody_mean$perc_woody,1)
perc_woody_mean<-perc_woody_mean[-c(3)]
#head(perc_woody_mean)
fractional_npp<-merge(fractional_npp,perc_woody_mean,by=c('x','y'))
#head(fractional_npp)
rm(perc_woody_mean)

#merge with dryland npp dataset
rangeland_npp_covariates<-merge(rangeland_npp_covariates,fractional_npp,
                                by=c('x','y','year'))

mean_woody_map<-aggregate(perc_woody_mean~ mm.y + x + y + region,mean,data=rangeland_npp_covariates)
head(mean_woody_map)

mean_woody_map_ordered <- arrange(mutate(mean_woody_map,
                                        region=factor(region,levels=neworder.2)),region)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/woody_map_correlations.pdf',
    width=8,height=6)
ggplot(mean_woody_map_ordered,aes(x=mm.y,y=perc_woody_mean)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  geom_point(size=0.75,pch=1,alpha=0.75) +
  #stat_smooth(method = 'lm') +
  # scale_colour_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
  #                              california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
  #                     labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
  #                              california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('% Woody NPP') +
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

#####get summaries of % functional group NPP by ecoregion #####
fractional_npp_summary<-readRDS(('/Volumes/GoogleDrive/My Drive/range-resilience/Sensitivity/Processing NPP Data/Hebaceous NPP Data processing/Hebaceous_NPP_Processing/Fractional_NPP_Western_US.rds'))
#head(fractional_npp_summary)
#str(fractional_npp_summary)

#merge with ecoregion data
#head(rangeland_npp_covariates[c(1,2,3,4)])
fractional_npp_summary <-merge(fractional_npp_summary,rangeland_npp_covariates[c(1:4)],by=c('x','y','year'))

fractional_npp_summary <-fractional_npp_summary %>%
  dplyr:: group_by(x,y,region) %>%
  dplyr:: summarise_if(is.numeric,mean)
fractional_npp_summary<-data.frame(fractional_npp_summary)
#head(fractional_npp_summary)

#get % NPP of each functional type.
denom<-(fractional_npp_summary$annual_grass_forb + fractional_npp_summary$perennial_grass_forb + 
          fractional_npp_summary$shrub + fractional_npp_summary$tree)

fractional_npp_summary$annual_grass_forb_perc <- round(fractional_npp_summary$annual_grass_forb/denom*100,1)
fractional_npp_summary$perennial_grass_forb_perc <- round(fractional_npp_summary$perennial_grass_forb/denom*100,1)
fractional_npp_summary$shrub_perc <- round(fractional_npp_summary$shrub/denom*100,1)
fractional_npp_summary$tree_perc <- round(fractional_npp_summary$tree/denom*100,1)
#head(fractional_npp_summary)

fractional_npp_summary_mean <-fractional_npp_summary %>%
  dplyr:: group_by(region) %>%
  dplyr:: summarise_if(is.numeric,mean)
fractional_npp_summary_mean<-data.frame(fractional_npp_summary_mean)
fractional_npp_summary_mean$stat<-'mean'

fractional_npp_summary_ci <-fractional_npp_summary %>%
  dplyr:: group_by(region) %>%
  dplyr:: summarise_if(is.numeric,error.95)
fractional_npp_summary_ci<-data.frame(fractional_npp_summary_ci)
fractional_npp_summary_ci$stat<-'95ci'

fractional_npp_summary_final <- rbind(fractional_npp_summary_ci,fractional_npp_summary_mean,
                                by=c('stat'))
rm(fractional_npp_summary_ci,fractional_npp_summary_mean,fractional_npp_summary)
head(fractional_npp_summary)

write.csv(fractional_npp_summary_final[-c(2,3)],
          file = '/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/tables/pft_summary.csv')


#####

#### bic #########
unique(bic.binded$model)
bic.binded.2<-bic.binded %>%
  dplyr::filter(model==c("herb.map","ecoregion","herb.region","no.veg"))
unique(bic.binded.2$model)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/bic.pdf',
    width=8,height=6)
ggplot(bic.binded.2,aes(x=bic ,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(166000,191700)) +
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
#####
##### %herbaceous distributions by ecoregion #######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/herb_distributions.pdf',
    width=8,height=6)
ggplot(mean_herb_map_ordered,aes(x=perc_herb_mean,fill=region)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  #stat_smooth(method = 'lm') +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                               california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                      labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                               california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('Density') +
  xlab('% Herbaceous NPP') +
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

##### %woody distributions by ecoregion #######
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/supporting/woody_distributions.pdf',
    width=8,height=6)
ggplot(mean_woody_map_ordered,aes(x=perc_woody_mean,fill=region)) +
  facet_wrap(~region,scales = "free",labeller = as_labeller(veg_names)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  #geom_vline(xintercept=0,color='black',size=1,linetype='dashed') +
  geom_density(color='black',alpha=0.75,aes(y=..scaled..)) +
  #stat_smooth(method = 'lm') +
  scale_fill_manual(values=c('shortgrass_steppe'='green4','northern_mixed_prairies'='lightblue',
                             california_annuals='grey',cold_deserts='gold',hot_deserts='firebrick3'),
                    labels=c('shortgrass_steppe'='Shortgrass steppe','northern_mixed_prairies'='Northern mixed prairies',
                             california_annuals='California annuals',cold_deserts='Cold deserts',hot_deserts='Hot deserts')) +
  ylab('Density') +
  xlab('% Woody NPP') +
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