# figure 2

###### 2A: AIC comparisons ######

unique(aic.binded$model)
aic.binded.2<-aic.binded %>%
  dplyr::filter(model==c("herb.map","ecoregion","herb.region","no.veg"))
unique(aic.binded$model)
pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/Figure_2/aic.pdf',
    width=8,height=6)
ggplot(aic.binded.2,aes(x=aic ,color=model)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.02)) +
  scale_x_continuous(expand = c(0,0),limits = c(161900,191950)) +
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
    legend.title = element_text(size=10),
    legend.text = element_text(size=9),
    legend.position = c(0.5,0.6),
    #legend.position = 'top'5
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
dev.off()
########
####### 2B: Temporal sensitivity among ecoregions #######
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
  ylab('') +
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
# 2C: change in sensitivity per mm map ########
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
# 2D: change in sensitivity % herbaceous NPP#######
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
  xlab('Change in sensitivity per % herbaceous cover') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=15),
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