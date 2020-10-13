#look at r-square for each ecoregion

region.list<-c('shortgrass_steppe','northern_mixed_prairies','hot_deserts','cold_deserts','california_annuals')

r.square.list.map<-list()
r.square.list.veg<-list()
r.square.list.full<-list()
r.square.list.no.int<-list()

for(iRegion in 1:length(region.list)){
  
  ecoregion <- region.list[iRegion]
  i = ecoregion
  #subset
  region<-subset(rangeland_npp_covariates,region==ecoregion)
  
  #map model
  map<-lm(npp.x~mm.y + mm.dev + mm.dev:mm.y, region)
  r.square.list.map[[iRegion]] <-df.r.squared.create(map)
  
  #veg model
  veg<-lm(npp.x~mm.y + mm.dev + perc_herb + mm.dev:perc_herb, region)
  r.square.list.veg[[iRegion]] <-df.r.squared.create(veg)
  
  #full model
  full<-lm(npp.x~mm.y + mm.dev + perc_herb + mm.dev:perc_herb + mm.dev:mm.y, region)
  r.square.list.full[[iRegion]] <-df.r.squared.create(full)
  
  #no.int
  no.int<-lm(npp.x~mm.y + mm.dev, region)
  r.square.list.no.int[[iRegion]] <-df.r.squared.create(no.int)
}

#make into dataframe
map<-list_to_df_initial(r.square.list.map)
map$model<-'map'

veg<-list_to_df_initial(r.square.list.veg)
veg$model<-'veg'

full<-list_to_df_initial(r.square.list.full)
full$model<-'full'

no.int<-list_to_df_initial(r.square.list.no.int)
no.int$model<-'no.int'

full.veg<-rbind(full,veg)
full.veg.map<-rbind(full.veg,map)
full.veg.map.no.int<-rbind(full.veg.map,no.int)

pdf(file='/Volumes/GoogleDrive/My Drive/range-resilience/manuscripts/Vegetation_precipitation_interactions/Paper/figures/new/ecregion.r.squares.pdf',
    width=8,height=6)
ggplot(full.veg.map.no.int,aes(model,coefficient.)) +
  facet_wrap(~run.id) +
  stat_summary(geom = 'bar',fun.y='mean') +
  ylab('R-squared') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=10, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
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

#double check
#cold deserts
cd<-subset(rangeland_npp_covariates,region=='cold_deserts')
dim(cd)
#map model
map.cd<-lm(npp.x~mm.y + mm.dev + mm.dev:mm.y, cd)
summary(map.cd)


#sgs
sgs<-subset(rangeland_npp_covariates,region=='shortgrass_steppe')
dim(sgs)
#map model
map.sgs<-lm(npp.x~mm.y + mm.dev + mm.dev:mm.y,sgs)
summary(map.sgs)

dim(cd[1])/dim(sgs[1])
