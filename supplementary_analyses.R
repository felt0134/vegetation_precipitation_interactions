#supplmentary analyses and graphs

#see how sd of PPT and MAP varies according to vegetation type
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