#mean npp-map spatial model across veg types
mean_production<-aggregate(npp.x~ x + y,mean,data=rangeland_npp_covariates_deviations_1)
head(mean_production)
list.coefficients.mean<-list()
for(i in 1:1000)
{
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
  
  stratified_final_mean<-merge(test.strat, mean_production,by=c('x','y'))
  #print(stratified_final)
  
  stratified_final_lm_mean<-lm(npp.x~mm*region
                          ,stratified_final_mean)
  
 
  newcoef1 <- stratified_final_lm_mean$coefficients 
  df.mean<-data.frame(newcoef1)
  df.mean$id = i
  list.coefficients.mean[[i]] <- data.frame(df.mean)
  
  
}

summary(stratified_final_lm_mean)
df.coefficients.mean <- do.call("rbind", list.coefficients.mean)
head(df.coefficients.mean)
df.coefficients.mean.2 <- cbind(rownames(df.coefficients.mean), data.frame(df.coefficients.mean, row.names=NULL))

colnames(df.coefficients.mean.2)  <- c("predictor","coefficient","run.id")

df.coefficients.mean.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub(':', '_', df.coefficients.mean.2$predictor)
df.coefficients.mean.2$predictor<-gsub('-', '_', df.coefficients.mean.2$predictor)

df2_mean<-reshape(df.coefficients.mean.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(df2_mean)
summary(df2_mean)

#spatial slopes

#cold deserts
df2_mean$cold_deserts_spatial_slope <- df2_mean$coefficient.mm + df2_mean$coefficient.mm_regioncold_deserts
#hot_deserts
df2_mean$hot_deserts_spatial_slope <-  df2_mean$coefficient.mm + df2_mean$coefficient.mm_regionhot_deserts
#northern mixed
df2_mean$northern_mixed_spatial_slope <- df2_mean$coefficient.mm + df2_mean$coefficient.mm_regionnorthern_mixed_prairies
#sgs
df2_mean$sgs_spatial_slope <- df2_mean$coefficient.mm + df2_mean$coefficient.mm_regionsemi_arid_steppe

spatial_slopes<-subset(df2_mean,select=c('sgs_spatial_slope','northern_mixed_spatial_slope','hot_deserts_spatial_slope','cold_deserts_spatial_slope',
                                    'coefficient.mm'))
head(spatial_slopes)
data_long <- gather(spatial_slopes, site, slope, factor_key=TRUE)
head(data_long)
summary(data_long)

#rename the vegetation types
rename_sites<- c(hot_deserts_spatial_slope="Hot deserts", cold_deserts_spatial_slope="Cold deserts",
                 sgs_spatial_slope="Shortgrass steppe", coefficient.mm="California annuals", 
                 northern_mixed_spatial_slope="Northern mixed prairies")

data_long$site <- as.character(rename_sites[data_long$site])

#change the order
data_long$site <- factor(data_long$site, levels = c("Hot deserts", "Cold deserts", "California annuals",
                                                    "Shortgrass steppe", "Northern mixed prairies"))

data_long$site <- factor(data_long$site, levels = c("hot_deserts_spatial_slope", "cold_deserts_spatial_slope", 
                                                    "coefficient.mm","sgs_spatial_slope", 
                                                    "northern_mixed_spatial_slope"))

ag.test<-aggregate(slope~site,median,data=data_long)
#plot the spatial slopes
#spatial coefficients plot

ggplot(data_long,aes(x=slope)) +
  geom_histogram(binwidth = .01,fill='white',color='black') +
  #geom_density() +
  facet_wrap(~site,nrow=5,scales='free_y') +
  #geom_histogram(color='black',size=.5,alpha=.7) +
  #geom_density_ridges(size=1,alpha=0.5,color='black',calc_ecdf = TRUE) +
  #scale_fill_viridis(name = "Tail probability", direction = -1)
  #geom_density_ridges_gradient(stat = "binline", binwidth = 0.01,color='black',fill='white')
  #geom_point(size=.1,pch=19,alpha=.1) +
  #scale_y_discrete(limits=c('hot_deserts_spatial_slope','cold_deserts_spatial_slope',
                         #   'sgs_spatial_slope','coefficient.mm','northern_mixed_spatial_slope'),
                   #labels=c('coefficient.mm'='California annuals','northern_mixed_spatial_slope'='Mixed prairies',
                         #   'hot_deserts_spatial_slope' = 'Hot deserts','cold_deserts_spatial_slope'='Cold deserts',
                          #  'sgs_spatial_slope'='Semi-arid steppe')) +
  
  #scale_fill_manual(values=c('coefficient.mm'='gray28','northern_mixed_spatial_slope'='dodgerblue',
                             #'hot_deserts_spatial_slope' = 'tomato3','cold_deserts_spatial_slope'='gold',
                             #'sgs_spatial_slope'='green3')) +
 
  
  xlab(bquote('Spatial sensitivity ('*g/m^2/mm*')')) +
  
  
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  ylab('') +
  
  theme(
    
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=12),
    
    axis.title = element_text(color='black',size=23),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    
    legend.position = c('none'),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))
