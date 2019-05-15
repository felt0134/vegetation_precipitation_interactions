
#make lists to put looped fles into
list.coefficients.final<-list()
list.variograms<-list()
list.residual.rasters<-list()
list.residuals.full <- list()


#reduce columns in dataset to use

head(rangeland_npp_covariates_deviations_1)
rangeland_npp_covariates_deviations_reduced <-subset(rangeland_npp_covariates_deviations_1,select=c('x','y','year','npp.x',
                                                                                                  'mm.y','mm.dev','region.x'))
head(rangeland_npp_covariates_deviations_reduced)

#inspect autocorrelation
for(i in 1:1000)
  {
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
 
stratified_final<-merge(test.strat, rangeland_npp_covariates_deviations_reduced,by=c('x','y'))
#print(stratified_final)

stratified_final_lm<-lm(npp.x~mm.y*region.x*mm.dev
                        ,stratified_final)

newcoef1 <- stratified_final_lm$coefficients 
df<-data.frame(newcoef1)
df$id = i
list.coefficients.final[[i]] <- data.frame(df)

#look all residuals
stratified_final$resids <-residuals(stratified_final_lm)
list.residuals.full[[i]] <- stratified_final

#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)

#make rasters
residual.raster<-rasterFromXYZ(mean.resids)
#residual.plot<-plot(residual.raster)
list.residual.rasters[[i]] <- data.frame(mean.resids)

#variogram of mean
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)
variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
list.variograms[[i]] <- variogram.plot

}

#inspect variograms of mean residuals for each run
list.variograms[1:50]

#look at coefficients
library(reshape2)
library(tidyverse)
df.coefficients <- do.call("rbind", list.coefficients.final)
head(df.coefficients)
df.coefficients.2 <- cbind(rownames(df.coefficients), data.frame(df.coefficients, row.names=NULL))

colnames(df.coefficients.2)  <- c("predictor","coefficient","run.id")

df.coefficients.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub(':', '_', df.coefficients.2$predictor)
df.coefficients.2$predictor<-gsub('-', '_', df.coefficients.2$predictor)

df2<-reshape(df.coefficients.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(df2)
summary(df2)

#spatial slopes

#cold deserts
df2$cold_deserts_spatial_slope <- df2$coefficient.mm.y + df2$coefficient.mm.y_region.xcold_deserts
#hot_deserts
df2$hot_deserts_spatial_slope <-  df2$coefficient.mm.y + df2$coefficient.mm.y_region.xhot_deserts
#northern mixed
df2$northern_mixed_spatial_slope <- df2$coefficient.mm.y + df2$coefficient.mm.y_region.xnorthern_mixed_prairies
#sgs
df2$sgs_spatial_slope <- df2$coefficient.mm.y  + df2$coefficient.mm.y_region.xsemi_arid_steppe

spatial_slopes<-subset(df2,select=c('sgs_spatial_slope','northern_mixed_spatial_slope','hot_deserts_spatial_slope','cold_deserts_spatial_slope',
                                    'coefficient.mm.y','run.id'))
head(spatial_slopes)
data_long <- gather(spatial_slopes, site, slope, sgs_spatial_slope:coefficient.mm.y, factor_key=TRUE)
head(data_long)

#cold deserts
spatial_slope_cd<-subset(coefficients_long,rownames(coefficients_df_wide) =='mm.y:region.xcold_deserts')
hist(spatial_slope_cd$value)
median(spatial_slope_cd$value)



#########look at residuals#############

df.residuals <- do.call("rbind", list.residuals.full)
head(df.residuals)


###scrap code###########

list.results<-list()

#inspect plots of residuals
list.residual.rasters[1:50]
colnames <- c("x","y","resids") 
extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)
conus_split_3<-lapply(list.residual.rasters, setNames, colnames)
conus_split_4<-lapply(conus_split_3, rasterFromXYZ) #change each year into its own raster
conus_split_5<-lapply(conus_split_4, resample)
conus_npp_procossed_stack <-stack(conus_split_4,quick=TRUE) #stack each raster
plot(conus_npp_procossed_stack)
extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)

for(i in 1:length(conus_split_4)) {
  #extent.all<-extent(-121.75, -97.5625, 29.3125, 48.9375)
  r <-conus_split_4[[i]] # raster(files[i])
  rc <- crop(r, extent.all)
  list.results[[i]] <- rc
  env_data<- stack(list.results)}
if(sum(as.matrix(extent(rc))!=as.matrix(extent.all)) == 0){ # edited
  # You can't mask with extent, only with a Raster layer, RStack or RBrick
}

list.results[[i]] <- rc
env_data<- stack(list.results)

}

plot(list.residual.rasters[10])
plot(rc)
plot(residual.raster)
?rasterFromXYZ
plot(list.residual.rasters[5]$x,list.residual.rasters[5]$y,list.residual.rasters[5]$resids)

list.variograms[1:50]
filenames_plan <- names(list.variograms)


for (i in 1:length(list.variograms)){
  #outname <- paste("C:/Users/A02296270/Desktop/variogram_test/", list.variograms[i], "_.jpeg",sep='')
  jpeg(list.variograms[[i]], filename = 'outname.jpeg')
}

summary(data_long)
#ggplot
library(ggplot2)
ggplot(data_long,aes(data_long$slope,color=site)) +
  #geom_point(alpha=.1) +
  #geom_histogram(binwidth = .01) +
  #geom_density(color='black',size=.5,alpha=.7) +
  geom_density(size=1.5,fill="grey",alpha=.25) +
  xlab('Slope of spatial model') +
  scale_y_continuous(expand=c(0,0),limits=c(0,15)) +
  scale_colour_manual(values=c('coefficient.mm.y'='black','northern_mixed_spatial_slope'='orange',
                             'hot_deserts_spatial_slope' = 'red','cold_deserts_spatial_slope'='blue',
                             'sgs_spatial_slope'='darkgreen'),name="Vegetaton type",
                    
                    labels=c('coefficient.mm.y'='California annuals','northern_mixed_spatial_slope'='Mixed prairies',
                             'hot_deserts_spatial_slope' = 'Hot deserts','cold_deserts_spatial_slope'='Cold deserts',
                             'sgs_spatial_slope'='Semi-arid steppe')) +
  
  #geom_smooth(method = "lm", formula=y ~ poly(x, 2, raw=TRUE),se = FALSE,size=.5,color="red") +
  
  #stat_summary(geom="bar",fun.y="mean",size=.5,color="black",aes(fill=as.factor(Rainfall.pattern))) +
  
  #scale_fill_manual(values=c('Experimental'='blue','Observational'='red'),name="Rainfall pattern",
  
  #labels=c('Uniform'='Experimental','Ambient'='Observational')) + #change names
  
  #stat_summary(geom="bar",fun.y="mean",size=.5,color="black",fill="red") +
  
ylab("Density") +

#xlab("Mean event size (mm)") +

#ylab(bquote('ANPP ('*g/m^2*')')) +

#scale_x_discrete(labels=c('Ambient'="Variable","Reduced"="Uniform")) +

#xlab("Maximum event size (mm)") +

#labs(x=expression(Excess~rainfall~-~R[">30"]~("% of GSP"))) +

#ylab("Experimental event event size (mm)") +

#xlab("May - August precipitation (mm)") +
  
  #ylab("") +
  
  #ggtitle("SD event size = 33.53, PUE= .78, 2003") +
  
  theme(
    
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    
    axis.text.y = element_text(color='black',size=20),
    
    axis.title = element_text(color='black',size=23),
    
    axis.ticks = element_line(color='black'),
    
    legend.key = element_blank(),
    
    #legend.title = element_blank(),
    
    legend.text = element_text(size=12),
    
    legend.position = c(.6,.85),
    
    panel.background = element_rect(fill=NA),
    
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    
    axis.line.x = element_line(colour = "black"),
    
    axis.line.y = element_line(colour = "black"))
