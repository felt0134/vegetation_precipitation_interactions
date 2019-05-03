####3d plotting#########

library(lattice)

#hot deserts
hot_deserts_surface<-subset(stratified_final,region.x=='hot_deserts')
hot_deserts.loess<-lm(npp.dev~mm.dev*mm.y,data=hot_deserts_surface)
summary(hot_deserts_surface)
hot_deserts_fit<-expand.grid(list(mm.dev=seq(-100,300,25),mm.y=seq(80,650,25)))
hot_deserts_fit[1:20,]
z = predict(hot_deserts.loess,hot_deserts_fit)
hot_deserts_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = hot_deserts_fit,
          xlab = list("% Precipitation change",rot=-50,cex=1.4), zlab = list("% NPP change",rot=92,cex=1.4),ylab = list('Mean annual precipitation',rot=22,cex=1.4),
          main = "Hot deserts",
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("orange", "green"))(100),
          screen = list(z = -60, x = -60)
)

#california
cali_annuals_surface<-subset(stratified_final,region.x=='california_annuals')
cali_annuals.loess<-lm(npp.dev~mm.dev*mm.y,data=cali_annuals_surface)
summary(cali_annuals_surface)
cali_annuals_fit<-expand.grid(list(mm.dev=seq(-75,325,25),mm.y=seq(175,825,25)))
cali_annuals_fit[1:20,]
z = predict(cali_annuals.loess,cali_annuals_fit)
cali_annuals_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = cali_annuals_fit,
          xlab = "", ylab = "",zlab = '',
          main = "California annuals",
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("orange", "green"))(100),
          screen = list(z = -60, x = -60)
)

#cold deserts
cold_deserts_surface<-subset(stratified_final,region.x=='cold_deserts')
cold_deserts.loess<-lm(npp.dev~mm.dev*mm.y,data=cold_deserts_surface)
summary(cold_deserts_surface)
cold_deserts_fit<-expand.grid(list(mm.dev=seq(-75,200,25),mm.y=seq(100,825,25)))
cold_deserts_fit[1:20,]
z = predict(cold_deserts.loess,cold_deserts_fit)
cold_deserts_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = cold_deserts_fit,
          xlab = "", ylab = "",zlab = '',
          main = "Cold deserts",
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("orange", "green"))(100),
          screen = list(z = -60, x = -60)
)


#shortgrass steppe
sgs_surface<-subset(stratified_final,region.x=='semi-arid_steppe')
sgs.loess<-lm(npp.dev~mm.dev*mm.y,data=sgs_surface)
summary(sgs_surface)
sgs_fit<-expand.grid(list(mm.dev=seq(-75,100,25),mm.y=seq(275,650,25)))
sgs_fit[1:20,]
z = predict(sgs.loess,sgs_fit)
sgs_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = sgs_fit,
          xlab = "", ylab = "",zlab = '',
          main = "shortgrass steppe",
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("orange", "green"))(100),
          screen = list(z = -60, x = -60)
)
?colorkey
#northern mixed prairies
northern_mixed_surface<-subset(stratified_final,region.x=='northern_mixed_prairies')
northern_mixed.loess<-lm(npp.dev~mm.dev*mm.y,data=northern_mixed_surface)
summary(northern_mixed_surface)
northern_mixed_fit<-expand.grid(list(mm.dev=seq(-75,100,25),mm.y=seq(150,775,25)))
northern_mixed_fit[1:20,]
z = predict(northern_mixed.loess,northern_mixed_fit)
northern_mixed_fit$npp <- as.numeric(z)

wireframe(npp ~ mm.dev*mm.y, data = northern_mixed_fit,
          xlab = "% PPT change", ylab = "Mean annual precipitation",zlab = '% NPP change',
          main = "northern mixed prairies",
          drape = TRUE,
          colorkey = TRUE,
          col.regions = colorRampPalette(c("orange", "green"))(100),
          screen = list(z = -60, x = -60)
)


#####other#########
#plot
library(plotly)
packageVersion('plotly')
devtools::install_github("r-lib/later")
head(volcano)
summary(data)
data <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/3d-line1.csv')
data$color <- as.factor(data$color)
p <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 6, color = ~color, reverscale = FALSE))
#test
head(rangeland_npp_covariates_deviations_1)
summary(rangeland_npp_covariates_deviations_1)
str(data)
rangeland_npp_covariates_deviations_1$region.x <-as.factor(rangeland_npp_covariates_deviations_1$region.x)
sd(rangeland_npp_covariates_deviations_1$npp.dev)

p_test <- plot_ly(rangeland_npp_covariates_deviations_1, x = ~mm.dev, y = ~mm.y,z  = ~npp.dev, 
                  type = 'scatter3d',mode='lines',
                  opacity = 1, line = list(width = 6, color= ~region.x, reverscale = FALSE))
plot(p_test)
