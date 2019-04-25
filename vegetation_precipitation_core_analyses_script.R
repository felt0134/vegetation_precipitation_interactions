#script to repeat
test.strat<-stratified(stratified_df, c("region","map"), 0.1)
#test.strat.raster<-rasterFromXYZ(test.strat)
#plot(test.strat.raster)
#head(test.strat)
#summary(test.strat)

stratified_final<-merge(test.strat, rangeland_npp_covariates_deviations_1,by=c('x','y'))
#head(stratified_final)

#get residuals of LM
test.strat_lm<-lm(npp.x~mm.y*region.x*mm.dev
                  ,stratified_final)
#summary(test.strat_lm)
stratified_final$resids<-resid(test.strat_lm)

#make a spatial neighborhood weights list
xy_stratified<-cbind(stratified_final$x,stratified_final$y)
wd5_xy_stratified <- dnearneigh(xy_stratified, 0, 5) #range of 5 degrees
W_xy_stratified <- as(nb2listw(wd5_xy_stratified, style="W", zero.policy=TRUE), "CsparseMatrix") #create a sparse weights matrix
sparse_neighbor_weights_xy_stratified <- mat2listw(W_xy_stratified)
#moran.mc(stratified_final$resids, sparse_neighbor_weights_xy_stratified, nsim=999)
lag_full <- lagsarlm(npp.x~mm.y*region.x*mm.dev,data=stratified_final, listw = sparse_neighbor_weights_xy_stratified)
#summary(lag_full) 
coefs<-lag_full$coefficients
coefs.df<-data.frame(coefs)

#code to export this to a folder....