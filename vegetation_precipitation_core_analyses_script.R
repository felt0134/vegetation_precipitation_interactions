#script to repeat
test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)

test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                  test.strat.semiarid_steppe, test.strat.hot_deserts)
#head(test.strat)
#summary(test.strat)
#test.strat.visualize = subset(test.strat, select = -c(region,map) )
#test.strat.raster<-rasterFromXYZ(test.strat.visualize)
#plot(test.strat.raster)

#final dataframe to put into model
stratified_final<-merge(test.strat, rangeland_npp_covariates_deviations_1,by=c('x','y'))

#make a distance-based spatial neighborhood weights list
xy_stratified<-cbind(stratified_final$x,stratified_final$y)
wd5_xy_stratified <- dnearneigh(xy_stratified, 0, 5) #range of 5 degrees
W_xy_stratified <- as(nb2listw(wd5_xy_stratified, style="W", zero.policy=TRUE), "CsparseMatrix") #create a sparse weights matrix
sparse_neighbor_weights_xy_stratified <- mat2listw(W_xy_stratified)

#spatial error model
stratified_error<-errorsarlm(npp.x~mm.y*region.x*mm.dev,data=stratified_final, listw = sparse_neighbor_weights_xy_stratified, tol.solve = 1e-30)
summary(stratified_error)
system.time(stratified_error)
#put coefficients into dataframe
coefs_spatial_error<-test_error_model$coefficients
coefs_spatial_error_df<-data.frame(coefs_spatial_error)
