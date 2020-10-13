
#written using R version 3.5.1

#change back to this workign directory
# current_path <- getActiveDocumentContext()$path
# setwd(dirname(current_path ))
#lists to store outputs in

#coefficients
list.coefficients.region<-list()
list.coefficients.herb.map<-list()
list.coefficients.herb.region<-list()
list.coefficients.noveg<-list()
list.coefficients.herb.region.no.threeway<-list()
list.coefficients.full<-list()
list.coefficients.no.int<-list()
list.coefficients.ecoregion.null<-list()

#aic
list.aic.region<-list()
list.aic.herb.map<-list()
list.aic.herb.region<-list()
list.aic.noveg<-list()
list.aic.herb.region.no.threeway<-list()
list.aic.full<-list()
list.aic.no.int<-list()
list.aic.full.noecoregion<-list()
list.aic.ecoregion.null<-list()

#bic
list.bic.region<-list()
list.bic.herb.map<-list()
list.bic.herb.region<-list()
list.bic.noveg<-list()
list.bic.herb.region.no.threeway<-list()
list.bic.full<-list()
list.bic.no.int<-list()

#r-squared: total
list.r.squared.region<-list()
list.r.squared.herb.map<-list()
list.r.squared.herb.region<-list()
list.r.squared.noveg<-list()
list.r.squared.region.no.threeway<-list()
list.r.squared.full<-list()
list.r.squared.no.int<-list()
list.r.squared.full.noecoregion<-list()
list.r.squared.ecoregion.null<-list()

#r-squared comparisons
compare.r.squared.region.list<-list()
compare.r.squared.herb.list<-list()
ompare.r.squared.map.list<-list()

#rmse comparisons
compare.rmse.region.list<-list()
compare.rmse.herb.list<-list()
ompare.rmse.map.list<-list()

# #r-squared: spatial
# list.spatial.r.squared.map.null<-list()
# list.spatial.r.squared.veg.null<-list()
# list.spatial.r.squared.ecoregion.null<-list()
# 
# #r-squared: temporal
# list.temporal.r.squared.map.null<-list()
# list.temporal.r.squared.veg.null<-list()
# list.temporal.r.squared.ecoregion.null<-list()

#residual and variagroms
# list.variograms<-list()
# list.residual.rasters<-list()
# list.residuals.full <- list()


#stratified spatial block boostrapping loop

for(i in 1:1000)
  {
  
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)

stratified_final<-merge(test.strat, rangeland_npp_covariates,by=c('x','y','region'))

#model 1: climate interaction, no ecoregion
stratified_final_lm_noveg<-lm(npp.x~mm.y + mm.dev + mm.dev:mm.y
                              ,stratified_final)

#model 2: veg interaction, no ecoregion
stratified_final_lm_herb_map<-lm(npp.x~mm.y + mm.dev  +  mm.dev:perc_herb_mean
                                 ,stratified_final)

#+ perc_herb_mean + this term removed as main effect for the moment

# # model 3: climate interaction, vary with ecoregion
# stratified_final_lm_region<-lm(npp.x~mm.dev + region + mm.y + mm.dev:region + mm.dev:mm.y
#                                + region:mm.y + mm.dev:region:mm.y,stratified_final)
# 
# # model 4: veg interaction, vary with ecoregion
# stratified_final_lm_herb_region<-lm(npp.x~mm.dev + region + mm.y + perc_herb_mean + mm.dev:region + mm.dev:perc_herb_mean +
#                                 + region:mm.y + region:perc_herb_mean + mm.dev:region:perc_herb_mean,stratified_final)
# 
# #model 5: model 4 but remove the three-way interaction
# stratified_final_lm_herb.region.no.threeway<-lm(npp.x~mm.dev + region + mm.y + perc_herb_mean + mm.dev:region + mm.dev:perc_herb_mean +
#                                       + region:mm.y + region:perc_herb_mean,stratified_final)
# 
# #model 6: full model with both climate and veg three-way interactions
# stratified_final_lm_full<-lm(npp.x~mm.dev + region + mm.y + perc_herb_mean + mm.dev:region + mm.dev:perc_herb_mean +
#                                + region:mm.y + region:perc_herb_mean
#                              + mm.dev:region:perc_herb_mean + mm.dev:region:mm.y,stratified_final)
# 
# #model 7: no spatiotemporal interactions, but climate efefct varies by ecoregion
# 
# stratified_final_lm_no_int <-lm(npp.x~mm.y + mm.dev + mm.y:region + mm.dev:region,
#                                 data=stratified_final)

#model 8: full model with both climate and veg no ecoregion
# stratified_final_lm_full_noecoreion<-lm(npp.x~mm.dev + mm.y + perc_herb_mean + mm.dev:perc_herb_mean +
#                                + mm.dev:mm.y,stratified_final)

# ecoregion null model
stratified_final_ecoregion.null<-lm(npp.x ~ mm.y + mm.dev  +  mm.dev:region,stratified_final)
   # + region

#Coefficients
# list.coefficients.region[[i]] <- df.coef.create(stratified_final_lm_region)
list.coefficients.herb.map[[i]] <- df.coef.create(stratified_final_lm_herb_map)
# list.coefficients.herb.region[[i]] <- df.coef.create(stratified_final_lm_herb_region)
list.coefficients.noveg[[i]] <- df.coef.create(stratified_final_lm_noveg)
# list.coefficients.herb.region.no.threeway[[i]] <- df.coef.create(stratified_final_lm_herb.region.no.threeway) #drop ecoregion from the 3-way interaction
# list.coefficients.full[[i]] <- df.coef.create(stratified_final_lm_full)
# list.coefficients.no.int[[i]] <- df.coef.create(stratified_final_lm_no_int)
list.coefficients.ecoregion.null[[i]] <- df.coef.create(stratified_final_ecoregion.null)

#AIC
# list.aic.region[[i]] <- df.aic.create(stratified_final_lm_region)
list.aic.herb.map[[i]] <- df.aic.create(stratified_final_lm_herb_map)
# list.aic.herb.region[[i]] <- df.aic.create(stratified_final_lm_herb_region)
list.aic.noveg[[i]] <- df.aic.create(stratified_final_lm_noveg)
# list.aic.herb.region.no.threeway[[i]] <-df.aic.create(stratified_final_lm_herb.region.no.threeway)
# list.aic.full[[i]] <-df.aic.create(stratified_final_lm_full)
# list.aic.no.int[[i]] <-df.aic.create(stratified_final_lm_no_int)
#list.aic.full.noecoregion[[i]] <-df.aic.create(stratified_final_lm_full_noecoreion)
list.aic.ecoregion.null[[i]]<-df.aic.create(stratified_final_ecoregion.null)

#BIC
# # list.bic.region[[i]] <- df.bic.create(stratified_final_lm_region)
# list.bic.herb.map[[i]] <- df.bic.create(stratified_final_lm_herb_map)
# list.bic.herb.region[[i]] <- df.bic.create(stratified_final_lm_herb_region)
# list.bic.noveg[[i]] <- df.bic.create(stratified_final_lm_noveg)
# list.bic.herb.region.no.threeway[[i]] <-df.bic.create(stratified_final_lm_herb.region.no.threeway)
# # list.bic.full[[i]] <-df.bic.create(stratified_final_lm_full)
# # list.bic.no.int[[i]] <-df.bic.create(stratified_final_lm_no_int)

#r-squared: total
# list.r.squared.region[[i]] <- df.r.squared.create(stratified_final_lm_region)
list.r.squared.herb.map[[i]] <- df.r.squared.create(stratified_final_lm_herb_map)
# list.r.squared.herb.region[[i]] <- df.r.squared.create(stratified_final_lm_herb_region)
list.r.squared.noveg[[i]] <- df.r.squared.create(stratified_final_lm_noveg)
# list.r.squared.region.no.threeway[[i]] <-df.r.squared.create(stratified_final_lm_herb.region.no.threeway)
# list.r.squared.full[[i]]<-df.r.squared.create(stratified_final_lm_full)
# list.r.squared.no.int[[i]]<-df.r.squared.create(stratified_final_lm_no_int)
# list.r.squared.full.noecoregion[[i]]<-df.r.squared.create(stratified_final_lm_full_noecoreion)
list.r.squared.ecoregion.null[[i]]<-df.r.squared.create(stratified_final_ecoregion.null)

#r-squared comparisons
compare.r.squared.region.list[[i]]<-compare_fit_2(model=stratified_final_ecoregion.null,stratified_final,metric = 'R2',main='same')
compare.r.squared.herb.list[[i]]<-compare_fit_2(model=stratified_final_lm_herb_map,stratified_final,metric = 'R2',main='same')
ompare.r.squared.map.list[[i]]<-compare_fit_2(model=stratified_final_lm_noveg,stratified_final,metric = 'R2',main='same')

#RMSE comparisons
compare.rmse.region.list[[i]]<-compare_fit_2(model=stratified_final_ecoregion.null,stratified_final,metric = 'RMSE',main='same')
compare.rmse.herb.list[[i]]<-compare_fit_2(model=stratified_final_lm_herb_map,stratified_final,metric = 'RMSE',main='diff')
ompare.rmse.map.list[[i]]<-compare_fit_2(model=stratified_final_lm_noveg,stratified_final,metric = 'RMSE',main='same')

# #look all residuals
# stratified_final$resids <-residuals(stratified_final_lm_region))
# list.residuals.region[[i]] <- stratified_final
# 
# #look at mean residuals
# mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
# 
# #make rasters
# #residual.raster<-rasterFromXYZ(mean.resids)
# #residual.plot<-plot(residual.raster)
# #list.residual.rasters[[i]] <- data.frame(mean.resids)
# 
# #variogram of mean
# # coordinates(mean.resids)= ~ x+y
# # TheVariogram_mean=variogram(resids~1, data=mean.resids)
# # variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
# # list.variograms[[i]] <- variogram.plot
# 
# #import r-squareds to list
# 
# #ecoregion
# r.square.veg<-summary(stratified_final_lm)$r.squared
# r.square.veg.df<-data.frame(r.square.veg)
# r.square.veg.df$id <- i
# list.r.square.veg[[i]] <- data.frame(r.square.veg.df)
# 
# #% herbaceous
# r.square.herb<-summary(stratified_final_lm_herb)$r.squared
# r.square.herb.df<-data.frame(r.square.herb)
# r.square.herb.df$id <- i
# list.r.square.herb[[i]] <- data.frame(r.square.herb.df)

# #noveg
# r.square.noveg<-summary(stratified_final_lm_noveg)$r.squared
# r.square.noveg.df<-data.frame(r.square.noveg)
# r.square.noveg.df$id <- i
# list.r.square.noveg[[i]] <- data.frame(r.square.noveg.df)

}

#list.r.square.noveg[50]
#list.r.square.veg[50]
#inspect variograms of mean residuals for each run
#list.variograms[1:50]

########extra#######
#r-squared: spatial
# list.spatial.r.squared.map.null[[i]]<-spatial_r_squared(stratified_final_lm_noveg)
# list.spatial.r.squared.veg.null[[i]]<-spatial_r_squared(stratified_final_lm_noveg)
# list.spatial.r.squared.ecoregion.null[[i]]<-spatial_r_squared(stratified_final_ecoregion.null)
# 
# #r-squared: temporal
# list.temporal.r.squared.map.null[[i]]<-temporal_r_squared(stratified_final_lm_noveg)
# list.temporal.r.squared.veg.null[[i]]<-temporal_r_squared(stratified_final_lm_herb_map)
# list.temporal.r.squared.ecoregion.null[[i]]<-temporal_r_squared(stratified_final_ecoregion.null)

#####


#done move on to dataframe production