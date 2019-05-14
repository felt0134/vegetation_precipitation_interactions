
list.coefficients.final<-list()

for(i in 1:1000)
  {
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
 
stratified_final<-merge(test.strat, rangeland_npp_covariates_deviations_1,by=c('x','y'))
print(stratified_final)

stratified_final_lm<-lm(npp.x~mm.y*region.x*mm.dev
                        ,stratified_final)

newcoef1 <- stratified_final_lm$coefficients 
df<-data.frame(newcoef1)
list.coefficients.final[[i]] <- data.frame(df)

}

library(reshape2)
coefficients_df_wide<-data.frame(list.coefficients.final)
coefficients_id <- cbind(rownames(coefficients_df_wide), data.frame(coefficients_df_wide, row.names=NULL))
head(coefficients_id)
coefficients_long<-melt(coefficients_id, id.vars=c("rownames(coefficients_df_wide)"))
summary(coefficients_long)
#find intercept distribution
intercept<-subset(coefficients_long,rownames(coefficients_df_wide) =='(Intercept)')
hist(intercept$value)
median(intercept$value)

#spatial slope
spatial_slope<-subset(coefficients_long,rownames(coefficients_df_wide) =='mm.y')
hist(spatial_slope$value)
median(spatial_slope$value)
