library(tidyverse)
library(sf)
library(mgcv)
library(effects)
library(phyr)

convert_pop_back = function(modx, msdpop, to = FALSE){
  if(to){
    x = (log10(modx + 1) - msdpop[1]) / msdpop[2]
  } else {
    x = 10^(modx * msdpop[2] + msdpop[1]) -1
  }
  x
}

convert_temp_back = function(modx, msdpop){
  modx * msdpop[2] + msdpop[1]
}

sum_lmm_fix = function(x, m = "Temp. × Pop."){
  broom.mixed::tidy(x, "fixed") %>% 
    mutate(p = ifelse(p.value < 0.001, "***", 
                      ifelse(p.value < 0.01, "**", 
                             ifelse(p.value < 0.05, "*", ""))),
           est = paste0(round(estimate, 3), p),
           model = m) %>% 
    dplyr::select(model, term, est)
}


  dat_leaf_aggr = readRDS("data_output/dat_leaf_aggr.rds")
  envi_cells = readRDS("data_output/envi_cells.rds")
  all(unique(dat_leaf_aggr$nth_cell) %in% unique(envi_cells$nth_cell))
  n_distinct(dat_leaf_aggr$nth_cell)
  dat_leaf_aggr = left_join(dat_leaf_aggr,
                            filter(envi_cells, nth_cell %in% unique(dat_leaf_aggr$nth_cell)) %>% 
                              dplyr::select(nth_cell, pop_km2:HFI_2009) %>% distinct())
  
  all(dat_leaf_aggr$temp == dat_leaf_aggr$temp_5_10_ave_delaware)
  
  
  # try annual temp of 2011, instead of May to October? ----
  names(dat_leaf_aggr)
  dat_leaf_aggr = mutate(dat_leaf_aggr, 
                         temp_raw = temp, 
                         pop_km2_log10 = log10(pop_km2 + 1)) 
  dat_leaf_aggr = drop_na(dat_leaf_aggr, temp_ave_delaware, pop_km2_log10, HFI_2009, UHI_LST)
  # so that all models are fit to the same data
  
  range(dat_leaf_aggr$temp)
  range(dat_leaf_aggr$pop_km2)
  mean(dat_leaf_aggr$temp) # 14.87096 # May to October
  sd(dat_leaf_aggr$temp) # 2.4985
  mean(dat_leaf_aggr$temp_ave_delaware, na.rm = T) # 8.8807 annual average temp
  sd(dat_leaf_aggr$temp_ave_delaware, na.rm = T) # 2.3699
  mean(dat_leaf_aggr$pop_km2_log10) # 2.3362
  sd(dat_leaf_aggr$pop_km2_log10) # 0.5712
  mean(dat_leaf_aggr$HFI_2009) # 21.06645
  sd(dat_leaf_aggr$HFI_2009) # 10.37657
  mean(dat_leaf_aggr$UHI_LST) # 0.2497
  sd(dat_leaf_aggr$UHI_LST) # 0.5243
  
  dat_leaf_aggr[, c("temp", "temp_ave_delaware", "pop_km2_log10", "day_length", "UHI_LST", "HFI_2009")] = 
    scale(dat_leaf_aggr[, c("temp", "temp_ave_delaware", "pop_km2_log10", "day_length", "UHI_LST", "HFI_2009")])
  

hist(dat_leaf_aggr$doy_median, xlab = "Leaf senescence (day of year)", main = "")
# fine to use normal distribution (LMM)

# don't use day length
plot(dat_leaf_aggr$day_length, dat_leaf_aggr$doy_median, xlab = "Day length", ylab = "Leaf senescence (day of year)")

# annual average temperature & Human footprint index vs Pop density ----
mod_1 = lmerTest::lmer(doy_median ~ temp_ave_delaware + 
                         (1 | sp) + (1|nth_cell) + 
                         (0 + temp_ave_delaware | sp),
                       data = dat_leaf_aggr)
summary(mod_1)
AIC(mod_1)

mod_2 = lmerTest::lmer(doy_median ~ pop_km2_log10 + 
                         (1 | sp) + (1|nth_cell) + 
                         (0 + pop_km2_log10 | sp),
                       data = dat_leaf_aggr)
summary(mod_2)

mod_2_2 = lmerTest::lmer(doy_median ~ HFI_2009 + 
                           (1 | sp) + (1|nth_cell) + 
                           (0 + HFI_2009 | sp),
                         data = dat_leaf_aggr)
summary(mod_2_2)

mod_3 = lmerTest::lmer(doy_median ~ temp_ave_delaware + pop_km2_log10 + 
                         (1 | sp) + (1|nth_cell) + 
                         (0 + temp_ave_delaware | sp) +
                         (0 + pop_km2_log10 | sp),
                       data = dat_leaf_aggr)
summary(mod_3)

mod_3_2 = lmerTest::lmer(doy_median ~ temp_ave_delaware + HFI_2009 + 
                           (1 | sp) + (1|nth_cell) + 
                           (0 + temp_ave_delaware | sp) +
                           (0 + HFI_2009 | sp),
                         data = dat_leaf_aggr)
summary(mod_3_2)

mod_4 = lmerTest::lmer(doy_median ~ temp_ave_delaware * pop_km2_log10 + 
                         (1 | sp) + (1|nth_cell) + 
                         (0 + temp_ave_delaware | sp) +
                         (0 + pop_km2_log10 | sp) +
                         (0 + temp_ave_delaware:pop_km2_log10 | sp),
                       data = dat_leaf_aggr)
summary(mod_4)

mod_4_2 = lmerTest::lmer(doy_median ~ temp_ave_delaware * HFI_2009 + 
                           (1 | sp) + (1|nth_cell) + 
                           (0 + temp_ave_delaware | sp) +
                           (0 + HFI_2009 | sp) +
                           (0 + temp_ave_delaware:HFI_2009 | sp),
                         data = dat_leaf_aggr)
summary(mod_4_2)

MuMIn::r.squaredGLMM(mod_4) 

xaic = AIC(mod_1, mod_2, mod_3, mod_4) %>% as.data.frame()
xaic$delta = xaic$AIC - min(xaic$AIC)
xaic$w = as.vector(MuMIn::Weights(AIC(mod_1, mod_2, mod_3, mod_4)))

x = effects::effect("temp_ave_delaware:pop_km2_log10", mod_4)
plot(x, multiline = T)

# figure 2
as_tibble(x) %>% 
  mutate(pop_raw = convert_temp_back(pop_km2_log10, c(2.3362, 0.5172)),
         temp_raw = convert_temp_back(temp_ave_delaware, c(8.8807, 2.3699))) %>%
  ggplot(aes(x = pop_raw, y = fit, group = temp_raw, color  = temp_raw)) +
  geom_line() +
  geom_rug(data = dat_leaf_aggr, aes(x = log10(pop_km2 + 1)), alpha = 0.8, size = 0.15, inherit.aes = F) +
  labs(x = "Log10-transformed human population density (per square km)",
       y = "Leaf senescence (day of year)",
       color = "Annual Average\nTemperature (°C)") +
  colorspace::scale_color_continuous_sequential(palette = "Viridis", n_interp = 8, rev = F) +
  cowplot::theme_cowplot(font_size = 15) +
  theme(legend.position = c(0.7, .25))

# table 1
mod_all_sene_empi = bind_rows(sum_lmm_fix(mod_1, "Temp. only"),
                              sum_lmm_fix(mod_2, "Pop. only"),
                              sum_lmm_fix(mod_3, "Temp. + Pop."),
                              sum_lmm_fix(mod_4, "Temp. × Pop.")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_ave_delaware, Pop. = pop_km2_log10,
         `Temp. : Pop.` = "temp_ave_delaware:pop_km2_log10",
         `ΔAIC` = delta, `Model weight` = w)

# phylogeny 
phy = ape::read.tree("data_output/phy.tre")
# confirm results with PGLMMs

mod_4p_1 = phyr::pglmm(doy_median ~ temp_ave_delaware + pop_km2_log10 + inte +
                         (1 | sp__) + (1|nth_cell) +
                         (inte | sp__),
                       data = mutate(dat_leaf_aggr, sp = gsub(" ", "_", sp),
                                     inte = temp_ave_delaware * pop_km2_log10), 
                       bayes = T, 
                       cov_ranef = list(sp = phy))

# table S1
fixef(mod_4p_1) %>% 
  mutate(Terms = c("Intercept", "Temp.", "Pop.", "Temp.:Pop.")) %>% 
  dplyr::select(Terms, Value, lower.CI, upper.CI) %>% 
  rename(Estimate = Value, `95% credible interval (lower)` = lower.CI,
         `95% credible interval (upper)` = upper.CI) 

# human footprint vs pop density; for reviewers' comments
xaic2 = AIC(mod_1, mod_2_2, mod_3_2, mod_4_2) %>% as.data.frame()
xaic2$delta = xaic2$AIC - min(xaic2$AIC)
xaic2$w = as.vector(MuMIn::Weights(AIC(mod_1, mod_2_2, mod_3_2, mod_4_2)))
mod_all_sene_empi_HFI = bind_rows(sum_lmm_fix(mod_1, "Temp. only"),
                                  sum_lmm_fix(mod_2_2, "HFI only"),
                                  sum_lmm_fix(mod_3_2, "Temp. + HFI"),
                                  sum_lmm_fix(mod_4_2, "Temp. × HFI")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic2) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_ave_delaware, HFI = HFI_2009,
         `Temp. : HFI` = "temp_ave_delaware:HFI_2009",
         `ΔAIC` = delta, `Model weight` = w)


# UHI ----

mod_1 = lmerTest::lmer(doy_median ~ temp_ave_delaware + 
                         (1 | sp) + (1|nth_cell) + 
                         (0 + temp_ave_delaware | sp),
                       data = dat_leaf_aggr)
summary(mod_1)

sene_temp_uhi = lmerTest::lmer(doy_median ~ temp_ave_delaware + UHI_LST +
                                 (1 | sp) + (1|nth_cell) + 
                                 (0 + temp_ave_delaware | sp) +
                                 (0 + UHI_LST | sp),
                               data = dat_leaf_aggr)
summary(sene_temp_uhi)


sene_temp_x_uhi = lmerTest::lmer(doy_median ~ temp_ave_delaware * UHI_LST +
                                   (1 | sp) + (1|nth_cell) + 
                                   (0 + temp_ave_delaware | sp) +
                                   (0 + UHI_LST | sp) +
                                   (0 + temp_ave_delaware:UHI_LST | sp),
                                 data = dat_leaf_aggr)
summary(sene_temp_x_uhi)
plot(effect("temp_ave_delaware:UHI_LST", sene_temp_x_uhi), multiline = T)

sene_temp_uhi_pop = lmerTest::lmer(doy_median ~ temp_ave_delaware + UHI_LST + pop_km2_log10 +
                                     (1 | sp) + (1|nth_cell) + 
                                     (0 + temp_ave_delaware | sp) +
                                     (0 + UHI_LST | sp) +
                                     (0 + pop_km2_log10 | sp),
                                   data = dat_leaf_aggr)
summary(sene_temp_uhi_pop)

# mod_uhi_3 = lmerTest::lmer(doy_median ~ temp_ave_delaware * UHI_LST + pop_km2_log10 +
#                              (1 | sp) + (1|nth_cell) + 
#                              (0 + temp_ave_delaware | sp) +
#                              (0 + UHI_LST | sp) +
#                              (0 + temp_ave_delaware:UHI_LST | sp) +
#                              (0 + pop_km2_log10 | sp),
#                            data = dat_leaf_aggr)
# summary(mod_uhi_3)

sene_uhi_temp_x_pop = lmerTest::lmer(doy_median ~ temp_ave_delaware * pop_km2_log10 + UHI_LST +
                                       (1 | sp) + (1|nth_cell) + 
                                       (0 + temp_ave_delaware | sp) +
                                       (0 + pop_km2_log10 | sp) +
                                       (0 + temp_ave_delaware:pop_km2_log10 | sp) +
                                       (0 + UHI_LST | sp),
                                     data = dat_leaf_aggr)
summary(sene_uhi_temp_x_pop)
car::vif(sene_uhi_temp_x_pop)


xaic = AIC(sene_temp_uhi, sene_temp_x_uhi, mod_3, mod_4, sene_temp_uhi_pop, sene_uhi_temp_x_pop) %>% as.data.frame()
xaic$delta = xaic$AIC - min(xaic$AIC)
xaic$w = as.vector(MuMIn::Weights(AIC(sene_temp_uhi, sene_temp_x_uhi, mod_3, mod_4, sene_temp_uhi_pop, sene_uhi_temp_x_pop)))

# Table 3
sene_uhi_models = bind_rows(sum_lmm_fix(sene_temp_uhi, "Temp. + UHI"),
                            sum_lmm_fix(sene_temp_x_uhi, "Temp. × UHI"),
                            sum_lmm_fix(mod_3, "Temp. + Pop."),
                            sum_lmm_fix(mod_4, "Temp. × Pop."),
                            sum_lmm_fix(sene_temp_uhi_pop, "Temp. + UHI + Pop."),
                            sum_lmm_fix(sene_uhi_temp_x_pop, "Temp. × Pop. + UHI")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_ave_delaware, UHI = UHI_LST, Pop. = pop_km2_log10,
         `Temp. : UHI` = "temp_ave_delaware:UHI_LST",
         `Temp. : Pop.` = "temp_ave_delaware:pop_km2_log10",
         `ΔAIC` = delta, `Model weight` = w) %>% 
  arrange(desc(`ΔAIC`))



# growing season duration ----
leaf_both2 = readRDS("data_output/leaf_both2.rds")
envi_cells = readRDS("data_output/envi_cells.rds")
all(unique(leaf_both2$nth_cell) %in% unique(envi_cells$nth_cell))
n_distinct(leaf_both2$nth_cell)
leaf_both2 = left_join(leaf_both2,
                       filter(envi_cells, nth_cell %in% unique(leaf_both2$nth_cell)) %>% 
                         dplyr::select(nth_cell, HFI_2009, UHI_LST) %>% distinct())

leaf_both2 = mutate(leaf_both2, pop_log10 = log10(pop_km2 + 1))
leaf_both2 = na.omit(leaf_both2)
mean(leaf_both2$temp_annual) # 9.2706
sd(leaf_both2$temp_annual) # 2.8553
mean(leaf_both2$pop_log10) # 2.2248
sd(leaf_both2$pop_log10) # 0.7249
summary(leaf_both2$pop_log10)
n_distinct(leaf_both2$sp) # 54
leaf_both2[, c("pop_log10", "temp_annual", "HFI_2009", "UHI_LST")] = 
  scale(leaf_both2[, c("pop_log10", "temp_annual", "HFI_2009", "UHI_LST")])

# original version
m1_gsl = lmerTest::lmer(duration ~ temp_annual + (1|sp) + (1|nth_cell) +
                          (0 + temp_annual|sp), data = leaf_both2)
m2_gsl = lmerTest::lmer(duration ~ pop_log10 + (1|sp) + (1|nth_cell) +
                          (0 + pop_log10|sp), data = leaf_both2)
m3_gsl = lmerTest::lmer(duration ~ temp_annual + pop_log10 + (1|sp) + (1|nth_cell) +
                          (0 + temp_annual|sp) + (0 + pop_log10|sp), data = leaf_both2)
m4_gsl = lmerTest::lmer(duration ~ temp_annual * pop_log10 + (1|sp) + (1|nth_cell) + 
                          (0 + temp_annual|sp) + # sigular problem
                          # (0 + pop_log10|sp) +
                          (0 + temp_annual:pop_log10|sp), data = leaf_both2)
summary(m4)
MuMIn::Weights(AIC(m1_gsl, m2_gsl, m3_gsl, m4_gsl))

xaic = AIC(m1_gsl, m2_gsl, m3_gsl, m4_gsl) %>% as.data.frame()
xaic$delta = xaic$AIC - min(xaic$AIC)
xaic$w = as.vector(MuMIn::Weights(AIC(m1_gsl, m2_gsl, m3_gsl, m4_gsl)))

# table 3
mod_all_dur_empi = bind_rows(sum_lmm_fix(m1_gsl, "Temp. only"),
                             sum_lmm_fix(m2_gsl, "Pop. only"),
                             sum_lmm_fix(m3_gsl, "Temp. + Pop."),
                             sum_lmm_fix(m4_gsl, "Temp. × Pop.")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_annual, Pop. = pop_log10,
         `Temp. : Pop.` = "temp_annual:pop_log10",
         `ΔAIC` = delta, `Model weight` = w)

# using HFI instead of human pop density, to solve reviewers' comments

m1 = lmerTest::lmer(duration ~ temp_annual + (1|sp) + (1|nth_cell) +
                      (0 + temp_annual|sp), data = leaf_both2)
m2 = lmerTest::lmer(duration ~ HFI_2009 + (1|sp) + (1|nth_cell) +
                      (0 + HFI_2009|sp), data = leaf_both2)
m3 = lmerTest::lmer(duration ~ temp_annual + HFI_2009 + (1|sp) + (1|nth_cell) +
                      (0 + temp_annual|sp) + (0 + HFI_2009|sp), data = leaf_both2)
m4 = lmerTest::lmer(duration ~ temp_annual * HFI_2009 + (1|sp) + (1|nth_cell) + 
                      (0 + temp_annual|sp) + # sigular problem
                      # (0 + HFI_2009|sp) +
                      (0 + temp_annual:HFI_2009|sp), data = leaf_both2)

# m1 = lmerTest::lmer(duration ~ temp_annual + (1|sp) + (1|nth_cell), data = leaf_both2)
# m2 = lmerTest::lmer(duration ~ HFI_2009 + (1|sp) + (1|nth_cell), data = leaf_both2)
# m3 = lmerTest::lmer(duration ~ temp_annual + HFI_2009 + (1|sp) + (1|nth_cell), data = leaf_both2)
# m4 = lmerTest::lmer(duration ~ temp_annual * HFI_2009 + (1|sp) + (1|nth_cell) + 
#             (0 + temp_annual:HFI_2009|sp), data = leaf_both2)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
MuMIn::Weights(AIC(m1, m2, m3, m4))
xaic = AIC(m1, m2, m3, m4) %>% as.data.frame()
xaic$delta = xaic$AIC - min(xaic$AIC)
xaic$w = as.vector(MuMIn::Weights(AIC(m1, m2, m3, m4)))

mod_all_dur_empi_HFI = bind_rows(sum_lmm_fix(m1, "Temp. only"),
                                 sum_lmm_fix(m2, "HFI only"),
                                 sum_lmm_fix(m3, "Temp. + HFI"),
                                 sum_lmm_fix(m4, "Temp. × HFI")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_annual, HFI = HFI_2009,
         `Temp. : HFI` = "temp_annual:HFI_2009",
         `ΔAIC` = delta, `Model weight` = w)

saveRDS(list(dur_original = mod_all_dur_empi, dur_HFI = mod_all_dur_empi_HFI),
        "data_output/dur_pop_vs_HFI_revision.rds")

readRDS("data_output/dur_pop_vs_HFI_revision.rds")

# table S2
m2p = phyr::pglmm(duration ~ temp_annual + pop_log10 + inte +
                    (1|sp__) + (1|nth_cell) + (inte | sp__), 
                  data = as.data.frame(mutate(leaf_both2, sp = gsub(" ", "_", sp),
                                              inte = temp_annual * pop_log10)), 
                  bayes = T, verbose = T,
                  cov_ranef = list(sp = phy))
# Linear mixed model fit by restricted maximum likelihood
# 
# Call:duration ~ temp_annual + pop_log10 + inte
# 
# logLik    AIC    BIC 
# -10678  21376  21423 
# 
# Random effects:
#   Variance Std.Dev
# 1|sp        70.05378  8.3698
# 1|sp__     173.90583 13.1873
# 1|nth_cell 176.30571 13.2780
# inte|sp      7.01322  2.6482
# inte|sp__    0.03014  0.1736
# residual   171.53611 13.0972
# 
# Fixed effects:
#                   Value Std.Error  Zscore    Pvalue    
#   (Intercept) 169.96441  22.47188  7.5634 3.926e-14 ***
#   temp_annual   9.70133   0.62919 15.4187 < 2.2e-16 ***
#   pop_log10     5.12635   0.62602  8.1887 2.640e-16 ***
#   inte         -2.75374   0.75024 -3.6705 0.0002421 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fixef(m2p) %>% 
  mutate(Terms = c("Intercept", "Temp.", "Pop.", "Temp.:Pop.")) %>% 
  dplyr::select(Terms, Value, lower.CI, upper.CI) %>% 
  rename(Estimate = Value, `95% credible interval (lower)` = lower.CI,
         `95% credible interval (upper)` = upper.CI)


# UHI of growing season
gsl_temp_uhi = lmerTest::lmer(duration ~ temp_annual + UHI_LST + (1|sp) + (1|nth_cell) + 
                                (0 + temp_annual|sp) + (0 + UHI_LST|sp),
                              data = leaf_both2)
summary(gsl_temp_uhi)

gsl_temp_x_uhi = lmerTest::lmer(duration ~ temp_annual * UHI_LST + (1|sp) + (1|nth_cell) + 
                                  (0 + temp_annual|sp) + (0 + UHI_LST|sp) + (0 + temp_annual:UHI_LST | sp),
                                data = leaf_both2)
summary(gsl_temp_x_uhi)

gsl_temp_uhi_pop = lmerTest::lmer(duration ~ temp_annual + UHI_LST + pop_log10 + (1|sp) + (1|nth_cell) + 
                                    (0 + temp_annual|sp) + (0 + UHI_LST|sp) + (0 + pop_log10|sp),
                                  data = leaf_both2)
summary(gsl_temp_uhi_pop)

gsl_uhi_temp_x_pop = lmerTest::lmer(duration ~ temp_annual * pop_log10 + UHI_LST + (1|sp) + (1|nth_cell) + 
                                      (0 + temp_annual|sp) + (0 + UHI_LST|sp) + (0 + pop_log10|sp) +
                                      (0 + temp_annual:pop_log10 | sp),
                                    data = leaf_both2)
summary(gsl_uhi_temp_x_pop)

m6_uhi = lmerTest::lmer(duration ~ temp_annual * pop_log10 + temp_annual * UHI_LST + (1|sp) + (1|nth_cell) + 
                          # (0 + temp_annual|sp) + (0 + UHI_LST|sp) + (0 + pop_log10|sp) + # 
                          (0 + temp_annual:pop_log10 | sp) + (0 + temp_annual:UHI_LST | sp),
                        data = leaf_both2)
summary(m6_uhi)

m3_gsl
m4_gsl

xaic = AIC(gsl_temp_uhi, gsl_temp_x_uhi, m3_gsl, m4_gsl, gsl_temp_uhi_pop, gsl_uhi_temp_x_pop) %>% as.data.frame()
xaic$delta = xaic$AIC - min(xaic$AIC)
xaic$w = as.vector(MuMIn::Weights(AIC(gsl_temp_uhi, gsl_temp_x_uhi, m3_gsl, m4_gsl, gsl_temp_uhi_pop, gsl_uhi_temp_x_pop)))

# Table 3
gsl_uhi_models = bind_rows(sum_lmm_fix(gsl_temp_uhi, "Temp. + UHI"),
                           sum_lmm_fix(gsl_temp_x_uhi, "Temp. × UHI"),
                           sum_lmm_fix(m3_gsl, "Temp. + Pop."),
                           sum_lmm_fix(m4_gsl, "Temp. × Pop."),
                           sum_lmm_fix(gsl_temp_uhi_pop, "Temp. + UHI + Pop."),
                           sum_lmm_fix(gsl_uhi_temp_x_pop, "Temp. × Pop. + UHI")) %>% 
  pivot_wider(id_cols = "model", names_from = "term", values_from = est) %>% 
  bind_cols(xaic) %>% 
  dplyr::select(-df) %>% 
  rename(Intercept = "(Intercept)",
         Temp. = temp_annual, UHI = UHI_LST, Pop. = pop_log10,
         `Temp. : UHI` = "temp_annual:UHI_LST",
         `Temp. : Pop.` = "temp_annual:pop_log10",
         `ΔAIC` = delta, `Model weight` = w) %>% 
  arrange(desc(`ΔAIC`))
