# # download data -----
# # http://phenology.cr.usgs.gov
# # https://phenology.cr.usgs.gov/get_data_Aqua_C6_250e.php
# # https://phenology.cr.usgs.gov/viewer/
# # start of season - time
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/west/SOST/C6_eMODIS_West_SOST"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_West_SOST", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
# }
# 
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/east/SOST/C6_eMODIS_East_SOST"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_East_SOST", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
#   file.remove(files_save[i])
# }
# 
# # end of season - time
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/east/EOST/C6_eMODIS_East_EOST"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_East_EOST", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
#   file.remove(files_save[i])
# }
# 
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/west/EOST/C6_eMODIS_West_EOST"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_West_EOST", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
#   file.remove(files_save[i])
# }
# 
# # duration of season - time
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/east/DUR/C6_eMODIS_East_DUR"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_East_DUR", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
#   file.remove(files_save[i])
# }
# 
# base_url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/phenology/downloads/rspzip/eMODIS/west/DUR/C6_eMODIS_West_DUR"
# yrs = 2003:2018
# urls = paste0(base_url, yrs, ".zip")
# files_save = paste0("data/modis/C6_eMODIS_West_DUR", yrs, ".zip")
# for (i in 1:length(urls)){
#   download.file(urls[i], files_save[i])
#   unzip(files_save[i], exdir = stringr::str_sub(files_save[i], 1, -5))
#   file.remove(files_save[i])
# }
# 
# # system.time({
# #   x1 = raster::aggregate(x, fact = 4)
# #   writeRaster(x1, "test.tif")
# # }) # about 52 seconds
# x1[x1 == -1000] = NA
# x1[x1 == 1000] = NA
# plot(x1)
# system.time({x[x == -1000] = NA})

library(tidyverse)
library(sf)
library(raster)
library(rnaturalearth)
library(rasterVis)
library(mgcv)
library(effects)


# other data ----
popu_usa_m5_2010_agg = raster("data/popu_usa_m5_2010_agg.tif")
ecoregion = readRDS("data/ecoregion.rds")
plot(ecoregion["NA_L1NAME"])
new_proj = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
east_temp_forest = filter(ecoregion, NA_L1NAME == "EASTERN TEMPERATE FORESTS")
plot(east_temp_forest["NA_L1NAME"])
east_temp_forest = as(east_temp_forest, "Spatial")

plot(popu_usa_m5_2010_agg)
popu_usa_m5_2010_agg = projectRaster(popu_usa_m5_2010_agg, crs = new_proj)
east_pop = crop(popu_usa_m5_2010_agg, east_temp_forest)
east_pop = mask(east_pop, east_temp_forest)
plot(east_pop)

temp_usa_ave_delaware = raster("data/temp_usa_ave_delaware.tif")
temp_usa_ave_delaware = projectRaster(temp_usa_ave_delaware, crs = new_proj)
east_temp = crop(temp_usa_ave_delaware, east_temp_forest)
east_temp = mask(east_temp, east_temp_forest)
plot(east_temp)

# explore data ----
# dur_east_2017 = raster("data/modis/C6_eMODIS_East_DUR2017/dur2017e/w001001.adf")
# # 250m by 250m
# # The unit of the DUR is in days and valid values range from 1 to 365.
# # In the DUR data layer, a cell value of 1000 represents water bodies and a cell
# # value of -1000 represents the area where a DUR could not be detected due to
# # insufficient change in time-series NDVI or due to lack of sufficient input data
# projection(dur_east_2017) # seems wrong
# # based on the meta data, it should be new_proj
# projection(dur_east_2017) = new_proj
#
# projection(dur_east_2017)  == projection(east_pop)
# projection(dur_east_2017)  == projection(east_temp)
# projection(dur_east_2017)  == projection(east_temp_forest)
#
# plot(dur_east_2017)
# dur_east_2017[(dur_east_2017 < 1) | (dur_east_2017 > 365)] = NA
# dur_east_2017 = aggregate(dur_east_2017, fact = 10, fun = mean, na.rm = T) # 2500m
# dur_east_2017 = crop(dur_east_2017, east_temp_forest)
# dur_east_2017 = mask(dur_east_2017, east_temp_forest)
# dur_east_2017_2 = align_rasters(dur_east_2017, east_pop)
# plot(dur_east_2017)
# plot(dur_east_2017_2)

# for(yr in 2003:2018){
#   cat("yr: ", yr, "\n")
#   folder = paste0("data/modis/C6_eMODIS_East_SOST", yr, "/sost_", yr, "e/w001001.adf")
#   sost_east = raster(folder)
#   projection(sost_east) = new_proj
#   sost_east[(sost_east < -150) | (sost_east > 365)] = NA
#   sost_east = aggregate(sost_east, fact = 10, fun = median, na.rm = T) # 2500m
#   sost_east = crop(sost_east, east_temp_forest)
#   sost_east = mask(sost_east, east_temp_forest)
#   sost_east = align_rasters(sost_east, east_pop)
#   writeRaster(sost_east, paste0("data_output/modis/sost_east_", yr, ".tiff"))
# }
#
# for(yr in 2003:2018){
#   cat("yr: ", yr, "\n")
#   folder = paste0("data/modis/C6_eMODIS_East_EOST", yr, "/eost_", yr, "e/w001001.adf")
#   eost_east = raster(folder)
#   projection(eost_east) = new_proj
#   eost_east[(eost_east < 1) | (eost_east > 450)] = NA
#   eost_east = aggregate(eost_east, fact = 10, fun = mean, na.rm = T) # 2500m
#   eost_east = crop(eost_east, east_temp_forest)
#   eost_east = mask(eost_east, east_temp_forest)
#   eost_east = align_rasters(eost_east, east_pop)
#   writeRaster(eost_east, paste0("data_output/modis/eost_east_", yr, ".tiff"))
# }
#
# # read raster, aggregate, then crop to east temp forest
# for(yr in 2003:2018){
#   cat("yr: ", yr, "\n")
#   folder = paste0("data/modis/C6_eMODIS_East_DUR", yr, "/dur_", yr, "e/w001001.adf")
#   dur_east = raster(folder)
#   projection(dur_east) = new_proj
#   dur_east[(dur_east < 1) | (dur_east > 365)] = NA
#   dur_east = aggregate(dur_east, fact = 10, fun = mean, na.rm = T) # 2500m
#   dur_east = align_rasters(dur_east, east_pop)
#   dur_east = crop(dur_east, east_temp_forest)
#   dur_east = mask(dur_east, east_temp_forest)
#   writeRaster(dur_east, paste0("data_output/modis/dur_east_", yr, ".tiff"))
# }
# levelplot(dur_east, margin = F)
# levelplot(dur_east2, margin = F)
#
# pdf("figures/duration_east_raw.pdf", onefile = T, width = 8, height = 8)
# for(yr in 2003:2018){
#   cat("yr: ", yr, "\n")
#   folder = paste0("data/modis/C6_eMODIS_East_DUR", yr, "/dur_", yr, "e/w001001.adf")
#   dur_east = raster(folder)
#   projection(dur_east) = new_proj
#   dur_east[(dur_east < 1) | (dur_east > 365)] = NA
#   dur_east = aggregate(dur_east, fact = 4, fun = mean, na.rm = T) # 1km
#   rasterVis::levelplot(dur_east, margin = F, zscaleLog = F, main = paste0("Year: ", yr))
#   writeRaster(dur_east, paste0("data_output/modis/dur_east_", yr, "_1km.tiff"))
# }
# dev.off()
#
# pdf("figures/duration_east_raw.pdf", onefile = T, width = 8, height = 8)
# for(yr in 2003:2018){
#   cat("yr: ", yr, "\n")
#   folder = paste0("data_output/modis/dur_east_", yr, "_1km.tif")
#   dur_east = raster(folder)
#   print(rasterVis::levelplot(dur_east, margin = F, zscaleLog = F, main = paste0("Year: ", yr)))
# }
# dev.off()
#
# plot(dur_east)
# pdf("figures/duration_east.pdf", onefile = T, width = 7, height = 7)
# for(i in 2003:2018){
#   plot(raster(paste0("data_output/modis/dur_east_", i, ".tif")))
# }
# dev.off()

dur_list = vector("list", 10)
names(dur_list) = paste0("yr_", 2009:2018)
for(i in 2009:2018){
  dur_list[[i - 2008]] = raster(paste0("data/modis/dur_east_", i, ".tif"))
}
dur_stack = stack(dur_list)
dur_stack_ave = mean(dur_stack, na.rm = T)
plot(dur_stack_ave)
plot(east_temp)

# range(getValues(east_temp), na.rm = T)
# qts = seq(2.39, 23.90, length.out = 4)[2:3]
# qts = quantile(getValues(east_temp), c(0.33, 0.66), na.rm = T)

qts = 14.71 # based on ground data
qts = 17.3 # based on https://www.pnas.org/content/117/8/4228
# qts = 20 # based on later plots

regions_14.71 = east_temp
regions_14.71[regions_14.71 <= 14.71] = 1
regions_14.71[regions_14.71 > 14.71] = 2
regions_17.3 = east_temp
regions_17.3[regions_17.3 <= 17.3] = 1
regions_17.3[regions_17.3 > 17.3] = 2
regions_20 = east_temp
regions_20[regions_20 <= 20] = 1
regions_20[regions_20 > 20] = 2
lu = ratify(regions_14.71)
rat <- levels(lu)[[1]]
rat$Region <- c("Cold","Warm")
levels(lu) <- rat
p_14.71 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), scales = list(draw = FALSE),
                               main = "Annual average temperature threshold = 14.71 °C")
p_14.71_2 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), 
                                scales = list(draw = FALSE), 
                                colorkey = list(space = "bottom", width = 0.5, height = 0.5),
                                margin = list(axis = F))
p_14.71_2 = update(p_14.71_2, par.settings = list(axis.line = list(col = "transparent")))

lu = ratify(regions_17.3)
rat <- levels(lu)[[1]]
rat$Region <- c("Cold","Warm")
levels(lu) <- rat
p_17.3 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), scales = list(draw = FALSE),
                               main = "Annual average temperature threshold = 17.3 °C")
p_17.3_2 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), 
                                scales = list(draw = FALSE), 
                                colorkey = list(space = "bottom", width = 0.5, height = 0.5),
                                margin = list(axis = F))
p_17.3_2 = update(p_17.3_2, par.settings = list(axis.line = list(col = "transparent")))
lu = ratify(regions_20)
rat <- levels(lu)[[1]]
rat$Region <- c("Cold","Warm")
levels(lu) <- rat
p_20 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), scales = list(draw = FALSE),
                               main = "Annual average temperature threshold = 20 °C")
p_20_2 = rasterVis::levelplot(lu, att = "Region", par.settings=BuRdTheme(), 
                                scales = list(draw = FALSE), 
                                colorkey = list(space = "bottom", width = 0.5, height = 0.5),
                                margin = list(axis = F))
p_20_2 = update(p_20_2, par.settings = list(axis.line = list(col = "transparent")))

pdf("figures/cold_warm_regions_3_cutoffs.pdf", width = 15, height = 5)
print(gridExtra::grid.arrange(p_14.71, p_17.3, p_20, nrow = 1))
dev.off()

png("figures/cold_warm_regions_3_cutoffs.png", width = 15, height = 5, units = "in", res = 300)
print(gridExtra::grid.arrange(p_14.71, p_17.3, p_20, nrow = 1))
dev.off()

pdf("figures/cold_warm_regions_17.3c.pdf", width = 7, height = 7)
p_17.3
dev.off()

# # land use data
# gdalUtils::gdalwarp(srcfile = "/Users/dli/Documents/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img",
#                     dstfile = "land_use3.tif", t_srs = new_proj,
#                     r = 'mode',
#                     multi = TRUE, tr = c(900, 900), output_Raster = TRUE)
#
# land_use = raster("land_use3.tif")
# plot(land_use)
#
# land_use = crop(land_use, east_temp_forest)
# land_use = mask(land_use, east_temp_forest)
# writeRaster(land_use, "land_use4.tif")
# align??
land_use = raster("data/land_use4.tif")
land_use2 = aggregate(land_use, fact = 10, fun = modal, na.rm = T)
land_use2 = resample(land_use, east_pop, method = "ngb")
plot(land_use2)
sort(table(getValues(land_use2)), decreasing = T)
land_use2[getValues(land_use2) %in% c(0, 82, 81, 11, 12, 31, 42)] = NA
# values(land_use2) = as.factor(values(land_use2))
lu = ratify(land_use2)
levels(lu)
rat <- levels(lu)[[1]]
rat$landcover <- c("Developed open space",
                   "Developed low intensity",
                   "Developed mid intensity",
                   "Developed high intensity",
                   "Deciduous forest",
                   "Mixed forest",
                   "Shrub scrub",
                   "Grassland",
                   "Woody forest/wetland",
                   "Herbaceous wetland")
rat$code <- rat$ID
levels(lu) <- rat
rasterVis::levelplot(lu, att = "landcover")
pdf("figures/land_cover_used.pdf", width = 8, height = 6)
print(rasterVis::levelplot(lu, att = "landcover", margin = F, scales = list(draw = FALSE),
                           par.settings = viridisTheme()))
dev.off()
png("figures/land_cover_used.png", width = 8, height = 6, units = "in", res = 300)
print(rasterVis::levelplot(lu, att = "landcover", margin = F, scales = list(draw = FALSE),
                           par.settings = viridisTheme()))
dev.off()

dur_stack_ave2 = crop(dur_stack_ave, lu)
dur_stack_ave2 = mask(dur_stack_ave2, lu)

pdf("figures/avergage_dur_2009_2018.pdf", width = 8, height = 8)
print(levelplot(dur_stack_ave, margin = F, scales = list(draw = FALSE)))
dev.off()
png("figures/avergage_dur_2009_2018.png", width = 8, height = 8, units = "in", res = 300)
print(levelplot(dur_stack_ave, margin = F, scales = list(draw = FALSE)))
dev.off()

pdf("figures/avergage_dur_2009_2018_no_cropland.pdf", width = 8, height = 8)
print(levelplot(dur_stack_ave2, margin = F, scales = list(draw = FALSE)))
dev.off()
png("figures/avergage_dur_2009_2018_no_cropland.png", width = 8, height = 8, units = "in", res = 300)
print(levelplot(dur_stack_ave2, margin = F, scales = list(draw = FALSE)))
dev.off()

# cowplot::plot_grid(levelplot(dur_stack_ave2, margin = F, scales = list(draw = FALSE)),
#                    rasterVis::levelplot(lu, att = "landcover", margin = F, scales = list(draw = FALSE),
#                                         par.settings = viridisTheme())
#                    )

# dur_stack_ave[is.na(getValues(land_use2))] = NA
levelplot(dur_stack)

dat = tibble(
  g_14.71 = getValues(regions_14.71),
  g_17.3 = getValues(regions_17.3),
  g_20 = getValues(regions_20),
  temp = getValues(east_temp),
  pop = getValues(east_pop),
  dur = getValues(dur_stack_ave),
  land_cover = getValues(land_use2)
) %>%
  na.omit() %>%
  mutate(g_14.71 = recode(g_14.71, "1" = "Cold", "2" = "Warm"),
         g_14.71 = factor(g_14.71, levels = c("Cold", "Warm")),
         g_17.3 = recode(g_17.3, "1" = "Cold", "2" = "Warm"),
         g_17.3 = factor(g_17.3, levels = c("Cold", "Warm")),
         g_20 = recode(g_20, "1" = "Cold", "2" = "Warm"),
         g_20 = factor(g_20, levels = c("Cold", "Warm")),
         cell = paste0("cell_", 1:n()))
dat = mutate(dat, pop_log = log10(pop + 1))

ggplot(dat, aes(x = g_17.3, y = dur)) + geom_boxplot()

# 0: ocean
# 11: open water
# 12: Perenniel Ice/Snow
# 31: Barren land
# 42: Evergreen forest
# 81: Pasture/Hay-area planted for livestock
# 82: Cultivated crops

dat2 = filter(dat, !land_cover %in% c(0, 82, 81, 11, 12, 31, 42))
mean(dat2$temp) # 13.488
sd(dat2$temp) # 3.96
dat2[, c("temp", "pop_log")] = scale(dat2[, c("temp", "pop_log")])
dat2$land_cover = recode(dat2$land_cover,
                         "21" = "Developed open space",
                         "22" = "Developed low intensity",
                         "23" = "Developed mid intensity",
                         "24" = "Developed high intensity",
                         "41" = "Deciduous forest",
                         "43" = "Mixed forest",
                         "52" = "Shrub scrub",
                         "71" = "Grassland",
                         "90" = "Woody forest/wetland",
                         "95" = "Herbaceous wetland"
                         ) %>% 
  factor(levels = c("Developed open space",
                    "Developed low intensity",
                    "Developed mid intensity",
                    "Developed high intensity",
                    "Deciduous forest",
                    "Mixed forest",
                    "Shrub scrub",
                    "Grassland",
                    "Woody forest/wetland",
                    "Herbaceous wetland"))

ggplot(dat2, aes(x = temp, y = dur)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x))

ggplot(dat2, aes(x = temp, y = dur, color = g)) + 
  geom_point(alpha = 0.3) +
  facet_wrap(~land_cover)



ggplot(dat2, aes(x = pop_log, y = dur, color = g)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~g)

group_by(dat2, g) %>% 
  do(broom::tidy(lm(dur ~  temp + I(temp^2) + pop_log, data = .)))

library(mgcv)
bg = gam(dur ~ s(temp), data = dat2)
summary(bg)
plot(bg, pages = 1)
dat2$resid = resid(bg)

# xx = lm(resid ~ g * pop_log, data = dat2)
# summary(xx)
# plot(xx)
# library(effects)
# xe = effect("g:pop_log", mod = xx)
# plot(xe, multiline = T)

# xx0 = lmerTest::lmer(resid ~ temp + pop_log + (1|land_cover) +
#                        (0 + temp | land_cover) +
#                        (0 + pop_log | land_cover),
#                      data = dat2, REML = F)
# 
# xx1 = lmerTest::lmer(resid ~ temp * pop_log + (1|land_cover) +
#                        (0 + temp | land_cover) +
#                        (0 + pop_log | land_cover) + 
#                        (0 + temp:pop_log | land_cover),
#                      data = dat2, REML = F)
# summary(xx1)
# broom.mixed::tidy(xx1, "fixed") %>% 
#   saveRDS("data_output/temp_pop_resid_MODIS.rds")
# # Estimate Std. Error      df t value Pr(>|t|)    
# # (Intercept)   -0.4150     0.4472  6.2407  -0.928 0.387939    
# # temp          -0.1769     0.4766 10.0182  -0.371 0.718257    
# # pop_log        2.1045     0.3476  7.3129   6.054 0.000434 ***
# #   temp:pop_log  -0.5383     0.1764  6.0316  -3.052 0.022301 *  
# 
# anova(xx0, xx1)
# MuMIn::Weights(AIC(xx0, xx1)) # 0.002 0.998
# 
# xx2 = lmerTest::lmer(resid ~ g_17.3 * pop_log + (1|land_cover) + (0 + g_17.3:pop_log | land_cover),
#            data = dat2)
# summary(xx2)
# library(effects)
# xe2 = effect("g:pop_log", mod = xx2)
# as_tibble(xe2) %>% arrange(g)
# plot(xe2, multiline = T)
# 
# broom.mixed::tidy(xx2, "fixed") %>% 
#   dplyr::select(-effect) %>% 
#   saveRDS("data_output/table_dur_region_pop_MODIS.rds")

if(!file.exists("data_output/dur_region_pop_MODIS_3_cutoffs.rds")){
  m_summ = pivot_longer(dat2, cols = starts_with("g_"), names_to = "cutoff", values_to = "Region") %>% 
    group_by(cutoff) %>% 
    do(broom.mixed::tidy(lmerTest::lmer(resid ~ Region * pop_log + (1|land_cover) + 
                                          (0 + Region:pop_log | land_cover), 
                                        data = .)))
  saveRDS(m_summ, "data_output/dur_region_pop_MODIS_3_cutoffs.rds")
} else {
  m_summ = readRDS("data_output/dur_region_pop_MODIS_3_cutoffs.rds")
}

filter(m_summ, effect == "fixed")

## qts = 14.71 based on ground data
# term          estimate std.error statistic       df  p.value
# <chr>            <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#   1 (Intercept)      -1.14     0.590     -1.93     6.69 0.0965  
# 2 gWarm             1.45     0.152      9.56 10513.   0       
# 3 pop_log           2.86     0.460      6.21     7.86 0.000276
# 4 gWarm:pop_log    -2.06     0.674     -3.05     8.52 0.0147  

## qts = 20 
# term          estimate std.error statistic      df p.value
# <chr>            <dbl>     <dbl>     <dbl>   <dbl>   <dbl>
#   1 (Intercept)      -1.57     0.676     -2.32    6.89 0.0540 
# 2 gWarm             3.10     0.314      9.85 1859.   0      
# 3 pop_log           2.70     0.536      5.03    6.53 0.00186
# 4 gWarm:pop_log    -3.28     0.635     -5.17    6.85 0.00139

a = mean(log10(dat2$pop + 1)) # 1.500916
s = sd(log10(dat2$pop + 1)) # 0.6084806

# cold; warm = 0: y = -2.346 + 2.806 * pop_log = -4.909 + 1.708 * pop_log10_unscaled
# warm: cold = 0, y = -2.346 +3.757 + (2.806 - 3.049) * pop_log = 1.411 - 0.243 * pop_log = 1.633 - 0.148 * pop_log10_unscaled
# pop_log10 = log10(pop+1)
# pop_log = (pop_log10 - mean(pop_log10)) / sd(pop_log10)


pp1 = ggplot(dplyr::select(dat2, pop_log, resid, land_cover, g_17.3), 
             aes(x = pop_log, y = resid, color = land_cover)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = F, size = 0.8) +
  colorspace::scale_color_discrete_sequential("Viridis", rev = F) +
  # scale_color_manual(values = c("#721F81FF", "#F1605DFF")) +
  facet_wrap(~g_17.3, scales = "free_x")

pp2 = pp1 + geom_abline(data = 
                    tibble(g_17.3 = c("Cold", "Warm"),
                           # a = c(-1.14, 0.31), # 14.71 c
                           # b = c(2.86, 0.80)
                           a = c(-2.22, 1.5), # 17.3 c
                           b = c(2.80, -0.26)
                           # a = c(-1.57, 1.53), # 20 c
                           # b = c(2.70, -0.58)
                           ),
                  aes(intercept = a, slope = b), size = 1.2, color = "darkorange1"
) +
  labs(x = "Scaled log10-transformed human population density",
       y = "Residuals of growing season durations vs. temperature",
       color = "Land Cover") +
  # theme(legend.justification = "bottom")
  theme(plot.margin = unit(c(5.5, 170, 5.5, 5.5), "pt"),
        legend.position = c(1.16, 0.25))

pp3 = cowplot::ggdraw(pp2) +
  cowplot::draw_plot(p_17.3_2, x = 0.63, y = 0.53, width = 0.5, height = 0.5)

ggsave("figures/predict_dur_pop_MODIS_17.3.pdf", plot = pp3, width = 10, height = 6)
ggsave("figures/predict_dur_pop_MODIS_17.3.png", plot = pp3, width = 10, height = 6)

# same plot with 14.71 cutoff 
pp1 = ggplot(dplyr::select(dat2, pop_log, resid, land_cover, g_14.71), 
             aes(x = pop_log, y = resid, color = land_cover)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = F, size = 0.8) +
  colorspace::scale_color_discrete_sequential("Viridis", rev = F) +
  # scale_color_manual(values = c("#721F81FF", "#F1605DFF")) +
  facet_wrap(~g_14.71, scales = "free_x")

pp2 = pp1 + geom_abline(data = 
                          tibble(g_14.71 = c("Cold", "Warm"),
                                 a = c(-1.14, 0.31), # 14.71 c
                                 b = c(2.86, 0.80)
                                 # a = c(-2.22, 1.5), # 17.3 c
                                 # b = c(2.80, -0.26)
                                 # a = c(-1.57, 1.53), # 20 c
                                 # b = c(2.70, -0.58)
                          ),
                        aes(intercept = a, slope = b), size = 1.2, color = "darkorange1"
) +
  labs(x = "Scaled log10-transformed human population density",
       y = "Residuals of growing season durations vs. temperature",
       color = "Land Cover") +
  # theme(legend.justification = "bottom")
  theme(plot.margin = unit(c(5.5, 170, 5.5, 5.5), "pt"),
        legend.position = c(1.16, 0.25))

pp3 = cowplot::ggdraw(pp2) +
  cowplot::draw_plot(p_14.71_2, x = 0.63, y = 0.53, width = 0.5, height = 0.5)

ggsave("figures/predict_dur_pop_MODIS_14.71.pdf", plot = pp3, width = 10, height = 6)
ggsave("figures/predict_dur_pop_MODIS_14.71.png", plot = pp3, width = 10, height = 6)

# same plot with 20 cutoff 
pp1 = ggplot(dplyr::select(dat2, pop_log, resid, land_cover, g_20), 
             aes(x = pop_log, y = resid, color = land_cover)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = F, size = 0.8) +
  colorspace::scale_color_discrete_sequential("Viridis", rev = F) +
  # scale_color_manual(values = c("#721F81FF", "#F1605DFF")) +
  facet_wrap(~g_20, scales = "free_x")

pp2 = pp1 + geom_abline(data = 
                          tibble(g_20 = c("Cold", "Warm"),
                                 # a = c(-1.14, 0.31), # 14.71 c
                                 # b = c(2.86, 0.80)
                                 # a = c(-2.22, 1.5), # 17.3 c
                                 # b = c(2.80, -0.26)
                                 a = c(-1.57, 1.53), # 20 c
                                 b = c(2.70, -0.58)
                          ),
                        aes(intercept = a, slope = b), size = 1.2, color = "darkorange1"
) +
  labs(x = "Scaled log10-transformed human population density",
       y = "Residuals of growing season durations vs. temperature",
       color = "Land Cover") +
  # theme(legend.justification = "bottom")
  theme(plot.margin = unit(c(5.5, 170, 5.5, 5.5), "pt"),
        legend.position = c(1.16, 0.25))

pp3 = cowplot::ggdraw(pp2) +
  cowplot::draw_plot(p_20_2, x = 0.63, y = 0.53, width = 0.5, height = 0.5)

ggsave("figures/predict_dur_pop_MODIS_20.pdf", plot = pp3, width = 10, height = 6)
ggsave("figures/predict_dur_pop_MODIS_20.png", plot = pp3, width = 10, height = 6)


mutate(dat2, pop_log10_unscale = log10(pop + 1)) %>% 
  ggplot(aes(x = pop_log, y = dur)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~g)

