library(sf)
library(raster)
library(tidyverse)

#bring in ads
ads <- raster('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Workspace/jfortier/ADS_2000_2020_SeverityWeightedAcres.tif')

library(exactextractr)

#bring in hucs

hucs <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/Josh/josh_wcc.gdb', layer = 'forest_cover_loss')


hucs$ads_sum <- exact_extract(ads, hucs, 'sum')

#convert to tree acres and calculate a percentage
hucs <- hucs %>% mutate(tree_acres = (sum_tree_cov*900)/4046.86,
                            ads_percent = ads_sum/tree_acres)

hucs_df <- hucs %>% st_drop_geometry()

library(arcgisbinding)
arc.check_product()

arc.write('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/Josh/josh_wcc.gdb/ads_hucs', data = hucs)


#visualising

hucs_df %>% ggplot(aes(ads_percent)) + geom_density() + scale_x_log10(labels = scales::comma)

dist_ads <- tibble(value = quantile(hucs_df$ads_percent, seq(0,1,.05), na.rm = T),
       percentiles = seq(0,1,0.05))

d1 <- dist_ads %>% ggplot(aes(percentiles, value)) + geom_line() +
  labs(y = 'Percentage Affected', title = 'Percentage of HUC 12s affected by Insects and Disease') +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  theme_bw()


library(plotly)
ggplotly(d1)
#exploring hazard ratings
hucs_haz <- hucs %>% select(ads_sum, tree_acres, ads_percent)
hazard <- raster('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Workspace/jfortier/Regional_InsectHazard_SUM.tif')
# Values 67, 133 or 201 == moderate, 100,167, 200, 234 and 367 == high
hazard_rc <- raster::reclassify(hazard, c(0,1,0, 1, Inf, 1))
hazard_high <- raster::reclassify(hazard, c(0, 67, 0, 67,100,1,100, 133, 0, 133, 200, 1, 200,201, 0, 201, 255, 1))
hazard_mod <- raster::reclassify(hazard, c(0, 67, 1, 67,100,0,100, 133, 1, 133, 200, 0, 200,201, 1, 201, 255, 0))


plot(hazard_mod)
plot(hazard_high)

hucs_haz$haz_sum_high <- exact_extract(hazard_high, hucs_haz, 'sum')
hucs_haz$haz_sum_mod <- exact_extract(hazard_mod, hucs_haz, 'sum')
hucs_haz$haz_sum <- exact_extract(hazard_rc, hucs_haz, 'sum')
hucs_haz_df <- hucs_haz %>% st_drop_geometry()
hucs_haz_df <-  hucs_haz_df %>% mutate(haz_acres = (haz_sum*32400)/4046.86,
                                       haz_per = haz_acres/tree_acres,
                                       haz_mod_acres = (haz_sum_mod*32400)/4046.86,
                                       haz_high_acres = (haz_sum_high*32400)/4046.86,
                                       haz_per_high = haz_high_acres/tree_acres,
                                       haz_per_mod = haz_mod_acres/tree_acres)

#plotting hazards

all <- hucs_haz_df %>% filter(haz_per <= 1) %>%
  ggplot(aes(haz_per)) + geom_histogram(position = 'identity') +
  theme_bw() + labs(x = 'Hazard Percentage per HUC12', title = 'Hazard percentages per HUC12',
                    subtitle = 'Hazard Percentage = (sum of haz acres/sum of tree acres)')

hucs_haz_df %>% filter(haz_per <= 1) %>% ggplot(aes(haz_per)) + geom_histogram() +
  theme_bw()

high <- hucs_haz_df %>% ggplot(aes(haz_per_high)) + geom_histogram() +
  theme_bw() + labs(x = 'High Hazard Percentage per HUC12', title = 'High Severity')


mod <- hucs_haz_df %>% ggplot(aes(haz_per_mod)) + geom_histogram() +
  theme_bw() + labs(x = 'Moderate Hazard Percentage per HUC12', title = 'Moderate Severity')

library(patchwork)
all / (high|mod)




# yo bro ------------------------------------------------------------------

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)
