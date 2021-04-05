library(sf)
library(raster)
library(tidyverse)
library(patchwork)
library(exactextractr)
library(arcgisbinding)
arc.check_product()

#bring in ads
ads <- raster('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Workspace/jfortier/ADS_2000_2020_SeverityWeightedAcres.tif')


#bring in hucs

hucs <- read_sf('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/Josh/josh_wcc.gdb', layer = 'forest_cover_loss')

#extract ads
hucs$ads_sum <- exact_extract(ads, hucs, 'sum')

#convert to tree acres and calculate a percentage
hucs <- hucs %>% mutate(tree_acres = (sum_tree_cov*900)/4046.86,
                            ads_percent = ads_sum/tree_acres)


# write to gdb if you want to
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

#exploring hazard ratings we'll add to the final class...
hucs_haz <- hucs
hazard <- raster('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Workspace/jfortier/Regional_InsectHazard_SUM.tif')
# Values 67, 133 or 201 == moderate, 100,167, 200, 234 and 367 == high

#normal reclass
hazard_rc <- raster::reclassify(hazard, c(0,1,0, 1, Inf, 1))

# now reclass with high
hazard_high <- raster::reclassify(hazard, c(0, 67, 0, 67,100,1,100, 133, 0, 133, 200, 1, 200,201, 0, 201, 255, 1))

#now reclass with mod
hazard_mod <- raster::reclassify(hazard, c(0, 67, 1, 67,100,0,100, 133, 1, 133, 200, 0, 200,201, 1, 201, 255, 0))

# plot if you want
plot(hazard)

plot(hazard_mod)
plot(hazard_high)

# now extract from the reclassified rasters into hucs_haz which is essentially hucs...
hucs_haz$haz_sum_high <- exact_extract(hazard_high, hucs_haz, 'sum')
hucs_haz$haz_sum_mod <- exact_extract(hazard_mod, hucs_haz, 'sum')
hucs_haz$haz_sum <- exact_extract(hazard_rc, hucs_haz, 'sum')

#create data.frame to do stuff
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

#plot together
all / (high|mod)


#now bring it all back together

hucs_ts <- hucs %>% select(HUC_12, Shape) %>%
  left_join(hucs_haz_df, by = 'HUC_12') %>%
  select(-c('sum_forest_loss','percent_loss','class')) %>%
  st_as_sf() %>%
  rename(geometry = 'Shape')

path <- 'T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/Josh/josh_wcc.gdb'

#now write to esri gdb
arc.write(paste0(path, '/ads_hucs'), data = hucs_ts,validate = TRUE, overwrite = TRUE)



#now if we want to we can seperate even more by the hazards

ads_hucs_hazard_exploration <- arc.open(paste0(path, '/ads_hucs'))

ads_hucs_hazard_exploration <- arc.data2sf(arc.select(ads_hucs_hazard_exploration))

ads_hucs_ex_df <- ads_hucs_hazard_exploration %>% st_drop_geometry()

ads_hucs_ex_df <- ads_hucs_ex_df %>% mutate(class_with_future = ifelse(ads_percent >= 0.30, '3',
                                                           ifelse(haz_per_high <= 0.26, '1', '2')),
                                            class_without_future = ifelse(ads_percent >= 0.30, '3',
                                                                       ifelse(ads_percent <= 0.10, '1', '2')))

ads_hucs_ex_df %>% pivot_longer(contains('class')) %>%
  ggplot() + geom_bar(aes(name)) + facet_wrap(~value)

st_write(hucs, 'images/fh.gpkg', layer = 'fh', driver = 'GPKG')
hucs <- read_sf('images/fh.gpkg', layer = 'fh')
hucs <- hucs %>% select(-ADS_method, -haz_fraction, -haz_acres, -haz_sum)

#write the final rating to the gdb
arc.write('T:/FS/NFS/R01/Program/7140Geometronics/GIS/Project/zz_R1WCC_Jan2021/Data/OutputDatasets_Review.gdb/WCC_HUC12_r1_ForestHealth_Rating', data = hucs, overwrite = TRUE, validate = TRUE)
