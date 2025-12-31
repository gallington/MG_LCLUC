library(terra)
library(raster)
library(ggplot2)
library(magrittr)
library(dplyr)
# install.packages("maps")
# set wd
proj_wd <- "/Users/gra38/Library/CloudStorage/Box-Box/Project_Files/LCLUC_Mongolia/data/veg_model_outputs/final_veg_layers"
wd <- "~/Library/CloudStorage/Box-Box/Repositories/MG_LCLUC/"
getwd()
setwd(wd)

# Load the rasters ---------------------------
# Create a SpatRaster from the files
# need to mask AOA in new rasters. old rasters are AOA masked veg predictions

## slope rasters -----------------------------------------------------------

  # veg ht slope.  ## TWO BANDS: intercept and slope
ht_slope_file <- paste0(proj_wd, "/ht_slope.tif")
ht_slope <- rast(ht_slope_file)

  # veg cover slope   ## TWO BANDS: intercept and slope
cov_slope_file <- paste0(proj_wd, "/cover_slope.tif")
cov_slope <- rast(cov_slope_file)

  # vol_slope. ## TWO BANDS: intercept and slope
vol_slope_file <- paste0(proj_wd, "/vol_slope.tif")
vol_slope <- rast(vol_slope_file)

plot(ht_slope$slope_2019_2024, main = "Slope of Veg Height")
summary(tif_name)

## older raster versions ---------------------------------------------------

# veg cover [OLD]
#cov23_file <- paste0(proj_wd, "/clipped_outputs/2023_cover_mean.tiff")
#cov23 <- rast(cov23_file)
#cov19_file <- paste0(proj_wd, "/clipped_outputs/2019_cover_mean.tiff")
#cov19 <- rast(cov19_file)
  # veg height [OLD]
#ht23_file <- paste0(proj_wd, "/clipped_outputs/2023_ht_mean.tiff")
#ht23 <- rast(ht23_file)
#ht19_file <- paste0(proj_wd, "/clipped_outputs/2019_ht_mean.tiff")
#ht19 <- rast(ht19_file)


## new veg rasters all yrs -------------------------------------------------

# NEW RASTERS as of June 2025: 
  # this tif has all years and AOA for each year as diff bands
ht_file <- paste0(proj_wd, "/ht_strat_predictions.tiff")
ht_all <- rast(ht_file) # all years
# select the band that you want
ht_24 <- ht_all$ht_strat_pred_2023
ht_19 <- ht_all$ht_strat_pred_2019

# veg ht .  ##
ht19_file <- paste0(proj_wd, "/ht_2019.tif")
ht19 <- rast(ht19_file)
plot(ht19)
ht24_file <- paste0(proj_wd, "/ht_2024.tif")
ht24 <- rast(ht24_file)
plot(ht24)

# MAKE A DATAFRAME-------------
## old rasters------------
# turn it into a df for use w ggplot
cov23_df <- as.data.frame(cov23, xy = TRUE)
# have to rename the predicted column in the clipped versions for some reason
# label it with the year so can combine them into one df for ggplot
cov23_df %<>% dplyr::rename(predicted23 = spat_24a46f995e46_9380)
cov19_df <- as.data.frame(cov19, xy = TRUE)
cov19_df %<>% dplyr::rename(predicted19 = spat_24a43e734ebb_9380)
# can't bind them anymore bc the AOA mask means there are diff # of pixels each year

ht23_df <- as.data.frame(ht23, xy = TRUE)
ht19_df <- as.data.frame(ht19, xy = TRUE)
ht23_df %<>% dplyr::rename(predicted23 = spat_24a411434db3_9380)
ht19_df %<>% dplyr::rename(predicted19 = spat_24a4784f5f37_9380)

## new rasters: as of June 2025:-------
ht24_df <- as.data.frame(ht_24, xy = TRUE)
ht19_df <- as.data.frame(ht_19, xy = TRUE)
#ht24_df %>% dplyr::rename(ht_24 = "ht_strat_pred_2023"). # Why aren't these working????
#ht19_df %>% dplyr::rename(ht_19 = ht_strat_pred_2019)

# Calc medians---------
#med23 <- median(cov23_df$predicted)
#med19 <- median(cov19_df$predicted)
ht24med <- median(ht24_df$ht_strat_pred_2023)
ht19med <- median(ht19_df$ht_strat_pred_2019) #, na.rm = TRUE)
#ht19med <- 0.1013

# trying to combine to one df
#pred_df <- as.data.frame(cov23_df$predicted23, cov19_df$predicted19, ht23_df$predicted23, ht19_df$predicted19)
#colnames(pred_df) <- c("pred23cpv", "pred")

# Plotting histograms--------
## baseR version ------------
cov24hist <- hist(cov24, breaks = 200,
                  main="2023 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Cover (%)")  # label the x-axis
abline(v=med24, col="#FF33CC")

cov19hist <- hist(cov19, breaks = 200,
                  main="2019 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Cover (%)")  # label the x-axis)
abline(v=med19, col = "#FF33CC")

ht19hist <- hist(ht_19, breaks = 200,
                  main="2019 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Height (m)")  # label the x-axis)
abline(v=ht19med, col = "#FF33CC")


ht24hist <- hist(ht_24, breaks = 200,
                 main="2024",
                 col="lightblue",  # changes bin color
                 xlab= "Vegetation Height (m)")  # label the x-axis)
abline(v=ht24med, col = "#FF33CC")



## ggplot version------------------

 # ggplot() +
 #   geom_raster(data = cov23_df , aes(x = x, y = y)) +
 #   scale_fill_viridis_c() +
 #   coord_quickmap()

c<- ggplot(cov23_df, aes(x= predicted23)) +
  geom_density(aes(x = predicted23), color = "#226e75", linewidth = 1.5)+
  geom_density(data = cov19_df, aes(x= predicted19), colour = "#FF33CC", linewidth = 1.5)+
  theme_bw()+
  labs(x = "Vegetation cover (%)") +
  geom_vline( xintercept =med23, linetype = "longdash", linewidth = .6, color = "#226e75") +
  geom_vline( xintercept =med19, linetype = "longdash", linewidth = .6, color = "#FF33CC")

h<- ggplot(ht24_df, aes(x= ht_strat_pred_2023)) +
  geom_density(aes(x = ht_strat_pred_2023), color = "#226e75", linewidth = 1.5)+
  geom_density(data = ht19_df, aes(x= ht_strat_pred_2019), colour = "#FF33CC", linewidth = 1.5)+
  theme_bw()+
  labs(x = "Vegetation height (m)") +
  geom_vline( xintercept =ht24med, linetype = "longdash", linewidth = .6, color = "#226e75") +
  geom_vline( xintercept =ht19med, linetype = "longdash", linewidth = .6, color = "#FF33CC")

ggsave("./ht_dist.png",h,  width = 3, height = 2)


ks.test(cov23_df$predicted23, cov19_df$predicted19)

ks.test(ht23_df$predicted23)

# plot them together
# coverlist <- list(
#   cov19clip_df,
#   cov23clip_df
# )
# df <- lapply(names(coverlist), function(i) {
#   data.frame(
#     rastername = i,
#     value = as.vector(coverlist[[i]])
#   )
# })
# cov_df <- do.call(rbind, df)

med23<- median(veg_cov_df$predicted)
med19<- median(veg_cov_df$predicted19)
pc <- ((med19-med23)/med19)

# Can't plot the AOA masked versions together bc they have diff #s of obsv... unless combine into tidy table as year/type/predctn
vc <- ggplot(veg_cov_df, aes(x = predicted)) +
  geom_density(aes(x = predicted), color = "lightblue") +
  geom_density(aes(x = predicted19), colour = "#FF33CC") +
  theme_bw()+
  labs(x = "Vegetation cover (%)") +
vc +  geom_vline(aes(xintercept = med23, color = "lightblue", linetype = "longdash", linewidth = 1)) +
  geom_vline(xintercept = med19, aes(color = "#FF33CC", linetype = 2, linewidth = 1))