library(terra)
library(raster)
library(ggplot2)
library(magrittr)
library(dplyr)
# install.packages("maps")
# set wd
proj_wd <- "/Users/gra38/Library/CloudStorage/Box-Box/Project_Files/LCLUC_Mongolia/data/veg_model_outputs"
wd <- "~/Library/CloudStorage/Box-Box/Repositories/MG_LCLUC/"
getwd()
setwd(wd)
# Load the AOA masked veg predictions
# Create a SpatRaster from the files
  # veg cover
cov23_file <- paste0(proj_wd, "/clipped_outputs/2023_cover_mean.tiff")
cov23 <- rast(cov23_file)

cov19_file <- paste0(proj_wd, "/clipped_outputs/2019_cover_mean.tiff")
cov19 <- rast(cov19_file)

  # veg height
ht23_file <- paste0(proj_wd, "/clipped_outputs/2023_ht_mean.tiff")
ht23 <- rast(ht23_file)

ht19_file <- paste0(proj_wd, "/clipped_outputs/2019_ht_mean.tiff")
ht19 <- rast(ht19_file)



# plot(ht23, main = "Vegetation Height 2023 (m)")
# summary(cov23)



# MAKE A DATAFRAME
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


# medians
med23 <- median(cov23_df$predicted)
med19 <- median(cov19_df$predicted)
ht23med <- median(ht23_df$predicted)
ht19med <- median(ht19_df$predicted, na.rm = TRUE)
#ht19med <- 0.1013

# trying to combine to one df
#pred_df <- as.data.frame(cov23_df$predicted23, cov19_df$predicted19, ht23_df$predicted23, ht19_df$predicted19)
#colnames(pred_df) <- c("pred23cpv", "pred")

# baseR version
cov23hist <- hist(cov23, breaks = 200,
                  main="2023 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Cover (%)")  # label the x-axis
abline(v=med23, col="#FF33CC")

cov19hist <- hist(cov19, breaks = 200,
                  main="2019 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Cover (%)")  # label the x-axis)
abline(v=med19, col = "#FF33CC")

ht19hist <- hist(ht19, breaks = 200,
                  main="2019 ",
                  col="lightblue",  # changes bin color
                  xlab= "Vegetation Height (m)")  # label the x-axis)
abline(v=ht19med, col = "#FF33CC")


ht23hist <- hist(ht23, breaks = 200,
                 main="2023",
                 col="lightblue",  # changes bin color
                 xlab= "Vegetation Height (m)")  # label the x-axis)
abline(v=ht23med, col = "#FF33CC")



#ggplot version

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

h<- ggplot(ht23_df, aes(x= predicted23)) +
  geom_density(aes(x = predicted23), color = "#226e75", linewidth = 1.5)+
  geom_density(data = ht19_df, aes(x= predicted19), colour = "#FF33CC", linewidth = 1.5)+
  theme_bw()+
  labs(x = "Vegetation height (m)") +
  geom_vline( xintercept =ht23med, linetype = "longdash", linewidth = .6, color = "#226e75") +
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