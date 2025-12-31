# Load required libraries
library(terra)
install.packages("spdep")
library(spdep)
install.packages("exactextractor")
library(exactextractor)
library(sf)

# Set file paths
proj_wd <- "/Users/gra38/Library/CloudStorage/Box-Box/Project_Files/LCLUC_Mongolia/data/veg_model_outputs"
wd <- "~/Library/CloudStorage/Box-Box/Repositories/MG_LCLUC/"
#raster_file <- "path/to/your_raster.tif"
admin_shp <- "/Users/gra38/Library/CloudStorage/Box-Box/Project_Files/LCLUC_Mongolia/data/spatial_data/test_bag.shp"

# Load raster and shapefile
#r <- rast(raster_file)
admin <- vect(admin_shp)

# NEW RASTERS as of June 2025: 
# this tif has all years and AOA for each year as diff bands
ht_file <- paste0(proj_wd, "/ht_strat_predictions.tiff")
ht_all <- rast(ht_file) # all years
# select the band that you want
ht_24_bag <- ht_all$ht_strat_pred_2023
ht_19 <- ht_all$ht_strat_pred_2019


# Optional: mask raster to administrative boundaries
ht_24_bag <- mask(ht_24_bag, admin)



# --- 1. Global Moran's I ---
vals <- values(ht_24_bag, mat = FALSE)
coords <- xyFromCell(ht_24_bag, 1:ncell(ht_24_bag))
valid <- !is.na(vals)
nb <- dnearneigh(coords, 0, 500)  # Adjust distance threshold - here i set it to 500 m
lw <- nb2listw(nb, style = "W")
moran <- moran.test(vals[valid], lw)

cat("Global Moran's I:", moran$estimate[1], "\n")
cat("P-value:", moran$p.value, "\n")

# --- 2. Focal Variance (3x3 window) ---
focal_var <- focal(ht_24_bag, w = matrix(1, 3, 3), fun = var, na.policy = "omit")
plot(focal_var, main = "Focal Variance (3x3)")

# --- 3. Zonal Mean ---
zonal_mean <- exact_extract(ht_24_bag, admin, 'mean')
print(zonal_mean)
