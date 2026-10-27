# =============================================================================
# NLM Null Model Analysis for Vegetation Spatial Patterns around Camps
# =============================================================================
#
# This script:
#   PART 1 — Calibrates roughness (nlm_mpd) from annual vegetation volume
#             rasters and builds a temporal "deviation from null" time series
#   PART 2 — Uses the pre-computed slope raster to classify vegetation
#             change, calculate landscape metrics, and compare to NLM nulls
#
# NLM METHOD: nlm_mpd (midpoint displacement) — replaces deprecated nlm_fbm.
#   Roughness (0–1) controls spatial texture; autocorrelation range emerges
#   from roughness × landscape size and is validated via semivariogram plots.
#   
#
# INPUTS (set paths in the CONFIG section below):
#   - slope_raster      : veg volume slope
#   - annual_rasters    : annual veg volume, 2019-2024
#   - camp_points       : winter camp locations point shapefile
#   - bag_polygons      : bag boundaries shapefile
#   - bag_id_field      : name of the bag ID field in camp_points
#
# OUTPUTS (written to output_dir):
#   - roughness_calibration.csv           : per-camp calibration results
#   - roughness_semivariogram_plots/      : observed vs simulated svgm plots
#   - temporal_range_null_comparison.csv  : range z-scores per camp × year
#   - camp_metrics_observed.csv           : observed landscape metrics per camp
#   - camp_null_comparison.csv            : z-scores and p-values per camp
#   - bag_null_comparison.csv             : same aggregated to bag level
#   - sensitivity_roughness.csv           : z-scores under ± 0.1 roughness
#   - plots/                              : figure
#   - session_info.txt                    : R session info for reproducibility
#
# REQUIREMENTS:
#   install.packages(c("terra", "sf", "NLMR", "landscapemetrics",
#                      "gstat", "sp", "tidyverse", "patchwork", "furrr"))
#   furrr for parallel processing [not sure if this is working and won't need in same way going forward anyway]
# =============================================================================


# =============================================================================
# 0. CONFIGURATION — edit these paths and settings before running
# =============================================================================

cfg <- list(

  # --- File paths ---
  slope_raster   = "data/veg_rasters/pslope_aoa.tiff",

  # Annual rasters: named vector, names must be the year as character
  annual_rasters = c(
    "2019" = "data/veg_rasters/vol_2019.tif",
    "2020" = "data/veg_rasters/vol_2020.tif",
    "2021" = "data/veg_rasters/vol_2021.tif",
    "2022" = "data/veg_rasters/vol_2022.tif",
    "2023" = "data/veg_rasters/vol_2023.tif",
    "2024" = "data/veg_rasters/vol_2024.tif"
  ),
  
  camp_points = "data/roi_camps_test.shp",  # this is a subset, just for testing
#  camp_points    = "data/roi_camps.shp",   # this is the full set of camps
  bag_polygons   = "data/bags_in_roi.shp",

  # Name of the bag ID field in your camp points shapefile
  bag_id_field   = "bag_name",   # <-- actual field name

  # --- Analysis settings ---
  buffer_m       = 2500,    # camp buffer radius in meters
  pixel_size     = 20,      # raster pixel size in meters
  n_sims         = 99,       #999,     # null model replicates (999 recommended; 99 for testing)
  n_calib_reps   =  20,      # replicates per roughness value during calibration
  max_svgm_pts   = 1000,   #10000,   # max pixels sampled per camp for semivariogram (set to 1000 for testing)
  svgm_cutoff_m  = 2500,     # semivariogram max lag distance in meters
  svgm_width_m   = 40,      # semivariogram lag bin width in meters

  # roughness values to test during calibration (0-1 scale; 0 = smooth, 1 = rough)
  # Expected best value for Mongolian grassland: 0.3-0.6
  roughness_seq  =    seq(0.1, 0.9, by = 0.1), #seq(0.2, 0.8, by = 0.3), [trying diff steps...]

  # Semivariogram model type: "Sph" (spherical) or "Exp" (exponential)
  svgm_model     = "Exp", # can try both to see what's best

  # Calibration scoring weights (range, sill, nugget) — must sum to 1
  calib_weights  = c(range = 0.7, sill = 0.2, nugget = 0.1), # can tweak these, but i think bulk should come from range [or just use range?]

  # Calibration: how many camps to use (NULL = all camps; integer = random sample)
  n_calib_camps  = 10,  # need to think about how to set this value ...

  # Slope classification percentile thresholds
  # Bottom X% of slope values = loss; top X% = gain
  # CHECK THESE WITH RYAN!!!!!!
  thresh_loss_pct = 0.10,       # change this to match Ryan's 2%
  thresh_gain_pct = 0.80,

  # Years to include in temporal analysis
  years          = 2019:2024,

  # --- Temporal analysis (Part 1B) ---
  # Set skip_temporal = TRUE to skip entirely and go straight to Part 2.
  # n_temporal_sims controls replicates for the temporal section only
  # (can be lower than n_sims to save time — 29 is enough for a p-value).
  # n_temporal_camps: integer to subsample camps, or NULL to use all camps.
  skip_temporal    = TRUE,   # <-- set TRUE to disable --saves time if you want to skip it
  n_temporal_sims  = 29,
  n_temporal_camps =  25, #NULL,

  # --- Output ---
  output_dir     = "nlm_outputs",

  # Use parallel processing? Requires furrr package
  parallel       = TRUE,
  n_workers      = 4
)


# =============================================================================
# 1. SETUP
# =============================================================================

library(terra)
library(sf)
library(NLMR)
library(landscapemetrics)
library(gstat)
library(sp)
library(tidyverse)
library(patchwork)
library(furrr)

# Optional parallel backend
if (cfg$parallel) {
  library(furrr)
  plan(multisession, workers = cfg$n_workers)
  map_fn     <- furrr::future_map
  map_dfr_fn <- furrr::future_map_dfr
} else {
  map_fn     <- purrr::map
  map_dfr_fn <- purrr::map_dfr
}

# Create output directories
dirs <- file.path(cfg$output_dir,
                  c("", "roughness_semivariogram_plots", "plots"))
walk(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

message("=== Setup complete ===")


# =============================================================================
# 2. LOAD DATA
# =============================================================================

message("Loading data...")

slope_r <- rast(cfg$slope_raster)
camps   <- st_read(cfg$camp_points, quiet = TRUE)

# Bag polygons are optional — only needed for the heatmap figure
bags <- if (!is.null(cfg$bag_polygons)) {
  st_read(cfg$bag_polygons, quiet = TRUE)
} else {
  NULL
}

# Ensure consistent CRS (UTM)
target_crs <- crs(slope_r)
camps <- st_transform(camps, target_crs)
if (!is.null(bags)) bags <- st_transform(bags, target_crs)

# Load annual rasters as a named list
annual_rasts <- map(cfg$annual_rasters, rast)
names(annual_rasts) <- names(cfg$annual_rasters)

# Add a unique camp ID if not present
if (!"camp_id" %in% names(camps)) {
  camps$camp_id <- seq_len(nrow(camps))
}

message(sprintf("  %d camps loaded", nrow(camps)))
if (!is.null(bags)) message(sprintf("  %d bags loaded", nrow(bags)))


# =============================================================================
# 3. HELPER FUNCTIONS
# =============================================================================

#' Snap n to the nearest valid nlm_mpd dimension: (2^k) + 1
#' nlm_mpd requires dimensions of this form (e.g., 33, 65, 129, 257, 513)
nearest_mpd_size <- function(n) {
  exponent <- ceiling(log2(max(n - 1, 1)))
  2^exponent + 1L
}

#' Buffer a camp point and crop a raster to it
#' Returns a masked terra SpatRaster or NULL if insufficient data
extract_camp_raster <- function(camp_geom, raster, buffer_m,
                                min_cells = 100) {
  buf    <- st_buffer(camp_geom, buffer_m)
  buf_v  <- vect(buf)
  r_crop <- tryCatch(crop(raster, buf_v, mask = TRUE),
                     error = function(e) NULL)
  if (is.null(r_crop)) return(NULL)
  if (sum(!is.na(values(r_crop))) < min_cells) return(NULL)
  r_crop
}


#' Fit an empirical semivariogram to a terra SpatRaster
#' Returns a fitted variogramModel or NULL on failure
fit_semivariogram <- function(r, cutoff_m, width_m,
                              model_type = "Exp", max_pts = 10000) {
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "val")
  
  if (nrow(df) > max_pts) df <- df[sample(nrow(df), max_pts), ]
  
  coordinates(df) <- ~x + y
  
  vgm_emp <- tryCatch(
    variogram(val ~ 1, data = df, cutoff = cutoff_m, width = width_m),
    error = function(e) NULL
  )
  if (is.null(vgm_emp) || nrow(vgm_emp) < 3) return(NULL)
  
  # Seed initial values from the empirical variogram
  # to help fit.variogram converge
  init_sill   <- max(vgm_emp$gamma, na.rm = TRUE)
  init_nugget <- min(vgm_emp$gamma, na.rm = TRUE)
  init_range  <- vgm_emp$dist[which.max(diff(vgm_emp$gamma))]
  init_range  <- max(init_range, cutoff_m * 0.1)  # floor to avoid zero
  
  tryCatch(
    fit.variogram(
      vgm_emp,
      model = vgm(
        psill  = max(init_sill - init_nugget, 1e-6),
        model  = model_type,
        range  = init_range,
        nugget = max(init_nugget, 0)
      ),
      fit.method = 7   # weighted least squares, more robust than default
    ),
    warning = function(w) {
      # On convergence warning, retry with relaxed settings
      tryCatch(
        fit.variogram(
          vgm_emp,
          model = vgm(
            psill  = max(init_sill - init_nugget, 1e-6),
            model  = model_type,
            range  = init_range,
            nugget = max(init_nugget, 0)
          ),
          fit.method = 1,      # OLS, most forgiving
          fit.ranges = FALSE   # fix range, only fit sill/nugget
        ),
        error = function(e) NULL
      )
    },
    error = function(e) NULL
  )
}

#' Extract nugget, sill, range from a fitted variogramModel
extract_svgm_params <- function(vgm_fit) {
  if (is.null(vgm_fit)) return(list(nugget = NA, sill = NA, svgm_range = NA))
  
  # find structural component — nugget row always has range = 0
  structural_row <- which.max(vgm_fit$range)
  
  if (length(structural_row) == 0) {
    return(list(nugget = NA, sill = NA, svgm_range = NA))
  }
  
  list(
    nugget     = vgm_fit$psill[1],
    sill       = sum(vgm_fit$psill),
    svgm_range = vgm_fit$range[structural_row]   # renamed to avoid base R range()
  )
}


#' Generate a classified NLM using nlm_mpd, matching observed PLAND proportions
#' Classes: 1 = loss, 2 = no change, 3 = gain
#'
#' @param n_pixels  Landscape dimension (must be (2^k)+1 — use nearest_mpd_size)
#' @param pixel_size Pixel resolution in metres
#' @param roughness  nlm_mpd roughness parameter (0–1); lower = smoother
#' @param pland_loss Proportion of landscape in loss class
#' @param pland_gain Proportion of landscape in gain class
make_null_landscape <- function(n_pixels, pixel_size, roughness,
                                pland_loss, pland_gain) {

  nl <- nlm_mpd(
    ncol       = n_pixels,
    nrow       = n_pixels,
    resolution = pixel_size,
    roughness  = roughness
  )

  v              <- values(nl, na.rm = FALSE)
  pland_nochange <- 1 - pland_loss - pland_gain

  t_loss     <- quantile(v, pland_loss,                    na.rm = TRUE)
  t_nochange <- quantile(v, pland_loss + pland_nochange,   na.rm = TRUE)

  nl_class        <- nl
  nl_class[]      <- 2L          # default: no change
  nl_class[nl <= t_loss]    <- 1L   # loss
  nl_class[nl > t_nochange] <- 3L   # gain

  nl_class
}

#' Calculate NP, cohesion, and PLAND for classes 1 (loss) and 3 (gain)
#' Returns a single-row tibble in wide format, or NULL on failure
calc_patch_metrics <- function(r_class) {
  m <- tryCatch(
    calculate_lsm(r_class,
                  what = c("lsm_c_np", "lsm_c_cohesion", "lsm_c_pland")),
    error = function(e) NULL
  )
  if (is.null(m)) return(NULL)

  m %>%
    filter(class %in% c(1, 3)) %>%
    select(class, metric, value) %>%
    # values_fn = first prevents list-columns when landscapemetrics returns
    # duplicate class/metric rows for some landscapes (e.g. multi-part patches)
    pivot_wider(names_from  = c(metric, class),
                names_glue  = "{metric}_c{class}",
                values_from = value,
                values_fn   = first)
}


# =============================================================================
# 4. PART 1A — CALIBRATE roughness FROM ANNUAL RASTERS
# =============================================================================



# Compute the landscape pixel dimension once — snapped to valid mpd size
n_px_raw <- round((cfg$buffer_m * 2) / cfg$pixel_size)
n_px     <- nearest_mpd_size(n_px_raw)
message(sprintf("  Landscape size: %d × %d pixels (snapped from %d for nlm_mpd)",
                n_px, n_px, n_px_raw))

# Select calibration camps
set.seed(42)
calib_ids <- if (!is.null(cfg$n_calib_camps) &&
                  cfg$n_calib_camps < nrow(camps)) {
  sample(camps$camp_id, cfg$n_calib_camps)
} else {
  camps$camp_id
}

calib_camps <- camps %>% filter(camp_id %in% calib_ids)
calib_rast  <- annual_rasts[["2023"]]   # use 2023 for calibration

message(sprintf("  Calibrating on %d camps using 2023 raster...",
                nrow(calib_camps)))

calib_results <- map_dfr(seq_len(nrow(calib_camps)), function(i) {

  camp    <- calib_camps[i, ]
  camp_id <- camp$camp_id
  r_camp  <- extract_camp_raster(st_geometry(camp), calib_rast, cfg$buffer_m)

  if (is.null(r_camp)) {
    message(sprintf("    Camp %s: insufficient data, skipping", camp_id))
    return(NULL)
  }

  # Observed semivariogram
  vgm_obs    <- fit_semivariogram(r_camp, cfg$svgm_cutoff_m,
                                   cfg$svgm_width_m, cfg$svgm_model,
                                   cfg$max_svgm_pts)
  obs_params <- extract_svgm_params(vgm_obs)

  if (any(is.na(unlist(obs_params)))) {
    message(sprintf("    Camp %s: semivariogram fit failed, skipping", camp_id))
    return(NULL)
  }

  # Test each roughness candidate
  rgh_scores <- map_dfr(cfg$roughness_seq, function(rgh) {

    rep_params <- map_dfr(seq_len(cfg$n_calib_reps), function(r) {

      nl <- tryCatch(
        nlm_mpd(ncol       = n_px,
                nrow       = n_px,
                resolution = cfg$pixel_size,
                roughness  = rgh),
        error = function(e) NULL
      )
      if (is.null(nl)) return(NULL)

      vgm_sim <- fit_semivariogram(nl, cfg$svgm_cutoff_m,
                                    cfg$svgm_width_m, cfg$svgm_model,
                                    cfg$max_svgm_pts)
      p <- extract_svgm_params(vgm_sim)
      as_tibble(p)
    })

    if (nrow(rep_params) == 0) return(NULL)

    rep_params %>%
      summarise(
        roughness   = rgh,
        range_mean  = mean(svgm_range,  na.rm = TRUE),
        sill_mean   = mean(sill,   na.rm = TRUE),
        nugget_mean = mean(nugget, na.rm = TRUE)
      )
  })

  if (nrow(rgh_scores) == 0) return(NULL)

  # Score each roughness value against observed semivariogram parameters
  w <- cfg$calib_weights
  rgh_scores <- rgh_scores %>%
    mutate(
      range_err  = abs(range_mean  - obs_params$svgm_range) / obs_params$svgm_range,
      sill_err   = abs(sill_mean   - obs_params$sill)       / obs_params$sill,
      nugget_err = abs(nugget_mean - obs_params$nugget)     /
                     (obs_params$nugget + 1e-6),
      total_err  = w["range"]  * range_err  +
                   w["sill"]   * sill_err   +
                   w["nugget"] * nugget_err
    ) %>%
    arrange(total_err)

  best <- rgh_scores[1, ]

  message(sprintf("    Camp %s: best roughness = %.1f (err = %.3f)",
                  camp_id, best$roughness, best$total_err))

  tibble(
    camp_id        = camp_id,
    obs_range      = obs_params$svgm_range,
    obs_sill       = obs_params$sill,
    obs_nugget     = obs_params$nugget,
    best_roughness = best$roughness,
    calib_error    = best$total_err
  )
})

write_csv(calib_results,
          file.path(cfg$output_dir, "roughness_calibration.csv"))

# Global roughness: median across calibration camps
global_roughness <- median(calib_results$best_roughness, na.rm = TRUE)
message(sprintf("\n  Global roughness (median): %.2f", global_roughness))
message(sprintf("  Range across camps: %.1f – %.1f",
                min(calib_results$best_roughness, na.rm = TRUE),
                max(calib_results$best_roughness, na.rm = TRUE)))


# =============================================================================
# 4B. SEMIVARIOGRAM DIAGNOSTIC PLOTS
# =============================================================================



walk(seq_len(nrow(calib_camps)), function(i) {

  camp    <- calib_camps[i, ]
  camp_id <- camp$camp_id
  r_camp  <- extract_camp_raster(st_geometry(camp), calib_rast, cfg$buffer_m)
  if (is.null(r_camp)) return(invisible(NULL))

  # Observed empirical semivariogram points
  df <- as.data.frame(r_camp, xy = TRUE, na.rm = TRUE)
  colnames(df) <- c("x", "y", "val")
  if (nrow(df) > cfg$max_svgm_pts)
    df <- df[sample(nrow(df), cfg$max_svgm_pts), ]
  coordinates(df) <- ~x + y
  vgm_emp <- variogram(val ~ 1, data = df,
                        cutoff = cfg$svgm_cutoff_m,
                        width  = cfg$svgm_width_m)
  obs_df <- as_tibble(vgm_emp)
  # Normalize each semivariogram by its own sill for plotting against null bc that will be 0-1
  obs_df <- obs_df %>% mutate(gamma_norm = gamma / max(gamma, na.rm = TRUE))
  
  calib_row <- calib_results %>% filter(camp_id == !!camp_id)
  if (nrow(calib_row) == 0) return(invisible(NULL))

  best_rgh <- calib_row$best_roughness

  # Simulate 10 null landscapes with best roughness and extract semivariograms
  sim_svgms <- map_dfr(1:10, function(r) {
    nl <- tryCatch(
      nlm_mpd(ncol = n_px, nrow = n_px,
              resolution = cfg$pixel_size,
              roughness  = best_rgh),
      error = function(e) NULL
    )
    if (is.null(nl)) return(NULL)

    nl_df <- as.data.frame(nl, xy = TRUE, na.rm = TRUE)
    colnames(nl_df) <- c("x", "y", "val")
    if (nrow(nl_df) > cfg$max_svgm_pts)
      nl_df <- nl_df[sample(nrow(nl_df), cfg$max_svgm_pts), ]
    coordinates(nl_df) <- ~x + y

    vgm_sim <- variogram(val ~ 1, data = nl_df,
                          cutoff = cfg$svgm_cutoff_m,
                          width  = cfg$svgm_width_m)
    as_tibble(vgm_sim) %>% mutate(rep = r)
    

  })
  sim_svgms <- sim_svgms %>% 
    group_by(rep) %>% 
    mutate(gamma_norm = gamma / max(gamma, na.rm = TRUE)) %>%
    ungroup()
  
  p <- ggplot() +
    geom_line(data  = sim_svgms,   
              aes(x = dist, y = gamma_norm, group = rep),
              color = "steelblue", alpha = 0.4, linewidth = 0.6) +
    geom_point(data = obs_df, 
               aes(x = dist, y = gamma_norm,),
               color = "firebrick", size = 2.5) +
    labs(
      title    = sprintf("Camp %s  |  roughness = %.1f", camp_id, best_rgh),
      subtitle = sprintf("Obs. range = %.0f m  |  Sill = %.3f  |  Nugget = %.3f",
                         calib_row$obs_range,
                         calib_row$obs_sill,
                         calib_row$obs_nugget),
      x        = "Distance (m)",
      y        = "Semivariance",
      caption  = "Blue = simulated NLM envelopes (n = 10)  |  Red = observed"
    ) +
    theme_minimal(base_size = 11)

  ggsave(
    file.path(cfg$output_dir, "roughness_semivariogram_plots",
              sprintf("camp_%s_svgm.png", camp_id)),
    p, width = 7, height = 4.5, dpi = 150
  )
})


# =============================================================================
# 5. PART 1B — TEMPORAL DEVIATION ANALYSIS (annual rasters)
# =============================================================================

if (cfg$skip_temporal) {

  message("\n=== PART 1B: Temporal deviation from null — SKIPPED (skip_temporal = TRUE) ===")
  temporal_results <- tibble()
  write_csv(temporal_results,
            file.path(cfg$output_dir, "temporal_range_null_comparison.csv"))

} else {

  

  # Subsample camps if requested
  temporal_camps <- if (!is.null(cfg$n_temporal_camps) &&
                         cfg$n_temporal_camps < nrow(camps)) {
    set.seed(123)
    camps[sample(nrow(camps), cfg$n_temporal_camps), ]
  } else {
    camps
  }

  message(sprintf("  Running on %d camps × %d years × %d null sims each",
                  nrow(temporal_camps), length(cfg$years), cfg$n_temporal_sims))

  # For each camp × year: fit semivariogram, generate null landscapes,
  # compare observed range to null distribution of ranges.
  # Note: because nlm_mpd does not accept an explicit range parameter,
  # the null distribution captures the range that emerges from the
  # calibrated roughness at this landscape size — which is what we
  # want to test the observed range against.

  temporal_results <- map_dfr(seq_len(nrow(temporal_camps)), function(i) {

    camp    <- temporal_camps[i, ]
    camp_id <- camp$camp_id
    bag_id  <- camp[[cfg$bag_id_field]]

    # Use camp-specific roughness if calibrated, else global median
    rgh <- if (camp_id %in% calib_results$camp_id) {
      calib_results$best_roughness[calib_results$camp_id == camp_id]
    } else {
      global_roughness
    }

    map_dfr(as.character(cfg$years), function(yr) {

      r_annual <- annual_rasts[[yr]]
      r_camp   <- extract_camp_raster(st_geometry(camp), r_annual, cfg$buffer_m)
      if (is.null(r_camp)) return(NULL)

      # Observed semivariogram range for this year
      vgm_obs <- fit_semivariogram(r_camp, cfg$svgm_cutoff_m,
                                    cfg$svgm_width_m, cfg$svgm_model,
                                    cfg$max_svgm_pts)
      obs_p <- extract_svgm_params(vgm_obs)
      if (is.na(obs_p$svgm_range)) return(NULL)

      # Null distribution of semivariogram ranges from nlm_mpd landscapes
      null_ranges <- map_dbl(seq_len(cfg$n_temporal_sims), function(s) {
        nl <- tryCatch(
          nlm_mpd(ncol       = n_px,
                  nrow       = n_px,
                  resolution = cfg$pixel_size,
                  roughness  = rgh),
          error = function(e) NULL
        )
        if (is.null(nl)) return(NA_real_)
        vgm_s <- fit_semivariogram(nl, cfg$svgm_cutoff_m,
                                   cfg$svgm_width_m, cfg$svgm_model,
                                   cfg$max_svgm_pts)
        val <- extract_svgm_params(vgm_s)$svgm_range
        if (is.null(val) || length(val) == 0) return(NA_real_)
        as.double(val)
      })

      null_ranges <- null_ranges[!is.na(null_ranges)]
      if (length(null_ranges) < 10) return(NULL)

      tibble(
        camp_id   = camp_id,
        bag_id    = bag_id,
        year      = as.integer(yr),
        roughness = rgh,
        obs_range = obs_p$svgm_range,
        null_mean = mean(null_ranges),
        null_sd   = sd(null_ranges),
        z_score   = (obs_p$svgm_range - mean(null_ranges)) / sd(null_ranges),
        p_lower   = mean(null_ranges <= obs_p$svgm_range),
        p_upper   = mean(null_ranges >= obs_p$svgm_range),
        n_null    = length(null_ranges)
      )
    })
  })

  write_csv(temporal_results,
            file.path(cfg$output_dir, "temporal_range_null_comparison.csv"))

  message(sprintf("  Temporal analysis complete: %d camp-year records",
                  nrow(temporal_results)))

} # end 


# =============================================================================
# 6. PART 2 — SLOPE RASTER: CLASSIFY, METRICS, NLM NULL COMPARISON
# =============================================================================

# --- 6A. Classify slope raster into 3 classes ---
# Class 1 = loss   (slope below thresh_loss_pct quantile)
# Class 2 = stable (slope between thresholds)
# Class 3 = gain   (slope above thresh_gain_pct quantile)
#
# Default uses 20th / 80th percentiles. Change thresh_loss_pct /
# thresh_gain_pct in cfg, or replace quantile calls below with
# absolute domain-specific thresholds if preferred.


slope_vals  <- values(slope_r, na.rm = TRUE)
thresh_loss <- quantile(slope_vals, cfg$thresh_loss_pct)
thresh_gain <- quantile(slope_vals, cfg$thresh_gain_pct)

message(sprintf("  Slope thresholds: loss < %.4f, gain > %.4f",
                thresh_loss, thresh_gain))

slope_class              <- slope_r
slope_class[]            <- 2L
slope_class[slope_r <= thresh_loss] <- 1L
slope_class[slope_r >= thresh_gain] <- 3L


# --- 6B. Per-camp: extract classified raster, compute observed metrics ---

message("  Computing observed landscape metrics per camp...")

observed_metrics <- map_dfr(seq_len(nrow(camps)), function(i) {

  camp    <- camps[i, ]
  camp_id <- camp$camp_id
  bag_id  <- camp[[cfg$bag_id_field]]

  r_class <- extract_camp_raster(st_geometry(camp), slope_class, cfg$buffer_m)
  if (is.null(r_class)) return(NULL)

  total_cells <- sum(!is.na(values(r_class)))
  n_loss      <- sum(values(r_class) == 1, na.rm = TRUE)
  n_gain      <- sum(values(r_class) == 3, na.rm = TRUE)
  pland_loss  <- n_loss / total_cells
  pland_gain  <- n_gain / total_cells

  m <- calc_patch_metrics(r_class)
  if (is.null(m)) return(NULL)

  bind_cols(
    tibble(camp_id    = camp_id,
           bag_id     = bag_id,
           pland_loss = pland_loss,
           pland_gain = pland_gain),
    m
  )
})

write_csv(observed_metrics,
          file.path(cfg$output_dir, "camp_metrics_observed.csv"))

message(sprintf("  Observed metrics computed for %d camps",
                nrow(observed_metrics)))


# --- 6C. Per-camp: generate null landscapes and compute null distributions ---

message(sprintf("  Running %d null simulations per camp...", cfg$n_sims))
message("  (This may take a while — set parallel = TRUE in cfg to speed up)")

null_comparison <- map_dfr(seq_len(nrow(observed_metrics)), function(i) {

  row     <- observed_metrics[i, ]
  camp_id <- row$camp_id
  bag_id  <- row$bag_id

  # Use camp-specific roughness if calibrated, else global median
  rgh <- if (camp_id %in% calib_results$camp_id) {
    calib_results$best_roughness[calib_results$camp_id == camp_id]
  } else {
    global_roughness
  }

  # Generate null landscapes and compute patch metrics for each
  null_metrics_list <- map(seq_len(cfg$n_sims), function(s) {
    nl_class <- tryCatch(
      make_null_landscape(n_px, cfg$pixel_size, rgh,
                          row$pland_loss, row$pland_gain),
      error = function(e) NULL
    )
    if (is.null(nl_class)) return(NULL)
    calc_patch_metrics(nl_class)
  })

  null_metrics_df <- bind_rows(null_metrics_list)
  if (nrow(null_metrics_df) == 0) return(NULL)

  # Compare observed to null for each metric
  metrics_of_interest <- c("np_c1", "cohesion_c1", "pland_c1",
                             "np_c3", "cohesion_c3", "pland_c3")

  comparison <- map_dfr(metrics_of_interest, function(met) {

    obs_val  <- row[[met]]
    null_vec <- null_metrics_df[[met]]
    null_vec <- null_vec[!is.na(null_vec)]

    if (is.null(obs_val) || is.na(obs_val) || length(null_vec) < 10) {
      return(tibble(metric    = met,
                    obs       = NA_real_,
                    null_mean = NA_real_,
                    null_sd   = NA_real_,
                    z_score   = NA_real_,
                    p_lower   = NA_real_,
                    p_upper   = NA_real_,
                    n_null    = NA_integer_))
    }

    tibble(
      metric    = met,
      obs       = obs_val,
      null_mean = mean(null_vec),
      null_sd   = sd(null_vec),
      z_score   = (obs_val - mean(null_vec)) / sd(null_vec),
      p_lower   = mean(null_vec <= obs_val),   # P(null ≤ obs)
      p_upper   = mean(null_vec >= obs_val),   # P(null ≥ obs)
      n_null    = length(null_vec)
    )
  })

  bind_cols(tibble(camp_id = camp_id, bag_id = bag_id,
                   roughness = rgh), comparison)
})

null_comparison <- null_comparison %>% filter(!grepl("pland", metric))
write_csv(null_comparison,
          file.path(cfg$output_dir, "camp_null_comparison.csv"))


# --- 6D. Aggregate to bag level ---

bag_null_comparison <- null_comparison %>%
  group_by(bag_id, metric) %>%
  summarise(
    n_camps        = n(),
    obs_median     = median(obs,       na.rm = TRUE),
    obs_iqr_lo     = quantile(obs,     0.25, na.rm = TRUE),
    obs_iqr_hi     = quantile(obs,     0.75, na.rm = TRUE),
    null_mean_med  = median(null_mean, na.rm = TRUE),
    z_score_med    = median(z_score,   na.rm = TRUE),
    z_score_iqr_lo = quantile(z_score, 0.25, na.rm = TRUE),
    z_score_iqr_hi = quantile(z_score, 0.75, na.rm = TRUE),
    pct_sig_lower  = mean(p_lower < 0.05, na.rm = TRUE),
    pct_sig_upper  = mean(p_upper < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

bag_null_comparison <- bag_null_comparison %>% filter(!grepl("pland", metric))
write_csv(bag_null_comparison,
          file.path(cfg$output_dir, "bag_null_comparison.csv"))

message(sprintf("  Null comparison complete: %d camp-metric records",
                nrow(null_comparison)))


# =============================================================================
# 7. PLOTS
# =============================================================================

message("\n=== Generating figures ===")

# --- Figure 1: roughness calibration summary ---

p_calib <- ggplot(calib_results, aes(x = best_roughness)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue",
                 color = "white", alpha = 0.85) +
  geom_vline(xintercept = global_roughness,
             color = "firebrick", linewidth = 1, linetype = "dashed") +
  annotate("text",
           x = global_roughness + 0.03, y = Inf,
           vjust = 1.5, hjust = 0,
           label = sprintf("Median = %.2f", global_roughness),
           color = "firebrick", size = 3.5) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(title   = "nlm_mpd roughness calibration across camps",
       subtitle = "Lower = smoother surface; higher = more fine-grained texture",
       x = "Best roughness value", y = "Number of camps") +
  theme_minimal(base_size = 12)

ggsave(file.path(cfg$output_dir, "plots", "01_roughness_calibration.png"),
       p_calib, width = 6, height = 4, dpi = 150)


# --- Figure 2: Temporal semivariogram range — observed vs null ---
# (skipped when temporal analysis was not run)

if (nrow(temporal_results) > 0) {

  temporal_summary <- temporal_results %>%
    group_by(year) %>%
    summarise(
      obs_range_med = median(obs_range,  na.rm = TRUE),
      obs_range_lo  = quantile(obs_range, 0.25, na.rm = TRUE),
      obs_range_hi  = quantile(obs_range, 0.75, na.rm = TRUE),
      null_mean_med = median(null_mean,  na.rm = TRUE),
      null_mean_lo  = quantile(null_mean, 0.25, na.rm = TRUE),
      null_mean_hi  = quantile(null_mean, 0.75, na.rm = TRUE),
      z_med         = median(z_score,    na.rm = TRUE),
      z_lo          = quantile(z_score,  0.25, na.rm = TRUE),
      z_hi          = quantile(z_score,  0.75, na.rm = TRUE),
      .groups = "drop"
    )

  p_range_time <- ggplot(temporal_summary, aes(x = year)) +
    geom_ribbon(aes(ymin = null_mean_lo, ymax = null_mean_hi),
                fill = "steelblue", alpha = 0.2) +
    geom_line(aes(y = null_mean_med),
              color = "steelblue", linetype = "dashed", linewidth = 0.8) +
    geom_ribbon(aes(ymin = obs_range_lo, ymax = obs_range_hi),
                fill = "firebrick", alpha = 0.2) +
    geom_line(aes(y  = obs_range_med),
              color = "firebrick", linewidth = 1) +
    geom_point(aes(y = obs_range_med),
               color = "firebrick", size = 2.5) +
    labs(
      title    = "Semivariogram range: observed vs null expectation",
      subtitle = "Red = observed  |  Blue dashed = null median (IQR shaded)",
      x = "Year", y = "Semivariogram range (m)"
    ) +
    scale_x_continuous(breaks = cfg$years) +
    theme_minimal(base_size = 12)

  p_zscore_time <- ggplot(temporal_summary, aes(x = year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = z_lo, ymax = z_hi),
                fill = "purple", alpha = 0.2) +
    geom_line(aes(y  = z_med), color = "purple", linewidth = 1) +
    geom_point(aes(y = z_med), color = "purple", size = 2.5) +
    labs(
      title    = "Deviation of observed range from null (z-score)",
      subtitle = "Negative = more fragmented than null expectation",
      x = "Year", y = "Z-score (IQR shaded)"
    ) +
    scale_x_continuous(breaks = cfg$years) +
    theme_minimal(base_size = 12)

  p_temporal <- p_range_time / p_zscore_time

  ggsave(file.path(cfg$output_dir, "plots", "02_temporal_range_vs_null.png"),
         p_temporal, width = 8, height = 7, dpi = 150)

} else {
  message("  Figure 2 skipped — no temporal results (skip_temporal = TRUE)")
}


# --- Figure 3: Z-score distributions for loss patch metrics ---

loss_metrics <- null_comparison %>%
  filter(metric %in% c("np_c1", "cohesion_c1")) %>%
  mutate(
    metric_label = recode(metric,
      np_c1       = "Number of loss patches (NP)",
      cohesion_c1 = "Loss patch cohesion",
      pland_c1    = "Loss PLAND (%)"
    )
  )

p_metrics_loss <- ggplot(loss_metrics, aes(x = z_score)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_histogram(aes(fill = metric_label), bins = 30,
                 color = "white", alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~metric_label, scales = "free") +
  labs(
    title    = "Z-scores: observed vs null for vegetation-loss patch metrics",
    subtitle = "Positive = more than null expectation  |  Negative = less than null",
    x        = "Z-score  (observed − null mean) / null SD",
    y        = "Number of camps"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(cfg$output_dir, "plots",
                 "03_zscore_distributions_loss_patches.png"),
       p_metrics_loss, width = 10, height = 4, dpi = 150)


# --- Figure 4: Bag-level z-score heatmap ---

bag_heatmap_data <- bag_null_comparison %>%
  filter(metric %in% c("np_c1", "cohesion_c1",
                         "np_c3", "cohesion_c3")) %>%
  mutate(
    metric_label = recode(metric,
      np_c1       = "NP (loss)",
      cohesion_c1 = "Cohesion (loss)",
      pland_c1    = "PLAND (loss)",
      np_c3       = "NP (gain)",
      cohesion_c3 = "Cohesion (gain)",
      pland_c3    = "PLAND (gain)"
    )
  )

p_heatmap <- ggplot(bag_heatmap_data,
                     aes(x = metric_label,
                         y = factor(bag_id),
                         fill = z_score_med)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(z_score_med, 1)),
            size = 3, color = "black") +
  scale_fill_gradient2(low      = "steelblue",
                        mid      = "white",
                        high     = "firebrick",
                        midpoint = 0,
                        name     = "Median\nz-score") +
  labs(
    title = "Bag-level deviation from null: vegetation patch metrics",
    x = NULL, y = "Bag ID"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

n_bags_for_plot <- if (!is.null(bags)) nrow(bags) else
  length(unique(bag_null_comparison$bag_id))
ggsave(file.path(cfg$output_dir, "plots", "04_bag_zscore_heatmap.png"),
       p_heatmap,
       width  = 9,
       height = max(5, n_bags_for_plot * 0.4),
       dpi    = 150)


# =============================================================================
# 8. SENSITIVITY ANALYSIS — roughness robustness (± 0.1)
# =============================================================================

message("\n=== Sensitivity analysis: roughness robustness ===")

# Test ± 0.1 around global roughness on a random sample of 5 camps.
# A narrower step than the fract_dim equivalent (±0.2) because roughness
# is on a 0–1 scale and is more sensitive near the extremes.

roughness_sensitivity_vals <- c(global_roughness - 0.1,
                                  global_roughness,
                                  global_roughness + 0.1)
roughness_sensitivity_vals <- pmax(0.01, pmin(0.99, roughness_sensitivity_vals))

sens_camps <- observed_metrics %>%
  slice_sample(n = min(5, nrow(observed_metrics)))

sensitivity_results <- map_dfr(roughness_sensitivity_vals, function(rgh_test) {

  map_dfr(seq_len(nrow(sens_camps)), function(i) {

    row <- sens_camps[i, ]

    null_m <- map_dfr(seq_len(min(99, cfg$n_sims)), function(s) {
      nl_class <- tryCatch(
        make_null_landscape(n_px, cfg$pixel_size, rgh_test,
                            row$pland_loss, row$pland_gain),
        error = function(e) NULL
      )
      if (is.null(nl_class)) return(NULL)
      calc_patch_metrics(nl_class)
    })

    if (nrow(null_m) == 0) return(NULL)

    tibble(
      roughness_tested = rgh_test,
      camp_id          = row$camp_id,
      z_np_c1 = (row$np_c1 - mean(null_m$np_c1, na.rm = TRUE)) /
                   sd(null_m$np_c1, na.rm = TRUE),
      z_co_c1 = (row$cohesion_c1 - mean(null_m$cohesion_c1, na.rm = TRUE)) /
                   sd(null_m$cohesion_c1, na.rm = TRUE)
    )
  })
})

write_csv(sensitivity_results,
          file.path(cfg$output_dir, "sensitivity_roughness.csv"))

p_sens <- sensitivity_results %>%
  pivot_longer(c(z_np_c1, z_co_c1),
               names_to  = "metric",
               values_to = "z_score") %>%
  mutate(
    metric = recode(metric,
      z_np_c1 = "NP (loss)",
      z_co_c1 = "Cohesion (loss)")
  ) %>%
  ggplot(aes(x    = factor(round(roughness_tested, 2)),
             y    = z_score,
             fill = factor(camp_id))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~metric) +
  labs(
    title    = "Sensitivity of z-scores to roughness choice (nlm_mpd)",
    subtitle = sprintf("Global roughness = %.2f;  ± 0.1 tested",
                       global_roughness),
    x    = "Roughness tested",
    y    = "Z-score",
    fill = "Camp ID"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(cfg$output_dir, "plots", "05_sensitivity_roughness.png"),
       p_sens, width = 9, height = 4.5, dpi = 150)



