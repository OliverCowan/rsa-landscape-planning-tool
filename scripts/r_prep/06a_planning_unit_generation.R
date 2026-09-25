# =============================================================================
# 06a: PLANNING UNIT GENERATION
# =============================================================================
#
# PURPOSE:
#   Generates hexagonal planning unit grids for each province and national
#   extent. Each PU is assigned:
#     - A unique integer ID (pu_id)
#     - The composite cost value for each zone (bio / refs / agri)
#       extracted from the composite rasters
#     - Province identifier
#
#   PUs are generated in Africa Albers Equal Area Conic (ESRI:102022)
#   for accurate area representation and saved as GeoPackage (.gpkg),
#   which can be read directly by both R/prioritizr and ArcGIS Pro.
#
# EDGE HEXAGONS:
#   Hexagons are selected where they intersect the boundary and retained
#   whole (not clipped), so all PUs within a scope are of equal area.
#   Edge hexagons therefore extend partly beyond coastlines and provincial
#   borders.
#
# RESOLUTION (hexagon area per province):
#   National      500 Ha
#   Eastern Cape  100 Ha
#   Free State    100 Ha
#   Gauteng        10 Ha
#   KwaZulu-Natal 100 Ha
#   Limpopo       100 Ha
#   Mpumalanga     50 Ha
#   North West    100 Ha
#   Northern Cape 200 Ha
#   Western Cape  100 Ha
#
# COST EXTRACTION:
#   Each composite raster is reprojected to Albers (bilinear) and the mean
#   value within each hexagon extracted. PUs with no valid cells are filled
#   by focal interpolation (3x3 neighbourhood mean, up to 5 iterations),
#   with any remaining NAs assigned the layer mean.
#
# INPUT:
#   data/boundaries/{province}.shp              (Statistics South Africa, 2023;
#                                                national file: sa_national.shp)
#   data/composite/{province}/{zone}_composite.tif   (from 03b)
#
# OUTPUT:
#   data/planning_units/{province}/pu_{province}.gpkg
#   data/planning_units/pu_summary.csv
#
# =============================================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir        <- here::here()
boundaries_dir  <- file.path(base_dir, "data/boundaries")
composite_dir   <- file.path(base_dir, "data/composite")
pu_dir          <- file.path(base_dir, "data/planning_units")
dir.create(pu_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SETUP
# =============================================================================

# Africa Albers Equal Area Conic
ALBERS_CRS <- "ESRI:102022"

# Province boundary filenames (without .shp)
province_map <- list(
  national      = "sa_national",
  eastern_cape  = "eastern_cape",
  free_state    = "free_state",
  gauteng       = "gauteng",
  kwazulu_natal = "kwazulu_natal",
  limpopo       = "limpopo",
  mpumalanga    = "mpumalanga",
  north_west    = "north_west",
  northern_cape = "northern_cape",
  western_cape  = "western_cape"
)

# Hexagon cell area in hectares per province
pu_resolution_ha <- c(
  national      = 500,
  eastern_cape  = 100,
  free_state    = 100,
  gauteng       =  10,
  kwazulu_natal = 100,
  limpopo       = 100,
  mpumalanga    =  50,
  north_west    = 100,
  northern_cape = 200,
  western_cape  = 100
)

# Convert hexagon area (ha) to st_make_grid cellsize (m).
# For hexagons, st_make_grid's cellsize is the edge-to-edge distance d.
# A regular hexagon with edge-to-edge distance d has area (sqrt(3)/2) * d^2,
# so d = sqrt(2 * area / sqrt(3)).
ha_to_cellsize_m <- function(ha) {
  area_m2 <- ha * 10000
  sqrt(area_m2 * 2 / sqrt(3))
}

zones <- c("bio", "refs", "agri")

summary_rows <- list()

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  PLANNING UNIT GENERATION\n")
cat("=============================================================\n\n")

for (prov in names(pu_resolution_ha)) {

  cat(sprintf("-------------------------------------------------------------\n"))
  cat(sprintf("  %s  |  %.0f Ha hexagons\n",
              toupper(prov), pu_resolution_ha[[prov]]))
  cat(sprintf("-------------------------------------------------------------\n"))

  # -------------------------------------------------------------------------
  # Load and reproject boundary
  # -------------------------------------------------------------------------
  boundary_path <- file.path(boundaries_dir,
                             paste0(province_map[[prov]], ".shp"))

  if (!file.exists(boundary_path)) {
    cat(sprintf("  [SKIP] Boundary not found: %s\n", boundary_path))
    next
  }

  boundary <- st_read(boundary_path, quiet = TRUE) %>%
    st_transform(ALBERS_CRS) %>%
    st_union() %>%
    st_make_valid()

  cat(sprintf("  Boundary loaded and projected\n"))

  # -------------------------------------------------------------------------
  # Generate hexagonal grid
  # -------------------------------------------------------------------------
  cellsize <- ha_to_cellsize_m(pu_resolution_ha[[prov]])

  cat(sprintf("  Generating hexagons (cellsize: %.1f m)...\n", cellsize))

  hex_grid <- st_make_grid(
    boundary,
    cellsize    = cellsize,
    square      = FALSE,  # hexagons
    flat_topped = FALSE   # pointy-topped orientation
  ) %>%
    st_sf(geometry = .) %>%
    st_filter(boundary) %>%   # keep hexagons intersecting boundary (not clipped)
    mutate(pu_id = row_number(),
           province = prov)

  n_pu <- nrow(hex_grid)
  cat(sprintf("  PUs generated: %d\n", n_pu))

  # Sense check - warn if unexpectedly high or low
  if (n_pu < 50000)
    cat(sprintf("  [NOTE] Low PU count - consider finer resolution?\n"))
  if (n_pu > 500000)
    cat(sprintf("  [WARN] Very high PU count - may cause slow prioritizr runs\n"))

  # -------------------------------------------------------------------------
  # Extract composite cost values per zone
  # -------------------------------------------------------------------------
  cat("  Extracting composite costs...\n")

  for (zone in zones) {

    comp_path <- file.path(composite_dir, prov,
                           paste0(zone, "_composite.tif"))

    if (!file.exists(comp_path)) {
      cat(sprintf("  [SKIP cost] %s composite not found\n", zone))
      hex_grid[[paste0("cost_", zone)]] <- NA_real_
      next
    }

    r <- rast(comp_path)

    # Reproject raster to Albers for extraction
    r_albers <- project(r, ALBERS_CRS, method = "bilinear")

    # Extract mean composite value per hexagon
    extracted <- terra::extract(r_albers, vect(hex_grid),
                                fun = "mean", na.rm = TRUE,
                                ID = TRUE)

    hex_grid[[paste0("cost_", zone)]] <- extracted[, 2]

    rm(r, r_albers); gc()
    cat(sprintf("  [OK] cost_%s extracted\n", zone))
  }

  # -------------------------------------------------------------------------
  # Handle NAs in cost columns
  # -------------------------------------------------------------------------
  # Strategy:
  #   1. Focal interpolation from valid raster neighbours (spatially sensible)
  #   2. Fall back to layer mean only if focal still leaves NAs
  for (zone in zones) {

    col  <- paste0("cost_", zone)
    n_na <- sum(is.na(hex_grid[[col]]))
    if (n_na == 0) next

    comp_path <- file.path(composite_dir, prov, paste0(zone, "_composite.tif"))

    if (file.exists(comp_path)) {

      r        <- rast(comp_path)
      r_albers <- project(r, ALBERS_CRS, method = "bilinear")

      # Fill NA raster cells using 3x3 neighbourhood mean, up to 5 iterations
      r_filled <- r_albers
      for (iter in 1:5) {
        na_before <- global(is.na(r_filled), "sum")[1,1]
        if (na_before == 0) break
        r_filled  <- focal(r_filled, w = 3, fun = "mean",
                           na.policy = "only", na.rm = TRUE)
        na_after  <- global(is.na(r_filled), "sum")[1,1]
        if (na_after == na_before) break
      }

      # Re-extract for NA PUs only
      na_idx <- which(is.na(hex_grid[[col]]))
      if (length(na_idx) > 0) {
        refill <- terra::extract(r_filled, vect(hex_grid[na_idx, ]),
                                 fun = "mean", na.rm = TRUE, ID = TRUE)
        hex_grid[[col]][na_idx] <- refill[, 2]
      }

      rm(r, r_albers, r_filled); gc()
    }

    # Final fallback: any remaining NAs get layer mean
    still_na <- sum(is.na(hex_grid[[col]]))
    if (still_na > 0) {
      layer_mean <- mean(hex_grid[[col]], na.rm = TRUE)
      hex_grid[[col]][is.na(hex_grid[[col]])] <- layer_mean
      cat(sprintf("  [NA fill] %s: %d focal + %d fallback mean (%.2f)\n",
                  col, n_na - still_na, still_na, layer_mean))
    } else {
      cat(sprintf("  [NA fill] %s: %d NAs filled via focal interpolation\n",
                  col, n_na))
    }
  }

  # -------------------------------------------------------------------------
  # Save outputs
  # -------------------------------------------------------------------------
  prov_out_dir <- file.path(pu_dir, prov)
  dir.create(prov_out_dir, showWarnings = FALSE, recursive = TRUE)

  gpkg_path <- file.path(prov_out_dir, sprintf("pu_%s.gpkg", prov))

  st_write(hex_grid, gpkg_path, delete_dsn = TRUE, quiet = TRUE)

  cat(sprintf("  [SAVED] %s\n", gpkg_path))

  # Cost summary
  cost_summary <- sapply(zones, function(z) {
    col <- paste0("cost_", z)
    sprintf("%.1f-%.1f (mean: %.1f)",
            min(hex_grid[[col]], na.rm = TRUE),
            max(hex_grid[[col]], na.rm = TRUE),
            mean(hex_grid[[col]], na.rm = TRUE))
  })

  cat(sprintf("  Cost ranges:\n"))
  for (z in zones)
    cat(sprintf("    %-5s: %s\n", z, cost_summary[[z]]))

  summary_rows[[prov]] <- data.frame(
    province      = prov,
    resolution_ha = pu_resolution_ha[[prov]],
    n_pu          = n_pu,
    cost_bio_min   = min(hex_grid$cost_bio,  na.rm = TRUE),
    cost_bio_max   = max(hex_grid$cost_bio,  na.rm = TRUE),
    cost_bio_mean  = mean(hex_grid$cost_bio, na.rm = TRUE),
    cost_refs_min  = min(hex_grid$cost_refs,  na.rm = TRUE),
    cost_refs_max  = max(hex_grid$cost_refs,  na.rm = TRUE),
    cost_refs_mean = mean(hex_grid$cost_refs, na.rm = TRUE),
    cost_agri_min  = min(hex_grid$cost_agri,  na.rm = TRUE),
    cost_agri_max  = max(hex_grid$cost_agri,  na.rm = TRUE),
    cost_agri_mean = mean(hex_grid$cost_agri, na.rm = TRUE)
  )

  rm(hex_grid); gc()
  cat(sprintf("\n"))
}

# =============================================================================
# SAVE SUMMARY
# =============================================================================

summary_df <- bind_rows(summary_rows)
write_csv(summary_df, file.path(pu_dir, "pu_summary.csv"))

cat("=============================================================\n")
cat("  PLANNING UNIT GENERATION COMPLETE\n")
cat("=============================================================\n\n")
print(summary_df %>% select(province, resolution_ha, n_pu))

cat(sprintf("\nSummary saved to: %s\n",
            file.path(pu_dir, "pu_summary.csv")))
