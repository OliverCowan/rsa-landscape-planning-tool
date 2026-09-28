# =============================================================================
# APP DATA PREPARATION: EXPANDED PLANNING UNITS FOR THE SHINY APP
# =============================================================================
#
# PURPOSE:
#   Builds the planning unit files the Shiny app reads. For each scope it
#   takes the workflow planning units (06a/06b) and adds one column per
#   input layer per zone, so the app can rebuild cost surfaces at runtime
#   when a user changes individual layer weights.
#
# COST SCALE (IMPORTANT):
#   All costs stay on the 0-100 scale used throughout the workflow.
#     cost_bio / cost_refs / cost_agri
#       copied unchanged from the 06a planning units, so the app's default
#       solve uses exactly the same costs as 07.
#     {layer_id}_{zone}
#       mean reclassified value (0-100) per planning unit, no rescaling.
#   At runtime, the app computes a custom cost as the plain weighted mean
#   of these per-layer columns. Because averaging is linear, a custom solve
#   with the default weights reproduces the default costs (checked by
#   validate_province()).
#
# CHANGE LOG:
#   v2: Removed per-province min-max stretching of each layer and of each
#       zone composite, and stopped recomputing composites here. Stretching
#       changed effective layer weights and the relative scale of the three
#       zones, so app solutions differed from 07 and from the method as
#       documented, and a given BLM was ~100x stronger in the app than in
#       the calibration (08). Requires the matching change in app.R.
#
#   v2.1: Also copies each scope's boundary matrix (06c) and writes a scope
#       outline into data/app (prepare_support_files()), so the app reads
#       only from data/app. Replaces the separate data/provinces/.../
#       precomputed and boundary files the app previously read.
#
# INPUT:
#   data/planning_units/{scope}/pu_{scope}.gpkg          (from 06a + 06b)
#   data/planning_units/{scope}/boundary_matrix_{scope}.rds (from 06c)
#   data/boundaries/{scope}.shp (national: sa_national.shp)
#   data/reclassified/{scope}/{zone}/{layer_id}.tif      (from 02)
#   weights/{scope}_weights.csv                          (from 03a)
#
# OUTPUT:
#   {APP_PU_DIR}/{scope}/spatial/planning_units.gpkg
#   {APP_PU_DIR}/{scope}/precomputed/boundary_matrix.rds
#   {APP_PU_DIR}/{scope}/spatial/boundary.gpkg
#
# USAGE:
#   check_rasters("eastern_cape")        # confirm inputs exist
#   prepare_province("eastern_cape")     # one scope (includes support files)
#   prepare_support_files("eastern_cape")  # boundary matrix + outline only
#   validate_province("eastern_cape")    # checks, incl. cost consistency
#   prepare_all_provinces()              # all scopes (run overnight)
#
# RUNTIME: roughly 5-15 minutes per scope.
#
# =============================================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(here)

sf::sf_use_s2(FALSE)
terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")
terra::terraOptions(threads = 8)

# =============================================================================
# CONFIG
# =============================================================================

base_dir       <- here::here()
WF_PU_DIR      <- file.path(base_dir, "data/planning_units")
WF_WEIGHTS_DIR <- file.path(base_dir, "weights")
WF_RECLASS_DIR <- file.path(base_dir, "data/reclassified")
WF_BOUNDS_DIR  <- file.path(base_dir, "data/boundaries")

ALBERS_CRS <- "ESRI:102022"

# Where the app reads its planning units from; must match app.R
APP_PU_DIR     <- file.path(base_dir, "data/app/provinces")

ZONES <- c("bio", "refs", "agri")

PROVINCES <- c(
  "national",
  "eastern_cape",
  "free_state",
  "gauteng",
  "kwazulu_natal",
  "limpopo",
  "mpumalanga",
  "north_west",
  "northern_cape",
  "western_cape"
)

# =============================================================================
# CORE FUNCTION: Process a single scope
# =============================================================================

prepare_province <- function(province, overwrite = FALSE) {

  cat("\n", strrep("=", 60), "\n")
  cat("Processing:", province, "\n")
  cat(strrep("=", 60), "\n")

  out_path <- file.path(APP_PU_DIR, province, "spatial", "planning_units.gpkg")
  if (file.exists(out_path) && !overwrite) {
    cat("  Output already exists. Set overwrite = TRUE to reprocess.\n")
    cat("  Refreshing support files only.\n")
    prepare_support_files(province)
    return(invisible(NULL))
  }

  # --- Weights ---
  weights_path <- file.path(WF_WEIGHTS_DIR, paste0(province, "_weights.csv"))
  if (!file.exists(weights_path)) {
    warning("Weights CSV not found: ", weights_path, " - skipping.")
    return(invisible(NULL))
  }
  weights <- read_csv(weights_path, show_col_types = FALSE)

  # --- Workflow planning units (costs from 06a, locks from 06b) ---
  pu_path <- file.path(WF_PU_DIR, province, paste0("pu_", province, ".gpkg"))
  if (!file.exists(pu_path)) {
    warning("PU GPKG not found: ", pu_path, " - skipping.")
    return(invisible(NULL))
  }

  cat("  Loading workflow planning units...\n")
  pu <- st_read(pu_path, quiet = TRUE)
  cat("  PUs loaded:", nrow(pu), "\n")

  required <- c("pu_id", "cost_bio", "cost_refs", "cost_agri",
                "locked_pa", "locked_refs", "locked_agri")
  missing_cols <- setdiff(required, names(pu))
  if (length(missing_cols) > 0) {
    stop("Planning units missing columns (re-run 06a/06b): ",
         paste(missing_cols, collapse = ", "))
  }

  pu_vect <- vect(pu)

  # --- Extract per-layer, per-zone values (0-100, no rescaling) ---
  layer_cols  <- list()
  missing_log <- list()

  cat("  Extracting per-layer values (one stacked extraction per zone)...\n")

  for (zone in ZONES) {

    include_col  <- paste0("include_", zone)
    zone_dir     <- file.path(WF_RECLASS_DIR, province, zone)
    zone_weights <- weights %>%
      filter(.data[[include_col]] == TRUE | .data[[include_col]] == "TRUE")

    if (nrow(zone_weights) == 0) {
      cat("  [SKIP] No layers for zone:", zone, "\n")
      next
    }

    valid_ids   <- character(0)
    valid_paths <- character(0)
    for (layer_id in zone_weights$layer_id) {
      raster_path <- file.path(zone_dir, paste0(layer_id, ".tif"))
      if (file.exists(raster_path)) {
        valid_ids   <- c(valid_ids, layer_id)
        valid_paths <- c(valid_paths, raster_path)
      } else {
        cat("    [MISSING]", zone, "/", layer_id, "\n")
        missing_log[[length(missing_log) + 1]] <- list(zone = zone, layer_id = layer_id)
      }
    }
    if (length(valid_paths) == 0) {
      cat("  [SKIP] No valid rasters for zone:", zone, "\n")
      next
    }

    cat(sprintf("  [%s] Stacking %d rasters and extracting...\n",
                toupper(zone), length(valid_paths)))

    stack        <- rast(valid_paths)
    names(stack) <- valid_ids
    pu_proj      <- if (!same.crs(pu_vect, stack)) project(pu_vect, crs(stack)) else pu_vect

    extracted <- terra::extract(stack, pu_proj, fun = "mean", na.rm = TRUE, ID = FALSE)

    for (layer_id in valid_ids) {
      vals <- extracted[[layer_id]]
      # PUs with no valid cells (boundary edges): use the layer mean, as the
      # final fallback in 06a does, rather than 0, which would make edge
      # PUs look artificially cheap in custom-weight solves
      vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
      layer_cols[[paste0(layer_id, "_", zone)]] <- vals
    }

    cat(sprintf("  [%s] Done - %d layers extracted\n", toupper(zone), length(valid_ids)))
    rm(stack, extracted); gc()
  }

  if (length(missing_log) > 0) {
    cat("\n  WARNING - missing rasters (", length(missing_log), "):\n")
    for (m in missing_log) cat("     ", m$zone, "/", m$layer_id, "\n")
  }

  # --- Assemble output: workflow columns + per-layer columns ---
  cat("\n  Binding", length(layer_cols), "layer columns...\n")
  pu_out <- cbind(pu, as.data.frame(layer_cols))

  id_cols        <- intersect(c("pu_id", "province", "Area_km2"), names(pu_out))
  locked_cols    <- c("locked_pa", "locked_refs", "locked_agri")
  composite_cols <- c("cost_bio", "cost_refs", "cost_agri")
  pu_out <- pu_out[, c(id_cols, locked_cols, composite_cols, names(layer_cols))]

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cat("  Writing output GPKG...\n")
  st_write(pu_out, out_path, delete_dsn = TRUE, quiet = TRUE)

  cat(sprintf("  Done: %s | PUs: %d | Cols: %d | Size: %.1f MB\n",
              basename(out_path), nrow(pu_out), ncol(pu_out) - 1,
              file.size(out_path) / 1e6))

  prepare_support_files(province)

  invisible(pu_out)
}

# =============================================================================
# SUPPORT FILES: boundary matrix and scope outline
# =============================================================================
# The app also needs, per scope, the 06c boundary matrix (BLM) and an outline
# of the scope (preview map, clipping of display layers). Both are placed in
# data/app so the app reads only from there. The matrix is copied unchanged:
# prepare_province() keeps the 06a row order, so it stays aligned with the
# planning units (the app also checks the dimensions).

prepare_support_files <- function(province) {

  scope_dir <- file.path(APP_PU_DIR, province)

  # --- Boundary matrix (06c) ---
  bm_src <- file.path(WF_PU_DIR, province, paste0("boundary_matrix_", province, ".rds"))
  bm_dst <- file.path(scope_dir, "precomputed", "boundary_matrix.rds")
  if (file.exists(bm_src)) {
    dir.create(dirname(bm_dst), showWarnings = FALSE, recursive = TRUE)
    file.copy(bm_src, bm_dst, overwrite = TRUE)
    cat("  [OK] Boundary matrix copied\n")
  } else {
    cat("  [MISSING] Boundary matrix (run 06c):", bm_src, "\n")
  }

  # --- Scope outline (WGS84, lightly simplified; display only) ---
  bnd_name <- if (province == "national") "sa_national" else province
  bnd_src  <- file.path(WF_BOUNDS_DIR, paste0(bnd_name, ".shp"))
  bnd_dst  <- file.path(scope_dir, "spatial", "boundary.gpkg")
  if (file.exists(bnd_src)) {
    outline <- st_read(bnd_src, quiet = TRUE) %>%
      st_transform(ALBERS_CRS) %>%
      st_union() %>%
      st_simplify(dTolerance = 100, preserveTopology = TRUE) %>%
      st_make_valid() %>%
      st_transform(4326)
    dir.create(dirname(bnd_dst), showWarnings = FALSE, recursive = TRUE)
    st_write(st_sf(geometry = outline), bnd_dst, delete_dsn = TRUE, quiet = TRUE)
    cat("  [OK] Scope outline written\n")
  } else {
    cat("  [MISSING] Boundary shapefile:", bnd_src, "\n")
  }

  invisible(NULL)
}

# =============================================================================
# BATCH FUNCTION
# =============================================================================

prepare_all_provinces <- function(overwrite = FALSE) {
  cat("\nStarting app planning unit preparation\n")
  cat("Time started:", format(Sys.time()), "\n")
  for (province in PROVINCES) {
    tryCatch(
      prepare_province(province, overwrite = overwrite),
      error = function(e) cat("  ERROR in", province, ":", conditionMessage(e), "\n")
    )
  }
  cat("\nAll scopes complete.\n")
  cat("Time finished:", format(Sys.time()), "\n")
}

# =============================================================================
# VALIDATION
# =============================================================================
# Includes a consistency check: rebuilding each zone cost from the per-layer
# columns with the default weights (as the app does for custom weights)
# should closely match the workflow cost columns. Small differences are
# expected from NA handling at boundary edges and from averaging at the
# planning-unit rather than raster level.

validate_province <- function(province) {

  out_path <- file.path(APP_PU_DIR, province, "spatial", "planning_units.gpkg")
  if (!file.exists(out_path)) {
    cat("Output not found:", out_path, "\n")
    return(invisible(NULL))
  }

  pu      <- st_read(out_path, quiet = TRUE)
  weights <- read_csv(file.path(WF_WEIGHTS_DIR, paste0(province, "_weights.csv")),
                      show_col_types = FALSE)

  cat("\n=== VALIDATION:", province, "===\n")
  cat("Rows        :", nrow(pu), "\n")
  cat("Columns     :", ncol(pu) - 1, "(excl. geom)\n")
  cat("CRS         :", st_crs(pu)$input, "\n")
  cat("Locked PA   :", sum(pu$locked_pa,   na.rm = TRUE), "\n")
  cat("Locked REFS :", sum(pu$locked_refs, na.rm = TRUE), "\n")
  cat("Locked AGRI :", sum(pu$locked_agri, na.rm = TRUE), "\n")

  bm_path <- file.path(APP_PU_DIR, province, "precomputed", "boundary_matrix.rds")
  if (file.exists(bm_path)) {
    bm_n <- nrow(readRDS(bm_path))
    cat("Boundary mat:", bm_n, "rows",
        if (bm_n == nrow(pu)) "[OK]" else "[FAIL: does not match PU count]", "\n")
  } else {
    cat("Boundary mat: [MISSING]\n")
  }

  cat("\nCost consistency (default-weight rebuild vs workflow cost):\n")
  for (zone in ZONES) {
    wcol <- paste0(zone, "_weight"); icol <- paste0("include_", zone)
    zw <- weights %>%
      filter(.data[[icol]] == TRUE | .data[[icol]] == "TRUE", !is.na(.data[[wcol]]))
    cols <- paste0(zw$layer_id, "_", zone)
    keep <- cols %in% names(pu)
    if (!any(keep)) { cat(sprintf("  %-5s: no layer columns\n", zone)); next }

    m       <- as.matrix(st_drop_geometry(pu)[, cols[keep], drop = FALSE])
    rebuilt <- as.vector(m %*% zw[[wcol]][keep]) / sum(zw[[wcol]][keep])
    target  <- pu[[paste0("cost_", zone)]]

    cat(sprintf("  %-5s: r = %.4f | median abs diff = %.2f | 95th pct abs diff = %.2f (0-100 scale)\n",
                zone, cor(rebuilt, target, use = "complete.obs"),
                median(abs(rebuilt - target), na.rm = TRUE),
                quantile(abs(rebuilt - target), 0.95, na.rm = TRUE)))
  }

  na_total <- sum(is.na(pu$cost_bio)) + sum(is.na(pu$cost_refs)) + sum(is.na(pu$cost_agri))
  cat(if (na_total > 0) sprintf("\nWARNING: %d NA values in cost columns\n", na_total)
      else "\nNo NA values in cost columns\n")

  layer_cols <- grep("_(bio|refs|agri)$", names(pu), value = TRUE)
  zero_cols  <- layer_cols[sapply(layer_cols, function(col) all(pu[[col]] == 0, na.rm = TRUE))]
  if (length(zero_cols) > 0) {
    cat("WARNING: all-zero layer columns (check extraction):\n")
    for (z in zero_cols) cat("    ", z, "\n")
  } else {
    cat("No all-zero layer columns\n")
  }
}

# =============================================================================
# QUICK RASTER CHECK
# =============================================================================

check_rasters <- function(province) {
  weights <- read_csv(file.path(WF_WEIGHTS_DIR, paste0(province, "_weights.csv")),
                      show_col_types = FALSE)
  cat("\n=== RASTER CHECK:", province, "===\n")
  found <- 0; missing <- 0
  for (zone in ZONES) {
    icol <- paste0("include_", zone)
    for (layer_id in weights$layer_id[weights[[icol]] == TRUE | weights[[icol]] == "TRUE"]) {
      if (file.exists(file.path(WF_RECLASS_DIR, province, zone, paste0(layer_id, ".tif")))) {
        found <- found + 1
      } else {
        cat("  [MISSING]", zone, "/", layer_id, "\n"); missing <- missing + 1
      }
    }
  }
  cat("  Found  :", found, "\n  Missing:", missing, "\n")
  if (missing == 0) cat("  All rasters present\n")
}
