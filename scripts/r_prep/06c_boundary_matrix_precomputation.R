# =============================================================================
# 06c: BOUNDARY MATRIX PRECOMPUTATION
# =============================================================================
#
# PURPOSE:
#   Precomputes and saves boundary length matrices for each province and
#   national extent. These matrices are used in script 07 (and the Shiny app)
#   to apply boundary length penalties (BLM) in the prioritizr problem, which
#   encourages spatially compact, clumped solutions rather than fragmented ones.
#
#   Computing boundary matrices on-the-fly during optimisation is slow,
#   especially for large provinces. Precomputing once and saving as .rds
#   allows near-instant loading in both the core workflow and Shiny app.
#
# METHOD:
#   prioritizr::boundary_matrix() computes a sparse matrix where:
#     - Diagonal entries relate to each PU's own boundary length
#     - Off-diagonal entries = shared boundary length between adjacent PUs
#   This is the standard input for add_boundary_penalties() in prioritizr.
#
# VALIDATION:
#   After computation, a basic sanity check confirms:
#     - Matrix dimensions match PU count
#     - Diagonal values are positive
#     - Stored values are non-negative
#     - Matrix is symmetric
#
# WHEN TO RE-RUN:
#   Only when planning unit GEOMETRY changes (i.e. after re-running 06a).
#   Changes to cost or lock columns (03b, 06b) do not affect the matrix.
#   Existing matrices are skipped; delete an .rds file to force recompute.
#
# INPUT:
#   data/planning_units/{province}/pu_{province}.gpkg   (from 06a)
#
# OUTPUT:
#   data/planning_units/{province}/boundary_matrix_{province}.rds
#   data/planning_units/boundary_summary.csv
#
# RUNTIME NOTES:
#   Boundary matrix computation scales with PU count. Approximate times:
#     Gauteng   (183k PUs, 10 Ha)  -> ~5-10 min
#     National  (246k PUs, 500 Ha) -> ~10-15 min
#     Larger provinces             -> 15-30 min each
#
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(prioritizr)
library(Matrix)
library(here)

# =============================================================================
# CONFIGURATION
# =============================================================================

base_dir <- here::here()
pu_dir   <- file.path(base_dir, "data/planning_units")

ALBERS_CRS <- "ESRI:102022"

# Edit to a single scope to test (e.g. "gauteng"), or leave all 10
provinces <- c(
  "gauteng",
  "national",
  "eastern_cape",
  "free_state",
  "kwazulu_natal",
  "limpopo",
  "mpumalanga",
  "north_west",
  "northern_cape",
  "western_cape"
)

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  BOUNDARY MATRIX PRECOMPUTATION\n")
cat("=============================================================\n\n")

summary_rows <- list()

for (prov in provinces) {

  cat(sprintf("-------------------------------------------------------------\n"))
  cat(sprintf("  %s\n", toupper(prov)))
  cat(sprintf("-------------------------------------------------------------\n"))

  gpkg_path <- file.path(pu_dir, prov, sprintf("pu_%s.gpkg", prov))
  out_path  <- file.path(pu_dir, prov, sprintf("boundary_matrix_%s.rds", prov))

  if (!file.exists(gpkg_path)) {
    cat(sprintf("  [SKIP] PU file not found: %s\n\n", gpkg_path))
    next
  }

  # Skip if already computed (delete the .rds to force recompute)
  if (file.exists(out_path)) {
    cat(sprintf("  [SKIP] Already exists: %s\n\n", out_path))
    next
  }

  # Load PUs
  cat("  Loading planning units...\n")
  pu <- st_read(gpkg_path, quiet = TRUE)
  cat(sprintf("  %d planning units loaded\n", nrow(pu)))

  # Ensure Albers projection for accurate boundary lengths (metres)
  if (st_crs(pu)$input != ALBERS_CRS) {
    cat("  Reprojecting to Albers...\n")
    pu <- st_transform(pu, ALBERS_CRS)
  }

  # Keep only pu_id + geometry - boundary_matrix doesn't need attributes
  pu_geom <- pu["pu_id"]
  rm(pu); gc()

  # -------------------------------------------------------------------------
  # Compute boundary matrix
  # -------------------------------------------------------------------------
  cat("  Computing boundary matrix...\n")
  start_time <- Sys.time()

  bm <- tryCatch({
    boundary_matrix(pu_geom)
  }, error = function(e) {
    cat(sprintf("  [ERROR] boundary_matrix failed: %s\n", e$message))
    NULL
  })

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

  if (is.null(bm)) {
    cat(sprintf("  [SKIP] Could not compute boundary matrix for %s\n\n", prov))
    next
  }

  cat(sprintf("  Computed in %.1f minutes\n", elapsed))

  # -------------------------------------------------------------------------
  # Validate
  # -------------------------------------------------------------------------
  n_pu <- nrow(pu_geom)

  checks <- list(
    dimensions_ok = all(dim(bm) == c(n_pu, n_pu)),
    diagonal_ok   = all(Matrix::diag(bm) > 0),
    values_ok     = all(bm@x >= 0),
    symmetric_ok  = Matrix::isSymmetric(bm)
  )

  cat("  Validation:\n")
  for (chk in names(checks)) {
    status <- if (checks[[chk]]) "[OK]" else "[FAIL]"
    cat(sprintf("    %s %s\n", status, chk))
  }

  if (!all(unlist(checks))) {
    cat("  [WARN] Validation failed - matrix saved but review before use\n")
  }

  # Basic stats, from the strict upper triangle (shared edges only,
  # each adjacent pair counted once, diagonal excluded)
  off_diag    <- Matrix::triu(as(bm, "generalMatrix"), k = 1)
  n_adjacent  <- Matrix::nnzero(off_diag)
  mean_perim  <- round(mean(Matrix::diag(bm)), 1)
  mean_shared <- round(mean(off_diag@x[off_diag@x > 0]), 1)
  rm(off_diag)

  cat(sprintf("  Matrix stats:\n"))
  cat(sprintf("    Dimensions    : %d x %d\n", dim(bm)[1], dim(bm)[2]))
  cat(sprintf("    Adjacent pairs: %d\n", n_adjacent))
  cat(sprintf("    Mean diagonal : %.1f m\n", mean_perim))
  cat(sprintf("    Mean shared BL: %.1f m\n", mean_shared))

  # Ensure sparse dgCMatrix format (required by prioritizr)
  if (!inherits(bm, "dgCMatrix")) {
    bm <- as(bm, "dgCMatrix")
    cat("  Converted to dgCMatrix\n")
  }

  # -------------------------------------------------------------------------
  # Save
  # -------------------------------------------------------------------------
  saveRDS(bm, out_path)
  file_size_mb <- round(file.size(out_path) / 1024^2, 1)
  cat(sprintf("  [SAVED] %s (%.1f MB)\n", out_path, file_size_mb))

  summary_rows[[prov]] <- data.frame(
    province      = prov,
    n_pu          = n_pu,
    n_adjacent    = n_adjacent,
    mean_diag_m   = mean_perim,
    mean_shared_m = mean_shared,
    compute_mins  = elapsed,
    file_size_mb  = file_size_mb,
    valid         = all(unlist(checks))
  )

  rm(pu_geom, bm); gc()
  cat("\n")
}

# =============================================================================
# SAVE SUMMARY
# =============================================================================

if (length(summary_rows) > 0) {

  summary_df <- bind_rows(summary_rows)
  write_csv(summary_df, file.path(pu_dir, "boundary_summary.csv"))

  cat("=============================================================\n")
  cat("  BOUNDARY MATRIX PRECOMPUTATION COMPLETE\n")
  cat("=============================================================\n\n")

  print(summary_df %>% select(province, n_pu, n_adjacent, compute_mins,
                              file_size_mb, valid))

  cat(sprintf("\nSummary saved to: %s\n",
              file.path(pu_dir, "boundary_summary.csv")))

} else {
  cat("No provinces processed.\n")
}

# =============================================================================
# OPTIONAL: COMPARE AGAINST AN EXISTING MATRIX
# =============================================================================
# To confirm a newly computed matrix matches an older copy (e.g. one already
# deployed with the Shiny app), set the paths below and run this block.
#
# old_bm_path <- "path/to/existing/boundary_matrix.rds"
# new_bm_path <- file.path(pu_dir, "gauteng", "boundary_matrix_gauteng.rds")
#
# old_bm <- readRDS(old_bm_path)
# new_bm <- readRDS(new_bm_path)
#
# cat(sprintf("Dimensions match : %s\n", identical(dim(old_bm), dim(new_bm))))
# cat(sprintf("Max difference   : %.6f\n", max(abs(old_bm - new_bm))))
# cat(sprintf("Effectively equal: %s\n", max(abs(old_bm - new_bm)) < 1e-6))
