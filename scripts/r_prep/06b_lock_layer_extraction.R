# =============================================================================
# 06b: LOCK LAYER EXTRACTION (RASTER-BASED)
# =============================================================================
#
# PURPOSE:
#   Identifies which planning units overlap existing Protected Areas, REFS
#   Facilities, and Agriculture Zones, and flags them with binary lock columns.
#   These columns are used in script 07 to lock PUs into their existing zone
#   in the prioritizr problem.
#
#   Lock logic:
#     locked_pa   = 1  -> PU overlaps a Protected Area
#                        -> locked INTO Conservation zone in prioritizr
#     locked_refs = 1  -> PU overlaps an existing REFS facility
#                        -> locked INTO REFS zone
#     locked_agri = 1  -> PU overlaps an existing Agriculture zone
#                        -> locked INTO Agriculture zone
#
# APPROACH:
#   Rather than vector intersection (slow on complex geometries), each locked
#   layer is rasterized to the SA national template grid as a binary raster
#   (1 = locked, 0 = not locked, cell assigned by its centre), then
#   terra::extract() computes the mean cell value per hexagon. Because
#   unlocked cells are 0, this mean is the proportion of the PU covered by the
#   locked layer. PUs where this proportion >= MIN_OVERLAP (33%) are flagged.
#
# CONFLICT RESOLUTION:
#   Priority order: PA > REFS > Agri. A PU is assigned to at most one zone.
#
# CHANGE LOG:
#   v2: Corrected overlap calculation. Previous version rasterized with
#       background = NA and touches = TRUE; because extract() then dropped NA
#       cells before averaging, any PU touching a single locked cell received
#       an overlap of 1.0, so the 33% threshold was never applied and locked
#       areas were overstated (notably Agriculture). Now background = 0 and
#       touches = FALSE, giving a true proportional overlap.
#       Outputs from 06b onward (07, 08, app planning units) must be
#       regenerated after this change.
#
# INPUT:
#   data/planning_units/{province}/pu_{province}.gpkg   (from 06a)
#   data/locked_pus/protected_areas.gpkg     (SAPAD, marine PAs removed)
#   data/locked_pus/refs_facilities.gpkg     (REEA, approved/in-process
#                                             solar and wind only)
#   data/locked_pus/agriculture_zones.gpkg   (SANLC 2022 cultivation classes)
#   data/sa_national_template.tif            (reference raster)
#
# OUTPUT:
#   data/planning_units/{province}/pu_{province}.gpkg   (updated in place)
#   data/planning_units/lock_summary.csv
#
# =============================================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(here)

# =============================================================================
# CONFIGURATION
# =============================================================================

base_dir    <- here::here()
pu_dir      <- file.path(base_dir, "data/planning_units")
locked_dir  <- file.path(base_dir, "data/locked_pus")
template    <- file.path(base_dir, "data/sa_national_template.tif")

ALBERS_CRS   <- "ESRI:102022"
MIN_OVERLAP  <- 0.33   # proportion of PU area that must be locked to flag

provinces <- c(
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
# STEP 1: RASTERIZE LOCKED LAYERS
# =============================================================================
# Each locked layer is burned to the SA template grid as a binary raster
# (1 = locked, 0 = not locked). Done once, reused for all provinces.

cat("=============================================================\n")
cat("  RASTERIZING LOCKED LAYERS\n")
cat("=============================================================\n\n")

tmpl <- rast(template)
cat(sprintf("  Template: %d x %d cells, res = %.6f deg\n",
            nrow(tmpl), ncol(tmpl), res(tmpl)[1]))

rasterize_locked <- function(gpkg_path, label) {

  cat(sprintf("  Rasterizing %s...\n", label))

  lyr <- st_read(gpkg_path, quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(crs(tmpl, proj = FALSE))  # match template CRS (WGS84)

  # Burn presence = 1
  lyr$burn <- 1L

  # background = 0 so unlocked cells count towards the PU mean;
  # touches = FALSE assigns cells by centre, for an unbiased area estimate
  r <- rasterize(vect(lyr), tmpl, field = "burn", background = 0,
                 touches = FALSE)

  cat(sprintf("  [OK] %s rasterized (%.1f%% of template cells covered)\n",
              label,
              100 * global(r, "sum", na.rm = TRUE)[1,1] / ncell(r)))
  r
}

r_pa   <- rasterize_locked(file.path(locked_dir, "protected_areas.gpkg"),  "Protected Areas")
r_refs <- rasterize_locked(file.path(locked_dir, "refs_facilities.gpkg"),   "REFS Facilities")
r_agri <- rasterize_locked(file.path(locked_dir, "agriculture_zones.gpkg"), "Agriculture Zones")

cat("\n")

# =============================================================================
# STEP 2: EXTRACT OVERLAP PER PU FOR EACH PROVINCE
# =============================================================================

cat("=============================================================\n")
cat("  LOCK LAYER EXTRACTION\n")
cat("=============================================================\n\n")

summary_rows <- list()

for (prov in provinces) {

  cat(sprintf("-------------------------------------------------------------\n"))
  cat(sprintf("  %s\n", toupper(prov)))
  cat(sprintf("-------------------------------------------------------------\n"))

  gpkg_path <- file.path(pu_dir, prov, sprintf("pu_%s.gpkg", prov))

  if (!file.exists(gpkg_path)) {
    cat(sprintf("  [SKIP] PU file not found: %s\n\n", gpkg_path))
    next
  }

  pu <- st_read(gpkg_path, quiet = TRUE)
  cat(sprintf("  Loaded %d planning units\n", nrow(pu)))

  # Ensure Area_km2 exists
  if (!"Area_km2" %in% names(pu)) {
    pu_albers   <- st_transform(pu, ALBERS_CRS)
    pu$Area_km2 <- as.numeric(st_area(pu_albers)) / 1e6
    cat("  [INFO] Area_km2 calculated and added\n")
  }

  # PUs in WGS84 for extraction (rasters are in WGS84)
  pu_wgs <- st_transform(pu, crs(tmpl, proj = FALSE))
  pu_v   <- vect(pu_wgs)

  # ---------------------------------------------------------------------------
  # Extract mean cell value per PU (= proportion of PU area that is locked)
  # ---------------------------------------------------------------------------
  extract_overlap <- function(r, label) {
    cat(sprintf("  Extracting %s overlap...\n", label))

    # Crop raster to province extent first (speeds up extraction)
    r_crop <- crop(r, ext(pu_v) + 0.1)   # small buffer around extent

    ex <- terra::extract(r_crop, pu_v, fun = "mean", na.rm = TRUE, ID = TRUE)
    overlap <- ex[, 2]
    overlap[is.na(overlap)] <- 0   # PUs outside template coverage = 0

    cat(sprintf("  [OK] %s: %d PUs with any overlap, %d flagged (>= %.0f%%)\n",
                label,
                sum(overlap > 0),
                sum(overlap >= MIN_OVERLAP),
                MIN_OVERLAP * 100))
    overlap
  }

  ov_pa   <- extract_overlap(r_pa,   "PA")
  ov_refs <- extract_overlap(r_refs, "REFS")
  ov_agri <- extract_overlap(r_agri, "Agri")

  # ---------------------------------------------------------------------------
  # Apply threshold to get binary flags
  # ---------------------------------------------------------------------------
  flag_pa   <- ov_pa   >= MIN_OVERLAP
  flag_refs <- ov_refs >= MIN_OVERLAP
  flag_agri <- ov_agri >= MIN_OVERLAP

  # ---------------------------------------------------------------------------
  # Conflict resolution: PA > REFS > Agri
  # ---------------------------------------------------------------------------
  n_conf_refs <- sum(flag_pa & flag_refs)
  n_conf_agri <- sum((flag_pa | flag_refs) & flag_agri)

  if (n_conf_refs > 0)
    cat(sprintf("  [Conflict] %d PUs PA+REFS -> assigned to PA\n", n_conf_refs))
  if (n_conf_agri > 0)
    cat(sprintf("  [Conflict] %d PUs Agri vs PA/REFS -> Agri flag removed\n", n_conf_agri))

  flag_refs[flag_pa]             <- FALSE
  flag_agri[flag_pa | flag_refs] <- FALSE

  # ---------------------------------------------------------------------------
  # Add lock columns and save
  # ---------------------------------------------------------------------------
  pu$locked_pa   <- as.integer(flag_pa)
  pu$locked_refs <- as.integer(flag_refs)
  pu$locked_agri <- as.integer(flag_agri)

  n_total  <- sum(flag_pa | flag_refs | flag_agri)
  pct_lock <- round(n_total / nrow(pu) * 100, 1)

  cat(sprintf("  Total locked : %d / %d (%.1f%%)\n", n_total, nrow(pu), pct_lock))
  cat(sprintf("    PA   : %d\n", sum(pu$locked_pa)))
  cat(sprintf("    REFS : %d\n", sum(pu$locked_refs)))
  cat(sprintf("    Agri : %d\n", sum(pu$locked_agri)))

  if (pct_lock > 70)
    cat("  [WARN] >70% of PUs locked - optimiser has limited freedom\n")

  st_write(pu, gpkg_path, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("  [SAVED] %s\n", gpkg_path))

  summary_rows[[prov]] <- data.frame(
    province       = prov,
    n_pu           = nrow(pu),
    n_locked_pa    = sum(pu$locked_pa),
    n_locked_refs  = sum(pu$locked_refs),
    n_locked_agri  = sum(pu$locked_agri),
    n_locked_total = n_total,
    pct_locked     = pct_lock
  )

  rm(pu, pu_wgs, pu_v); gc()
  cat("\n")
}

# =============================================================================
# SAVE SUMMARY
# =============================================================================

summary_df <- bind_rows(summary_rows)
write_csv(summary_df, file.path(pu_dir, "lock_summary.csv"))

cat("=============================================================\n")
cat("  LOCK LAYER EXTRACTION COMPLETE\n")
cat("=============================================================\n\n")

print(summary_df)
cat(sprintf("\nSummary saved to: %s\n", file.path(pu_dir, "lock_summary.csv")))
