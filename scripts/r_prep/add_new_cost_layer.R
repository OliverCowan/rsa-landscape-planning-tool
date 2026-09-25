# =============================================================================
# ADD NEW COST LAYER
# =============================================================================
#
# PURPOSE:
#   Adds a single new raw raster to the pipeline:
#     Step 1  RAW -> PROCESSED       (done here)
#     Step 2  PROCESSED -> RECLASSIFIED  (handed over to 02_reclassification.R)
#   so that new layers are reclassified by exactly the same code as all
#   existing layers.
#
# STEP 1 IS THE WORKFLOW'S ONLY SCRIPTED RECORD OF LAYER STANDARDISATION:
#   - reproject to the template CRS (WGS84) and resample to the SA national
#     template grid (~90 m), both nearest neighbour
#   - crop and mask to each national/provincial boundary
#   - NoData treatment "make 0": NA filled with 0 BEFORE masking, so gaps
#     inside the boundary become 0 while cells outside remain NA
#   The processed rasters it writes are the inputs assumed by
#   02_reclassification.R.
#
# PRE-REQUISITES:
#   1. Raw raster placed in: data/raw/{layer_id}.tif
#   2. Layer row added to layer_config_template.csv, status = "confirmed"
#   3. Verify invert_bio, invert_refs, invert_agri carefully before running
#
# CONFIG COLUMNS USED:
#   Step 1 (this script):
#     - layer_id          : must match raw filename (without .tif)
#     - nodata_treatment  : "make 0" / "leave as is"
#     - national ... western_cape : TRUE/FALSE per scope
#     - status            : must be "confirmed"
#   Step 2 (02_reclassification.R): see the header of that script, including
#     processing_method (binary / manual / jenks), jenks_classes,
#     manual_values (numeric codes only, e.g. "0=0; 1=100"), zone and
#     invert_* columns.
#
# USAGE:
#   1. Set LAYER_ID below to match your new layer's filename (without .tif)
#   2. Run the entire script
#   3. Check the validation summary at the end
#   4. Inspect outputs in ArcGIS Pro before using in analysis
#   5. Then run 03a (if weights need regenerating) and 03b onward
#
# CHANGE LOG:
#   v2: Removed this script's own reclassification step, which differed from
#       02_reclassification.R (jenks on all cells vs a sample, unmapped manual
#       values set to NA, category-name remaps, percentile method). Step 2 is
#       now performed by 02 itself. Category-name remaps and the percentile
#       method are no longer supported.
#
# INPUT:
#   data/raw/{layer_id}.tif
#   layer_config_template.csv
#   data/boundaries/{province}.shp
#   data/sa_national_template.tif
#
# OUTPUT:
#   data/processed/{province}/{layer_id}.tif
#   data/reclassified/{province}/{zone}/{layer_id}.tif   (via 02)
#
# =============================================================================

library(terra)
library(sf)
library(readr)
library(dplyr)
library(here)

# =============================================================================
# USER INPUT - SET THIS BEFORE RUNNING
# =============================================================================

LAYER_ID <- "RSA_PA_CBA_ESA_int"   # <-- change to your new layer_id

# =============================================================================
# PATHS
# =============================================================================

base_dir       <- here::here()
raw_dir        <- file.path(base_dir, "data/raw")
processed_dir  <- file.path(base_dir, "data/processed")
reclass_dir    <- file.path(base_dir, "data/reclassified")
boundaries_dir <- file.path(base_dir, "data/boundaries")
template_path  <- file.path(base_dir, "data/sa_national_template.tif")
config_path    <- file.path(base_dir, "layer_config_template.csv")
reclass_script <- file.path(base_dir, "scripts/r_prep/02_reclassification.R")

# =============================================================================
# SETUP
# =============================================================================

tmp_dir <- file.path(tempdir(), "terra_tmp")
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
terraOptions(memfrac = 0.8, tempdir = tmp_dir)

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

# =============================================================================
# LOAD & VALIDATE CONFIG
# =============================================================================

cat("=============================================================\n")
cat(sprintf("  ADD NEW LAYER: %s\n", LAYER_ID))
cat("=============================================================\n\n")

config <- read_csv(config_path, show_col_types = FALSE)
row    <- config %>% filter(layer_id == LAYER_ID, status == "confirmed")

if (nrow(row) == 0) {
  stop(sprintf(
    "Layer '%s' not found in config with status='confirmed'.\n  Check layer_id matches exactly and status is set to 'confirmed'.",
    LAYER_ID))
}
if (nrow(row) > 1) {
  stop(sprintf("Multiple confirmed rows found for '%s' - check config for duplicates.", LAYER_ID))
}

cat("Config row found:\n")
cat(sprintf("  processing_method: %s\n", row$processing_method))
cat(sprintf("  nodata_treatment:  %s\n", row$nodata_treatment))
cat(sprintf("  zone:              %s\n", row$zone))
cat(sprintf("  invert_bio:        %s\n", row$invert_bio))
cat(sprintf("  invert_refs:       %s\n", row$invert_refs))
cat(sprintf("  invert_agri:       %s\n", row$invert_agri))
cat(sprintf("  manual_values:     %s\n", ifelse(is.na(row$manual_values), "NA", row$manual_values)))

# 02 supports binary / manual / jenks only - fail early rather than mid-run
method <- tolower(trimws(row$processing_method))
if (!method %in% c("binary", "manual", "jenks")) {
  stop(sprintf("processing_method '%s' is not supported by 02_reclassification.R (use binary, manual or jenks).",
               method))
}

# Validate raw file exists
raw_path <- file.path(raw_dir, paste0(LAYER_ID, ".tif"))
if (!file.exists(raw_path)) {
  stop(sprintf("Raw file not found: %s", raw_path))
}
cat(sprintf("\nRaw file: %.1f MB\n", file.size(raw_path) / 1024^2))

# Target provinces
target_provinces <- names(province_map)[sapply(names(province_map), function(p) {
  col_val <- row[[p]]
  !is.na(col_val) && as.logical(col_val)
})]
cat(sprintf("Target provinces: %s\n", paste(target_provinces, collapse = ", ")))

# Active zones (same parsing rule as 02)
zone <- tolower(trimws(row$zone))
active_components <- if (grepl("all", zone)) {
  c("bio", "refs", "agri")
} else {
  trimws(strsplit(zone, ";")[[1]])
}
cat(sprintf("Active components: %s\n\n", paste(active_components, collapse = ", ")))

# =============================================================================
# STEP 1: PROCESS (MASK + NODATA TREATMENT)
# =============================================================================

cat("-------------------------------------------------------------\n")
cat("STEP 1: PROCESS (mask to boundaries + NoData treatment)\n")
cat("-------------------------------------------------------------\n\n")

template <- rast(template_path)
nodata   <- tolower(trimws(row$nodata_treatment))

# Load and snap raw raster once
cat("Loading raw raster...\n")
r_raw <- rast(raw_path)
if (!same.crs(r_raw, template)) {
  cat("  Reprojecting to WGS84...\n")
  r_raw <- project(r_raw, template, method = "near")
}
cat("  Resampling to template grid...\n")
r_raw <- resample(r_raw, template, method = "near")

step1_results <- list()

for (prov in target_provinces) {

  out_dir  <- file.path(processed_dir, prov)
  out_path <- file.path(out_dir, paste0(LAYER_ID, ".tif"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Overwrite if exists - this is a deliberate reprocess
  if (file.exists(out_path)) {
    file.remove(out_path)
    cat(sprintf("  [OVERWRITE] %s\n", prov))
  }

  boundary <- vect(st_read(
    file.path(boundaries_dir, paste0(province_map[[prov]], ".shp")),
    quiet = TRUE))

  result <- tryCatch({
    r <- crop(r_raw, boundary)

    if (nodata == "make 0") {
      r <- subst(r, NA, 0)   # fill NoData BEFORE mask
      r <- mask(r, boundary) # mask LAST
    } else {
      r <- mask(r, boundary)
    }

    writeRaster(r, out_path, overwrite = TRUE,
                gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
    rm(r); gc()
    "done"
  }, error = function(e) paste0("error: ", e$message))

  cat(sprintf("  [%-6s] %s\n", toupper(result), prov))
  step1_results[[prov]] <- result
}

rm(r_raw); gc()
unlink(list.files(tmp_dir, full.names = TRUE, recursive = TRUE))

step1_errors <- sum(grepl("^error", unlist(step1_results)))
cat(sprintf("\nStep 1 complete: %d done, %d errors\n\n",
            length(target_provinces) - step1_errors, step1_errors))

if (step1_errors > 0) stop("Step 1 had errors - fix before proceeding to reclassification.")

# =============================================================================
# STEP 2: HAND OVER TO 02_reclassification.R
# =============================================================================
# 02 skips outputs that already exist, so any previous reclassified files for
# this layer are removed first. All other layers already have outputs and are
# skipped quickly, so in practice 02 only processes this layer.

cat("-------------------------------------------------------------\n")
cat("STEP 2: RECLASSIFY (via 02_reclassification.R)\n")
cat("-------------------------------------------------------------\n\n")

if (!file.exists(reclass_script)) {
  stop(sprintf("02_reclassification.R not found at: %s", reclass_script))
}

old_outputs <- unlist(lapply(target_provinces, function(prov) {
  file.path(reclass_dir, prov, c("bio", "refs", "agri"), paste0(LAYER_ID, ".tif"))
}))
old_outputs <- old_outputs[file.exists(old_outputs)]
if (length(old_outputs) > 0) {
  file.remove(old_outputs)
  cat(sprintf("  Removed %d previous reclassified file(s) for %s\n\n",
              length(old_outputs), LAYER_ID))
}

# Run 02 in its own environment so its variables do not overwrite ours
source(reclass_script, local = new.env())

# =============================================================================
# VALIDATION
# =============================================================================

cat("\n-------------------------------------------------------------\n")
cat(sprintf("VALIDATION: %s\n", LAYER_ID))
cat("-------------------------------------------------------------\n\n")

val_issues <- 0

for (comp in active_components) {
  for (prov in target_provinces) {
    f <- file.path(reclass_dir, prov, comp, paste0(LAYER_ID, ".tif"))
    if (!file.exists(f)) {
      cat(sprintf("  [MISSING] %s / %s\n", comp, prov))
      val_issues <- val_issues + 1
      next
    }
    check <- tryCatch({
      r    <- rast(f)
      vals <- spatSample(r, 5000, na.rm = FALSE)[,1]
      vals <- vals[!is.na(vals)]
      rm(r); gc()
      if (length(vals) == 0) stop("No valid values")
      rng  <- range(vals)
      if (rng[1] < 0 || rng[2] > 100)
        stop(sprintf("Values out of 0-100 range: %.2f to %.2f", rng[1], rng[2]))
      sprintf("OK | range: %.0f-%.0f | n_valid: %d", rng[1], rng[2], length(vals))
    }, error = function(e) paste("FAIL:", e$message))

    status <- if (grepl("^OK", check)) "OK" else "FAIL"
    if (status == "FAIL") val_issues <- val_issues + 1
    cat(sprintf("  [%s] %s / %s - %s\n", status, comp, prov, check))
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================\n")
cat(sprintf("  LAYER: %s\n", LAYER_ID))
cat("=============================================================\n")
cat(sprintf("  Step 1 (processed):  %d outputs, %d errors\n",
            length(target_provinces) - step1_errors, step1_errors))
cat(sprintf("  Validation issues:   %d\n", val_issues))

if (step1_errors == 0 && val_issues == 0) {
  cat("\n  Pipeline complete - inspect outputs in ArcGIS Pro\n")
  cat(sprintf("\n  Processed outputs:    %s\n",
              file.path(processed_dir, "<province>", paste0(LAYER_ID, ".tif"))))
  cat(sprintf("  Reclassified outputs: %s\n",
              file.path(reclass_dir, "<province>/<component>", paste0(LAYER_ID, ".tif"))))
  cat("\n  Next: run 03a (if weights need regenerating) and 03b onward.\n")
} else {
  cat("\n  ISSUES FOUND - review errors above before using in analysis\n")
}
