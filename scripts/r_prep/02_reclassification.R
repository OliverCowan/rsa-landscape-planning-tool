# =============================================================================
# 02: RECLASSIFICATION OF PROCESSED RASTERS
# =============================================================================
#
# ENTRY POINT OF THE WORKFLOW
#   This is the first scripted step. It assumes every input cost layer has
#   already been "processed", meaning that for each layer and each planning
#   scope (national plus the nine provinces) a single-band GeoTIFF exists at:
#
#     data/processed/{province}/{layer_id}.tif
#
#   and that each of these rasters has been:
#     - reprojected to WGS84 (EPSG:4326)
#     - resampled and snapped to the SA national template grid
#       (data/sa_national_template.tif, ~90 m / 0.000878 decimal degrees)
#     - clipped and masked to the relevant national or provincial boundary
#     - rasterised first, if the source data was a vector layer
#
#   Values are left in their original units (e.g. species counts, wind speed,
#   class codes). Rescaling to 0-100 happens in this script, not before.
#   The {layer_id} file name must exactly match the layer_id column in
#   layer_config_template.csv.
#
# LAYER REGISTRY: layer_config_template.csv (repo root)
#   One row per input layer. This script reads the following columns:
#     layer_id           unique short name; matches the .tif file name
#     status             only rows marked "confirmed" are processed
#     processing_method  binary / manual / jenks
#     jenks_classes      number of Jenks classes (defaults to 5 if blank)
#     manual_values      "value=score;value=score" remap string (manual only)
#     zone               zones the layer feeds: bio, refs, agri, or "all"
#                        (multiple zones separated by ";")
#     invert_bio / invert_refs / invert_agri
#                        TRUE where the layer is a benefit for that zone and
#                        must be inverted (100 - value) to act as a cost
#     national, eastern_cape, ... western_cape
#                        TRUE/FALSE: which scopes the layer applies to
#   The default per-zone weights (component_weight_bio / _refs / _agri)
#   are also stored here but are read by 03a, not by this script.
#   To add a new layer, add a row here and place the processed rasters in
#   data/processed/ (see also add_new_cost_layer.R).
#
# PURPOSE:
#   Reclassifies each standardised input layer to a common 0-100 cost scale,
#   separately for each zone (bio / refs / agri) in which the layer is used.
#   Three methods are supported, set per layer in layer_config_template.csv:
#     binary : presence = 100, absence = 0
#     manual : categorical remap from a "value=score;value=score" string
#     jenks  : continuous values split into n Jenks natural breaks classes,
#              then linearly rescaled to 0-100
#   Benefit layers are inverted (100 - value) per zone where flagged in the
#   config, so that all layers contribute to cost in a consistent direction.
#
# INPUT:
#   layer_config_template.csv                     (layer registry)
#   data/processed/{province}/{layer_id}.tif      (standardised to template grid)
#
# OUTPUT:
#   data/reclassified/{province}/{zone}/{layer_id}.tif
#   data/reclassified/reclassification_log.csv
#
# NOTES:
#   Re-running is safe: outputs that already exist are skipped.
#   Jenks breaks are computed once per layer per province from a 10,000 cell
#   sample and reused across all zones for that layer.
#
# =============================================================================

library(terra)
library(readr)
library(dplyr)
library(classInt)
library(here)

# --- PATHS -------------------------------------------------------------------

base_dir      <- here::here()
processed_dir <- file.path(base_dir, "data/processed")
reclass_dir   <- file.path(base_dir, "data/reclassified")
config_path   <- file.path(base_dir, "layer_config_template.csv")

# --- SETUP -------------------------------------------------------------------

tmp_dir <- file.path(tempdir(), "terra_tmp")
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
terraOptions(memfrac = 0.8, tempdir = tmp_dir)

province_map <- c("national", "eastern_cape", "free_state", "gauteng",
                  "kwazulu_natal", "limpopo", "mpumalanga", "north_west",
                  "northern_cape", "western_cape")

config <- read_csv(config_path, show_col_types = FALSE) %>%
  filter(status == "confirmed")

cat(sprintf("Loaded config: %d confirmed layers\n\n", nrow(config)))

# --- HELPER: Parse manual_values string --------------------------------------

parse_manual_values <- function(manual_str) {
  pairs  <- strsplit(trimws(manual_str), ";")[[1]]
  result <- c()
  for (pair in pairs) {
    parts <- strsplit(trimws(pair), "=")[[1]]
    if (length(parts) != 2) next
    key        <- trimws(parts[1])
    result[key] <- as.numeric(trimws(parts[2]))
  }
  return(result)
}

# --- HELPER: Compute jenks breaks --------------------------------------------
# Returns NULL if insufficient unique values

compute_jenks_breaks <- function(vals, jenks_n) {
  unique_vals <- unique(vals[!is.na(vals)])
  n_unique    <- length(unique_vals)

  if (n_unique == 0) return(NULL)
  if (n_unique == 1) return(list(type = "single", value = unique_vals))

  # Cap jenks classes to number of unique values
  n_classes <- min(jenks_n, n_unique)
  breaks    <- suppressWarnings(
    classIntervals(vals[!is.na(vals)], n = n_classes, style = "jenks")$brks
  )
  breaks <- unique(breaks)
  return(list(type = "jenks", breaks = breaks, n_classes = length(breaks) - 1))
}

# --- HELPER: Reclassify raster -----------------------------------------------

reclass_raster <- function(r, method, invert, manual_str = NULL,
                           jenks_breaks = NULL) {
  if (method == "binary") {
    r_out <- ifel(r == 0, 0, 100)

  } else if (method == "manual") {
    if (is.null(manual_str) || is.na(manual_str) || trimws(manual_str) == "")
      stop("manual_values required for method='manual'")

    int_map <- parse_manual_values(manual_str)
    from    <- as.numeric(names(int_map))
    to      <- as.numeric(unlist(int_map))

    r_out <- subst(r, from, to)

  } else if (method == "jenks") {
    if (is.null(jenks_breaks)) stop("jenks_breaks required")

    if (jenks_breaks$type == "single") {
      # Single unique value: assign 0
      r_out <- ifel(!is.na(r), 0, NA)

    } else {
      breaks     <- jenks_breaks$breaks
      n_classes  <- jenks_breaks$n_classes
      class_vals <- round(seq(0, 100, length.out = n_classes))
      rcl <- matrix(ncol = 3, nrow = n_classes)
      for (j in 1:n_classes) {
        rcl[j,] <- c(breaks[j], breaks[j+1], class_vals[j])
      }
      rcl[n_classes, 2] <- rcl[n_classes, 2] + 0.001
      r_out <- classify(r, rcl, include.lowest = TRUE, others = NA)
    }

  } else {
    stop(sprintf("Unknown processing_method: '%s'", method))
  }

  # Apply inversion
  if (isTRUE(invert)) r_out <- 100 - r_out

  # Preserve NoData mask
  r_out <- mask(r_out, r)

  return(r_out)
}

# --- TRACKING ----------------------------------------------------------------

log_rows        <- list()
skipped_missing <- c()

# --- MAIN LOOP ---------------------------------------------------------------

for (i in seq_len(nrow(config))) {

  row         <- config[i, ]
  layer_id    <- row$layer_id
  method      <- tolower(trimws(row$processing_method))
  jenks_n     <- ifelse(is.na(row$jenks_classes), 5, as.integer(row$jenks_classes))
  manual_vals <- ifelse(is.na(row$manual_values), "", trimws(row$manual_values))

  # Active components
  zone <- tolower(trimws(row$zone))
  active_components <- if (grepl("all", zone)) {
    c("bio", "refs", "agri")
  } else {
    trimws(strsplit(zone, ";")[[1]])
  }

  # Target provinces
  target_provinces <- province_map[sapply(province_map, function(p) {
    col_val <- row[[p]]
    !is.na(col_val) && as.logical(col_val)
  })]

  cat(sprintf("\n[%d/%d] %s | method: %s | components: %s\n",
              i, nrow(config), layer_id, method,
              paste(active_components, collapse = "/")))

  for (prov in target_provinces) {

    in_path <- file.path(processed_dir, prov, paste0(layer_id, ".tif"))

    if (!file.exists(in_path)) {
      skipped_missing <- c(skipped_missing, sprintf("%s / %s", layer_id, prov))
      cat(sprintf("  [SKIP - no processed file] %s\n", prov))
      next
    }

    # Check if ALL components already done for this province: skip raster load
    all_done <- all(sapply(active_components, function(comp) {
      file.exists(file.path(reclass_dir, prov, comp, paste0(layer_id, ".tif")))
    }))
    if (all_done) {
      cat(sprintf("  [SKIP - all done] %s\n", prov))
      for (comp in active_components) {
        log_rows[[length(log_rows)+1]] <- data.frame(
          layer_id = layer_id, province = prov,
          component = comp, status = "skipped")
      }
      next
    }

    # Load raster ONCE per province
    r <- tryCatch(rast(in_path),
                  error = function(e) {
                    cat(sprintf("  [ERROR loading] %s: %s\n", prov, e$message))
                    NULL
                  })
    if (is.null(r)) next

    # Compute jenks breaks ONCE per province (reused across all components)
    jenks_breaks <- NULL
    if (method == "jenks") {
      vals         <- spatSample(r, 10000, na.rm = FALSE)[,1]
      jenks_breaks <- compute_jenks_breaks(vals, jenks_n)
      if (is.null(jenks_breaks)) {
        cat(sprintf("  [ERROR] No valid values in %s\n", prov))
        rm(r); gc()
        next
      }
      if (jenks_breaks$type == "single") {
        cat(sprintf("  [JENKS FALLBACK] %s - single value, assigning 0\n", prov))
      }
    }

    # Process each component
    for (comp in active_components) {

      out_dir  <- file.path(reclass_dir, prov, comp)
      out_path <- file.path(out_dir, paste0(layer_id, ".tif"))
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

      if (file.exists(out_path)) {
        cat(sprintf("  [SKIP - exists] %s / %s\n", prov, comp))
        log_rows[[length(log_rows)+1]] <- data.frame(
          layer_id = layer_id, province = prov,
          component = comp, status = "skipped")
        next
      }

      invert <- isTRUE(as.logical(row[[paste0("invert_", comp)]]))

      result <- tryCatch({
        r_out <- reclass_raster(r, method, invert, manual_vals, jenks_breaks)
        writeRaster(r_out, out_path, overwrite = TRUE,
                    gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
        rm(r_out); gc()
        "done"
      }, error = function(e) paste0("error: ", e$message))

      cat(sprintf("  [%-6s] %s / %s | invert: %s\n",
                  toupper(result), prov, comp, invert))

      log_rows[[length(log_rows)+1]] <- data.frame(
        layer_id = layer_id, province = prov,
        component = comp, status = result)
    }

    rm(r); gc()
  }

  unlink(list.files(tmp_dir, full.names = TRUE, recursive = TRUE))
}

# --- SAVE LOG ----------------------------------------------------------------

log_df   <- bind_rows(log_rows)
log_path <- file.path(reclass_dir, "reclassification_log.csv")
write_csv(log_df, log_path)

# --- SUMMARY -----------------------------------------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("RECLASSIFICATION COMPLETE\n\n")
cat(sprintf("  Done:         %d\n", sum(log_df$status == "done")))
cat(sprintf("  Skipped:      %d  (already existed)\n", sum(log_df$status == "skipped")))
cat(sprintf("  Errors:       %d\n", sum(grepl("^error", log_df$status))))

if (length(skipped_missing) > 0) {
  cat(sprintf("\n  Skipped - no processed file (%d):\n", length(skipped_missing)))
  for (s in skipped_missing) cat(sprintf("    %s\n", s))
}

if (any(grepl("^error", log_df$status))) {
  cat("\n  ERRORS:\n")
  print(as.data.frame(log_df %>% filter(grepl("^error", status))), row.names = FALSE)
}
