# =============================================================================
# 03c: QA CHECK - NoData WITHIN PROVINCIAL BOUNDARIES
# =============================================================================
#
# PURPOSE:
#   Quantifies how much NoData (NA) each reclassified layer contains INSIDE
#   each provincial boundary. This matters because 03b treats a layer that is
#   NA in a given cell as contributing a cost of 0 (its weight remains in the
#   denominator), so large internal gaps would bias composite costs downward.
#
# METHOD:
#   For each province, up to 100,000 planning units are drawn at random and
#   their centroids used as sample points. Because planning units are
#   equal-area hexagons, this is a uniform spatial sample of the province.
#   Each reclassified layer is then extracted at these points and the share
#   of NA values recorded.
#
#   Each layer is checked once per province: a layer used in several zones
#   has the same NA pattern in each (inversion does not change NA cells).
#
# INTERPRETING THE OUTPUT:
#   A small share of edge hexagon centroids fall just outside the rasterised
#   province boundary and return NA for EVERY layer. This produces a common
#   baseline (typically ~1.3-1.5%) shared by all layers in a province,
#   including binary layers whose NoData was set to 0 and therefore cannot
#   contain internal NA. The baseline is estimated as the median across all
#   layers in the province. Only NA above the baseline (excess_over_baseline)
#   reflects genuine gaps in a layer.
#
# PREREQUISITES:
#   Runs after 03b AND 06a, since it uses the planning unit files as its
#   sampling frame (it is numbered 03c because it checks 02/03b outputs).
#
# INPUT:
#   data/reclassified/{province}/{zone}/{layer_id}.tif   (from 02)
#   data/planning_units/{province}/pu_{province}.gpkg    (from 06a)
#
# OUTPUT:
#   data/composite/na_check_summary.csv
#
# RESULT AT TIME OF PUBLICATION:
#   Maximum excess NA over provincial baseline < 0.5 percentage points
#   (wind speed, North West), i.e. confined to boundary edges.
#
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(readr)
library(here)

# =============================================================================
# SETTINGS
# =============================================================================

N_SAMPLE <- 1e5   # planning units sampled per province
set.seed(1)       # fixed seed so the check is reproducible

# =============================================================================
# PATHS
# =============================================================================

base_dir    <- here::here()
reclass_dir <- file.path(base_dir, "data/reclassified")
pu_dir      <- file.path(base_dir, "data/planning_units")
out_path    <- file.path(base_dir, "data/composite/na_check_summary.csv")

provinces <- c("eastern_cape", "free_state", "gauteng", "kwazulu_natal",
               "limpopo", "mpumalanga", "north_west", "northern_cape",
               "western_cape")

# =============================================================================
# MAIN LOOP
# =============================================================================

out <- list()

for (prov in provinces) {

  cat(sprintf("Province: %s\n", prov))

  pu_path <- file.path(pu_dir, prov, paste0("pu_", prov, ".gpkg"))
  if (!file.exists(pu_path)) {
    cat(sprintf("  [SKIP] planning units not found: %s\n", pu_path))
    next
  }

  # Sample planning unit centroids as points inside the province
  pu  <- st_read(pu_path, quiet = TRUE)
  pu  <- pu[sample(nrow(pu), min(N_SAMPLE, nrow(pu))), ]
  pts <- vect(st_transform(st_centroid(st_geometry(pu)), 4326))

  # One file per layer, taken from whichever zone folder it appears in first
  files <- list.files(file.path(reclass_dir, prov), pattern = "\\.tif$",
                      recursive = TRUE, full.names = TRUE)
  files <- files[!duplicated(basename(files))]

  for (f in files) {
    v <- terra::extract(rast(f), pts, ID = FALSE)[, 1]
    out[[length(out) + 1]] <- data.frame(
      province      = prov,
      layer_id      = tools::file_path_sans_ext(basename(f)),
      pct_na_inside = round(100 * mean(is.na(v)), 2)
    )
  }

  cat(sprintf("  done: %d layers\n", length(files)))
}

# =============================================================================
# BASELINE ADJUSTMENT AND OUTPUT
# =============================================================================

res <- bind_rows(out) %>%
  group_by(province) %>%
  mutate(
    province_baseline    = median(pct_na_inside),
    excess_over_baseline = round(pmax(pct_na_inside - province_baseline, 0), 2)
  ) %>%
  ungroup() %>%
  arrange(desc(excess_over_baseline))

write_csv(res, out_path)

cat("\n=============================================================\n")
cat("  NA CHECK COMPLETE\n")
cat("=============================================================\n")
cat(sprintf("  Layer x province combinations checked: %d\n", nrow(res)))
cat(sprintf("  Maximum excess NA over baseline:       %.2f%%\n",
            max(res$excess_over_baseline)))
cat(sprintf("  Saved: %s\n\n", out_path))

cat("  Top 10 by excess NA over provincial baseline:\n")
print(as_tibble(res) %>%
        select(province, layer_id, pct_na_inside,
               province_baseline, excess_over_baseline) %>%
        head(10))
