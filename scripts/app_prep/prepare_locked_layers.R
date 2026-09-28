# =============================================================================
# APP DATA PREPARATION: LOCKED-AREA DISPLAY LAYERS
# =============================================================================
#
# PURPOSE:
#   Builds lightweight map overlays of the locked planning units (Protected
#   Areas, REFS facilities, Agriculture) for display in the Shiny app.
#   Display only: nothing here affects the optimisation.
#
#   PA and REFS: locked PUs dissolved to a single boundary, then simplified
#   Agriculture: individual PUs, lightly simplified (too fragmented to
#                dissolve efficiently; individual PUs clip quickly by bbox)
#
# SOURCE OF LOCKED PUS:
#   Built from the locked_pa / locked_refs / locked_agri columns of the app's
#   national planning unit file (prepare_v2_planning_units.R), so the overlay
#   always shows exactly the PUs locked in the optimisation. The app clips
#   these national layers to provincial or custom scopes at runtime.
#
# WHEN TO RUN:
#   After prepare_v2_planning_units.R, whenever lock columns change
#   (i.e. after re-running 06b).
#
# CHANGE LOG:
#   v2: Previously read pre-made PU-level locked layers from
#       data/spatial/*.gpkg, which were not produced by any workflow script
#       and could fall out of step with the lock columns. Now derived
#       directly from the planning units.
#
# INPUT:
#   {APP_DATA_DIR}/provinces/national/spatial/planning_units.gpkg
#
# OUTPUT:
#   {APP_DATA_DIR}/spatial/display/protected_areas_display.gpkg
#   {APP_DATA_DIR}/spatial/display/refs_facilities_display.gpkg
#   {APP_DATA_DIR}/spatial/display/agriculture_zones_display.gpkg
#
# =============================================================================

library(sf)
library(here)
sf::sf_use_s2(FALSE)  # planar GEOS, consistent with the app

# =============================================================================
# PATHS
# =============================================================================

base_dir     <- here::here()
APP_DATA_DIR <- file.path(base_dir, "data/app")   # must match app.R
PU_PATH      <- file.path(APP_DATA_DIR, "provinces/national/spatial/planning_units.gpkg")
DISPLAY_DIR  <- file.path(APP_DATA_DIR, "spatial/display")
dir.create(DISPLAY_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(PU_PATH)) {
  stop("National app planning units not found (run prepare_v2_planning_units.R): ", PU_PATH)
}

cat("Reading national planning units...\n")
pu <- st_read(PU_PATH, quiet = TRUE)
cat(sprintf("  %d planning units\n", nrow(pu)))

# =============================================================================
# PA and REFS: dissolve to single boundary, then simplify
# =============================================================================

dissolved_layers <- list(
  list(col = "locked_pa",   label = "Protected Areas",
       output = file.path(DISPLAY_DIR, "protected_areas_display.gpkg"), tolerance = 500),
  list(col = "locked_refs", label = "REFS Facilities",
       output = file.path(DISPLAY_DIR, "refs_facilities_display.gpkg"), tolerance = 500)
)

for (lyr in dissolved_layers) {
  cat(sprintf("\nProcessing: %s (dissolve + simplify)\n", lyr$label))

  locked <- st_as_sf(st_geometry(pu[pu[[lyr$col]] == 1, ]))
  cat(sprintf("  Locked PUs: %d\n", nrow(locked)))
  if (nrow(locked) == 0) { cat("  [SKIP] none locked\n"); next }

  locked <- st_transform(locked, 3857)

  cat("  Dissolving to single boundary...\n")
  dissolved <- st_make_valid(st_as_sf(st_union(locked)))

  cat(sprintf("  Simplifying (tolerance = %dm)...\n", lyr$tolerance))
  simplified <- st_simplify(dissolved, dTolerance = lyr$tolerance, preserveTopology = TRUE)
  simplified <- st_make_valid(simplified)
  simplified <- simplified[!st_is_empty(simplified), ]
  simplified <- st_make_valid(st_transform(simplified, 4326))

  st_write(simplified, lyr$output, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("  Written to %s (%.2f MB)\n", basename(lyr$output),
              file.size(lyr$output) / 1024^2))
}

# =============================================================================
# Agriculture: individual PUs, light simplification only
# =============================================================================
# Dissolving produces a very large multipolygon that is slow to clip.
# Individual PUs clip efficiently via st_crop() (bbox).

cat("\nProcessing: Agriculture (individual PUs + light simplify)\n")

agri <- st_as_sf(st_geometry(pu[pu$locked_agri == 1, ]))
cat(sprintf("  Locked PUs: %d\n", nrow(agri)))

if (nrow(agri) > 0) {
  agri <- st_transform(agri, 3857)
  # 100 m simplification smooths hex edges without losing individual PUs
  agri <- st_simplify(agri, dTolerance = 100, preserveTopology = TRUE)
  agri <- st_make_valid(agri)
  agri <- agri[!st_is_empty(agri), ]
  agri <- st_make_valid(st_transform(agri, 4326))

  agri_out <- file.path(DISPLAY_DIR, "agriculture_zones_display.gpkg")
  st_write(agri, agri_out, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("  Written to %s (%.2f MB)\n", basename(agri_out),
              file.size(agri_out) / 1024^2))
}

cat("\n=== DONE ===\n")
cat(sprintf("Display layers saved to %s\n", DISPLAY_DIR))
