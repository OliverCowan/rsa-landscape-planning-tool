# =============================================================================
# BIODIVERSITY OFFSET WORKFLOW — SCRIPT 01: SETUP & VEG TYPE EXTRACTION
# =============================================================================
#
# PURPOSE:
#   Given a target vegetation type (by T_MAPCODE or T_Name), this script:
#     1. Extracts matching NVM2024 polygons
#     2. Joins RLE 2021 threat status
#     3. Performs a fatal flaw check
#     4. Identifies which provinces the veg type overlaps
#     5. Clips NVM extract to each province
#     6. Calculates total potential extent and absolute conservation target
#     7. Saves all inputs needed for Script 02
#
# USER INPUT:
#   Set TARGET_MAPCODE or TARGET_NAME below. MAPCODE is preferred.
#
# OUTPUT:
#   outputs/{MAPCODE}/01_setup/
#     nvm_extract_{MAPCODE}.gpkg          Full NVM extract (all provinces)
#     nvm_{MAPCODE}_{province}.gpkg       Per-province NVM clip
#     offset_summary_{MAPCODE}.csv        Extent, target, RLE status
#     01_map_nvm_extent_{MAPCODE}.png     Map of veg type extent
#
# =============================================================================

library(terra)
library(sf)
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)

# =============================================================================
# USER INPUT — SET THESE
# =============================================================================

# Specify target vegetation type — MAPCODE preferred, name as fallback
TARGET_MAPCODE <- "Gm8"       # e.g. "SVmp1" | set to NULL to search by name
TARGET_NAME    <- NULL          # e.g. "Musina Mopane Bushveld" | used if MAPCODE is NULL

# Impact area (ha) — area of veg type impacted by development
# Used for reporting only (does not affect solve)
IMPACT_AREA_HA <- 150

# =============================================================================
# PATHS
# =============================================================================

base_dir    <- "C:/Users/OliverCowan/OneDrive - EWT/Documents/02. Data Analyses/03. JRS 30x30/R Workflow_30x30"
offset_dir  <- file.path(base_dir, "Biodiversity_Offset_Workflow")
input_dir   <- file.path(offset_dir, "input")
bounds_dir  <- file.path(base_dir, "data/boundaries")

nvm_path    <- file.path(input_dir, "NVM2024/NVM2024Final_IEM5_12_07012025.shp")
rle_path    <- file.path(input_dir, "RLE2021/RLE_Terr_2021_June2021_Remnants_ddw.dbf")

ALBERS_CRS  <- "ESRI:102022"

provinces_list <- c(
  "eastern_cape", "free_state", "gauteng", "kwazulu_natal",
  "limpopo", "mpumalanga", "north_west", "northern_cape", "western_cape"
)

PROVINCE_LABELS <- c(
  "eastern_cape"  = "Eastern Cape",
  "free_state"    = "Free State",
  "gauteng"       = "Gauteng",
  "kwazulu_natal" = "KwaZulu-Natal",
  "limpopo"       = "Limpopo",
  "mpumalanga"    = "Mpumalanga",
  "north_west"    = "North West",
  "northern_cape" = "Northern Cape",
  "western_cape"  = "Western Cape"
)

# =============================================================================
# STEP 1: LOAD NVM AND RLE
# =============================================================================

cat("=============================================================\n")
cat("  BIODIVERSITY OFFSET WORKFLOW — SCRIPT 01: SETUP\n")
cat("=============================================================\n\n")

cat("Loading NVM 2024...\n")
nvm <- vect(nvm_path)
cat(sprintf("  Loaded: %d features\n", nrow(nvm)))

cat("Loading RLE 2021...\n")
rle <- read.dbf(rle_path)
# Deduplicate RLE — keep one row per MAPCODE18 (some mapcodes repeated per polygon)
rle_unique <- rle %>%
  select(MAPCODE18, RLE2021, NAME) %>%
  distinct(MAPCODE18, .keep_all = TRUE)
cat(sprintf("  Loaded: %d unique veg types with RLE status\n\n", nrow(rle_unique)))

# =============================================================================
# STEP 2: IDENTIFY TARGET VEG TYPE
# =============================================================================

nvm_df <- as.data.frame(nvm)

if (!is.null(TARGET_MAPCODE)) {
  matches <- nvm_df[!is.na(nvm_df$T_MAPCODE) &
                      toupper(nvm_df$T_MAPCODE) == toupper(TARGET_MAPCODE), ]
  mapcode <- TARGET_MAPCODE
} else if (!is.null(TARGET_NAME)) {
  matches <- nvm_df[grepl(TARGET_NAME, nvm_df$T_Name, ignore.case = TRUE), ]
  mapcode <- unique(matches$T_MAPCODE)[1]
} else {
  stop("Specify either TARGET_MAPCODE or TARGET_NAME")
}

if (nrow(matches) == 0) stop(sprintf("No NVM features found for: %s", TARGET_MAPCODE))

veg_name   <- unique(matches$T_Name)[1]
nvm_target <- as.numeric(unique(matches$T_CNSRV_TR)[1])
biome      <- unique(matches$T_BIOME)[1]
bioregion  <- unique(matches$T_BIOREGIO)[1]

cat(sprintf("Target vegetation type identified:\n"))
cat(sprintf("  Name:      %s\n", veg_name))
cat(sprintf("  MAPCODE:   %s\n", mapcode))
cat(sprintf("  Biome:     %s\n", biome))
cat(sprintf("  Bioregion: %s\n", bioregion))
cat(sprintf("  NVM conservation target: %d%%\n\n", nvm_target))

# =============================================================================
# STEP 3: JOIN RLE 2021 THREAT STATUS
# =============================================================================

cat("Joining RLE 2021 threat status...\n")
rle_match <- rle_unique[toupper(rle_unique$MAPCODE18) == toupper(mapcode), ]

if (nrow(rle_match) == 0) {
  cat(sprintf("  [WARNING] No RLE 2021 match found for MAPCODE: %s\n", mapcode))
  cat(sprintf("  Using NVM T_CNSRV_TR target: %d%%\n", nvm_target))
  rle_status       <- "Unknown"
  conservation_target <- nvm_target
  target_source    <- "NVM2024 T_CNSRV_TR (NBA 2011)"
} else {
  rle_status          <- rle_match$RLE2021[1]
  conservation_target <- nvm_target  # NVM target used for prioritizr
  target_source       <- "NVM2024 T_CNSRV_TR (NBA 2011) | RLE status from RLE2021"
  cat(sprintf("  RLE 2021 status: %s\n", rle_status))
  cat(sprintf("  Conservation target (NVM): %d%%\n\n", conservation_target))
}

# RLE status descriptions
rle_desc <- c(
  "CR" = "Critically Endangered",
  "EN" = "Endangered",
  "VU" = "Vulnerable",
  "LC" = "Least Concern",
  "NE" = "Not Evaluated",
  "Unknown" = "Unknown"
)
rle_full <- ifelse(rle_status %in% names(rle_desc),
                   rle_desc[rle_status], rle_status)

# =============================================================================
# STEP 4: FATAL FLAW CHECK
# =============================================================================

cat("Performing fatal flaw check...\n")

fatal_flaw <- FALSE
fatal_flaw_reason <- NULL

if (rle_status == "CR") {
  # Calculate total remaining extent from NVM
  total_extent_m2 <- sum(matches$Shape_Area, na.rm = TRUE)
  total_extent_ha <- total_extent_m2 / 10000
  impact_pct      <- (IMPACT_AREA_HA / total_extent_ha) * 100
  
  cat(sprintf("  CRITICALLY ENDANGERED ecosystem detected\n"))
  cat(sprintf("  Total potential extent: %.0f ha\n", total_extent_ha))
  cat(sprintf("  Impact area: %.0f ha (%.1f%% of total extent)\n",
              IMPACT_AREA_HA, impact_pct))
  
  if (impact_pct > 5) {
    fatal_flaw <- TRUE
    fatal_flaw_reason <- sprintf(
      "Critically Endangered ecosystem (%s). Impact (%.0f ha) represents %.1f%% of total extent. Likely constitutes irreplaceable biodiversity loss.",
      veg_name, IMPACT_AREA_HA, impact_pct)
    cat(sprintf("\n  *** POTENTIAL FATAL FLAW DETECTED ***\n"))
    cat(sprintf("  %s\n", fatal_flaw_reason))
    cat(sprintf("  Offset may not be permissible. Consult conservation authority.\n\n"))
  } else {
    cat(sprintf("  Impact is small relative to total extent — offset may still be feasible.\n"))
    cat(sprintf("  Consult conservation authority given CR status.\n\n"))
  }
} else {
  cat(sprintf("  RLE status: %s — no fatal flaw triggered\n\n", rle_full))
}

# =============================================================================
# STEP 5: EXTRACT NVM POLYGONS AND REPROJECT
# =============================================================================

cat("Extracting NVM polygons for target veg type...\n")
nvm_extract <- nvm[!is.na(nvm$T_MAPCODE) &
                     toupper(nvm$T_MAPCODE) == toupper(mapcode), ]
nvm_extract <- project(nvm_extract, ALBERS_CRS)
cat(sprintf("  Extracted: %d polygons\n", nrow(nvm_extract)))

# Total potential extent in ha
total_extent_ha <- sum(nvm_extract$Shape_Area, na.rm = TRUE) / 10000
cat(sprintf("  Total potential extent: %.0f ha (%.0f km²)\n\n",
            total_extent_ha, total_extent_ha / 100))

# =============================================================================
# STEP 6: IDENTIFY OVERLAPPING PROVINCES
# =============================================================================

cat("Identifying overlapping provinces...\n")
overlapping_provinces <- c()

for (prov in provinces_list) {
  shp <- file.path(bounds_dir, sprintf("%s.shp", prov))
  if (!file.exists(shp)) next
  
  prov_vect <- vect(shp)
  prov_vect <- project(prov_vect, ALBERS_CRS)
  
  # Check if any NVM polygons intersect this province
  int <- terra::intersect(nvm_extract, prov_vect)
  if (nrow(int) > 0) {
    overlapping_provinces <- c(overlapping_provinces, prov)
    cat(sprintf("  Overlaps: %s (%d polygons)\n",
                PROVINCE_LABELS[prov], nrow(int)))
  }
}

cat(sprintf("\n  Total provinces with overlap: %d\n\n",
            length(overlapping_provinces)))

if (length(overlapping_provinces) == 0) {
  stop("No province overlap found. Check CRS alignment.")
}

# =============================================================================
# STEP 7: SAVE OUTPUTS
# =============================================================================

out_dir <- file.path(offset_dir, "outputs", mapcode, "01_setup")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Save full NVM extract
nvm_extract_sf <- st_as_sf(nvm_extract)
nvm_extract_sf <- nvm_extract_sf[ , !names(nvm_extract_sf) %in% c("FID", "FID_")]
full_extract_path <- file.path(out_dir, sprintf("nvm_extract_%s.gpkg", mapcode))
if (file.exists(full_extract_path)) file.remove(full_extract_path)
st_write(nvm_extract_sf, full_extract_path, quiet = TRUE)
cat(sprintf("Saved full NVM extract\n"))

# Save per-province clips
for (prov in overlapping_provinces) {
  prov_vect <- vect(file.path(bounds_dir, sprintf("%s.shp", prov)))
  prov_vect <- project(prov_vect, ALBERS_CRS)
  
  nvm_clip    <- terra::intersect(nvm_extract, prov_vect)
  nvm_clip_sf <- st_as_sf(nvm_clip)
  # Drop FID column if present — causes GDAL type conflict on write
  nvm_clip_sf <- nvm_clip_sf[ , !names(nvm_clip_sf) %in% c("FID", "FID_")]
  
  out_path <- file.path(out_dir,
                        sprintf("nvm_%s_%s.gpkg", mapcode, prov))
  if (file.exists(out_path)) file.remove(out_path)
  st_write(nvm_clip_sf, out_path, quiet = TRUE)
  
  clip_ha <- sum(nvm_clip$Shape_Area, na.rm = TRUE) / 10000
  cat(sprintf("  Saved %s clip: %.0f ha\n", PROVINCE_LABELS[prov], clip_ha))
}

# Save summary CSV
abs_target_ha <- total_extent_ha * (conservation_target / 100)

summary_df <- data.frame(
  mapcode              = mapcode,
  veg_name             = veg_name,
  biome                = biome,
  bioregion            = bioregion,
  rle_status           = rle_status,
  rle_status_full      = rle_full,
  conservation_target_pct = conservation_target,
  target_source        = target_source,
  total_extent_ha      = round(total_extent_ha, 1),
  abs_target_ha        = round(abs_target_ha, 1),
  impact_area_ha       = IMPACT_AREA_HA,
  fatal_flaw           = fatal_flaw,
  fatal_flaw_reason    = ifelse(is.null(fatal_flaw_reason), "", fatal_flaw_reason),
  overlapping_provinces = paste(overlapping_provinces, collapse = ", "),
  n_provinces          = length(overlapping_provinces),
  date_run             = Sys.Date()
)

write_csv(summary_df,
          file.path(out_dir, sprintf("offset_summary_%s.csv", mapcode)))
cat(sprintf("\nSummary CSV saved\n"))

# =============================================================================
# STEP 8: EXTENT MAP
# =============================================================================

cat("Generating extent map...\n")

# Load SA outline and province boundaries
sa_sf <- st_read(file.path(bounds_dir, "sa_national.shp"), quiet = TRUE)

prov_sf_list <- lapply(provinces_list, function(p) {
  st_read(file.path(bounds_dir, sprintf("%s.shp", p)), quiet = TRUE) %>%
    mutate(province = p)
})
prov_sf <- bind_rows(prov_sf_list)

# Highlight overlapping provinces
prov_sf <- prov_sf %>%
  mutate(highlight = province %in% overlapping_provinces)

nvm_sf_wgs <- st_as_sf(project(nvm_extract, "EPSG:4326"))

p_extent <- ggplot() +
  geom_sf(data = sa_sf, fill = "#F5F5F5", colour = "#AAAAAA", linewidth = 0.3) +
  geom_sf(data = prov_sf %>% filter(highlight),
          fill = "#E3F2FD", colour = "#1565C0", linewidth = 0.5) +
  geom_sf(data = nvm_sf_wgs, fill = "#2E7D32", colour = NA, alpha = 0.8) +
  geom_sf(data = sa_sf, fill = NA, colour = "#555555", linewidth = 0.6) +
  labs(
    title    = sprintf("%s (%s)", veg_name, mapcode),
    subtitle = sprintf("RLE 2021: %s  |  Conservation target: %d%%  |  Total extent: %.0f ha",
                       rle_full, conservation_target, total_extent_ha),
    caption  = sprintf("Provinces: %s\nEndangered Wildlife Trust — Biodiversity Offset Workflow",
                       paste(PROVINCE_LABELS[overlapping_provinces], collapse = ", "))
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, colour = "#555555"),
    plot.caption    = element_text(size = 8, colour = "#777777", hjust = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(10, 10, 10, 10)
  )

ggsave(file.path(out_dir, sprintf("01_map_nvm_extent_%s.png", mapcode)),
       p_extent, width = 12, height = 9, dpi = 300, bg = "white")
cat("  Extent map saved\n")

# =============================================================================
# STEP 9: EXISTING PROTECTION ANALYSIS
# How much of the conservation target is already met by existing PAs
# within the veg type extent? This contextualises what the offset must secure.
# =============================================================================

cat("Calculating existing PA coverage within veg type...\n")

pa_gpkg <- file.path(base_dir, "data/locked_pus/protected_areas.gpkg")

pa_coverage_rows <- list()

if (!file.exists(pa_gpkg)) {
  cat("  [WARNING] Protected areas file not found — skipping PA coverage analysis\n")
} else {
  pa_sf <- st_read(pa_gpkg, quiet = TRUE) %>% st_make_valid()
  
  for (prov in overlapping_provinces) {
    
    nvm_clip_path <- file.path(out_dir,
                               sprintf("nvm_%s_%s.gpkg", mapcode, prov))
    if (!file.exists(nvm_clip_path)) next
    
    nvm_clip_prov <- st_read(nvm_clip_path, quiet = TRUE) %>%
      st_transform(st_crs(pa_sf))
    
    # Total veg area in this province
    prov_veg_area_ha <- as.numeric(sum(st_area(nvm_clip_prov))) / 10000
    
    # Intersect veg type with PAs
    pa_prov <- st_filter(pa_sf, nvm_clip_prov)
    
    if (nrow(pa_prov) == 0) {
      pa_area_ha    <- 0
      pa_pct        <- 0
    } else {
      nvm_valid     <- st_make_valid(st_union(nvm_clip_prov))
      pa_valid      <- st_make_valid(st_union(pa_prov))
      pa_int        <- st_intersection(nvm_valid, pa_valid)
      pa_area_ha    <- as.numeric(st_area(pa_int)) / 10000
      pa_pct        <- (pa_area_ha / prov_veg_area_ha) * 100
    }
    
    target_ha     <- prov_veg_area_ha * (conservation_target / 100)
    gap_ha        <- max(0, target_ha - pa_area_ha)
    gap_pct       <- (gap_ha / target_ha) * 100
    target_met    <- pa_area_ha >= target_ha
    
    cat(sprintf("  %s:\n", PROVINCE_LABELS[prov]))
    cat(sprintf("    Veg type area:       %.0f ha\n", prov_veg_area_ha))
    cat(sprintf("    Already in PAs:      %.0f ha (%.1f%%)\n", pa_area_ha, pa_pct))
    cat(sprintf("    Conservation target: %.0f ha (%d%%)\n", target_ha, conservation_target))
    cat(sprintf("    Remaining gap:       %.0f ha (%.1f%% of target)\n", gap_ha, gap_pct))
    cat(sprintf("    Target already met:  %s\n\n", ifelse(target_met, "YES", "No")))
    
    pa_coverage_rows[[prov]] <- data.frame(
      mapcode              = mapcode,
      veg_name             = veg_name,
      province             = PROVINCE_LABELS[prov],
      prov_veg_area_ha     = round(prov_veg_area_ha, 1),
      pa_area_ha           = round(pa_area_ha, 1),
      pa_coverage_pct      = round(pa_pct, 1),
      conservation_target_pct = conservation_target,
      target_ha            = round(target_ha, 1),
      gap_ha               = round(gap_ha, 1),
      gap_pct_of_target    = round(gap_pct, 1),
      target_already_met   = target_met
    )
  }
  
  if (length(pa_coverage_rows) > 0) {
    pa_coverage_df <- bind_rows(pa_coverage_rows)
    write_csv(pa_coverage_df,
              file.path(out_dir, sprintf("pa_coverage_%s.csv", mapcode)))
    cat("  PA coverage summary saved\n")
  }
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n=============================================================\n")
cat("  SETUP COMPLETE\n")
cat("=============================================================\n\n")
cat(sprintf("  Vegetation type:     %s\n", veg_name))
cat(sprintf("  MAPCODE:             %s\n", mapcode))
cat(sprintf("  RLE 2021 status:     %s (%s)\n", rle_status, rle_full))
cat(sprintf("  Conservation target: %d%%\n", conservation_target))
cat(sprintf("  Total extent:        %.0f ha\n", total_extent_ha))
cat(sprintf("  Target area:         %.0f ha\n", abs_target_ha))
cat(sprintf("  Fatal flaw:          %s\n", ifelse(fatal_flaw, "YES — REVIEW REQUIRED", "No")))
cat(sprintf("  Provinces:           %s\n",
            paste(PROVINCE_LABELS[overlapping_provinces], collapse = ", ")))
cat(sprintf("\n  Outputs saved to: %s\n", out_dir))
cat(sprintf("\n  >>> Ready for Script 02: Offset Solve\n\n"))