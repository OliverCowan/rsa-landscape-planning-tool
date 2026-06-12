# =============================================================================
# BIODIVERSITY OFFSET WORKFLOW — SCRIPT 03: OUTPUTS & REPORTING
# =============================================================================
#
# PURPOSE:
#   Generates publication-quality maps zoomed to the vegetation type extent,
#   and a summary report with correct area statistics.
#
# OUTPUT:
#   outputs/{MAPCODE}/03_outputs/
#     map_offset_priority_{MAPCODE}_{province}.png
#     map_offset_national_{MAPCODE}.png
#     offset_report_{MAPCODE}.csv
#
# =============================================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(ggplot2)

# =============================================================================
# USER INPUT
# =============================================================================

TARGET_MAPCODE <- "Gm8"   # Must match Scripts 01 and 02
IMPACT_AREA_HA <- NULL     # Set to actual impact area (ha) when known e.g. 150

# NOTE — Cross-province targets:
# Where a veg type spans multiple provinces, the conservation target is applied
# independently per province based on the veg area within that province's PUs.
# For most offset cases the veg type will fall within a single province.
# Where a province contains only a small sliver (e.g. Gauteng for Gm12),
# review whether that province's result is ecologically meaningful before use.

# =============================================================================
# PATHS
# =============================================================================

base_dir   <- "C:/Users/OliverCowan/OneDrive - EWT/Documents/02. Data Analyses/03. JRS 30x30/R Workflow_30x30"
offset_dir <- file.path(base_dir, "Biodiversity_Offset_Workflow")
setup_dir  <- file.path(offset_dir, "outputs", TARGET_MAPCODE, "01_setup")
solve_dir  <- file.path(offset_dir, "outputs", TARGET_MAPCODE, "02_solve")
out_dir    <- file.path(offset_dir, "outputs", TARGET_MAPCODE, "03_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

bounds_dir <- file.path(base_dir, "data/boundaries")

# Colours
COL_CONSERVATION <- "#1B5E20"
COL_REFS         <- "#1565C0"
COL_AGRI         <- "#F9A825"
COL_UNSELECTED   <- "#EEEEEE"

PROVINCE_LABELS <- c(
  "eastern_cape"  = "Eastern Cape",   "free_state"    = "Free State",
  "gauteng"       = "Gauteng",        "kwazulu_natal" = "KwaZulu-Natal",
  "limpopo"       = "Limpopo",        "mpumalanga"    = "Mpumalanga",
  "north_west"    = "North West",     "northern_cape" = "Northern Cape",
  "western_cape"  = "Western Cape"
)

# =============================================================================
# LOAD SETUP SUMMARY
# =============================================================================

cat("=============================================================\n")
cat("  BIODIVERSITY OFFSET WORKFLOW — SCRIPT 03: OUTPUTS\n")
cat("=============================================================\n\n")

offset_summary <- read_csv(
  file.path(setup_dir, sprintf("offset_summary_%s.csv", TARGET_MAPCODE)),
  show_col_types = FALSE
)

veg_name              <- offset_summary$veg_name[1]
rle_status            <- offset_summary$rle_status[1]
rle_full              <- offset_summary$rle_status_full[1]
conservation_target   <- offset_summary$conservation_target_pct[1]
total_extent_ha       <- offset_summary$total_extent_ha[1]
fatal_flaw            <- offset_summary$fatal_flaw[1]
overlapping_provinces <- strsplit(offset_summary$overlapping_provinces[1], ", ")[[1]]

cat(sprintf("Generating outputs for: %s (%s)\n\n", veg_name, TARGET_MAPCODE))

# =============================================================================
# LOAD NVM EXTENT AND SA BOUNDARIES
# =============================================================================

nvm_extract <- st_read(
  file.path(setup_dir, sprintf("nvm_extract_%s.gpkg", TARGET_MAPCODE)),
  quiet = TRUE
)

sa_sf <- st_read(file.path(bounds_dir, "sa_national.shp"), quiet = TRUE)

prov_sf <- bind_rows(lapply(names(PROVINCE_LABELS), function(p) {
  f <- file.path(bounds_dir, sprintf("%s.shp", p))
  if (!file.exists(f)) return(NULL)
  st_read(f, quiet = TRUE) %>% mutate(province = p)
}))

# =============================================================================
# LOAD SOLUTIONS AND COMPUTE STATS
# =============================================================================

sol_list    <- list()
report_rows <- list()

for (prov in overlapping_provinces) {
  
  sol_path <- file.path(solve_dir,
                        sprintf("solution_%s_%s.gpkg", TARGET_MAPCODE, prov))
  if (!file.exists(sol_path)) {
    cat(sprintf("  [SKIP] Solution not found for %s\n", prov))
    next
  }
  
  sol <- st_read(sol_path, quiet = TRUE)
  sol_list[[prov]] <- sol
  
  # Load NVM clip for this province to get actual veg area
  nvm_clip_path <- file.path(setup_dir,
                             sprintf("nvm_%s_%s.gpkg", TARGET_MAPCODE, prov))
  nvm_clip <- st_read(nvm_clip_path, quiet = TRUE)
  prov_veg_area_ha <- as.numeric(sum(st_area(nvm_clip))) / 10000
  
  # Compute stats from solution
  # offset_priority = 1 means selected for Conservation and NOT a locked PA
  n_priority      <- sum(sol$offset_priority == 1, na.rm = TRUE)
  area_priority_ha <- sum(sol$Area_km2[sol$offset_priority == 1],
                          na.rm = TRUE) * 100
  
  # All Conservation-selected PUs (including locked PAs)
  n_cons_total     <- sum(sol$zone_assignment == "Conservation", na.rm = TRUE)
  area_cons_total  <- sum(sol$Area_km2[sol$zone_assignment == "Conservation"],
                          na.rm = TRUE) * 100
  
  # Target for this province
  prov_target_ha   <- prov_veg_area_ha * (conservation_target / 100)
  pct_met          <- round((area_cons_total / prov_target_ha) * 100, 1)
  
  cat(sprintf("  %s: %d priority PUs | %.0f ha selected | %.0f ha target | %.1f%% met\n",
              PROVINCE_LABELS[prov], n_priority, area_cons_total,
              prov_target_ha, pct_met))
  
  report_rows[[prov]] <- data.frame(
    mapcode                 = TARGET_MAPCODE,
    veg_name                = veg_name,
    province                = PROVINCE_LABELS[prov],
    rle_status              = rle_status,
    rle_status_full         = rle_full,
    conservation_target_pct = conservation_target,
    total_national_extent_ha = total_extent_ha,
    prov_veg_area_ha        = round(prov_veg_area_ha, 1),
    prov_target_ha          = round(prov_target_ha, 1),
    impact_area_ha          = ifelse(is.null(IMPACT_AREA_HA), NA, IMPACT_AREA_HA),
    fatal_flaw              = fatal_flaw,
    n_offset_pu             = n_priority,
    area_offset_ha          = round(area_priority_ha, 1),
    area_conservation_ha    = round(area_cons_total, 1),
    pct_target_met          = pct_met
  )
}

# =============================================================================
# FIGURE 1: PROVINCIAL PRIORITY MAPS — ZOOMED TO VEG TYPE EXTENT
# =============================================================================

cat("\nGenerating provincial priority maps...\n")

for (prov in names(sol_list)) {
  
  sol      <- sol_list[[prov]]
  prov_bound <- prov_sf[prov_sf$province == prov, ]
  
  nvm_clip <- st_read(
    file.path(setup_dir, sprintf("nvm_%s_%s.gpkg", TARGET_MAPCODE, prov)),
    quiet = TRUE
  ) %>% st_transform(st_crs(sol))
  
  # Zoom to NVM veg type extent + 15% buffer — no clipping needed
  nvm_bbox  <- st_bbox(nvm_clip)
  buf       <- max(nvm_bbox["xmax"] - nvm_bbox["xmin"],
                   nvm_bbox["ymax"] - nvm_bbox["ymin"]) * 0.15
  zoom_xlim <- c(nvm_bbox["xmin"] - buf, nvm_bbox["xmax"] + buf)
  zoom_ylim <- c(nvm_bbox["ymin"] - buf, nvm_bbox["ymax"] + buf)
  sol_zoom  <- sol  # coord_sf handles zoom, no spatial filter needed
  
  # Stats for subtitle
  rep        <- report_rows[[prov]]
  pct_met    <- rep$pct_target_met
  area_cons  <- rep$area_conservation_ha
  prov_target <- rep$prov_target_ha
  
  p_map <- ggplot() +
    geom_sf(data = sol_zoom, aes(fill = zone_assignment), colour = NA) +
    geom_sf(data = nvm_clip, fill = NA, colour = "#4CAF50",
            linewidth = 0.5, linetype = "dashed") +
    geom_sf(data = prov_bound %>% st_transform(st_crs(sol)),
            fill = NA, colour = "#222222", linewidth = 0.8) +
    scale_fill_manual(
      values = c(
        "Conservation" = COL_CONSERVATION,
        "Agriculture"  = COL_AGRI,
        "REFS"         = COL_REFS,
        "Not selected" = COL_UNSELECTED
      ),
      name     = "Zone",
      na.value = "white"
    ) +
    coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
    labs(
      title    = sprintf("Biodiversity Offset Priority — %s", PROVINCE_LABELS[prov]),
      subtitle = sprintf(
        "%s (%s)  |  RLE: %s  |  Target: %d%%  |  Met: %.1f%% (%.0f ha of %.0f ha target)",
        veg_name, TARGET_MAPCODE, rle_status, conservation_target,
        pct_met, area_cons, prov_target),
      caption  = sprintf(
        "Dark green = priority offset areas  |  Dashed outline = vegetation type potential extent\nImpact area: %.0f ha  |  Endangered Wildlife Trust — Biodiversity Offset Workflow",
        IMPACT_AREA_HA)
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle   = element_text(size = 9, hjust = 0.5, colour = "#444444"),
      plot.caption    = element_text(size = 8, colour = "#777777", hjust = 0.5,
                                     margin = margin(t = 8)),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 10),
      legend.text     = element_text(size = 9),
      plot.margin     = margin(10, 10, 10, 10)
    )
  
  map_path <- file.path(out_dir,
                        sprintf("map_offset_priority_%s_%s.png",
                                TARGET_MAPCODE, prov))
  ggsave(map_path, p_map, width = 13, height = 11, dpi = 300, bg = "white")
  cat(sprintf("  Saved: %s\n", basename(map_path)))
}

# =============================================================================
# FIGURE 1b: IRREPLACEABILITY MAPS — PER PROVINCE
# =============================================================================

cat("\nGenerating irreplaceability maps...\n")

IRR_COLOURS <- c(
  "0%"       = "#EEEEEE",   # grey       — never selected
  "1-10%"    = "#FFFFCC",   # pale yellow
  "11-20%"   = "#D9F0A3",   # yellow-green
  "21-30%"   = "#ADDD8E",   # light green
  "31-40%"   = "#78C679",   # mid green
  "41-50%"   = "#41AB5D",   # green
  "51-60%"   = "#238443",   # dark green
  "61-70%"   = "#006837",   # deeper green
  "71-80%"   = "#084594",   # mid blue
  "81-90%"   = "#2171B5",   # blue  
  "91-100%"  = "#08306B"    # dark blue  — most irreplaceable
)

for (prov in names(sol_list)) {
  
  sol       <- sol_list[[prov]]
  prov_bound <- prov_sf[prov_sf$province == prov, ]
  
  nvm_clip <- st_read(
    file.path(setup_dir, sprintf("nvm_%s_%s.gpkg", TARGET_MAPCODE, prov)),
    quiet = TRUE
  ) %>% st_transform(st_crs(sol))
  
  nvm_bbox  <- st_bbox(nvm_clip)
  buf       <- max(nvm_bbox["xmax"] - nvm_bbox["xmin"],
                   nvm_bbox["ymax"] - nvm_bbox["ymin"]) * 0.15
  zoom_xlim <- c(nvm_bbox["xmin"] - buf, nvm_bbox["xmax"] + buf)
  zoom_ylim <- c(nvm_bbox["ymin"] - buf, nvm_bbox["ymax"] + buf)
  
  # Ensure irr_class is a factor with correct level order
  sol$irr_class <- factor(sol$irr_class,
                          levels = names(IRR_COLOURS))
  
  p_irr <- ggplot() +
    geom_sf(data = sol, aes(fill = irr_class), colour = NA) +
    geom_sf(data = nvm_clip, fill = NA, colour = "#4CAF50",
            linewidth = 0.5, linetype = "dashed") +
    geom_sf(data = prov_bound %>% st_transform(st_crs(sol)),
            fill = NA, colour = "#222222", linewidth = 0.8) +
    scale_fill_manual(
      values   = IRR_COLOURS,
      name     = "Irreplaceability",
      na.value = "white",
      drop     = FALSE
    ) +
    coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
    labs(
      title    = sprintf("Offset Irreplaceability — %s", PROVINCE_LABELS[prov]),
      subtitle = sprintf(
        "%s (%s)  |  RLE: %s  |  Based on %d solutions",
        veg_name, TARGET_MAPCODE, rle_status, 100),
      caption  = "Colour = selection frequency across portfolio of solutions\nDark blue = selected in >75%% of solutions (most irreplaceable)\nEndangered Wildlife Trust — Biodiversity Offset Workflow"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle   = element_text(size = 9, hjust = 0.5, colour = "#444444"),
      plot.caption    = element_text(size = 8, colour = "#777777", hjust = 0.5,
                                     margin = margin(t = 8)),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 10),
      legend.text     = element_text(size = 9),
      plot.margin     = margin(10, 10, 10, 10)
    )
  
  irr_path <- file.path(out_dir,
                        sprintf("map_irreplaceability_%s_%s.png",
                                TARGET_MAPCODE, prov))
  ggsave(irr_path, p_irr, width = 13, height = 11, dpi = 300, bg = "white")
  cat(sprintf("  Saved: %s\n", basename(irr_path)))
}

# =============================================================================
# FIGURE 2: NATIONAL OVERVIEW — ZOOMED TO VEG TYPE NATIONAL EXTENT
# =============================================================================

cat("\nGenerating national overview map...\n")

all_sol <- bind_rows(lapply(names(sol_list), function(p) {
  sol_list[[p]] %>%
    mutate(province = p) %>%
    select(pu_id, zone_assignment, offset_priority, province)
}))

# Zoom to NVM national extent
nvm_wgs    <- nvm_extract %>% st_transform(st_crs(sa_sf))
nvm_bbox   <- st_bbox(nvm_wgs)
buf        <- max(nvm_bbox["xmax"] - nvm_bbox["xmin"],
                  nvm_bbox["ymax"] - nvm_bbox["ymin"]) * 0.2
zoom_xlim  <- c(nvm_bbox["xmin"] - buf, nvm_bbox["xmax"] + buf)
zoom_ylim  <- c(nvm_bbox["ymin"] - buf, nvm_bbox["ymax"] + buf)

all_sol_wgs <- all_sol %>% st_transform(st_crs(sa_sf))

p_national <- ggplot() +
  geom_sf(data = sa_sf, fill = "#F9F9F9", colour = "#AAAAAA", linewidth = 0.3) +
  geom_sf(data = prov_sf, fill = NA, colour = "#CCCCCC", linewidth = 0.3) +
  geom_sf(data = all_sol_wgs, aes(fill = zone_assignment), colour = NA) +
  geom_sf(data = nvm_wgs, fill = NA, colour = "#4CAF50",
          linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = prov_sf %>%
            filter(province %in% overlapping_provinces),
          fill = NA, colour = "#333333", linewidth = 0.6) +
  geom_sf(data = sa_sf, fill = NA, colour = "#222222", linewidth = 0.8) +
  scale_fill_manual(
    values = c(
      "Conservation" = COL_CONSERVATION,
      "Agriculture"  = COL_AGRI,
      "REFS"         = COL_REFS,
      "Not selected" = COL_UNSELECTED
    ),
    name     = "Zone",
    na.value = "white"
  ) +
  coord_sf(xlim = zoom_xlim, ylim = zoom_ylim, expand = FALSE) +
  labs(
    title    = "Biodiversity Offset Priority Areas — National Overview",
    subtitle = sprintf("%s (%s)  |  RLE: %s  |  Conservation target: %d%%",
                       veg_name, TARGET_MAPCODE, rle_full, conservation_target),
    caption  = "Dark green = priority offset areas  |  Dashed outline = vegetation type potential extent\nEndangered Wildlife Trust — Biodiversity Offset Workflow  |  SANLC 2022 / NVM 2024"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 10, hjust = 0.5, colour = "#444444"),
    plot.caption    = element_text(size = 8, colour = "#777777", hjust = 0.5),
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9),
    plot.margin     = margin(10, 10, 10, 10)
  )

ggsave(file.path(out_dir, sprintf("map_offset_national_%s.png", TARGET_MAPCODE)),
       p_national, width = 14, height = 11, dpi = 300, bg = "white")
cat("  National overview map saved\n")

# =============================================================================
# REPORT
# =============================================================================

report_df <- bind_rows(report_rows)
write_csv(report_df,
          file.path(out_dir, sprintf("offset_report_%s.csv", TARGET_MAPCODE)))

cat("\n=============================================================\n")
cat("  OFFSET REPORT\n")
cat("=============================================================\n\n")
cat(sprintf("  Vegetation type:     %s (%s)\n", veg_name, TARGET_MAPCODE))
cat(sprintf("  RLE 2021 status:     %s\n", rle_full))
cat(sprintf("  Conservation target: %d%%\n", conservation_target))
cat(sprintf("  National extent:     %.0f ha\n", total_extent_ha))
cat(sprintf("  Impact area:         %.0f ha\n", IMPACT_AREA_HA))
cat(sprintf("  Fatal flaw:          %s\n\n",
            ifelse(fatal_flaw, "YES — CONSULT AUTHORITY", "No")))
cat("  Per-province results:\n")
print(report_df %>% select(province, prov_veg_area_ha, prov_target_ha,
                           area_conservation_ha, pct_target_met,
                           n_offset_pu, area_offset_ha),
      row.names = FALSE)
cat(sprintf("\n  Outputs saved to:\n  %s\n", out_dir))
cat("=============================================================\n")

# =============================================================================
# GIS EXPORTS — Shapefile and KMZ for ArcGIS / QGIS / Google Earth
# =============================================================================

cat("\nExporting GIS outputs...\n")

# ------------------------------------------------------------------
# Styled KML writer — assigns fill colours per zone for Google Earth
# ------------------------------------------------------------------
write_styled_kml <- function(sf_data, filepath, title) {
  
  zone_styles <- list(
    "Conservation"      = list(fill = "CC1B5E20", line = "FF1B5E20"),  # dark green
    "Agriculture"       = list(fill = "CC1565C0", line = "FF1565C0"),  # amber
    "REFS"              = list(fill = "CCF9A825", line = "FFF9A825"),  # blue
    "Not selected"      = list(fill = "55AAAAAA", line = "FFAAAAAA"),  # grey
    "0%"       = list(fill = "55EEEEEE", line = "FFCCCCCC"),  # grey
    "1-10%"    = list(fill = "CCCCFFFF", line = "FFCCFFFF"),  # pale yellow (AABBGGRR)
    "11-20%"   = list(fill = "CCA3F0D9", line = "FFA3F0D9"),  # yellow-green
    "21-30%"   = list(fill = "CC8EDDAD", line = "FF8EDDAD"),  # light green
    "31-40%"   = list(fill = "CC79C678", line = "FF79C678"),  # mid green
    "41-50%"   = list(fill = "CC5DAB41", line = "FF5DAB41"),  # green
    "51-60%"   = list(fill = "CC438423", line = "FF438423"),  # dark green
    "61-70%"   = list(fill = "CC376800", line = "FF376800"),  # deeper green
    "71-80%"   = list(fill = "CC944508", line = "FF944508"),  # mid blue (AABBGGRR)
    "81-90%"   = list(fill = "CCB57121", line = "FFB57121"),  # blue
    "91-100%"  = list(fill = "CC6B3008", line = "FF6B3008")   # dark blue
  )
  
  # KML header
  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2">',
    '<Document>',
    sprintf('  <name>%s</name>', title)
  )
  
  # Write style definitions
  for (zone in names(zone_styles)) {
    s <- zone_styles[[zone]]
    safe_id <- gsub(" ", "_", zone)
    lines <- c(lines,
               sprintf('  <Style id="%s">', safe_id),
               '    <LineStyle>',
               sprintf('      <color>%s</color>', s$line),
               '      <width>0.5</width>',
               '    </LineStyle>',
               '    <PolyStyle>',
               sprintf('      <color>%s</color>', s$fill),
               '      <fill>1</fill>',
               '    </PolyStyle>',
               '  </Style>'
    )
  }
  
  # Write each feature as a Placemark
  for (i in seq_len(nrow(sf_data))) {
    row      <- sf_data[i, ]
    zone     <- as.character(row$Zone)
    safe_id  <- gsub(" ", "_", zone)
    coords   <- st_coordinates(row)[, c(1, 2)]
    coord_str <- paste(apply(coords, 1, function(r)
      sprintf("%.8f,%.8f", r[1], r[2])), collapse = " ")
    
    lines <- c(lines,
               '  <Placemark>',
               sprintf('    <name>%s</name>', zone),
               sprintf('    <styleUrl>#%s</styleUrl>', safe_id),
               '    <ExtendedData>',
               sprintf('      <Data name="pu_id"><value>%s</value></Data>', row$pu_id),
               sprintf('      <Data name="Zone"><value>%s</value></Data>', zone),
               sprintf('      <Data name="Priority"><value>%s</value></Data>', row$Priority),
               sprintf('      <Data name="VegAreaKm"><value>%.4f</value></Data>', row$VegAreaKm),
               sprintf('      <Data name="CostBio"><value>%.4f</value></Data>', row$CostBio),
               '    </ExtendedData>',
               '    <Polygon><outerBoundaryIs><LinearRing>',
               sprintf('      <coordinates>%s</coordinates>', coord_str),
               '    </LinearRing></outerBoundaryIs></Polygon>',
               '  </Placemark>'
    )
  }
  
  lines <- c(lines, '</Document>', '</kml>')
  writeLines(lines, filepath, useBytes = FALSE)
}

# Clean field names for shapefile (10 char limit)
clean_for_shp <- function(sol, prov) {
  sol %>%
    st_transform(4326) %>%           # WGS84 for KMZ/universal compatibility
    mutate(
      Zone      = zone_assignment,
      Priority  = as.integer(offset_priority),
      VegAreaKm = round(feat_veg, 4),
      CostBio   = round(cost_bio, 4),
      LockedPA  = as.integer(locked_pa),
      Province  = prov,
      Irr       = round(irreplaceability, 3),
      IrrClass  = irr_class
    ) %>%
    select(pu_id, Province, Zone, Priority, Irr, IrrClass, VegAreaKm, CostBio, LockedPA)
}

gis_dir <- file.path(out_dir, "GIS")
dir.create(gis_dir, showWarnings = FALSE, recursive = TRUE)

remove_shp <- function(path) {
  for (ext in c(".shp", ".dbf", ".shx", ".prj", ".cpg")) {
    f <- paste0(tools::file_path_sans_ext(path), ext)
    if (file.exists(f)) file.remove(f)
  }
}

for (prov in names(sol_list)) {
  
  sol_clean <- clean_for_shp(sol_list[[prov]], PROVINCE_LABELS[prov])
  
  shp_pri <- file.path(gis_dir, sprintf("offset_priority_%s_%s.shp", TARGET_MAPCODE, prov))
  remove_shp(shp_pri)
  st_write(sol_clean %>% filter(Priority == 1), shp_pri, quiet = TRUE)
  cat(sprintf("  Shapefile saved: offset_priority_%s_%s.shp\n", TARGET_MAPCODE, prov))
  
  shp_all <- file.path(gis_dir, sprintf("offset_allzones_%s_%s.shp", TARGET_MAPCODE, prov))
  remove_shp(shp_all)
  st_write(sol_clean, shp_all, quiet = TRUE)
  cat(sprintf("  Shapefile saved: offset_allzones_%s_%s.shp\n", TARGET_MAPCODE, prov))
  
  kml_all <- file.path(gis_dir, sprintf("offset_allzones_%s_%s.kml", TARGET_MAPCODE, prov))
  write_styled_kml(sol_clean,
                   kml_all,
                   sprintf("%s — All Zones (%s)", veg_name, PROVINCE_LABELS[prov]))
  cat(sprintf("  KML saved:       offset_allzones_%s_%s.kml\n", TARGET_MAPCODE, prov))
  
  kml_pri <- file.path(gis_dir, sprintf("offset_priority_%s_%s.kml", TARGET_MAPCODE, prov))
  write_styled_kml(sol_clean %>% filter(Priority == 1),
                   kml_pri,
                   sprintf("%s — Priority Offset Areas (%s)", veg_name, PROVINCE_LABELS[prov]))
  cat(sprintf("  KML saved:       offset_priority_%s_%s.kml\n", TARGET_MAPCODE, prov))
  
  # KML — irreplaceability (coloured by irr_class)
  kml_irr <- file.path(gis_dir, sprintf("offset_irreplaceability_%s_%s.kml", TARGET_MAPCODE, prov))
  write_styled_kml(sol_clean %>% mutate(Zone = IrrClass),
                   kml_irr,
                   sprintf("%s — Irreplaceability (%s)", veg_name, PROVINCE_LABELS[prov]))
  cat(sprintf("  KML saved:       offset_irreplaceability_%s_%s.kml\n", TARGET_MAPCODE, prov))
}

# NVM veg type extent
nvm_wgs <- nvm_extract %>% st_transform(4326) %>%
  select(T_MAPCODE, T_Name, T_BIOME, T_BIOREGIO, T_CNSRV_TR) %>%
  mutate(Extent_ha = round(total_extent_ha, 0))

nvm_shp <- file.path(gis_dir, sprintf("nvm_extent_%s.shp", TARGET_MAPCODE))
remove_shp(nvm_shp)
st_write(nvm_wgs, nvm_shp, quiet = TRUE)

nvm_kml <- file.path(gis_dir, sprintf("nvm_extent_%s.kml", TARGET_MAPCODE))
nvm_kml_data <- nvm_wgs %>% mutate(Zone = "Not selected")
write_styled_kml(nvm_kml_data, nvm_kml,
                 sprintf("%s — Vegetation Type Extent", veg_name))

cat(sprintf("  NVM extent shapefile and KML saved\n"))
cat(sprintf("\n  GIS outputs in: %s/GIS/\n", out_dir))