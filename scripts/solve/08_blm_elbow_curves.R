# =============================================================================
# 08: BLM CALIBRATION - ELBOW CURVE ANALYSIS
# =============================================================================
#
# PURPOSE:
#   Extracts boundary length and cost metrics from solutions across multiple
#   BLM scenarios and plots elbow curves to identify the optimal BLM value
#   per province - the point of maximum compactness per unit additional cost.
#
#   Also computes patch-level landscape metrics (mean patch size, number of
#   patches) as supplementary indicators of clumping.
#
# METHOD:
#   For each province x BLM combination:
#     - Load solution gpkg
#     - Extract total cost      (eval_cost_summary)
#     - Extract boundary length (eval_boundary_summary)
#     - Extract patch metrics   (sf-based, no extra packages needed)
#
#   Then plot:
#     1. Normalised elbow curve: boundary length vs cost trade-off
#     2. Total boundary length vs BLM (per province)
#     3. Mean patch size vs BLM, per zone            (Appendix A, Fig A.2)
#     4. Number of patches vs BLM, per zone          (Appendix A, Fig A.1)
#     5. Normalised cost vs normalised Conservation
#        mean patch size (elbow)                     (Appendix A, Fig A.3)
#     6. % cost increase above BLM = 0 baseline      (Appendix A, Fig A.4)
#
# BEFORE RUNNING:
#   Run 07 once per BLM value in BLM_SCENARIOS, with the same targets and
#   lock settings as below, and with SCENARIO_TAG <- sprintf("blm%g", BLM)
#   so solution files are named solution_{province}_blm0.gpkg,
#   solution_{province}_blm0.001.gpkg, etc.
#
# CHANGE LOG:
#   v2: Boundary matrix now passed explicitly to eval_boundary_summary()
#       (data = bm). Previously the problem had no geometry and, at BLM = 0,
#       no matrix, so boundary length silently returned NA. Added plots 5
#       and 6 so all Appendix A figures are produced here. Lock constraints
#       added in a single call, as in 07.
#
# INPUT:
#   outputs/solutions/{province}/solution_{province}_{tag}.gpkg   (from 07)
#   data/planning_units/{province}/pu_{province}.gpkg
#   data/planning_units/{province}/boundary_matrix_{province}.rds
#
# OUTPUT:
#   outputs/blm_analysis/blm_metrics.csv
#   outputs/blm_analysis/elbow_curves.png
#   outputs/blm_analysis/boundary_vs_blm.png
#   outputs/blm_analysis/mean_patch_size.png
#   outputs/blm_analysis/n_patches.png
#   outputs/blm_analysis/elbow_cost_vs_patch_size.png
#   outputs/blm_analysis/cost_penalty.png
#
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(prioritizr)
library(here)

# Gurobi: requires the gurobi R package and a valid licence (see README)
library(gurobi)

# =============================================================================
# CONFIGURATION
# =============================================================================

base_dir   <- here::here()
pu_dir     <- file.path(base_dir, "data/planning_units")
output_dir <- file.path(base_dir, "outputs/solutions")
out_dir    <- file.path(base_dir, "outputs/blm_analysis")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ALBERS_CRS <- "ESRI:102022"

# Must match targets used in 07
TARGET_CONSERVATION <- 0.33
TARGET_REFS         <- 0.12
TARGET_AGRI         <- 0.20

LOCK_PA   <- TRUE
LOCK_REFS <- TRUE
LOCK_AGRI <- TRUE

GUROBI_GAP     <- 0.05
GUROBI_TIMELIMIT <- 1800
GUROBI_THREADS <- 8

# --- BLM scenarios to analyse ---
# Each entry: list(blm = value, tag = "filename_tag"),
# where tag = sprintf("blm%g", blm) as set in 07. The first entry must be
# the BLM = 0 baseline (used for the % cost increase plot).
BLM_SCENARIOS <- list(
  list(blm = 0.000, tag = "blm0"),
  list(blm = 0.001, tag = "blm0.001"),
  list(blm = 0.005, tag = "blm0.005"),
  list(blm = 0.010, tag = "blm0.01")
)

# Provinces to include
provinces <- c(
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
# HELPER: Build problem (same as 07, needed for eval functions)
# =============================================================================

build_problem <- function(pu_tbl, targets_abs, bm, blm) {

  pu_tbl$feat_conservation <- pu_tbl$Area_km2
  pu_tbl$feat_refs         <- pu_tbl$Area_km2
  pu_tbl$feat_agri         <- pu_tbl$Area_km2

  p <- problem(
    x = pu_tbl,
    features = zones(
      "feat_conservation", "feat_refs", "feat_agri",
      zone_names = c("Conservation", "REFS", "Agriculture")
    ),
    cost_column = c("cost_bio", "cost_refs", "cost_agri")
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets_abs) %>%
    add_binary_decisions()

  # Lock layers in a single call (06b guarantees at most one lock per PU)
  n <- nrow(pu_tbl)
  lock_mat <- matrix(
    c(if (LOCK_PA)   as.logical(pu_tbl$locked_pa)   else rep(FALSE, n),
      if (LOCK_REFS) as.logical(pu_tbl$locked_refs) else rep(FALSE, n),
      if (LOCK_AGRI) as.logical(pu_tbl$locked_agri) else rep(FALSE, n)),
    ncol = 3,
    dimnames = list(NULL, c("Conservation", "REFS", "Agriculture")))
  if (any(lock_mat))
    p <- p %>% add_locked_in_constraints(lock_mat)

  if (blm > 0 && !is.null(bm)) {
    if (!inherits(bm, "dgCMatrix")) bm <- as(bm, "generalMatrix")
    p <- p %>% add_boundary_penalties(penalty = blm, data = bm)
  }

  p %>% add_gurobi_solver(
    gap        = GUROBI_GAP,
    time_limit = GUROBI_TIMELIMIT,
    threads    = GUROBI_THREADS,
    verbose    = FALSE
  )
}

# =============================================================================
# HELPER: Compute patch metrics from solution sf
# =============================================================================

compute_patch_metrics <- function(sol) {

  metrics <- list()
  zone_names <- c("1" = "Conservation", "2" = "REFS", "3" = "Agriculture")

  for (z in 1:3) {
    zone_sf <- sol[sol$zone_assignment == z, ]
    if (nrow(zone_sf) == 0) next
  
    # Dissolve into patches (connected components)
    patches <- zone_sf %>%
      st_union() %>%
      st_cast("POLYGON") %>%
      st_as_sf() %>%
      mutate(area_km2 = as.numeric(st_area(.)) / 1e6)
  
    metrics[[zone_names[as.character(z)]]] <- data.frame(
      zone          = zone_names[as.character(z)],
      n_patches     = nrow(patches),
      mean_patch_km2 = mean(patches$area_km2),
      max_patch_km2  = max(patches$area_km2),
      total_area_km2 = sum(patches$area_km2)
    )
  }

  bind_rows(metrics)
}

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  BLM ELBOW CURVE ANALYSIS\n")
cat("=============================================================\n\n")

all_metrics <- list()

for (prov in provinces) {

  cat(sprintf("  %s\n", toupper(gsub("_", " ", prov))))

  # Load PUs once
  gpkg_path <- file.path(pu_dir, prov, sprintf("pu_%s.gpkg", prov))
  bm_path   <- file.path(pu_dir, prov, sprintf("boundary_matrix_%s.rds", prov))

  if (!file.exists(gpkg_path)) { cat("    [SKIP] PU file not found\n"); next }

  pu     <- st_read(gpkg_path, quiet = TRUE)
  pu     <- st_transform(pu, ALBERS_CRS)
  if (!"Area_km2" %in% names(pu))
    pu$Area_km2 <- as.numeric(st_area(pu)) / 1e6
  pu_tbl  <- st_drop_geometry(pu)

  bm <- NULL
  if (file.exists(bm_path)) bm <- readRDS(bm_path)

  total_area  <- sum(pu_tbl$Area_km2, na.rm = TRUE)
  targets_abs <- matrix(
    c(total_area * TARGET_CONSERVATION,
      total_area * TARGET_REFS,
      total_area * TARGET_AGRI),
    nrow = 1, ncol = 3
  )

  for (scenario in BLM_SCENARIOS) {
  
    blm <- scenario$blm
    tag <- scenario$tag
    file_tag  <- if (nchar(tag) > 0) sprintf("_%s", tag) else ""
    sol_path  <- file.path(output_dir, prov,
                           sprintf("solution_%s%s.gpkg", prov, file_tag))
  
    if (!file.exists(sol_path)) {
      cat(sprintf("    [SKIP] BLM=%g solution not found: %s\n", blm, basename(sol_path)))
      next
    }
  
    cat(sprintf("    BLM=%g ... ", blm))
  
    sol     <- st_read(sol_path, quiet = TRUE)
    sol_tbl <- st_drop_geometry(sol)
    sol_df  <- sol_tbl %>%
      select(solution_1_Conservation, solution_1_REFS, solution_1_Agriculture) %>%
      as.data.frame()
  
    # Build problem for eval functions
    p <- tryCatch(
      build_problem(pu_tbl, targets_abs, bm, blm),
      error = function(e) NULL
    )
  
    if (is.null(p)) { cat("problem build failed\n"); next }
  
    # Cost summary
    cost_summary <- tryCatch(
      eval_cost_summary(p, sol_df),
      error = function(e) NULL
    )
  
    # Boundary summary: matrix passed explicitly, since the problem has no
    # geometry and (at BLM = 0) no boundary penalty attached
    bnd_summary <- tryCatch({
      if (!is.null(bm)) {
        bm_g <- if (inherits(bm, "dgCMatrix")) bm else as(bm, "generalMatrix")
        eval_boundary_summary(p, sol_df, data = bm_g)
      } else NULL
    }, error = function(e) {
      cat(sprintf("[boundary eval failed: %s] ", e$message)); NULL
    })
  
    # Patch metrics
    patch_metrics <- tryCatch(
      compute_patch_metrics(sol),
      error = function(e) NULL
    )
  
    # Extract values
    total_cost <- if (!is.null(cost_summary))
      cost_summary$cost[cost_summary$summary == "overall"] else NA
  
    total_boundary <- if (!is.null(bnd_summary))
      bnd_summary$boundary[bnd_summary$summary == "overall"] else NA
  
    # Compile row per zone
    for (z_name in c("Conservation", "REFS", "Agriculture")) {
      pm <- if (!is.null(patch_metrics))
        patch_metrics[patch_metrics$zone == z_name, ] else NULL
    
      all_metrics[[length(all_metrics) + 1]] <- data.frame(
        province       = prov,
        blm            = blm,
        zone           = z_name,
        total_cost     = total_cost,
        total_boundary = total_boundary,
        n_patches      = if (!is.null(pm) && nrow(pm) > 0) pm$n_patches     else NA,
        mean_patch_km2 = if (!is.null(pm) && nrow(pm) > 0) pm$mean_patch_km2 else NA,
        max_patch_km2  = if (!is.null(pm) && nrow(pm) > 0) pm$max_patch_km2  else NA
      )
    }
  
    cat(sprintf("cost=%.0f | boundary=%.0f\n",
                ifelse(is.na(total_cost), 0, total_cost),
                ifelse(is.na(total_boundary), 0, total_boundary)))
  }

  rm(pu, bm); gc()
  cat("\n")
}

# =============================================================================
# COMPILE AND SAVE METRICS
# =============================================================================

metrics_df <- bind_rows(all_metrics)
write_csv(metrics_df, file.path(out_dir, "blm_metrics.csv"))
cat(sprintf("Metrics saved to: %s\n\n", file.path(out_dir, "blm_metrics.csv")))

# =============================================================================
# PLOT 1: ELBOW CURVES (boundary length vs cost, normalised)
# =============================================================================

# Use overall metrics (not zone-specific) for elbow curves
overall_metrics <- metrics_df %>%
  select(province, blm, total_cost, total_boundary) %>%
  distinct() %>%
  group_by(province) %>%
  mutate(
    cost_norm     = (total_cost     - min(total_cost,     na.rm=TRUE)) /
                    (max(total_cost,     na.rm=TRUE) - min(total_cost,     na.rm=TRUE)),
    boundary_norm = (total_boundary - min(total_boundary, na.rm=TRUE)) /
                    (max(total_boundary, na.rm=TRUE) - min(total_boundary, na.rm=TRUE))
  ) %>%
  ungroup() %>%
  mutate(province_label = toupper(gsub("_", " ", province)))

# Elbow curve: normalised boundary length vs normalised cost
p1 <- ggplot(overall_metrics, aes(x = cost_norm, y = boundary_norm,
                                   colour = province_label,
                                   group  = province_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = factor(blm)), size = 3) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),
    labels = paste0("BLM=", sort(unique(overall_metrics$blm))),
    name   = "BLM value"
  ) +
  labs(
    title    = "BLM Calibration - Elbow Curves",
    subtitle = "Normalised boundary length vs normalised cost per province\nElbow = optimal BLM (maximum compactness per unit cost)",
    x        = "Normalised total cost (0=min, 1=max)",
    y        = "Normalised boundary length (0=min, 1=max)",
    colour   = "Province",
    caption  = "Endangered Wildlife Trust - 30x30 Conservation Planning"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "#555555"),
    legend.position = "right"
  )

ggsave(file.path(out_dir, "elbow_curves.png"),
       p1, width = 12, height = 8, dpi = 300, bg = "white")
cat("Elbow curve plot saved\n")

# =============================================================================
# PLOT 2: BOUNDARY LENGTH vs BLM (raw, faceted by province)
# =============================================================================

p2 <- ggplot(overall_metrics, aes(x = blm, y = total_boundary)) +
  geom_line(colour = "#1565C0", linewidth = 0.8) +
  geom_point(colour = "#1565C0", size = 3) +
  facet_wrap(~ province_label, scales = "free_y", ncol = 3) +
  labs(
    title   = "Total Boundary Length by BLM Value",
    x       = "BLM",
    y       = "Total boundary length (km)",
    caption = "Endangered Wildlife Trust - 30x30 Conservation Planning"
  ) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave(file.path(out_dir, "boundary_vs_blm.png"),
       p2, width = 14, height = 10, dpi = 300, bg = "white")
cat("Boundary vs BLM plot saved\n")

# =============================================================================
# PLOT 3: PATCH METRICS - mean patch size and n_patches by BLM
# =============================================================================

patch_df <- metrics_df %>%
  filter(!is.na(mean_patch_km2)) %>%
  mutate(province_label = toupper(gsub("_", " ", province)))

# Mean patch size
p3 <- ggplot(patch_df, aes(x = blm, y = mean_patch_km2, colour = zone)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c("Conservation" = "#2E7D32",
               "REFS"         = "#1565C0",
               "Agriculture"  = "#E65100"),
    name = "Zone"
  ) +
  facet_wrap(~ province_label, scales = "free_y", ncol = 3) +
  labs(
    title   = "Mean Patch Size by BLM Value",
    x       = "BLM",
    y       = "Mean patch size (km2)",
    caption = "Endangered Wildlife Trust - 30x30 Conservation Planning"
  ) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave(file.path(out_dir, "mean_patch_size.png"),
       p3, width = 14, height = 10, dpi = 300, bg = "white")
cat("Mean patch size plot saved\n")

# Number of patches
p4 <- ggplot(patch_df, aes(x = blm, y = n_patches, colour = zone)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c("Conservation" = "#2E7D32",
               "REFS"         = "#1565C0",
               "Agriculture"  = "#E65100"),
    name = "Zone"
  ) +
  facet_wrap(~ province_label, scales = "free_y", ncol = 3) +
  labs(
    title   = "Number of Patches by BLM Value",
    x       = "BLM",
    y       = "Number of patches",
    caption = "Endangered Wildlife Trust - 30x30 Conservation Planning"
  ) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave(file.path(out_dir, "n_patches.png"),
       p4, width = 14, height = 10, dpi = 300, bg = "white")
cat("Number of patches plot saved\n")

# =============================================================================
# PLOT 5: ELBOW - NORMALISED COST vs NORMALISED CONSERVATION PATCH SIZE
# =============================================================================
# Appendix A, Figure A.3. Each point is a BLM value; the elbow marks where
# compactness gains are greatest relative to cost increase.

elbow_patch <- metrics_df %>%
  filter(zone == "Conservation", !is.na(mean_patch_km2), !is.na(total_cost)) %>%
  group_by(province) %>%
  mutate(
    cost_norm  = (total_cost - min(total_cost)) /
                 (max(total_cost) - min(total_cost)),
    patch_norm = (mean_patch_km2 - min(mean_patch_km2)) /
                 (max(mean_patch_km2) - min(mean_patch_km2))
  ) %>%
  arrange(province, blm) %>%
  ungroup() %>%
  mutate(province_label = toupper(gsub("_", " ", province)))

p5 <- ggplot(elbow_patch, aes(x = cost_norm, y = patch_norm)) +
  geom_line(colour = "#2E7D32", linewidth = 0.8) +
  geom_point(colour = "#2E7D32", size = 2.5) +
  geom_text(aes(label = blm), vjust = -0.8, size = 2.8, colour = "#555555") +
  facet_wrap(~ province_label, ncol = 3) +
  labs(
    title   = "BLM Elbow Curves: Normalised Cost vs Conservation Patch Size",
    x       = "Normalised total cost (0 = min, 1 = max)",
    y       = "Normalised Conservation mean patch size (0 = min, 1 = max)",
    caption = "Point labels = BLM value"
  ) +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave(file.path(out_dir, "elbow_cost_vs_patch_size.png"),
       p5, width = 14, height = 10, dpi = 300, bg = "white")
cat("Cost vs patch size elbow plot saved\n")

# =============================================================================
# PLOT 6: % COST INCREASE ABOVE BLM = 0 BASELINE
# =============================================================================
# Appendix A, Figure A.4.

cost_penalty <- metrics_df %>%
  select(province, blm, total_cost) %>%
  distinct() %>%
  group_by(province) %>%
  mutate(pct_increase = 100 * (total_cost - total_cost[blm == 0]) /
                               total_cost[blm == 0]) %>%
  ungroup() %>%
  mutate(province_label = toupper(gsub("_", " ", province)))

p6 <- ggplot(cost_penalty, aes(x = blm, y = pct_increase,
                                colour = province_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#888888") +
  labs(
    title  = "Cost Penalty by BLM Value",
    x      = "BLM",
    y      = "Cost increase above BLM = 0 baseline (%)",
    colour = "Province"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave(file.path(out_dir, "cost_penalty.png"),
       p6, width = 10, height = 7, dpi = 300, bg = "white")
cat("Cost penalty plot saved\n")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n=============================================================\n")
cat("  BLM METRICS SUMMARY\n")
cat("=============================================================\n\n")

summary_tbl <- overall_metrics %>%
  select(province_label, blm, total_cost, total_boundary) %>%
  arrange(province_label, blm)

print(summary_tbl, n = Inf)
cat(sprintf("\nAll outputs saved to: %s\n", out_dir))
