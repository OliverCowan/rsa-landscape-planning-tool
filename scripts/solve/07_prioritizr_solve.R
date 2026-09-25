# =============================================================================
# 07: PRIORITIZR PROBLEM SETUP, SOLVE & OUTPUTS
# =============================================================================
#
# PURPOSE:
#   Builds and solves a multi-zone spatial prioritisation problem using
#   prioritizr and Gurobi for each province (and national extent).
#
#   Two output tiers per province:
#     CORE:      Zone assignment GeoPackage + summary CSV
#     PRIORITY:  Selection frequency (shuffle portfolio, optional) +
#                cost-efficiency
#
#   Three PNG maps per province:
#     1. Zone assignment map
#     2. Selection frequency map (zone-specific colour ramps)
#     3. Cost-efficiency map (within selected PUs)
#
# ZONES:
#   Zone 1 - Conservation  (locked: existing Protected Areas)
#   Zone 2 - REFS          (locked: existing REFS facilities)
#   Zone 3 - Agriculture   (locked: existing Agriculture zones)
#   Zone 0 - Available     (unallocated)
#
# TARGETS:
#   Area-based (proportion of total province area):
#     TARGET_CONSERVATION, TARGET_REFS, TARGET_AGRI (set below)
#
# LOCKED PLANNING UNITS:
#   Lock flags from 06b are combined into a single three-column matrix and
#   added with one add_locked_in_constraints() call. After each solve, a
#   check reports any locked PU not assigned to its locked zone (expect 0).
#
# SCRIPTED EQUIVALENT OF THE APP:
#   This script builds and solves the same multi-zone problem as the Shiny
#   app, for batch runs outside the app (e.g. all provinces, calibration,
#   sensitivity testing).
#
# DEFAULTS:
#   Targets (33 / 12 / 20%) are an example scenario only, not recommended
#   policy targets; in the app, users set their own. BLM = 0.001 is the
#   value recommended by the calibration in 08 / Appendix A, and is also
#   user-adjustable in the app. Change SCENARIO_TAG when running other
#   scenarios so outputs are not overwritten.
#
# INPUT:
#   data/planning_units/{province}/pu_{province}.gpkg
#   data/planning_units/{province}/boundary_matrix_{province}.rds
#
# OUTPUT (file names carry _{SCENARIO_TAG} when set):
#   outputs/solutions/{province}/solution_{province}.gpkg
#   outputs/solutions/{province}/solution_{province}_summary.csv
#   outputs/solutions/{province}/selection_frequency_{province}.gpkg (optional)
#   outputs/solutions/{province}/map_01_zone_assignment_{province}.png
#   outputs/solutions/{province}/map_02_selection_frequency_{province}.png (optional)
#   outputs/solutions/{province}/map_03_cost_efficiency_{province}.png
#   outputs/solutions/run_summary.csv
#
# REQUIREMENTS:
#   Gurobi with a valid licence and the gurobi R package (see main README).
#   R packages: sf, terra, dplyr, readr, prioritizr, ggplot2, patchwork,
#   ggnewscale, here.
#
# =============================================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(prioritizr)
library(ggplot2)
library(patchwork)
library(ggnewscale)
library(here)

# Gurobi: requires the gurobi R package and a valid licence (see README)
library(gurobi)

# =============================================================================
# CONFIGURATION - edit these values to define your scenario
# =============================================================================

base_dir   <- here::here()
pu_dir     <- file.path(base_dir, "data/planning_units")
output_dir <- file.path(base_dir, "outputs/solutions")

ALBERS_CRS <- "ESRI:102022"

# --- Targets (proportion of total province area; example scenario) ---
TARGET_CONSERVATION <- 0.33   # 33%
TARGET_REFS         <- 0.12   # 12%
TARGET_AGRI         <- 0.20   # 20%

# --- Boundary length modifier ---
# 0 = no spatial clustering penalty, higher = more compact solutions
# 0.001 = calibrated default (see 08_blm_elbow_curves.R / Appendix A)
BLM <- 0.001

# --- Scenario tag ---
# Appended to all output filenames to avoid overwriting previous runs
# Examples: "blm0", "blm0.01", "blm0.1", "t33_12_20"
# Set to "" to use default naming (no tag)
SCENARIO_TAG <- "blm0.001"

# --- Shuffle portfolio (selection frequency) ---
# Number of solutions to generate via randomised repeated solves
# More solutions = better frequency estimates but longer solve time
# shuffle_portfolio randomises PU order each run, generating diverse solutions
# (conceptually equivalent to Marxan's repeated-run selection frequency)
N_SOLUTIONS <- 10

# --- Gurobi solver settings ---
GUROBI_GAP     <- 0.05   # 5% optimality gap
GUROBI_TIMELIMIT <- 1800  # 30 minutes max per solve
GUROBI_THREADS <- 8

# --- Lock layer controls ---
# Set FALSE to disable a lock layer (e.g. for sensitivity testing)
LOCK_PA   <- TRUE   # lock existing Protected Areas into Conservation zone
LOCK_REFS <- TRUE   # lock existing REFS facilities into REFS zone
LOCK_AGRI <- TRUE   # lock existing Agriculture zones into Agriculture zone

# --- Optional outputs ---
COMPUTE_FREQUENCY <- FALSE   # set TRUE to compute selection frequency (slow)

# --- Provinces to run ---
# Comment out provinces you don't want to run
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
# COLOUR SCHEME (consistent across all maps)
# =============================================================================

ZONE_COLOURS <- c(
  "0" = "#CCCCCC",   # Available
  "1" = "#2E7D32",   # Conservation - dark green
  "2" = "#1565C0",   # REFS         - dark blue
  "3" = "#E65100"    # Agriculture  - dark orange
)

ZONE_LABELS <- c(
  "0" = "Available",
  "1" = "Conservation",
  "2" = "REFS",
  "3" = "Agriculture"
)

# Selection frequency ramps (white -> zone colour)
FREQ_RAMPS <- list(
  "1" = c("#FFFFFF", "#A5D6A7", "#2E7D32"),   # Conservation: white -> green
  "2" = c("#FFFFFF", "#90CAF9", "#1565C0"),   # REFS:         white -> blue
  "3" = c("#FFFFFF", "#FFCC80", "#E65100")    # Agriculture:  white -> orange
)

# =============================================================================
# HELPER: Lock matrix (one column per zone, single constraint call)
# =============================================================================
# 06b guarantees each PU is locked into at most one zone, so one combined
# matrix is valid. Columns are all FALSE for any lock layer switched off.

build_lock_matrix <- function(pu_tbl) {
  n <- nrow(pu_tbl)
  matrix(
    c(if (LOCK_PA)   as.logical(pu_tbl$locked_pa)   else rep(FALSE, n),
      if (LOCK_REFS) as.logical(pu_tbl$locked_refs) else rep(FALSE, n),
      if (LOCK_AGRI) as.logical(pu_tbl$locked_agri) else rep(FALSE, n)),
    ncol = 3,
    dimnames = list(NULL, c("Conservation", "REFS", "Agriculture"))
  )
}

# =============================================================================
# HELPER: Build and solve prioritizr problem
# =============================================================================

build_and_solve <- function(pu, bm, targets_abs, blm, label) {

  cat(sprintf("  Building prioritizr problem...\n"))

  # Add area feature columns (one per zone)
  pu$feat_conservation <- pu$Area_km2
  pu$feat_refs         <- pu$Area_km2
  pu$feat_agri         <- pu$Area_km2

  # Drop geometry for optimisation speed
  pu_tbl  <- st_drop_geometry(pu)
  pu_geom <- st_geometry(pu)

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

  # Lock layers (LOCK_PA / LOCK_REFS / LOCK_AGRI in config), single call
  lock_mat <- build_lock_matrix(pu_tbl)
  if (any(lock_mat))
    p <- p %>% add_locked_in_constraints(lock_mat)

  # Boundary penalties
  if (blm > 0 && !is.null(bm)) {
    cat(sprintf("  Adding boundary penalties (BLM = %g)...\n", blm))
    if (!inherits(bm, "dgCMatrix"))
      bm <- as(bm, "generalMatrix")
    p <- p %>% add_boundary_penalties(penalty = blm, data = bm)
  }

  # Return problem WITHOUT solver attached - solver added separately
  # for core solve and portfolio so they don't conflict
  p_no_solver <- p

  # Add solver for core solve only
  p_solve <- p_no_solver %>% add_gurobi_solver(
    gap        = GUROBI_GAP,
    time_limit = GUROBI_TIMELIMIT,
    threads    = GUROBI_THREADS,
    verbose    = FALSE
  )

  cat(sprintf("  Solving (%s)...\n", label))
  start <- Sys.time()
  sol   <- solve(p_solve)
  elapsed <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 2)
  cat(sprintf("  Solved in %.2f minutes\n", elapsed))

  # Reattach geometry
  sol <- st_sf(sol, geometry = pu_geom)

  # Return p_no_solver as the base problem for portfolio use
  list(solution = sol, problem = p_no_solver, solve_mins = elapsed,
       pu_geom = pu_geom, pu_tbl = pu_tbl)
}

# =============================================================================
# HELPER: Assign zone labels from solution columns
# =============================================================================

assign_zones <- function(sol) {
  sol$zone_assignment <- 0L
  sol$zone_assignment[sol$solution_1_Conservation == 1L] <- 1L
  sol$zone_assignment[sol$solution_1_REFS         == 1L] <- 2L
  sol$zone_assignment[sol$solution_1_Agriculture  == 1L] <- 3L
  sol$zone_label <- ZONE_LABELS[as.character(sol$zone_assignment)]
  sol
}

# =============================================================================
# HELPER: Compute selection frequency from gap portfolio
# =============================================================================

compute_selection_frequency <- function(pu_tbl, pu_geom, targets_abs,
                                        blm, bm, n_solutions) {

  cat(sprintf("  Computing selection frequency (%d solutions, shuffle portfolio)...\n",
              n_solutions))

  # Rebuild problem fresh to avoid serialisation issues with constraint matrices
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

  # Lock layers (LOCK_PA / LOCK_REFS / LOCK_AGRI in config), single call
  lock_mat <- build_lock_matrix(pu_tbl)
  if (any(lock_mat))
    p <- p %>% add_locked_in_constraints(lock_mat)

  if (blm > 0 && !is.null(bm)) {
    if (!inherits(bm, "dgCMatrix")) bm <- as(bm, "generalMatrix")
    p <- p %>% add_boundary_penalties(penalty = blm, data = bm)
  }

  p <- p %>%
    add_shuffle_portfolio(number_solutions = n_solutions,
                          threads = 1L) %>%
    add_gurobi_solver(
      gap        = GUROBI_GAP,
      time_limit = GUROBI_TIMELIMIT * 2,
      threads    = GUROBI_THREADS,
      verbose    = FALSE
    )

  start_t <- Sys.time()
  port <- tryCatch(solve(p), error = function(e) {
    cat(sprintf("  [WARN] Portfolio solve failed: %s\n", e$message))
    NULL
  })
  elapsed <- round(as.numeric(difftime(Sys.time(), start_t, units = "mins")), 2)

  if (is.null(port)) return(NULL)
  cat(sprintf("  Portfolio solved in %.2f minutes\n", elapsed))

  # Portfolio returns a wide data frame
  # Columns: solution_{n}_Conservation, solution_{n}_REFS, solution_{n}_Agriculture
  sol_cols_cons <- grep("_Conservation$", names(port), value = TRUE)
  sol_cols_refs <- grep("_REFS$",         names(port), value = TRUE)
  sol_cols_agri <- grep("_Agriculture$",  names(port), value = TRUE)
  n_sols        <- length(sol_cols_cons)
  cat(sprintf("  %d solutions returned\n", n_sols))

  if (n_sols == 0) {
    cat("  [WARN] No solution columns found - check column names above\n")
    return(NULL)
  }

  freq_cons <- rowMeans(matrix(as.numeric(unlist(port[, sol_cols_cons])),
                               nrow = nrow(port)), na.rm = TRUE)
  freq_refs <- rowMeans(matrix(as.numeric(unlist(port[, sol_cols_refs])),
                               nrow = nrow(port)), na.rm = TRUE)
  freq_agri <- rowMeans(matrix(as.numeric(unlist(port[, sol_cols_agri])),
                               nrow = nrow(port)), na.rm = TRUE)

  freq_overall <- pmax(freq_cons, freq_refs, freq_agri)

  st_sf(
    pu_id             = pu_tbl$pu_id,
    freq_conservation = freq_cons,
    freq_refs         = freq_refs,
    freq_agri         = freq_agri,
    freq_overall      = freq_overall,
    geometry          = pu_geom
  )
}


# =============================================================================
# HELPER: Compute cost efficiency within selected PUs
# =============================================================================

compute_cost_efficiency <- function(sol) {

  # Within each zone, rank PUs by cost (lower cost = higher efficiency)
  # Efficiency score: 1 - normalised cost (so higher = more efficient)

  sol$cost_efficiency <- NA_real_

  for (zone_id in 1:3) {
    zone_col <- c("cost_bio", "cost_refs", "cost_agri")[zone_id]
    mask     <- sol$zone_assignment == zone_id
    if (sum(mask) == 0) next
  
    costs    <- sol[[zone_col]][mask]
    min_c    <- min(costs, na.rm = TRUE)
    max_c    <- max(costs, na.rm = TRUE)
  
    if (max_c == min_c) {
      sol$cost_efficiency[mask] <- 1
    } else {
      # Invert so low cost = high efficiency score
      sol$cost_efficiency[mask] <- 1 - (costs - min_c) / (max_c - min_c)
    }
  }

  sol
}

# =============================================================================
# HELPER: PNG Map 1 - Zone Assignment
# =============================================================================

map_zone_assignment <- function(sol, prov, out_path) {

  sol$zone_char <- as.character(sol$zone_assignment)

  p <- ggplot(sol) +
    geom_sf(aes(fill = zone_char), colour = NA, size = 0) +
    scale_fill_manual(
      values = ZONE_COLOURS,
      labels = ZONE_LABELS,
      name   = "Zone"
    ) +
    labs(
      title    = sprintf("Zone Assignment - %s", toupper(gsub("_", " ", prov))),
      subtitle = sprintf("Conservation: %.0f%%  |  REFS: %.0f%%  |  Agriculture: %.0f%%",
                         TARGET_CONSERVATION * 100,
                         TARGET_REFS * 100,
                         TARGET_AGRI * 100),
      caption  = "Endangered Wildlife Trust - 30x30 Conservation Planning"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "#555555"),
      plot.caption  = element_text(size = 8,  colour = "#888888"),
      legend.position = "right"
    )

  ggsave(out_path, p, width = 12, height = 8, dpi = 300, bg = "white")
  cat(sprintf("  [PNG] Zone assignment map saved\n"))
}

# =============================================================================
# HELPER: PNG Map 2 - Selection Frequency
# =============================================================================

map_selection_frequency <- function(freq_sf, sol, prov, out_path) {

  # Merge zone assignment into frequency sf
  freq_sf$zone_assignment <- sol$zone_assignment
  freq_sf$zone_char       <- as.character(sol$zone_assignment)

  # For unselected PUs, frequency = 0 (shown in grey)
  # For selected PUs, colour by zone-specific frequency

  # Build three zone-specific layers + one grey layer
  pu_available <- freq_sf[freq_sf$zone_assignment == 0, ]
  pu_cons      <- freq_sf[freq_sf$zone_assignment == 1, ]
  pu_refs      <- freq_sf[freq_sf$zone_assignment == 2, ]
  pu_agri      <- freq_sf[freq_sf$zone_assignment == 3, ]

  p <- ggplot() +
    # Available PUs in grey
    geom_sf(data = pu_available, fill = "#DDDDDD", colour = NA) +
    # Conservation frequency (white -> green)
    geom_sf(data = pu_cons, aes(fill = freq_conservation), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["1"]],
      limits  = c(0, 1),
      name    = "Selection\nFrequency",
      guide   = guide_colorbar(order = 1)
    ) +
    # REFS frequency (white -> blue) - new scale
    ggnewscale::new_scale_fill() +
    geom_sf(data = pu_refs, aes(fill = freq_refs), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["2"]],
      limits  = c(0, 1),
      name    = "Selection\nFrequency (REFS)",
      guide   = guide_colorbar(order = 2)
    ) +
    # Agriculture frequency (white -> orange) - new scale
    ggnewscale::new_scale_fill() +
    geom_sf(data = pu_agri, aes(fill = freq_agri), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["3"]],
      limits  = c(0, 1),
      name    = "Selection\nFrequency (Agri)",
      guide   = guide_colorbar(order = 3)
    ) +
    labs(
      title   = sprintf("Selection Frequency - %s", toupper(gsub("_", " ", prov))),
      subtitle = sprintf("%d solutions (shuffle portfolio)", N_SOLUTIONS),
      caption = "Endangered Wildlife Trust - 30x30 Conservation Planning"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "#555555"),
      plot.caption  = element_text(size = 8,  colour = "#888888")
    )

  ggsave(out_path, p, width = 12, height = 8, dpi = 300, bg = "white")
  cat(sprintf("  [PNG] Selection frequency map saved\n"))
}

# =============================================================================
# HELPER: PNG Map 3 - Cost Efficiency
# =============================================================================

map_cost_efficiency <- function(sol, prov, out_path) {

  pu_available <- sol[sol$zone_assignment == 0, ]
  pu_cons      <- sol[sol$zone_assignment == 1, ]
  pu_refs      <- sol[sol$zone_assignment == 2, ]
  pu_agri      <- sol[sol$zone_assignment == 3, ]

  p <- ggplot() +
    geom_sf(data = pu_available, fill = "#DDDDDD", colour = NA) +
    geom_sf(data = pu_cons, aes(fill = cost_efficiency), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["1"]],
      limits  = c(0, 1),
      name    = "Cost\nEfficiency",
      guide   = guide_colorbar(order = 1)
    ) +
    ggnewscale::new_scale_fill() +
    geom_sf(data = pu_refs, aes(fill = cost_efficiency), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["2"]],
      limits  = c(0, 1),
      name    = "Cost\nEfficiency (REFS)",
      guide   = guide_colorbar(order = 2)
    ) +
    ggnewscale::new_scale_fill() +
    geom_sf(data = pu_agri, aes(fill = cost_efficiency), colour = NA) +
    scale_fill_gradientn(
      colours = FREQ_RAMPS[["3"]],
      limits  = c(0, 1),
      name    = "Cost\nEfficiency (Agri)",
      guide   = guide_colorbar(order = 3)
    ) +
    labs(
      title    = sprintf("Cost Efficiency - %s", toupper(gsub("_", " ", prov))),
      subtitle = "Higher score = lower cost relative to zone (more efficient selection)",
      caption  = "Endangered Wildlife Trust - 30x30 Conservation Planning"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "#555555"),
      plot.caption  = element_text(size = 8,  colour = "#888888")
    )

  ggsave(out_path, p, width = 12, height = 8, dpi = 300, bg = "white")
  cat(sprintf("  [PNG] Cost efficiency map saved\n"))
}

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  PRIORITIZR - MULTI-ZONE SOLVE\n")
cat(sprintf("  Targets: Conservation %.0f%% | REFS %.0f%% | Agri %.0f%%\n",
            TARGET_CONSERVATION * 100, TARGET_REFS * 100, TARGET_AGRI * 100))
cat(sprintf("  BLM: %g | Portfolio: %d solutions\n", BLM, N_SOLUTIONS))
cat("=============================================================\n\n")

run_summary <- list()

for (prov in provinces) {

  cat(sprintf("=============================================================\n"))
  cat(sprintf("  %s\n", toupper(gsub("_", " ", prov))))
  cat(sprintf("=============================================================\n"))

  freq_sf <- NULL

  # Output directory
  prov_out <- file.path(output_dir, prov)
  dir.create(prov_out, showWarnings = FALSE, recursive = TRUE)

  # Paths
  gpkg_path <- file.path(pu_dir, prov, sprintf("pu_%s.gpkg", prov))
  bm_path   <- file.path(pu_dir, prov, sprintf("boundary_matrix_%s.rds", prov))

  if (!file.exists(gpkg_path)) {
    cat(sprintf("  [SKIP] PU file not found\n\n"))
    next
  }

  # Load PUs
  cat("  Loading planning units...\n")
  pu <- st_read(gpkg_path, quiet = TRUE)
  cat(sprintf("  %d PUs loaded\n", nrow(pu)))

  # Ensure Albers
  if (st_crs(pu)$input != ALBERS_CRS)
    pu <- st_transform(pu, ALBERS_CRS)

  # Ensure Area_km2
  if (!"Area_km2" %in% names(pu))
    pu$Area_km2 <- as.numeric(st_area(pu)) / 1e6

  # Load boundary matrix
  bm <- NULL
  if (file.exists(bm_path) && BLM > 0) {
    bm <- readRDS(bm_path)
    cat("  Boundary matrix loaded\n")
  } else if (BLM > 0) {
    cat("  [WARN] Boundary matrix not found - running without BLM\n")
  }

  # Calculate absolute targets
  total_area <- sum(pu$Area_km2, na.rm = TRUE)
  targets_abs <- matrix(
    c(total_area * TARGET_CONSERVATION,
      total_area * TARGET_REFS,
      total_area * TARGET_AGRI),
    nrow = 1, ncol = 3
  )

  cat(sprintf("  Total area: %.0f km2\n", total_area))
  cat(sprintf("  Targets (km2): Conservation=%.0f | REFS=%.0f | Agri=%.0f\n",
              targets_abs[1,1], targets_abs[1,2], targets_abs[1,3]))

  # -------------------------------------------------------------------------
  # SOLVE
  # -------------------------------------------------------------------------
  result <- tryCatch(
    build_and_solve(pu, bm, targets_abs, BLM,
                    label = sprintf("%s core solve", prov)),
    error = function(e) {
      cat(sprintf("  [ERROR] Solve failed: %s\n", e$message))
      NULL
    }
  )

  if (is.null(result)) { cat("\n"); next }

  sol <- assign_zones(result$solution)
  sol <- compute_cost_efficiency(sol)

  # Lock check: locked PUs not assigned to their locked zone (expect 0)
  n_bad_pa   <- if (LOCK_PA)   sum(sol$locked_pa   == 1 & sol$zone_assignment != 1) else NA
  n_bad_refs <- if (LOCK_REFS) sum(sol$locked_refs == 1 & sol$zone_assignment != 2) else NA
  n_bad_agri <- if (LOCK_AGRI) sum(sol$locked_agri == 1 & sol$zone_assignment != 3) else NA
  cat(sprintf("  Lock check (locked PUs outside locked zone): PA=%s | REFS=%s | Agri=%s\n",
              n_bad_pa, n_bad_refs, n_bad_agri))
  if (any(c(n_bad_pa, n_bad_refs, n_bad_agri) > 0, na.rm = TRUE))
    cat("  [WARN] Some locked PUs were not assigned to their locked zone\n")

  # Zone summary
  zone_summary <- sol %>%
    st_drop_geometry() %>%
    group_by(zone_assignment) %>%
    summarise(
      zone_label = first(zone_label),
      n_pu       = n(),
      area_km2   = sum(Area_km2, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(pct_area = round(area_km2 / total_area * 100, 1))

  cat("\n  Zone allocation:\n")
  for (i in 1:nrow(zone_summary)) {
    cat(sprintf("    %-15s : %6d PUs  |  %8.0f km2  |  %.1f%%\n",
                zone_summary$zone_label[i],
                zone_summary$n_pu[i],
                zone_summary$area_km2[i],
                zone_summary$pct_area[i]))
  }

  # -------------------------------------------------------------------------
  # SAVE CORE OUTPUTS
  # -------------------------------------------------------------------------
  tag      <- if (nchar(SCENARIO_TAG) > 0) sprintf("_%s", SCENARIO_TAG) else ""
  sol_path <- file.path(prov_out, sprintf("solution_%s%s.gpkg", prov, tag))
  st_write(sol, sol_path, delete_dsn = TRUE, quiet = TRUE)
  cat(sprintf("\n  [SAVED] Solution gpkg\n"))

  csv_path <- file.path(prov_out, sprintf("solution_%s%s_summary.csv", prov, tag))
  write_csv(zone_summary, csv_path)
  cat(sprintf("  [SAVED] Summary CSV\n"))

  # -------------------------------------------------------------------------
  # MAP 1: ZONE ASSIGNMENT
  # -------------------------------------------------------------------------
  map_zone_assignment(
    sol       = st_transform(sol, 4326),
    prov      = prov,
    out_path  = file.path(prov_out, sprintf("map_01_zone_assignment_%s%s.png", prov, tag))
  )

  # -------------------------------------------------------------------------
  # SELECTION FREQUENCY (shuffle portfolio)
  # -------------------------------------------------------------------------
  if (COMPUTE_FREQUENCY) {
    freq_sf <- compute_selection_frequency(
      pu_tbl      = result$pu_tbl,
      pu_geom     = result$pu_geom,
      targets_abs = targets_abs,
      blm         = BLM,
      bm          = bm,
      n_solutions = N_SOLUTIONS
    )
  
    if (!is.null(freq_sf)) {
    
      # Save frequency gpkg
      freq_path <- file.path(prov_out, sprintf("selection_frequency_%s%s.gpkg", prov, tag))
      st_write(freq_sf, freq_path, delete_dsn = TRUE, quiet = TRUE)
      cat(sprintf("  [SAVED] Selection frequency gpkg\n"))
    
      # MAP 2: SELECTION FREQUENCY
      map_selection_frequency(
        freq_sf  = st_transform(freq_sf, 4326),
        sol      = st_transform(sol, 4326),
        prov     = prov,
        out_path = file.path(prov_out,
                             sprintf("map_02_selection_frequency_%s%s.png", prov, tag))
      )
    }
  
  } # end COMPUTE_FREQUENCY

  # -------------------------------------------------------------------------
  # MAP 3: COST EFFICIENCY
  # -------------------------------------------------------------------------
  map_cost_efficiency(
    sol      = st_transform(sol, 4326),
    prov     = prov,
    out_path = file.path(prov_out,
                         sprintf("map_03_cost_efficiency_%s%s.png", prov, tag))
  )

  # -------------------------------------------------------------------------
  # RUN SUMMARY
  # -------------------------------------------------------------------------
  run_summary[[prov]] <- data.frame(
    province            = prov,
    n_pu                = nrow(pu),
    total_area_km2      = round(total_area, 0),
    solve_mins          = result$solve_mins,
    target_cons_km2     = round(targets_abs[1,1], 0),
    target_refs_km2     = round(targets_abs[1,2], 0),
    target_agri_km2     = round(targets_abs[1,3], 0),
    achieved_cons_pct   = zone_summary$pct_area[zone_summary$zone_assignment == 1],
    achieved_refs_pct   = zone_summary$pct_area[zone_summary$zone_assignment == 2],
    achieved_agri_pct   = zone_summary$pct_area[zone_summary$zone_assignment == 3]
  )

  rm(pu, sol, bm, result, freq_sf); gc()
  cat("\n")
}

# =============================================================================
# SAVE RUN SUMMARY
# =============================================================================

if (length(run_summary) > 0) {

  summary_df <- bind_rows(run_summary)
  summary_path <- file.path(output_dir, sprintf("run_summary%s.csv", 
                                                if (nchar(SCENARIO_TAG) > 0) sprintf("_%s", SCENARIO_TAG) else ""))
  write_csv(summary_df, summary_path)

  cat("=============================================================\n")
  cat("  ALL PROVINCES COMPLETE\n")
  cat("=============================================================\n\n")

  print(summary_df %>%
          select(province, n_pu, solve_mins,
                 achieved_cons_pct, achieved_refs_pct, achieved_agri_pct))

  cat(sprintf("\nRun summary saved to: %s\n", summary_path))
}