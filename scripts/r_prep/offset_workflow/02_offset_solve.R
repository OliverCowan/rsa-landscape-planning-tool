# =============================================================================
# BIODIVERSITY OFFSET WORKFLOW — SCRIPT 02: PRIORITIZR SOLVE
# =============================================================================
#
# PURPOSE:
#   Single-zone prioritizr solve identifying Conservation priority areas for
#   offset placement. REFS and Agriculture PUs are locked out of selection.
#   Protected Areas are locked in.
#
# INPUT:
#   outputs/{MAPCODE}/01_setup/   (from Script 01)
#   data/planning_units/{province}/pu_{province}.gpkg
#
# OUTPUT:
#   outputs/{MAPCODE}/02_solve/
#     solution_{MAPCODE}_{province}.gpkg
#     target_coverage_{MAPCODE}_{province}.csv
#     irreplaceability_{MAPCODE}_{province}.csv
#
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(prioritizr)

# =============================================================================
# USER INPUT
# =============================================================================

TARGET_MAPCODE  <- "Gm8"   # Must match Script 01
N_SOLUTIONS     <- 100       # Portfolio size for irreplaceability analysis
BLM             <- 0.001     # Boundary length modifier — encourages spatial clumping

# =============================================================================
# PATHS
# =============================================================================

base_dir   <- "C:/Users/OliverCowan/OneDrive - EWT/Documents/02. Data Analyses/03. JRS 30x30/R Workflow_30x30"
offset_dir <- file.path(base_dir, "Biodiversity_Offset_Workflow")
setup_dir  <- file.path(offset_dir, "outputs", TARGET_MAPCODE, "01_setup")
out_dir    <- file.path(offset_dir, "outputs", TARGET_MAPCODE, "02_solve")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

pu_dir     <- file.path(base_dir, "data/planning_units")

# =============================================================================
# LOAD SETUP SUMMARY FROM SCRIPT 01
# =============================================================================

cat("=============================================================\n")
cat("  BIODIVERSITY OFFSET WORKFLOW — SCRIPT 02: SOLVE\n")
cat("=============================================================\n\n")

summary_path <- file.path(setup_dir,
                          sprintf("offset_summary_%s.csv", TARGET_MAPCODE))
if (!file.exists(summary_path)) {
  stop("Script 01 summary not found. Run 01_offset_setup.R first.")
}

offset_summary        <- read_csv(summary_path, show_col_types = FALSE)
conservation_target   <- offset_summary$conservation_target_pct[1] / 100
veg_name              <- offset_summary$veg_name[1]
overlapping_provinces <- strsplit(offset_summary$overlapping_provinces[1], ", ")[[1]]
fatal_flaw            <- offset_summary$fatal_flaw[1]

cat(sprintf("Target:    %s (%s)\n", veg_name, TARGET_MAPCODE))
cat(sprintf("Target:    %.0f%% conservation\n", conservation_target * 100))
cat(sprintf("Provinces: %s\n\n", paste(overlapping_provinces, collapse = ", ")))

if (fatal_flaw) {
  cat("*** WARNING: Fatal flaw flagged in Script 01. ***\n")
  cat("*** Proceeding with solve for reference only. ***\n\n")
}

# =============================================================================
# SOLVE PER PROVINCE
# =============================================================================

for (prov in overlapping_provinces) {
  
  cat(sprintf("-------------------------------------------------------------\n"))
  cat(sprintf("  Processing: %s\n", prov))
  cat(sprintf("-------------------------------------------------------------\n"))
  
  # ------------------------------------------------------------------
  # Load planning units
  # ------------------------------------------------------------------
  pu_path <- file.path(pu_dir, prov, sprintf("pu_%s.gpkg", prov))
  if (!file.exists(pu_path)) {
    cat(sprintf("  [SKIP] PU file not found: %s\n\n", pu_path))
    next
  }
  pu <- st_read(pu_path, quiet = TRUE)
  cat(sprintf("  PUs loaded: %d\n", nrow(pu)))
  
  # ------------------------------------------------------------------
  # Load NVM clip for this province (from Script 01)
  # ------------------------------------------------------------------
  nvm_clip_path <- file.path(setup_dir,
                             sprintf("nvm_%s_%s.gpkg", TARGET_MAPCODE, prov))
  if (!file.exists(nvm_clip_path)) {
    cat(sprintf("  [SKIP] NVM clip not found: %s\n\n", nvm_clip_path))
    next
  }
  nvm_clip <- st_read(nvm_clip_path, quiet = TRUE) %>%
    st_transform(st_crs(pu))
  
  # ------------------------------------------------------------------
  # Calculate veg type coverage per PU
  # ------------------------------------------------------------------
  cat("  Calculating veg type coverage per PU...\n")
  
  nvm_union <- st_union(nvm_clip)
  int       <- st_intersection(pu %>% select(pu_id), nvm_union)
  int_area  <- as.numeric(st_area(int))
  
  int_df <- data.frame(pu_id = int$pu_id, nvm_area_m2 = int_area)
  
  pu_features <- pu %>%
    st_drop_geometry() %>%
    select(pu_id, Area_km2) %>%
    left_join(int_df, by = "pu_id") %>%
    mutate(
      nvm_area_m2  = ifelse(is.na(nvm_area_m2), 0, nvm_area_m2),
      nvm_area_km2 = nvm_area_m2 / 1e6
    )
  
  n_with_veg <- sum(pu_features$nvm_area_km2 > 0)
  cat(sprintf("  PUs containing veg type: %d (%.1f%%)\n",
              n_with_veg, n_with_veg / nrow(pu) * 100))
  
  if (n_with_veg == 0) {
    cat(sprintf("  [SKIP] No PU coverage for veg type in %s\n\n", prov))
    next
  }
  
  # ------------------------------------------------------------------
  # Filter to veg-type PUs only
  # Offset solve should rank within the veg type, not province-wide
  # ------------------------------------------------------------------
  veg_idx     <- which(pu_features$nvm_area_km2 > 0)
  pu          <- pu[veg_idx, ]
  pu_features <- pu_features[veg_idx, ]
  cat(sprintf("  Filtered to %d PUs containing veg type\n", nrow(pu)))
  
  # ------------------------------------------------------------------
  # Targets
  # ------------------------------------------------------------------
  cat(sprintf("  Locked PUs — PAs: %d | REFS: %d | Agriculture: %d\n",
              sum(pu$locked_pa), sum(pu$locked_refs), sum(pu$locked_agri)))
  
  total_veg_area_km2 <- sum(pu_features$nvm_area_km2)
  abs_target_km2     <- total_veg_area_km2 * conservation_target
  
  cat(sprintf("  Total veg area in province PUs: %.0f ha\n",
              total_veg_area_km2 * 100))
  cat(sprintf("  Absolute target (%.0f%%): %.0f ha\n",
              conservation_target * 100, abs_target_km2 * 100))
  
  # ------------------------------------------------------------------
  # Build and solve problem
  # ------------------------------------------------------------------
  # ------------------------------------------------------------------
  # Feasibility check — available area must exceed target
  # Uses pu columns directly (pu_tbl not yet defined)
  # ------------------------------------------------------------------
  locked_out_lgl <- as.logical(pu$locked_refs == 1 | pu$locked_agri == 1)
  locked_pa_lgl  <- as.logical(pu$locked_pa)
  available_km2  <- sum(pu_features$nvm_area_km2[!locked_out_lgl])
  pa_km2         <- sum(pu_features$nvm_area_km2[locked_pa_lgl])
  
  cat(sprintf("  Available veg area (excl. locked out): %.0f ha\n",
              available_km2 * 100))
  cat(sprintf("  Already in PAs (locked in):            %.0f ha\n",
              pa_km2 * 100))
  
  if (available_km2 < abs_target_km2) {
    cat(sprintf("  [SKIP] Infeasible — available area (%.0f ha) < target (%.0f ha)\n",
                available_km2 * 100, abs_target_km2 * 100))
    cat(sprintf("  Province likely contains only a small sliver of veg type.\n\n"))
    next
  }
  
  cat("  Building prioritizr problem...\n")
  
  pu_tbl  <- st_drop_geometry(pu)
  pu_geom <- st_geometry(pu)
  
  pu_tbl$feat_veg      <- pu_features$nvm_area_km2
  pu_tbl$locked_pa_lgl <- as.logical(pu_tbl$locked_pa)
  pu_tbl$locked_out    <- as.logical(pu_tbl$locked_refs == 1 |
                                       pu_tbl$locked_agri == 1)
  
  # Compute boundary matrix for veg-type PU subset only
  # Computing fresh on the subset is cheaper than loading + subsetting
  # a full provincial matrix (which can be 150k x 150k)
  if (BLM > 0) {
    cat("  Computing boundary matrix for veg-type PUs...\n")
    bound_sub <- tryCatch({
      bm <- boundary_matrix(pu)
      use_blm <- TRUE
      cat(sprintf("  Boundary matrix computed: %d x %d\n", nrow(bm), ncol(bm)))
      bm
    }, error = function(e) {
      cat(sprintf("  [WARNING] Boundary matrix failed (%s) — BLM disabled\n",
                  conditionMessage(e)))
      use_blm <<- FALSE
      NULL
    })
  } else {
    use_blm <- FALSE
  }
  
  p <- problem(
    x           = pu_tbl,
    features    = "feat_veg",
    cost_column = "cost_bio"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(abs_target_km2) %>%
    add_binary_decisions()
  
  # Only add lock constraints if qualifying PUs exist within veg type extent
  if (any(pu_tbl$locked_pa_lgl)) {
    p <- p %>% add_locked_in_constraints("locked_pa_lgl")
  } else {
    cat("  No PAs within veg type extent — lock-in constraint skipped
")
  }
  if (any(pu_tbl$locked_out)) {
    p <- p %>% add_locked_out_constraints("locked_out")
  } else {
    cat("  No REFS/Agriculture within veg type extent — lock-out constraint skipped
")
  }
  
  if (use_blm) {
    p <- p %>% add_boundary_penalties(penalty = BLM, edge_factor = 0.5,
                                      data = bound_sub)
    cat(sprintf("  BLM = %.4f applied\n", BLM))
  }
  
  p <- p %>% add_gurobi_solver(gap = 0.01, verbose = FALSE)
  
  cat("  Solving...\n")
  t_start <- Sys.time()
  sol     <- solve(p, force = TRUE)
  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)
  cat(sprintf("  Solved in %.2f minutes\n", elapsed))
  cat(sprintf("  PUs selected: %d\n", sum(sol$solution_1)))
  cat(sprintf("  Veg area secured: %.0f ha\n",
              sum(sol$solution_1 * pu_tbl$feat_veg) * 100))
  
  # ------------------------------------------------------------------
  # Attach geometry and zone labels
  # ------------------------------------------------------------------
  sol <- st_sf(sol, geometry = pu_geom)
  sol$offset_priority <- sol$solution_1
  sol$zone_assignment <- ifelse(sol$solution_1 == 1,     "Conservation",
                                ifelse(pu_tbl$locked_refs == 1, "REFS",
                                       ifelse(pu_tbl$locked_agri == 1, "Agriculture",
                                              "Not selected")))
  
  # ------------------------------------------------------------------
  # Irreplaceability via shuffle portfolio
  # Run N_SOLUTIONS and calculate selection frequency per PU
  # ------------------------------------------------------------------
  cat(sprintf("  Running portfolio of %d solutions for irreplaceability...\n",
              N_SOLUTIONS))
  t_start2 <- Sys.time()
  
  p_port <- p %>%
    add_shuffle_portfolio(number_solutions = N_SOLUTIONS,
                          threads          = 1,
                          remove_duplicates = FALSE)
  
  sol_port  <- solve(p_port, force = TRUE)
  elapsed2  <- round(as.numeric(difftime(Sys.time(), t_start2, units = "mins")), 2)
  cat(sprintf("  Portfolio solved in %.2f minutes\n", elapsed2))
  
  # Selection frequency = proportion of solutions in which each PU was selected
  sol_cols  <- grep("^solution_", names(sol_port), value = TRUE)
  sel_matrix <- as.matrix(st_drop_geometry(sol_port)[, sol_cols])
  sel_freq   <- rowMeans(sel_matrix)
  
  # Attach to solution
  sol$irreplaceability <- sel_freq
  
  # Classify into 10 bins (0 = never selected, then 10% intervals)
  sol$irr_class <- cut(
    sel_freq,
    breaks = c(-Inf, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf),
    labels = c("0%", "1-10%", "11-20%", "21-30%", "31-40%",
               "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%"),
    right  = TRUE
  )
  sol$irr_class <- as.character(sol$irr_class)
  sol$irr_class[sel_freq == 0] <- "0%"
  
  cat(sprintf("  Irreplaceability distribution:\n"))
  print(table(sol$irr_class))
  
  # Save irreplaceability summary CSV
  irr_df <- st_drop_geometry(sol) %>%
    select(pu_id, irreplaceability, irr_class, zone_assignment) %>%
    arrange(desc(irreplaceability))
  write_csv(irr_df,
            file.path(out_dir,
                      sprintf("irreplaceability_%s_%s.csv",
                              TARGET_MAPCODE, prov)))
  cat(sprintf("  Irreplaceability saved\n\n"))
  
  # ------------------------------------------------------------------
  # Target coverage summary
  # ------------------------------------------------------------------
  cov <- eval_target_coverage_summary(p, sol %>%
                                        st_drop_geometry() %>%
                                        select(solution_1))
  write_csv(cov, file.path(out_dir,
                           sprintf("target_coverage_%s_%s.csv",
                                   TARGET_MAPCODE, prov)))
  
  # ------------------------------------------------------------------
  # Save solution
  # ------------------------------------------------------------------
  sol_path <- file.path(out_dir,
                        sprintf("solution_%s_%s.gpkg", TARGET_MAPCODE, prov))
  if (file.exists(sol_path)) file.remove(sol_path)
  st_write(sol, sol_path, quiet = TRUE)
  cat(sprintf("  Solution saved: %s\n\n", sol_path))
}

cat("=============================================================\n")
cat("  SOLVE COMPLETE — Ready for Script 03: Outputs\n")
cat("=============================================================\n")