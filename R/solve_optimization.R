# =============================================================================
# solve_optimization.R
# Wrapper function for prioritizr optimization in Shiny app
# =============================================================================

#' Run Multi-Zone Land Allocation Optimization
#' 
#' @param planning_units sf object with planning units
#' @param boundary_matrix boundary matrix (from boundary_matrix())
#' @param targets list with conservation, refs, agriculture percentages
#' @param blm numeric boundary length modifier (0-20)
#' @param locked_areas list of sf objects (pa, refs, agri) or NULL
#' @param solver character "gurobi" or "highs"
#' @param time_limit numeric max solve time in seconds
#' @param portfolio logical run portfolio optimization?
#' @param n_solutions integer number of portfolio solutions
#' 
#' @return list with solution (sf object), stats, solve_time
solve_landscape_optimization <- function(
  planning_units,
  boundary_matrix,
  targets = list(conservation = 35, refs = 10, agriculture = 25),
  blm = 10,
  locked_areas = NULL,
  solver = "gurobi",
  time_limit = 300,
  portfolio = FALSE,
  n_solutions = 10
) {
  
  # Start timer
  start_time <- Sys.time()
  
  cat("=================================================================\n")
  cat("RUNNING OPTIMIZATION\n")
  cat("=================================================================\n\n")
  
  # Print settings
  cat("Settings:\n")
  cat(sprintf("  Conservation: %d%%\n", targets$conservation))
  cat(sprintf("  REFS: %d%%\n", targets$refs))
  cat(sprintf("  Agriculture: %d%%\n", targets$agriculture))
  cat(sprintf("  BLM: %d\n", blm))
  cat(sprintf("  Solver: %s\n", solver))
  cat(sprintf("  Time limit: %d seconds\n", time_limit))
  cat("\n")
  
  # ============================================================================
  # SETUP PROBLEM
  # ============================================================================
  
  # Calculate target number of PUs for each zone
  n_total <- nrow(planning_units)
  n_conservation <- round(n_total * targets$conservation / 100)
  n_refs <- round(n_total * targets$refs / 100)
  n_agriculture <- round(n_total * targets$agriculture / 100)
  
  cat("Targets:\n")
  cat(sprintf("  Conservation: %d PUs\n", n_conservation))
  cat(sprintf("  REFS: %d PUs\n", n_refs))
  cat(sprintf("  Agriculture: %d PUs\n", n_agriculture))
  cat("\n")
  
  # Create zone features (dummy features for area allocation)
  planning_units$zone_conservation <- 1
  planning_units$zone_refs <- 1
  planning_units$zone_agriculture <- 1
  planning_units$zone_available <- 1
  
  # TODO: Implement multi-zone problem structure
  # This is where we'll integrate your prioritizr multi-zone syntax
  # when we get it working with Gurobi
  
  # For now: placeholder message
  cat("⚠ Optimization not yet implemented\n")
  cat("  Waiting for Gurobi license and working prioritizr syntax\n\n")
  
  # ============================================================================
  # SOLVE (PLACEHOLDER)
  # ============================================================================
  
  # This will be replaced with actual solve code:
  # problem <- problem(...) %>%
  #   add_min_set_objective() %>%
  #   add_manual_targets(...) %>%
  #   add_boundary_penalties(blm, data = boundary_matrix) %>%
  #   add_locked_in_constraints(...) %>%
  #   add_gurobi_solver(time_limit = time_limit)
  # 
  # solution <- solve(problem)
  
  # For now: return planning units with dummy solution
  solution <- planning_units
  solution$solution_zone <- sample(1:4, nrow(solution), replace = TRUE)
  
  # ============================================================================
  # CALCULATE STATISTICS
  # ============================================================================
  
  solve_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
  # Count PUs per zone
  zone_counts <- table(solution$solution_zone)
  
  stats <- list(
    n_conservation = as.numeric(zone_counts["1"]),
    n_refs = as.numeric(zone_counts["2"]),
    n_agriculture = as.numeric(zone_counts["3"]),
    n_available = as.numeric(zone_counts["4"]),
    total_cost = sum(solution$biodiversity_cost, na.rm = TRUE),  # Placeholder
    solve_time_min = solve_time,
    gap_percent = 0.0  # Will come from solver
  )
  
  cat("=================================================================\n")
  cat("OPTIMIZATION COMPLETE\n")
  cat("=================================================================\n\n")
  
  cat(sprintf("Solve time: %.1f minutes\n", solve_time))
  cat(sprintf("Conservation: %d PUs (%.1f%%)\n", 
              stats$n_conservation, 
              stats$n_conservation/n_total*100))
  cat(sprintf("REFS: %d PUs (%.1f%%)\n", 
              stats$n_refs,
              stats$n_refs/n_total*100))
  cat(sprintf("Agriculture: %d PUs (%.1f%%)\n",
              stats$n_agriculture,
              stats$n_agriculture/n_total*100))
  cat("\n")
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  return(list(
    solution = solution,
    stats = stats,
    solve_time = solve_time,
    targets = targets,
    blm = blm,
    timestamp = Sys.time()
  ))
}


#' Calculate Selection Frequency from Portfolio
#' 
#' @param portfolio_solutions list of solution sf objects
#' @return sf object with selection_freq field (0-1)
calculate_selection_frequency <- function(portfolio_solutions) {
  
  # TODO: Implement when portfolio optimization works
  # For each planning unit, calculate % of solutions where it was selected
  
  return(NULL)
}


#' Identify Core vs Flexible Areas
#' 
#' @param selection_freq sf object with selection_freq field
#' @param threshold numeric (0-1) threshold for "core" areas
#' @return sf object with area_type field ("core", "flexible", "rarely_selected")
classify_areas <- function(selection_freq, threshold = 0.75) {
  
  # TODO: Implement
  # Core: selected in >75% of solutions
  # Flexible: selected in 25-75% of solutions
  # Rarely selected: <25%
  
  return(NULL)
}
