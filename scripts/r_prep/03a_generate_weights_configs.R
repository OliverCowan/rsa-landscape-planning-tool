# =============================================================================
# 03a: GENERATE PROVINCE WEIGHTS CONFIGS
# =============================================================================
#
# PURPOSE:
#   Generates one weights CSV per province from the master config.
#   These CSVs control which layers are included in composite cost rasters
#   and what weight each layer receives per zone (bio / refs / agri).
#
# WHEN TO RUN:
#   - Once initially to generate default weights
#   - When new layers are added to layer_config_template.csv
#   - When province applicability changes in the master config
#
# IMPORTANT:
#   Re-running this script will OVERWRITE existing weights CSVs,
#   discarding any manual weight changes. Only run when you want to
#   reset weights to master config defaults.
#   To change weights for a run, edit weights/{province}_weights.csv
#   directly and run 03b_composite_cost.R instead.
#
# INPUT:
#   layer_config_template.csv   (repo root; see 02_reclassification.R header)
#     Columns read here: layer_id, layer_name, status, zone,
#     component_weight_bio, component_weight_refs, component_weight_agri,
#     and the per-scope TRUE/FALSE columns (national ... western_cape)
#
# OUTPUT:
#   weights/{province}_weights.csv for each of 10 scopes (national + 9)
#
# =============================================================================

library(readr)
library(dplyr)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir    <- here::here()
config_path <- file.path(base_dir, "layer_config_template.csv")
weights_dir <- file.path(base_dir, "weights")

# =============================================================================
# SETUP
# =============================================================================

dir.create(weights_dir, showWarnings = FALSE)

config <- read_csv(config_path, show_col_types = FALSE) %>%
  filter(status == "confirmed")

cat(sprintf("Confirmed layers in master config: %d\n\n", nrow(config)))

provinces <- c("national", "eastern_cape", "free_state", "gauteng",
               "kwazulu_natal", "limpopo", "mpumalanga", "north_west",
               "northern_cape", "western_cape")

# =============================================================================
# GENERATE PER-PROVINCE WEIGHTS CONFIGS
# =============================================================================

summary_rows <- list()

for (prov in provinces) {

  prov_weights <- config %>%
    # Only layers explicitly TRUE for this province
    filter(!is.na(.data[[prov]]) & as.logical(.data[[prov]])) %>%
    mutate(
      # Include flags: layer must be in zone AND have a non-NA weight
      include_bio  = grepl("bio|all",  zone) & !is.na(component_weight_bio),
      include_refs = grepl("refs|all", zone) & !is.na(component_weight_refs),
      include_agri = grepl("agri|all", zone) & !is.na(component_weight_agri),

      # Weights: NA if not applicable to zone
      bio_weight   = ifelse(include_bio,  component_weight_bio,  NA),
      refs_weight  = ifelse(include_refs, component_weight_refs, NA),
      agri_weight  = ifelse(include_agri, component_weight_agri, NA),

      # Empty notes column for manual annotation
      notes = ""
    ) %>%
    # Remove layers not active in any zone for this province
    filter(include_bio | include_refs | include_agri) %>%
    select(layer_id, layer_name,
           bio_weight, refs_weight, agri_weight,
           include_bio, include_refs, include_agri,
           notes)

  out_path <- file.path(weights_dir, paste0(prov, "_weights.csv"))
  write_csv(prov_weights, out_path)

  cat(sprintf("%-15s | %2d layers | bio: %2d | refs: %2d | agri: %2d\n",
              prov,
              nrow(prov_weights),
              sum(prov_weights$include_bio),
              sum(prov_weights$include_refs),
              sum(prov_weights$include_agri)))

  summary_rows[[prov]] <- data.frame(
    province    = prov,
    n_layers    = nrow(prov_weights),
    n_bio       = sum(prov_weights$include_bio),
    n_refs      = sum(prov_weights$include_refs),
    n_agri      = sum(prov_weights$include_agri)
  )
}

# =============================================================================
# SUMMARY
# =============================================================================

cat(sprintf("\nWeights configs saved to: %s\n", weights_dir))
cat("  Edit these CSVs to adjust weights before running 03b_composite_cost.R\n")
cat("  WARNING: Re-running this script will overwrite any manual changes.\n")
