# =============================================================================
# COST LAYER HEATMAP - ALL PROVINCES (Dark Theme)
# =============================================================================
#
# PURPOSE:
#   Generates cost layer weight heatmaps for all provinces and national
#   showing benefit (green) vs cost (red) layers across bio/refs/agri zones
#   Dark theme version for presentations
#
# INPUT:
#   layer_config_template.csv   (invert_* flags: benefit vs cost)
#   weights/{province}_weights.csv   (from 03a)
#
# OUTPUT:
#   outputs/figures/heatmaps/heatmap_{province}_dark.png
#
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir    <- here::here()
config_path <- file.path(base_dir, "layer_config_template.csv")
weights_dir <- file.path(base_dir, "weights")
out_dir     <- file.path(base_dir, "outputs/figures/heatmaps")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SETUP
# =============================================================================

provinces <- c("national", "eastern_cape", "free_state", "gauteng",
               "kwazulu_natal", "limpopo", "mpumalanga", "north_west",
               "northern_cape", "western_cape")

province_labels <- c(
  national      = "National",
  eastern_cape  = "Eastern Cape",
  free_state    = "Free State",
  gauteng       = "Gauteng",
  kwazulu_natal = "KwaZulu-Natal",
  limpopo       = "Limpopo",
  mpumalanga    = "Mpumalanga",
  north_west    = "North West",
  northern_cape = "Northern Cape",
  western_cape  = "Western Cape"
)

# Dark theme colours
bg_colour       <- "#1a1a2e"
panel_colour    <- "#16213e"
text_colour     <- "#e0e0e0"
subtitle_colour <- "#a8dadc"
grid_colour     <- "#2a2a4a"
na_colour       <- "#2d2d44"

config <- read_csv(config_path, show_col_types = FALSE) %>%
  filter(status == "confirmed") %>%
  select(layer_id, invert_bio, invert_refs, invert_agri) %>%
  distinct(layer_id, .keep_all = TRUE)

# =============================================================================
# HEATMAP FUNCTION
# =============================================================================

make_heatmap_dark <- function(prov, weights_dir, config, out_dir, province_labels) {

  prov_label   <- province_labels[[prov]]
  weights_path <- file.path(weights_dir, paste0(prov, "_weights.csv"))

  if (!file.exists(weights_path)) {
    cat(sprintf("  [SKIP] No weights config: %s\n", prov))
    return(invisible(NULL))
  }

  weights <- read_csv(weights_path, show_col_types = FALSE)

  df <- weights %>%
    left_join(config %>% select(layer_id, invert_bio, invert_refs, invert_agri),
              by = "layer_id")

  # Reshape weights to long
  weights_long <- df %>%
    select(layer_id, layer_name, bio_weight, refs_weight, agri_weight) %>%
    pivot_longer(cols = c(bio_weight, refs_weight, agri_weight),
                 names_to = "zone", values_to = "weight") %>%
    mutate(zone = gsub("_weight", "", zone))

  # Reshape include flags to long
  include_long <- df %>%
    select(layer_id, include_bio, include_refs, include_agri) %>%
    pivot_longer(cols = c(include_bio, include_refs, include_agri),
                 names_to = "zone", values_to = "included") %>%
    mutate(zone = gsub("include_", "", zone))

  # Reshape invert flags to long
  invert_long <- df %>%
    select(layer_id, invert_bio, invert_refs, invert_agri) %>%
    pivot_longer(cols = c(invert_bio, invert_refs, invert_agri),
                 names_to = "zone", values_to = "is_benefit") %>%
    mutate(zone = gsub("invert_", "", zone))

  # Join all
  plot_data <- weights_long %>%
    left_join(include_long, by = c("layer_id", "zone")) %>%
    left_join(invert_long,  by = c("layer_id", "zone")) %>%
    mutate(
      weight        = ifelse(included, weight, NA),
      is_benefit    = ifelse(included, is_benefit, NA),
      signed_weight = case_when(
        !included   ~ NA_real_,
        is_benefit  ~ weight,
        !is_benefit ~ -weight
      ),
      zone = factor(toupper(zone), levels = c("BIO", "REFS", "AGRI"))
    )

  # Order layers by dominant weight descending
  layer_order <- df %>%
    mutate(sort_val = coalesce(bio_weight, refs_weight, agri_weight)) %>%
    arrange(desc(sort_val), layer_name) %>%
    distinct(layer_name) %>%
    pull(layer_name)

  plot_data <- plot_data %>%
    mutate(layer_name = factor(layer_name, levels = rev(layer_order)))

  max_weight <- max(abs(plot_data$signed_weight), na.rm = TRUE)
  n_layers   <- nrow(df)
  plot_height <- max(8, n_layers * 0.32 + 2)

  p <- ggplot(plot_data, aes(x = zone, y = layer_name, fill = signed_weight)) +
    geom_tile(colour = grid_colour, linewidth = 0.6, na.rm = TRUE) +
    geom_text(aes(label = ifelse(!is.na(weight), sprintf("%.1f", weight), "")),
              colour = "white", size = 3, fontface = "bold") +
    scale_fill_gradientn(
      colours  = c("#b2182b", "#ef8a62", "#f4a582",
                   "#3a3a5c",
                   "#a1d99b", "#41ab5d", "#1a6b3c"),
      values   = scales::rescale(c(-max_weight, -2, -1, 0, 1, 2, max_weight)),
      limits   = c(-max_weight, max_weight),
      na.value = na_colour,
      name     = "Weight\n(+ Benefit / - Cost)",
      guide    = guide_colorbar(
        barwidth       = 1,
        barheight      = 10,
        title.position = "top",
        title.theme    = element_text(colour = text_colour, size = 9, face = "bold"),
        label.theme    = element_text(colour = text_colour, size = 8)
      )
    ) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title    = "Cost Layer Weights by Zone",
      subtitle = sprintf("%s  |  Green = Benefit layer  |  Red = Cost layer  |  Intensity = Weight  |  Grey = Not applicable",
                         prov_label),
      caption  = "Source: EWT 30x30 Conservation Planning | Weights derived from layer_config_template.csv",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      # Backgrounds
      plot.background  = element_rect(fill = bg_colour,    colour = NA),
      panel.background = element_rect(fill = panel_colour, colour = NA),
      legend.background = element_rect(fill = bg_colour,   colour = NA),
      legend.key        = element_rect(fill = bg_colour,   colour = NA),
    
      # Text
      plot.title    = element_text(colour = text_colour,    face = "bold",
                                   size = 14, hjust = 0),
      plot.subtitle = element_text(colour = subtitle_colour, size = 9,
                                   hjust = 0, margin = margin(b = 10)),
      plot.caption  = element_text(colour = "#666688",  size = 8, hjust = 0),
      axis.text.x   = element_text(colour = subtitle_colour, face = "bold", size = 11),
      axis.text.y   = element_text(colour = text_colour,  size = 9),
    
      # Grid
      panel.grid    = element_blank(),
    
      # Legend
      legend.position = "right",
      legend.title    = element_text(colour = text_colour, size = 9, face = "bold"),
      legend.text     = element_text(colour = text_colour, size = 8),
    
      plot.margin = margin(15, 15, 15, 15)
    )

  out_path <- file.path(out_dir, sprintf("heatmap_%s_dark.png", prov))
  ggsave(out_path, p,
         width  = 9,
         height = plot_height,
         dpi    = 300,
         bg     = bg_colour)

  cat(sprintf("  [DONE] %-15s | %2d layers | %s\n", prov, n_layers, out_path))
}

# =============================================================================
# RUN ALL PROVINCES
# =============================================================================

cat("=============================================================\n")
cat("  HEATMAP GENERATION - ALL PROVINCES (Dark Theme)\n")
cat("=============================================================\n\n")

for (prov in provinces) {
  make_heatmap_dark(prov, weights_dir, config, out_dir, province_labels)
}

cat(sprintf("\nAll dark theme heatmaps saved to: %s\n", out_dir))
