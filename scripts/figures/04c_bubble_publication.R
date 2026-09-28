# =============================================================================
# COST LAYER BUBBLE PLOT - ALL PROVINCES (Publication Style)
# =============================================================================
#
# PURPOSE:
#   Bubble plot showing each layer's weight per zone for all provinces
#   Bubble size = raw weight
#   Bubble colour = green (benefit) or red (cost) per zone
#   No bubble = not applicable
#   Ordered by bio_weight desc, then refs_weight desc, then agri_weight desc
#
# INPUT:
#   layer_config_template.csv   (invert_* flags: benefit vs cost)
#   weights/{province}_weights.csv   (from 03a)
#
# OUTPUT:
#   outputs/figures/bubble/bubble_{province}_publication.png
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
out_dir     <- file.path(base_dir, "outputs/figures/bubble")
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

config <- read_csv(config_path, show_col_types = FALSE) %>%
  filter(status == "confirmed") %>%
  select(layer_id, invert_bio, invert_refs, invert_agri) %>%
  distinct(layer_id, .keep_all = TRUE)

# =============================================================================
# BUBBLE PLOT FUNCTION
# =============================================================================

make_bubble <- function(prov, weights_dir, config, out_dir, province_labels) {

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

  # Reshape weights
  weights_long <- df %>%
    select(layer_id, layer_name, bio_weight, refs_weight, agri_weight) %>%
    pivot_longer(cols = c(bio_weight, refs_weight, agri_weight),
                 names_to = "zone", values_to = "weight") %>%
    mutate(zone = gsub("_weight", "", zone))

  # Reshape include flags
  include_long <- df %>%
    select(layer_id, include_bio, include_refs, include_agri) %>%
    pivot_longer(cols = c(include_bio, include_refs, include_agri),
                 names_to = "zone", values_to = "included") %>%
    mutate(zone = gsub("include_", "", zone))

  # Reshape invert flags
  invert_long <- df %>%
    select(layer_id, invert_bio, invert_refs, invert_agri) %>%
    pivot_longer(cols = c(invert_bio, invert_refs, invert_agri),
                 names_to = "zone", values_to = "is_benefit") %>%
    mutate(zone = gsub("invert_", "", zone))

  plot_data <- weights_long %>%
    left_join(include_long, by = c("layer_id", "zone")) %>%
    left_join(invert_long,  by = c("layer_id", "zone")) %>%
    filter(included, !is.na(weight)) %>%
    mutate(
      direction = ifelse(is_benefit, "Benefit", "Cost"),
      zone      = factor(toupper(zone), levels = c("BIO", "REFS", "AGRI"))
    )

  # Order: bio desc, refs desc, agri desc
  layer_order <- df %>%
    mutate(
      bio_weight  = replace_na(bio_weight,  0),
      refs_weight = replace_na(refs_weight, 0),
      agri_weight = replace_na(agri_weight, 0)
    ) %>%
    arrange(desc(bio_weight), desc(refs_weight), desc(agri_weight), layer_name) %>%
    distinct(layer_name) %>%
    pull(layer_name)

  plot_data <- plot_data %>%
    mutate(layer_name = factor(layer_name, levels = rev(layer_order)))

  n_layers    <- length(unique(plot_data$layer_name))
  plot_height <- max(8, n_layers * 0.35 + 2)

  p <- ggplot(plot_data,
              aes(x = zone, y = layer_name,
                  size   = weight,
                  colour = direction,
                  fill   = direction)) +

    # Alternating row bands
    geom_tile(
      data = df %>%
        distinct(layer_name) %>%
        mutate(
          layer_name = factor(layer_name, levels = rev(layer_order)),
          row_num    = as.integer(layer_name),
          fill_band  = ifelse(row_num %% 2 == 0, "even", "odd")
        ) %>%
        crossing(zone = factor(c("BIO","REFS","AGRI"),
                               levels = c("BIO","REFS","AGRI"))),
      aes(x = zone, y = layer_name, fill = fill_band),
      colour      = NA,
      height      = 1,
      width       = 1,
      inherit.aes = FALSE,
      alpha       = 0.4
    ) +

    scale_fill_manual(
      values = c(even    = "#f5f5f5",
                 odd     = "#ffffff",
                 Benefit = "#41ab5d",
                 Cost    = "#ef8a62"),
      guide  = "none"
    ) +

    # Bubbles
    geom_point(shape = 21, stroke = 0.8, alpha = 0.85) +

    # Weight label
    geom_text(aes(label = sprintf("%.1f", weight)),
              colour   = "white",
              size     = 2.8,
              fontface = "bold") +

    scale_colour_manual(
      values = c(Benefit = "#1a6b3c", Cost = "#b2182b"),
      guide  = "none"
    ) +

    scale_size_continuous(
      range  = c(6, 22),
      limits = c(1, 4),
      guide  = "none"
    ) +

    scale_x_discrete(position = "top", expand = expansion(add = c(0.5, 0.5))) +
    scale_y_discrete(expand = expansion(add = 0.6)) +

    labs(
      title    = "Cost Layer Weights by Zone",
      subtitle = sprintf("%s  |  Bubble size & label = weight  |  Green = Benefit  |  Red = Cost",
                         prov_label),
      caption  = "Source: EWT 30x30 Conservation Planning | Weights derived from layer_config_template.csv",
      x = NULL,
      y = NULL
    ) +

    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, hjust = 0),
      plot.subtitle    = element_text(size = 9, colour = "grey40", hjust = 0,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 8, colour = "grey60", hjust = 0),
      axis.text.x      = element_text(face = "bold", size = 12, colour = "grey20"),
      axis.text.y      = element_text(size = 9, colour = "grey20"),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position  = "none",
      plot.margin      = margin(15, 15, 15, 15),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )

  out_path <- file.path(out_dir, sprintf("bubble_%s_publication.png", prov))
  ggsave(out_path, p,
         width  = 9,
         height = plot_height,
         dpi    = 300,
         bg     = "white")

  cat(sprintf("  [DONE] %-15s | %2d layers | %s\n", prov, n_layers, out_path))
}

# =============================================================================
# RUN ALL PROVINCES
# =============================================================================

cat("=============================================================\n")
cat("  BUBBLE PLOT GENERATION - ALL PROVINCES (Publication)\n")
cat("=============================================================\n\n")

for (prov in provinces) {
  make_bubble(prov, weights_dir, config, out_dir, province_labels)
}

cat(sprintf("\nAll bubble plots saved to: %s\n", out_dir))
