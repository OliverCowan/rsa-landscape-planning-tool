# =============================================================================
# FACETTED COST LAYER MAPS - ALL PROVINCES x ZONES (Appendix Suite)
# =============================================================================
#
# PURPOSE:
#   Generates one image per province per zone showing all input cost layers
#   as small facetted maps. Useful as technical appendix showing the raw
#   ingredients that feed into each composite cost raster.
#
#   Layout: 4 columns, rows determined by layer count
#   Colour: viridis (perceptually uniform, consistent across all maps)
#   Labels: layer name below each map
#
# INPUT:
#   data/reclassified/{province}/{zone}/{layer_id}.tif   (from 02)
#   weights/{province}_weights.csv   (from 03a)
#
# OUTPUT:
#   outputs/figures/layer_facets/{zone}_{province}_layers.png  (30 files)
#
# =============================================================================

library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir      <- here::here()
reclass_dir   <- file.path(base_dir, "data/reclassified")
weights_dir   <- file.path(base_dir, "weights")
out_dir       <- file.path(base_dir, "outputs/figures/layer_facets")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SETUP
# =============================================================================

provinces <- c("national", "eastern_cape", "free_state",   "gauteng",
               "kwazulu_natal", "limpopo",   "mpumalanga",  "north_west",
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

zone_titles <- c(
  bio  = "Biodiversity Cost Layers",
  refs = "Renewable Energy Cost Layers",
  agri = "Agricultural Cost Layers"
)

N_COLS      <- 4       # columns in facet grid
AGG_FACTOR  <- 6       # aggregation factor for small maps (speed + memory)
MAP_SIZE    <- 2.2     # inches per map cell (width)

# =============================================================================
# HELPER: raster to ggplot (no axes, no legend, label below)
# =============================================================================

rast_to_mini <- function(r, layer_name) {

  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  df <- df[!is.na(df$value), ]
  ex <- ext(r)

  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(
      limits   = c(0, 100),
      na.value = "white",
      option   = "viridis",
      guide    = "none"
    ) +
    coord_equal(
      xlim   = c(ex[1], ex[2]),
      ylim   = c(ex[3], ex[4]),
      expand = FALSE
    ) +
    labs(title = layer_name) +
    theme_void(base_size = 7) +
    theme(
      plot.title      = element_text(size = 6.5, hjust = 0.5,
                                     colour = "grey20", lineheight = 1.1,
                                     margin = margin(b = 2)),
      plot.background = element_rect(fill = "white", colour = "grey88",
                                     linewidth = 0.3),
      plot.margin     = margin(4, 4, 4, 4)
    )
}

# =============================================================================
# HELPER: shared viridis legend as standalone ggplot
# =============================================================================

make_viridis_legend <- function() {

  df <- data.frame(x = 1, value = seq(0, 100, length.out = 500))

  ggplot(df, aes(x = x, y = value, fill = value)) +
    geom_tile(width = 1) +
    scale_y_continuous(
      breaks   = c(0, 25, 50, 75, 100),
      labels   = c("0", "25", "50", "75", "100"),
      expand   = c(0, 0),
      position = "right"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c(
      limits = c(0, 100),
      option = "viridis",
      guide  = "none"
    ) +
    labs(y = "Cost (0-100)") +
    theme_minimal(base_size = 9) +
    theme(
      axis.title.x    = element_blank(),
      axis.text.x     = element_blank(),
      axis.ticks.x    = element_blank(),
      axis.title.y    = element_text(size = 8, colour = "grey30",
                                     angle = 270, vjust = 0.5),
      axis.text.y     = element_text(size = 8, colour = "grey40"),
      panel.grid      = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(10, 5, 10, 5)
    )
}

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  FACETTED LAYER MAP EXPORT (Appendix Suite)\n")
cat("=============================================================\n\n")

p_legend <- make_viridis_legend()
results  <- list()

for (prov in provinces) {

  prov_label   <- province_labels[[prov]]
  weights_path <- file.path(weights_dir, paste0(prov, "_weights.csv"))

  if (!file.exists(weights_path)) {
    cat(sprintf("[SKIP] No weights config: %s\n", prov))
    next
  }

  weights <- readr::read_csv(weights_path, show_col_types = FALSE)

  cat(sprintf("\n%s\n", toupper(prov)))

  for (zone in c("bio", "refs", "agri")) {

    include_col <- paste0("include_", zone)
    weight_col  <- paste0(zone, "_weight")

    zone_layers <- weights %>%
      dplyr::filter(.data[[include_col]] == TRUE,
                    !is.na(.data[[weight_col]])) %>%
      dplyr::arrange(dplyr::desc(.data[[weight_col]]), layer_name)

    n_layers <- nrow(zone_layers)

    if (n_layers == 0) {
      cat(sprintf("  [SKIP] %s - no layers\n", zone))
      next
    }

    cat(sprintf("  %s | %d layers\n", toupper(zone), n_layers))

    # Build individual mini maps
    mini_plots <- list()

    for (i in seq_len(n_layers)) {

      layer_id   <- zone_layers$layer_id[i]
      layer_name <- zone_layers$layer_name[i]

      rast_path <- file.path(reclass_dir, prov, zone,
                             paste0(layer_id, ".tif"))

      if (!file.exists(rast_path)) {
        # Empty placeholder
        mini_plots[[i]] <- ggplot() +
          labs(title = layer_name) +
          theme_void(base_size = 7) +
          theme(
            plot.title      = element_text(size = 6.5, hjust = 0.5,
                                           colour = "grey60"),
            plot.background = element_rect(fill = "grey96",
                                           colour = "grey88",
                                           linewidth = 0.3),
            plot.margin     = margin(4, 4, 4, 4)
          )
        next
      }

      r <- rast(rast_path)
      r <- aggregate(r, fact = AGG_FACTOR, fun = "mean", na.rm = TRUE)

      # Wrap long layer names at 30 chars for label
      label <- layer_name
      if (nchar(label) > 30) {
        words     <- strsplit(label, " ")[[1]]
        lines     <- c()
        curr_line <- ""
        for (w in words) {
          test <- ifelse(curr_line == "", w, paste(curr_line, w))
          if (nchar(test) <= 30) {
            curr_line <- test
          } else {
            lines     <- c(lines, curr_line)
            curr_line <- w
          }
        }
        lines <- c(lines, curr_line)
        label <- paste(lines, collapse = "\n")
      }

      mini_plots[[i]] <- rast_to_mini(r, label)
      rm(r); gc()
    }

    # Pad to complete final row
    n_full_rows  <- ceiling(n_layers / N_COLS)
    n_total_cells <- n_full_rows * N_COLS
    n_pad         <- n_total_cells - n_layers

    if (n_pad > 0) {
      for (j in seq_len(n_pad)) {
        mini_plots[[n_layers + j]] <- ggplot() +
          theme_void() +
          theme(plot.background = element_rect(fill = "white", colour = NA))
      }
    }

    # Assemble grid
    map_grid <- plot_grid(
      plotlist    = mini_plots,
      ncol        = N_COLS,
      align       = "hv"
    )

    # Add legend
    body <- plot_grid(
      map_grid,
      p_legend,
      ncol       = 2,
      rel_widths = c(1, 0.05)
    )

    # Title block
    title_grob <- ggdraw() +
      draw_label(
        sprintf("%s  |  %s", zone_titles[[zone]], prov_label),
        x        = 0.015, y = 0.65,
        hjust    = 0, vjust = 1,
        fontface = "bold", size = 13, colour = "grey10"
      ) +
      draw_label(
        sprintf("EWT 30x30 Conservation Planning  |  %d reclassified input layers  |  Colour: viridis 0 (low cost) -> 100 (high cost)",
                n_layers),
        x     = 0.015, y = 0.25,
        hjust = 0, vjust = 0,
        size  = 8, colour = "grey40"
      )

    final_plot <- plot_grid(
      title_grob,
      body,
      ncol        = 1,
      rel_heights = c(0.06, 1)
    )

    # Dynamic output dimensions
    n_rows      <- n_full_rows
    plot_width  <- N_COLS * MAP_SIZE + 1.0   # +1 for legend
    plot_height <- n_rows * MAP_SIZE * 1.4 + 1.2  # *1.4 for aspect + title

    out_path <- file.path(out_dir,
                          sprintf("%s_%s_layers.png", zone, prov))

    save_plot(out_path, final_plot,
              base_width  = plot_width,
              base_height = plot_height,
              dpi         = 300,
              bg          = "white")

    cat(sprintf("    [SAVED] %s_%s_layers.png  (%d layers, %.0fx%.0f in)\n",
                zone, prov, n_layers, plot_width, plot_height))

    results[[paste(zone, prov, sep = "_")]] <- "done"

    rm(mini_plots, map_grid, body, final_plot); gc()
    terra::tmpFiles(current = TRUE, remove = TRUE)   # clear terra temp files
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

n_done <- sum(unlist(results) == "done")

cat("\n=============================================================\n")
cat(sprintf("  Exported: %d / 30\n", n_done))
cat(sprintf("  Output:   %s\n",      out_dir))
cat("=============================================================\n")
