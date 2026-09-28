# =============================================================================
# COMPOSITE RASTER MAP PANELS
# =============================================================================
#
# PURPOSE:
#   Generates one publication-quality map panel per zone (bio / refs / agri)
#   showing all 9 province composites in a 3x3 grid with the national
#   composite as a separate panel below.
#   Assembly uses cowplot::plot_grid() to avoid patchwork guide_area() bugs.
#
# INPUT:
#   data/composite/{province}/{zone}_composite.tif   (from 03b)
#
# OUTPUT:
#   outputs/figures/composite_maps/composite_map_{zone}_publication.png
#
# DEPENDENCIES:
#   terra, ggplot2, dplyr, cowplot
#
# =============================================================================

library(terra)
library(ggplot2)
library(dplyr)
library(cowplot)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir      <- here::here()
composite_dir <- file.path(base_dir, "data/composite")
out_dir       <- file.path(base_dir, "outputs/figures/composite_maps")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SETUP
# =============================================================================

province_grid <- c("eastern_cape",  "free_state",   "gauteng",
                   "kwazulu_natal", "limpopo",       "mpumalanga",
                   "northern_cape", "north_west",    "western_cape")

province_abbr <- c(
  eastern_cape  = "EC",
  free_state    = "FS",
  gauteng       = "GP",
  kwazulu_natal = "KZN",
  limpopo       = "LIM",
  mpumalanga    = "MP",
  northern_cape = "NC",
  north_west    = "NW",
  western_cape  = "WC",
  national      = "SA"
)

zone_colours <- list(
  # BIO: white -> dark red (high value = high biodiversity cost)
  bio  = c("#ffffff", "#fee5d9", "#fcbba1", "#fc9272",
           "#fb6a49", "#de2d26", "#a50f15", "#67000d"),
  # REFS: white -> dark orange-brown, aggressively dark mid-tones
  refs = c("#fffde7", "#ffe082", "#ffb300", "#fb8c00",
           "#e65100", "#bf360c", "#7f2000", "#4a1000"),
  # AGRI: white -> deep brown, stronger dark end
  agri = c("#fff5eb", "#fdd9b5", "#fdb97d", "#fd9243",
           "#f06b18", "#d94801", "#8c2d04", "#4a1500")
)

zone_titles <- c(
  bio  = "Biodiversity Cost Composite",
  refs = "Renewable Energy Cost Composite",
  agri = "Agricultural Cost Composite"
)

# =============================================================================
# HELPERS
# =============================================================================

# Raster to ggplot tile map
rast_to_gg <- function(r, colours, abbr, is_national = FALSE) {

  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  df <- df[!is.na(df$value), ]
  ex <- ext(r)

  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colours  = colours,
      limits   = c(0, 100),
      na.value = "white",
      guide    = "none"
    ) +
    coord_equal(
      xlim   = c(ex[1], ex[2]),
      ylim   = c(ex[3], ex[4]),
      expand = FALSE
    ) +
    annotate("text",
             x        = ex[1] + (ex[2] - ex[1]) * 0.05,
             y        = ex[3] + (ex[4] - ex[3]) * 0.08,
             label    = abbr,
             hjust    = 0,
             vjust    = 0,
             size     = ifelse(is_national, 5, 3.5),
             fontface = "bold",
             colour   = "grey20") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = "grey80",
                                     linewidth = 0.3),
      plot.margin     = margin(3, 3, 3, 3)
    )
}

# Standalone vertical colourbar as a ggplot
make_legend_gg <- function(colours) {

  df <- data.frame(
    x     = 0,
    value = seq(0, 100, length.out = 500)
  )

  ggplot(df, aes(x = x, y = value, fill = value)) +
    geom_tile(width = 1) +
    scale_y_continuous(
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      expand = c(0, 0),
      position = "right"
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(
      colours = colours,
      limits  = c(0, 100),
      guide   = "none"
    ) +
    labs(y = "Cost (0-100)") +
    theme_minimal(base_size = 9) +
    theme(
      axis.title.x     = element_blank(),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      axis.title.y     = element_text(size = 8, colour = "grey30",
                                      angle = 270, vjust = 0.5),
      axis.text.y      = element_text(size = 8, colour = "grey40"),
      panel.grid       = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(10, 5, 10, 5)
    )
}

# =============================================================================
# MAIN LOOP
# =============================================================================

for (zone in c("bio", "refs", "agri")) {

  cat(sprintf("\n--- Zone: %s ---\n", toupper(zone)))

  colours <- zone_colours[[zone]]

  # -------------------------------------------------------------------------
  # Load province rasters
  # -------------------------------------------------------------------------
  prov_plots <- list()

  for (prov in province_grid) {

    rast_path <- file.path(composite_dir, prov,
                           paste0(zone, "_composite.tif"))

    if (!file.exists(rast_path)) {
      cat(sprintf("  [MISSING] %s\n", prov))
      prov_plots[[prov]] <- ggplot() + theme_void() +
        theme(plot.background = element_rect(fill = "white", colour = "grey80",
                                             linewidth = 0.3))
      next
    }

    cat(sprintf("  [LOAD] %s\n", prov))
    r <- rast(rast_path)
    r <- aggregate(r, fact = 4, fun = "mean", na.rm = TRUE)
    prov_plots[[prov]] <- rast_to_gg(r, colours, province_abbr[[prov]])
    rm(r); gc()
  }

  # -------------------------------------------------------------------------
  # Load national raster
  # -------------------------------------------------------------------------
  nat_path <- file.path(composite_dir, "national",
                        paste0(zone, "_composite.tif"))

  if (file.exists(nat_path)) {
    cat("  [LOAD] national\n")
    r_nat     <- rast(nat_path)
    r_nat     <- aggregate(r_nat, fact = 6, fun = "mean", na.rm = TRUE)
    p_national <- rast_to_gg(r_nat, colours, "SA", is_national = TRUE)
    rm(r_nat); gc()
  } else {
    p_national <- ggplot() + theme_void()
  }

  # -------------------------------------------------------------------------
  # Build legend
  # -------------------------------------------------------------------------
  p_legend <- make_legend_gg(colours)

  # -------------------------------------------------------------------------
  # Assemble with cowplot
  # -------------------------------------------------------------------------
  cat("  Assembling...\n")

  # 3x3 province grid
  grid_3x3 <- plot_grid(
    prov_plots[[1]], prov_plots[[2]], prov_plots[[3]],
    prov_plots[[4]], prov_plots[[5]], prov_plots[[6]],
    prov_plots[[7]], prov_plots[[8]], prov_plots[[9]],
    ncol    = 3,
    nrow    = 3,
    align   = "hv",
    rel_widths  = c(1, 1, 1),
    rel_heights = c(1, 1, 1)
  )

  # Province grid + legend
  top_row <- plot_grid(
    grid_3x3,
    p_legend,
    ncol       = 2,
    rel_widths = c(1, 0.08)
  )

  # Top + national
  body <- plot_grid(
    top_row,
    p_national,
    ncol        = 1,
    rel_heights = c(3, 1.4)
  )

  # Title block
  title_grob <- ggdraw() +
    draw_label(zone_titles[[zone]],
               x = 0.015, y = 0.72,
               hjust = 0, vjust = 1,
               fontface = "bold", size = 16, colour = "grey10") +
    draw_label(
      sprintf("EWT 30x30 Conservation Planning  |  Cost scale: 0 (low) to 100 (high)  |  %s",
              format(Sys.Date(), "%B %Y")),
      x = 0.015, y = 0.35,
      hjust = 0, vjust = 1,
      size = 9, colour = "grey40") +
    draw_label(
      "Source: EWT 30x30 Conservation Planning | Composite derived from weighted mean of reclassified cost layers",
      x = 0.015, y = 0.05,
      hjust = 0, vjust = 0,
      size = 7, colour = "grey60")

  # Final assembly
  final_plot <- plot_grid(
    title_grob,
    body,
    ncol        = 1,
    rel_heights = c(0.07, 1)
  )

  # -------------------------------------------------------------------------
  # Save
  # -------------------------------------------------------------------------
  out_path <- file.path(out_dir,
                        sprintf("composite_map_%s_publication.png", zone))

  save_plot(out_path, final_plot,
            base_width  = 14,
            base_height = 14,
            dpi         = 300,
            bg          = "white")

  cat(sprintf("  [SAVED] %s\n", out_path))
}

cat(sprintf("\nAll composite map panels saved to: %s\n", out_dir))
