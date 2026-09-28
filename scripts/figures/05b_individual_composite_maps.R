# =============================================================================
# INDIVIDUAL COMPOSITE RASTER MAPS - ALL PROVINCES x ZONES
# =============================================================================
#
# PURPOSE:
#   Exports 30 individual PNG maps (10 provinces x 3 zones) of composite
#   cost rasters. Each map is tightly cropped to the province extent with
#   a consistent colour ramp per zone.
#
# INPUT:
#   data/composite/{province}/{zone}_composite.tif   (from 03b)
#
# OUTPUT:
#   outputs/figures/composite_maps/individual/
#     {zone}_{province}.png  (30 files total)
#
# NAMING CONVENTION:
#   bio_eastern_cape.png, refs_mpumalanga.png, agri_national.png etc.
#
# =============================================================================

library(terra)
library(ggplot2)
library(dplyr)
library(here)

# =============================================================================
# PATHS
# =============================================================================

base_dir      <- here::here()
composite_dir <- file.path(base_dir, "data/composite")
out_dir       <- file.path(base_dir, "outputs/figures/composite_maps/individual")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SETUP
# =============================================================================

provinces <- c("national", "eastern_cape", "free_state",   "gauteng",
               "kwazulu_natal", "limpopo",   "mpumalanga",  "north_west",
               "northern_cape", "western_cape")

province_abbr <- c(
  national      = "SA",
  eastern_cape  = "EC",
  free_state    = "FS",
  gauteng       = "GP",
  kwazulu_natal = "KZN",
  limpopo       = "LIM",
  mpumalanga    = "MP",
  north_west    = "NW",
  northern_cape = "NC",
  western_cape  = "WC"
)

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

zone_colours <- list(
  bio  = c("#ffffff", "#fee5d9", "#fcbba1", "#fc9272",
           "#fb6a49", "#de2d26", "#a50f15", "#67000d"),
  refs = c("#fffde7", "#ffe082", "#ffb300", "#fb8c00",
           "#e65100", "#bf360c", "#7f2000", "#4a1000"),
  agri = c("#fff5eb", "#fdd9b5", "#fdb97d", "#fd9243",
           "#f06b18", "#d94801", "#8c2d04", "#4a1500")
)

zone_titles <- c(
  bio  = "Biodiversity Cost Composite",
  refs = "Renewable Energy Cost Composite",
  agri = "Agricultural Cost Composite"
)

# =============================================================================
# HELPER: aspect ratio from raster extent
# =============================================================================

get_aspect <- function(r) {
  ex  <- ext(r)
  w   <- ex[2] - ex[1]
  h   <- ex[4] - ex[3]
  as.numeric(w / h)
}

# =============================================================================
# MAIN LOOP
# =============================================================================

cat("=============================================================\n")
cat("  INDIVIDUAL COMPOSITE MAP EXPORT\n")
cat("=============================================================\n\n")

results <- list()

for (zone in c("bio", "refs", "agri")) {

  colours    <- zone_colours[[zone]]
  zone_title <- zone_titles[[zone]]

  cat(sprintf("Zone: %s\n", toupper(zone)))

  for (prov in provinces) {

    rast_path <- file.path(composite_dir, prov,
                           paste0(zone, "_composite.tif"))

    if (!file.exists(rast_path)) {
      cat(sprintf("  [SKIP - missing] %s\n", prov))
      results[[paste(zone, prov, sep = "_")]] <- "missing"
      next
    }

    # Load and aggregate for plotting
    r    <- rast(rast_path)
    fact <- ifelse(prov == "national", 6, 4)
    r    <- aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)

    # Build data frame
    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- "value"
    df <- df[!is.na(df$value), ]

    ex     <- ext(r)
    aspect <- get_aspect(r)

    # Base width 6 inches, height adjusted to aspect ratio
    plot_width  <- 6
    plot_height <- round(plot_width / aspect, 2)

    # Build plot
    p <- ggplot(df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(
        colours  = colours,
        limits   = c(0, 100),
        na.value = "white",
        name     = "Cost\n(0-100)",
        breaks   = c(0, 25, 50, 75, 100),
        guide    = guide_colorbar(
          barwidth       = 0.6,
          barheight      = 6,
          title.position = "top",
          title.hjust    = 0.5,
          ticks.colour   = "grey40",
          frame.colour   = "grey40"
        )
      ) +
      coord_equal(
        xlim   = c(ex[1], ex[2]),
        ylim   = c(ex[3], ex[4]),
        expand = FALSE
      ) +
      # Province abbreviation label (bottom-left)
      annotate("text",
               x        = ex[1] + (ex[2] - ex[1]) * 0.03,
               y        = ex[3] + (ex[4] - ex[3]) * 0.05,
               label    = province_abbr[[prov]],
               hjust    = 0, vjust = 0,
               size     = 4,
               fontface = "bold",
               colour   = "grey20") +
      labs(
        title    = zone_title,
        subtitle = province_labels[[prov]],
        caption  = "Source: EWT 30x30 Conservation Planning"
      ) +
      theme_void(base_size = 10) +
      theme(
        plot.title       = element_text(face = "bold", size = 11,
                                        hjust = 0, colour = "grey10",
                                        margin = margin(b = 2)),
        plot.subtitle    = element_text(size = 9, colour = "grey40",
                                        hjust = 0, margin = margin(b = 6)),
        plot.caption     = element_text(size = 7, colour = "grey60",
                                        hjust = 0, margin = margin(t = 6)),
        legend.position  = "right",
        legend.title     = element_text(size = 8, face = "bold",
                                        colour = "grey20"),
        legend.text      = element_text(size = 7, colour = "grey40"),
        plot.background  = element_rect(fill = "white", colour = NA),
        plot.margin      = margin(10, 8, 8, 10)
      )

    # Save
    out_path <- file.path(out_dir, sprintf("%s_%s.png", zone, prov))

    ggsave(out_path, p,
           width  = plot_width + 1,   # +1 for legend
           height = plot_height + 0.8, # +0.8 for title/caption
           dpi    = 300,
           bg     = "white")

    rm(r, df); gc()

    cat(sprintf("  [DONE] %s_%s.png  (%.1f x %.1f in)\n",
                zone, prov,
                plot_width + 1,
                plot_height + 0.8))

    results[[paste(zone, prov, sep = "_")]] <- "done"
  }
  cat("\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

n_done    <- sum(unlist(results) == "done")
n_missing <- sum(unlist(results) == "missing")

cat("=============================================================\n")
cat(sprintf("  Exported:  %d / 30\n", n_done))
cat(sprintf("  Missing:   %d\n",      n_missing))
cat(sprintf("  Output:    %s\n",      out_dir))
cat("=============================================================\n")
