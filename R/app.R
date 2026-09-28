# =============================================================================
# RSA LANDSCAPE PLANNING TOOL: SHINY APP
# Endangered Wildlife Trust - Conservation Planning and Science Unit
# =============================================================================
#
# PURPOSE:
#   Interactive front end to the three-zone prioritisation solved offline in
#   scripts/solve/07_prioritizr_solve.R. Users choose a scope (national, a
#   province, or an uploaded area of interest), set area targets, the BLM and
#   which existing areas to lock, optionally reweight the three zones or the
#   individual input layers, and solve with prioritizr + Gurobi. With default
#   settings the app builds the same problem as 07: same costs, locks,
#   boundary matrix and solver settings.
#
# COST SCALE:
#   All costs stay on the workflow's 0-100 scale; nothing is rescaled per
#   scope. Default solves use cost_bio / cost_refs / cost_agri unchanged.
#   Custom layer weights rebuild each zone cost as the plain weighted mean of
#   the per-layer columns ({layer_id}_{zone}), so default weights reproduce
#   the default costs (see validate_province() in prepare_v2_planning_units.R).
#   Zone weights multiply each zone's cost by (weight / mean of the three
#   weights), so equal weights leave costs unchanged and the overall cost
#   level, and hence the BLM calibration in 08, is preserved.
#
# LOCKED PLANNING UNITS:
#   Lock flags from 06b are combined into a single three-column matrix and
#   added with one add_locked_in_constraints() call, as in 07 and 08. After
#   each solve, locked PUs not assigned to their locked zone are reported
#   (expect 0).
#
# INPUT:
#   data/app/provinces/{scope}/spatial/planning_units.gpkg      (prepare_v2_planning_units.R)
#   data/app/provinces/{scope}/precomputed/boundary_matrix.rds  (06c, copied by prepare_v2_planning_units.R)
#   data/app/provinces/{scope}/spatial/boundary.gpkg            (prepare_v2_planning_units.R)
#   data/app/spatial/display/*_display.gpkg                     (prepare_locked_layers.R)
#   weights/{scope}_weights.csv                                 (03a)
#   R/www/ewt_logo.png, R/www/header_animation.js
#
# OUTPUT:
#   Nothing is written to disk. Users download the solution as a zipped
#   shapefile, a GeoPackage and a summary CSV.
#
# REQUIREMENTS:
#   Gurobi with a valid licence and the gurobi R package (see main README).
#   prioritizr 8.0.3 (pinned, see main README). R packages: shiny,
#   shinydashboard, shinyjs, shinycssloaders, leaflet, sf, dplyr, DT, zip,
#   here. Paths resolve from the project root via here::here(), so the root
#   must be detectable when deployed (the repo's .here file does this).
#
# CHANGE LOG:
#   v4 (2026-09):
#     - Removed the per-scope 0-1 min-max rescaling of custom-weighted zone
#       costs. Costs now stay on the 0-100 scale, so app solves match 07 and
#       the Methods, and a given BLM has the same strength as in the
#       calibration (08); previously it was ~100x stronger in the app.
#     - All scopes, including national, read planning units from
#       data/app/provinces/{scope}/ and layer weights from
#       weights/{scope}_weights.csv; the separate national route
#       (cost_components_national_*.csv) is gone. Display layers are read
#       from data/app/spatial/display/.
#     - Locks: one add_locked_in_constraints() call with a combined lock
#       matrix (as 07/08) instead of three add_manual_locked_constraints()
#       calls, plus a post-solve lock check.
#     - Zone weight sliders are now applied to the solve (previously shown
#       but never used) and normalised so equal weights reproduce 07.
#     - BLM choices limited to the values calibrated in 08, default 0.001.
#     - Boundary matrix is checked against the planning unit count and kept
#       aligned when empty geometries are dropped.
#     - Target check now tests that each target is met (area >= target)
#       rather than matched to within 0.5%.
#     - Gauteng resolution corrected to 10 ha (was 50).
#     - Removed setwd(), the hardcoded Gurobi PATH and non-ASCII characters;
#       header animation moved to www/header_animation.js.
#
# =============================================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(prioritizr)
library(shinycssloaders)
library(shinyjs)
library(zip)
library(here)

# Gurobi: requires the gurobi R package and a valid licence (see README)
library(gurobi)

`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# CONFIGURATION
# =============================================================================

sf::sf_use_s2(FALSE)  # planar GEOS geometry, avoids S2 errors on complex polygons

APP_DATA_DIR <- here::here("data", "app")     # must match scripts/app_prep/
WEIGHTS_DIR  <- here::here("weights")
DISPLAY_DIR  <- file.path(APP_DATA_DIR, "spatial", "display")

if (!dir.exists(APP_DATA_DIR)) {
  stop("App data folder not found: ", APP_DATA_DIR,
       "\nRun scripts/app_prep/ first, and check that here::here() finds the project root.")
}

# Zones: internal keys (column suffixes) and prioritizr zone names
ZONE_KEYS  <- c("bio", "refs", "agri")
ZONE_NAMES <- c("Conservation", "REFS", "Agriculture")

# BLM choices: the values calibrated in 08 / Appendix A. Update after any
# recalibration.
BLM_CHOICES <- c("0 (no clustering, fastest)" = 0,
                 "0.001 (recommended)"        = 0.001,
                 "0.005"                      = 0.005,
                 "0.01"                       = 0.01)
BLM_DEFAULT <- 0.001

# Gurobi settings, as in 07
GUROBI_GAP       <- 0.05   # 5% optimality gap
GUROBI_TIMELIMIT <- 1800   # 30 minutes max per solve
GUROBI_THREADS   <- 8

cat("Loading RSA Landscape Planning Tool...\n")

# =============================================================================
# SCOPES
# =============================================================================
# All predefined scopes (national and provinces) share one file layout.
# Resolution is the hexagon area in hectares (06a).

scope_table <- data.frame(
  id    = c("national", "eastern_cape", "free_state", "gauteng", "kwazulu_natal",
            "limpopo", "mpumalanga", "north_west", "northern_cape", "western_cape"),
  name  = c("National", "Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
            "Limpopo", "Mpumalanga", "North West", "Northern Cape", "Western Cape"),
  resolution = c(500, 100, 100, 10, 100, 100, 50, 100, 200, 100),
  lng   = c(24.7, 26.5, 26.0, 28.2, 30.8, 29.5, 30.8, 25.8, 21.9, 19.5),
  lat   = c(-28.8, -32.3, -28.7, -26.1, -28.7, -23.9, -25.5, -26.6, -29.0, -33.2),
  zoom  = c(5, 6, 6, 8, 6, 7, 6, 7, 6, 6),
  stringsAsFactors = FALSE
)
scope_table$label <- sprintf("%s (%d Ha)", scope_table$name, scope_table$resolution)

make_scope <- function(row) {
  scope_dir <- file.path(APP_DATA_DIR, "provinces", row$id)
  list(
    id                    = row$id,
    resolution            = row$resolution,
    data_path             = file.path(scope_dir, "spatial", "planning_units.gpkg"),
    boundary_path         = file.path(scope_dir, "precomputed", "boundary_matrix.rds"),
    boundary_spatial_path = file.path(scope_dir, "spatial", "boundary.gpkg"),
    center                = c(lng = row$lng, lat = row$lat),
    zoom                  = row$zoom
  )
}

available_scopes <- setNames(
  lapply(seq_len(nrow(scope_table)), function(i) make_scope(scope_table[i, ])),
  scope_table$label
)

available_scopes[["Custom Area of Interest"]] <- list(
  id                    = "custom",
  resolution            = NA,
  data_path             = NULL,
  boundary_path         = NULL,
  boundary_spatial_path = NULL,  # no precomputed outline for a custom AOI
  center                = c(lng = 25, lat = -29),
  zoom                  = 5
)

# Shared lookup: scope id -> scope label
BASE_SCOPE_MAP <- setNames(scope_table$label, scope_table$id)

# =============================================================================
# HELPERS: layer weights, zone costs, locks
# =============================================================================

# Per-layer weight metadata for a scope, from weights/{scope}_weights.csv.
# One row per layer per zone it contributes to; field_name matches the
# {layer_id}_{zone} columns written by prepare_v2_planning_units.R.
load_component_metadata <- function(scope_id) {
  path <- file.path(WEIGHTS_DIR, paste0(scope_id, "_weights.csv"))
  if (!file.exists(path)) {
    cat("[WARN] Weights CSV not found:", path, "\n")
    return(NULL)
  }
  w <- read.csv(path, stringsAsFactors = FALSE)

  rows <- lapply(ZONE_KEYS, function(z) {
    include <- as.logical(w[[paste0("include_", z)]])
    weight  <- suppressWarnings(as.numeric(w[[paste0(z, "_weight")]]))
    keep    <- !is.na(include) & include & !is.na(weight)
    if (!any(keep)) return(NULL)
    data.frame(
      field_name     = paste0(w$layer_id[keep], "_", z),
      layer_id       = w$layer_id[keep],
      label          = w$layer_name[keep],
      category       = z,
      default_weight = weight[keep],
      stringsAsFactors = FALSE
    )
  })
  meta <- do.call(rbind, rows)
  cat(sprintf("Loaded %d layer weights from %s\n", nrow(meta), basename(path)))
  meta
}

# Zone costs on the 0-100 scale. With custom = FALSE, the workflow costs
# (cost_bio / cost_refs / cost_agri) are returned unchanged. With custom =
# TRUE, each zone cost is the weighted mean of its per-layer columns; a zone
# whose weights are all zero keeps its workflow cost. No rescaling.
build_zone_costs <- function(pu_tbl, meta = NULL, weights = NULL, custom = FALSE) {
  out <- lapply(ZONE_KEYS, function(z) pu_tbl[[paste0("cost_", z)]])
  names(out) <- ZONE_KEYS
  if (!custom || is.null(meta) || is.null(weights)) return(out)

  for (z in ZONE_KEYS) {
    m <- meta[meta$category == z, , drop = FALSE]
    w <- vapply(seq_len(nrow(m)), function(i)
      as.numeric(weights[[m$field_name[i]]] %||% m$default_weight[i]), numeric(1))
    present <- m$field_name %in% names(pu_tbl)
    if (any(!present)) {
      cat(sprintf("[WARN] %s: %d layer column(s) missing from planning units, skipped: %s\n",
                  z, sum(!present), paste(m$field_name[!present], collapse = ", ")))
    }
    use <- present & !is.na(w) & w > 0
    if (!any(use)) {
      cat(sprintf("[WARN] %s: all layer weights zero, using workflow cost\n", z))
      next
    }
    vals <- as.matrix(pu_tbl[, m$field_name[use], drop = FALSE])
    out[[z]] <- as.vector(vals %*% w[use]) / sum(w[use])
  }
  out
}

# Zone multipliers from the three zone weight sliders, normalised to a mean
# of 1 so equal weights leave costs (and the BLM balance) unchanged.
zone_multipliers <- function(w) {
  w <- as.numeric(w)
  if (sum(w) <= 0) return(NULL)
  setNames(w / mean(w), ZONE_KEYS)
}

# Combined lock matrix, one column per zone, as in 07 / 08. 06b guarantees
# each PU is locked into at most one zone. Columns are all FALSE for lock
# layers not selected.
build_lock_matrix <- function(pu_tbl, lock_selection) {
  n <- nrow(pu_tbl)
  lock_col <- function(key, field) {
    if (key %in% lock_selection && field %in% names(pu_tbl)) {
      x <- as.logical(pu_tbl[[field]])
      x[is.na(x)] <- FALSE
      x
    } else {
      rep(FALSE, n)
    }
  }
  matrix(
    c(lock_col("pa", "locked_pa"), lock_col("refs", "locked_refs"), lock_col("agri", "locked_agri")),
    ncol = 3,
    dimnames = list(NULL, ZONE_NAMES)
  )
}

# =============================================================================
# LOCKED-AREA DISPLAY LAYERS (display only; see prepare_locked_layers.R)
# =============================================================================

.load_locked <- function(path, label) {
  tryCatch({
    lyr <- st_read(path, quiet = TRUE)
    cat(sprintf("[OK] Loaded %s: %d features\n", label, nrow(lyr)))
    lyr
  }, error = function(e) {
    cat(sprintf("[WARN] Could not load %s: %s\n", label, e$message))
    NULL
  })
}

locked_layers <- list(
  pa   = .load_locked(file.path(DISPLAY_DIR, "protected_areas_display.gpkg"),   "Protected Areas"),
  refs = .load_locked(file.path(DISPLAY_DIR, "refs_facilities_display.gpkg"),   "REFS Facilities"),
  agri = .load_locked(file.path(DISPLAY_DIR, "agriculture_zones_display.gpkg"), "Agriculture Zones")
)

cat("App initialised\n")

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "ewt_logo.png", class = "ewt-left-logo")
    ),
    titleWidth = 350,
    tags$li(class = "dropdown",
            # Animated header banner and centred title (see www/header_animation.js)
            tags$script(src = "header_animation.js")
    ),
    tags$li(class = "dropdown",
            tags$a(href = "https://www.ewt.org.za", target = "_blank",
                   tags$img(src = "ewt_logo.png", height = "65px", 
                            style = "margin-top: 0.5px; margin-right: 0.5px;")
            )
    )
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 350,
    
    sidebarMenu(
      id = "tabs",
      menuItem("Scenario Builder", tabName = "scenario", icon = icon("sliders")),
      menuItem("Results", tabName = "results", icon = icon("map")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    
    hr(),
    
    # SCOPE SELECTOR
    h4("Analysis Scope", style = "padding-left: 15px;"),
    
    selectInput("scope",
                NULL,
                choices = names(available_scopes),
                selected = names(available_scopes)[1]),
    
    # Conditional UI for Custom AOI
    conditionalPanel(
      condition = "input.scope == 'Custom Area of Interest'",
      
      # Base layer selector
      h4("Base Planning Unit Layer", style = "padding-left: 15px; margin-top: 10px;"),
      
      selectInput("aoi_base_layer",
                  NULL,
                  choices = setNames(names(BASE_SCOPE_MAP), BASE_SCOPE_MAP),
                  selected = "national"),
      
      # Upload section
      h4("Upload Boundary File", style = "padding-left: 15px; margin-top: 15px;"),
      
      fileInput(
        "aoi_file",
        NULL,
        accept = c(
          ".kml", ".kmz",
          ".gpkg",
          ".geojson", ".json",
          ".zip",          # zipped shapefile
          ".shp", ".dbf", ".shx", ".prj"  # allow raw shapefile components
        ),
        placeholder = "Select file..."
      ),
      
      # Status box
      div(
        style = "padding: 15px; background-color: #F5F5F5; font-size: 16px; margin: 10px; border-radius: 5px; border: 2px solid #666666;",
        htmlOutput("aoi_status")
      ),
      
      hr()
    ),
    
    # Scope info box
    div(
      style = "padding: 12px 15px; background-color: #E8F5E9; margin: 10px; border: 2px solid #2E7D32; border-radius: 5px;",
      h5(htmlOutput("scope_info"), style = "margin: 0; font-weight: bold; color: #1B5E20; font-size: 20px;")
    ),
    
    hr(),
    
    # Scenario Controls
    h4("Land Use Targets", style = "padding-left: 15px;"),
    
    sliderInput("target_conservation",
                "Conservation (%)",
                min = 20, max = 50, value = 35, step = 1),
    
    sliderInput("target_refs",
                "Renewable Energy (%)",
                min = 0, max = 20, value = 10, step = 1),
    
    sliderInput("target_agriculture",
                "Agriculture (%)",
                min = 0, max = 40, value = 25, step = 1),
    
    # Auto-calculated remainder
    div(
      style = "padding: 12px 15px; background-color: #E8F5E9; margin: 10px; border: 2px solid #2E7D32; border-radius: 5px;",
      h4(textOutput("target_available"), style = "margin: 0; font-weight: bold; color: #1B5E20; text-align: center;")
    ),
    
    hr(),
    
    h4("Spatial Settings", style = "padding-left: 15px;"),
    
    selectInput("blm",
                "Boundary Length Modifier (Spatial Clustering)",
                choices = BLM_CHOICES,
                selected = BLM_DEFAULT),

    # Locks existing areas into their zone (combined lock matrix, see server)
    checkboxGroupInput("locked_areas",
                       "Lock Existing Areas",
                       choices = c("Protected Areas" = "pa",
                                   "REFS Facilities" = "refs",
                                   "Agriculture Zones" = "agri"),
                       selected = c("pa", "refs", "agri")),
    
    hr(),
    
    # Cost Weighting Controls (Basic)
    h4("Basic Cost Weighting", style = "padding-left: 15px;"),
    
    p(style = "padding-left: 15px; font-size: 12px; color: #666;",
      "Relative weight on each zone's cost. Equal weights (all 1) use the workflow costs unchanged;",
      "raising one zone's weight gives it first call on the planning units it suits best."),

    sliderInput("zone_weight_bio",
                "Biodiversity/Conservation",
                min = 0, max = 2, value = 1, step = 0.1),

    sliderInput("zone_weight_refs",
                "Renewable Energy",
                min = 0, max = 2, value = 1, step = 0.1),

    sliderInput("zone_weight_agri",
                "Agriculture",
                min = 0, max = 2, value = 1, step = 0.1),

    # Effective (normalised) multipliers
    div(
      style = "padding: 8px 15px; margin: 10px; background-color: #F5F5F5; border-radius: 5px;",
      htmlOutput("weight_total")
    ),
    
    hr(),
    
    # Per-layer weighting (modal)
    actionButton(
      "customize_components",
      "Customize Cost Components",
      icon = icon("sliders-h"),
      class = "btn-info",
      style = "width: 90%; margin: 10px;"
    ),

    p(style = "padding-left: 15px; font-size: 11px; color: #666;",
      "Advanced: fine-tune the weight of individual input layers"),
    
    hr(),
    
    # Action button
    actionButton("run_optimization",
                 "Run Optimization",
                 icon = icon("play"),
                 class = "btn-primary btn-lg",
                 width = "90%",
                 style = "margin: 10px;"),
    
    # Status
    div(
      style = "padding: 10px 15px;",
      textOutput("solve_status")
    )
  ),
  
  # Body
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
  background-color: #222d32 !important;
}
        .skin-blue .main-header .navbar {
          background-color: #2E7D32;
        }
        .btn-primary {
          background-color: #2E7D32 !important;
          border-color: #2E7D32 !important;
        }
        .btn-primary:hover {
          background-color: #1B5E20 !important;
          border-color: #1B5E20 !important;
        }
        .main-header {
      height: 60px;
      
    }
    .main-header .logo {
  width: 350px;
  height: 60px;
  line-height: 60px;
  padding: 2px 5px !important;
  position: relative !important;
  overflow: visible !important;
}
    .main-header .navbar {
      margin-left: 350px;
      min-height: 60px;
    }
    .main-header .logo img.ewt-left-logo {
  height: 65px !important;
  position: absolute !important;
  left: 10px !important;
  top: 55% !important;
  transform: translateY(-50%) !important;
  max-width: none !important;
  margin: 0 !important;
  padding: 0 !important;
}
  .main-header .sidebar-toggle {
  display: none !important;
  }
  .box.box-solid > .box-header {
  padding: 15px 10px;
  }
  .box > .box-header > .box-title {
  margin-top: 12px;
  }
  .ewt-left-logo {
    height: 65px !important;
    position: absolute !important;
    left: 10px !important;
    top: 50% !important;
    transform: translateY(-50%) !important;
  }
  #ewt-nav-title {
    position: absolute;
    left: 43%;
    top: 50%;
    transform: translate(-50%, -50%);
    font-size: 38px;
    font-weight: 700;
    color: white;
    white-space: nowrap;
    pointer-events: none;
    z-index: 999;
  }
  .main-sidebar, .left-side {
    top: 60px !important;
    padding-top: 33px !important;
  }
  body.skin-blue .main-sidebar {
    top: 60px !important;
  }
  "))
    ),
    
    
    useShinyjs(),
    
    tabItems(
      
      # Scenario Builder tab
      tabItem(
        tabName = "scenario",
        h2("Scenario Builder"),
        p("Configure your optimization scenario using the controls in the sidebar, then click 'Run Optimization'."),
        br(),
        
        # Small preview map - centered and narrower
        fluidRow(
          column(3),  # Empty column for centering
          column(6,
                 box(
                   title = "Study Area",
                   width = NULL,
                   #height = "400px",
                   withSpinner(
                     leafletOutput("preview_map", height = "400px")
                   )
                 )
          ),
          column(3)  # Empty column for centering
        ),
        
        # Scenario statistics - Row 1
        fluidRow(
          valueBoxOutput("scenario_pu_count", width = 4),
          valueBoxOutput("scenario_total_area", width = 4),
          valueBoxOutput("scenario_available_area", width = 4)
        ),
        
        # Scenario statistics - Row 2
        fluidRow(
          valueBoxOutput("scenario_conservation_target", width = 4),
          valueBoxOutput("scenario_refs_target", width = 4),
          valueBoxOutput("scenario_agriculture_target", width = 4)
        )
      ),
      
      # Results tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Optimized Land Allocation",
            width = 8,
            withSpinner(
              leafletOutput("solution_map", height = "600px")
            )
          ),
          
          box(
            title = "Solution Statistics",
            width = 4,
            fluidRow(
              column(12, valueBoxOutput("stat_cost",  width = 12)),
              column(12, valueBoxOutput("stat_time",  width = 12)),
              column(12, valueBoxOutput("stat_gap",   width = 12)),
              column(12,
                     hr(),
                     tags$div(
                       style = "padding: 0 8px;",
                       downloadButton("download_shapefile", "Download Shapefile",
                                      class = "btn-success",
                                      style = "width:100%; margin-bottom:6px; display:block;"),
                       downloadButton("download_gpkg", "Download GeoPackage",
                                      class = "btn-success",
                                      style = "width:100%; margin-bottom:10px; display:block;"),
                       downloadButton("download_csv", "Download Summary CSV",
                                      class = "btn-info",
                                      style = "width:100%; display:block;")
                     )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Area Allocation Results",
            width = 12,
            DTOutput("results_table")
          )
        )
      ),
      
      # About tab
      tabItem(
        tabName = "about",
        h2("About This Tool"),
        p("This tool uses systematic conservation planning to identify optimal land allocations."),
        h3("Technical Details"),
        p("Powered by Gurobi optimization engine and prioritizr R package.")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive values for results
  current_data <- reactiveVal(NULL)
  results <- reactiveValues(
    solution      = NULL,
    solution_map  = NULL,
    stats         = NULL,
    solve_time    = NULL,
    locked_display = list(pa = NULL, refs = NULL, agri = NULL)
  )
  
  # Custom AOI upload handler
  custom_aoi_data <- reactiveValues(
    uploaded = FALSE,
    boundary = NULL,
    planning_units = NULL,
    boundary_matrix = NULL,
    n_pus = 0,
    coverage = NA
  )
  
  observeEvent(input$aoi_file, {
    req(input$aoi_file)
    
    tryCatch({
      file_path <- input$aoi_file$datapath
      file_name <- input$aoi_file$name
      file_ext <- tolower(tools::file_ext(file_name))
      
      # Handle zipped shapefile
      if (file_ext == "zip") {
        temp_dir <- file.path(tempdir(), paste0("shp_", format(Sys.time(), "%Y%m%d%H%M%S")))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
        
        unzip(file_path, exdir = temp_dir)
        
        shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
        
        if (length(shp_files) == 0) {
          stop("No .shp file found inside ZIP archive.")
        }
        
        file_path <- shp_files[1]
        cat("Using shapefile:", file_path, "\n")
      }
      
      cat("Processing file:", file_name, "Extension:", file_ext, "\n")
      
      # Handle KMZ files (zipped KML)
      if (file_ext == "kmz") {
        temp_dir <- file.path(tempdir(), paste0("kmz_", format(Sys.time(), "%Y%m%d%H%M%S")))
        dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
        
        cat("Unzipping KMZ to:", temp_dir, "\n")
        unzip(file_path, exdir = temp_dir)
        
        # Find KML file (could be doc.kml or other names)
        all_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
        cat("Files in KMZ:", paste(basename(all_files), collapse = ", "), "\n")
        
        kml_files <- list.files(temp_dir, pattern = "\\.(kml|KML)$", full.names = TRUE, recursive = TRUE)
        
        if (length(kml_files) == 0) {
          stop("No KML file found inside KMZ. Contents: ", paste(basename(all_files), collapse = ", "))
        }
        
        file_path <- kml_files[1]
        cat("Using KML file:", file_path, "\n")
      }
      
      # Handle KML files directly
      if (file_ext == "kml") {
        cat("Reading KML directly\n")
      }
      
      # Read the boundary file
      cat("Attempting to read file...\n")
      
      # KML/KMZ often have multiple layers; explicitly select the first geometry layer
      if (file_ext %in% c("kml", "kmz")) {
        layers <- st_layers(file_path)$name
        cat("KML layers found:", paste(layers, collapse = ", "), "\n")
        # Use first layer that isn't a schema/folder layer
        geom_layer <- layers[1]
        boundary <- st_read(file_path, layer = geom_layer, quiet = FALSE)
      } else {
        boundary <- st_read(file_path, quiet = FALSE)
      }
      
      cat("Successfully read", nrow(boundary), "features\n")
      
      # CRITICAL: Remove Z/M dimensions (KML/KMZ often have Z, causes leaflet issues)
      boundary <- st_zm(boundary, drop = TRUE, what = "ZM")
      cat("Stripped Z/M dimensions if present\n")
      
      # Remove empty geometries (common in KML exports)
      boundary <- boundary[!st_is_empty(boundary), ]
      cat("Removed empty geometries, remaining features:", nrow(boundary), "\n")
      
      # Validate geometry
      if (!all(st_is_valid(boundary))) {
        boundary <- st_make_valid(boundary)
      }
      
      # Union if multiple features
      if (isTRUE(nrow(boundary) > 1)) {
        boundary <- st_union(boundary)
        boundary <- st_sf(geometry = boundary)
      }
      
      # Validate geometry
      if (!all(st_is_valid(boundary))) {
        boundary <- st_make_valid(boundary)
      }
      
      # Load base planning unit layer
      base_scope <- BASE_SCOPE_MAP[input$aoi_base_layer]
      base_path <- available_scopes[[base_scope]]$data_path
      
      showNotification("Loading base planning units...", duration = 5, type = "message")
      base_pu <- st_read(base_path, quiet = TRUE)
      
      # CRS HANDLING:
      # Uploaded files from ArcGIS often have missing or ambiguous CRS metadata.
      # Strategy: detect CRS, assign if missing, then reproject BOTH layers to
      # WGS84 (4326) as a neutral common CRS for intersection. This avoids
      # issues with mismatched projected CRS variants (3857 vs Albers etc).
      
      # Step 1: If boundary CRS is missing, infer from coordinate magnitude
      if (is.na(st_crs(boundary))) {
        coords <- st_coordinates(boundary)
        x_range <- range(coords[, 1], na.rm = TRUE)
        
        if (abs(x_range[1]) > 180) {
          # Coordinates are in metres - assume Africa Albers (used throughout project)
          cat("  CRS missing - coordinates appear projected, assuming ESRI:102022 (Africa Albers)\n")
          boundary <- st_set_crs(boundary, "ESRI:102022")
        } else {
          # Coordinates look like degrees - assume WGS84
          cat("  CRS missing - coordinates appear geographic, assuming EPSG:4326 (WGS84)\n")
          boundary <- st_set_crs(boundary, 4326)
        }
      }
      
      # Step 2: Reproject both to WGS84 as neutral common CRS for intersection
      cat("  Reprojecting boundary to WGS84...\n")
      boundary_wgs84 <- st_transform(boundary, 4326)
      boundary_wgs84 <- st_make_valid(boundary_wgs84)
      
      cat("  Reprojecting base PUs to WGS84...\n")
      base_pu_wgs84 <- st_transform(base_pu, 4326)
      
      # Step 3: Intersect in WGS84 space
      showNotification("Clipping planning units to boundary...", duration = 5, type = "message")
      cat("  Intersecting", nrow(base_pu_wgs84), "PUs with boundary...\n")
      pu_clipped <- st_intersection(base_pu_wgs84, st_union(boundary_wgs84))
      
      # Step 4: Reproject clipped PUs back to original base PU CRS
      pu_clipped <- st_transform(pu_clipped, st_crs(base_pu))
      
      # Remove Z/M dimensions from clipped units
      pu_clipped <- st_zm(pu_clipped, drop = TRUE, what = "ZM")
      
      # Remove empty geometries
      pu_clipped <- pu_clipped[!st_is_empty(pu_clipped), ]
      
      # Recalculate Area_km2 after clipping (intersection creates partial hexagons)
      pu_clipped <- st_make_valid(pu_clipped)
      pu_clipped$Area_km2 <- as.numeric(st_area(pu_clipped)) / 1e6
      
      # Remove slivers (< 5% of nominal cell area to avoid distorting targets)
      scope_config  <- available_scopes[[BASE_SCOPE_MAP[input$aoi_base_layer]]]
      min_area_km2  <- (scope_config$resolution * 0.05) / 100
      n_before      <- nrow(pu_clipped)
      pu_clipped    <- pu_clipped[pu_clipped$Area_km2 >= min_area_km2, ]
      n_removed     <- n_before - nrow(pu_clipped)
      if (n_removed > 0) cat(sprintf("  Removed %d sliver PUs (< %.4f km2)\n", n_removed, min_area_km2))
      
      cat(sprintf("  Final clipped PUs: %d\n", nrow(pu_clipped)))
      
      # AOI COVERAGE CHECK
      # Warn user if a significant portion of their uploaded boundary falls
      # outside the selected base layer extent (e.g. veg type crosses provinces)
      aoi_area     <- as.numeric(st_area(st_union(boundary_wgs84)))
      clipped_area <- as.numeric(sum(st_area(pu_clipped)))
      coverage_pct <- min((clipped_area / aoi_area) * 100, 100)  # cap at 100%
      
      cat(sprintf("  AOI coverage: %.1f%% of uploaded boundary covered by selected base layer\n", coverage_pct))
      
      if (coverage_pct < 90) {
        
        # Work out which scope would give better coverage
        if (input$aoi_base_layer != "national") {
          suggestion <- "Consider switching the base layer to <strong>National (500 Ha)</strong> for full AOI coverage."
        } else {
          suggestion <- "Your AOI may extend outside South Africa's boundaries."
        }
        
        showNotification(
          HTML(paste0(
            "<strong>Partial AOI coverage</strong><br/>",
            "Only <strong>", round(coverage_pct, 0), "%</strong> of your uploaded boundary ",
            "falls within the selected base layer.<br/>",
            suggestion
          )),
          type = "warning",
          duration = NULL  # stays until dismissed - user should see this
        )
        
      } else {
        cat("  [OK] AOI fully covered by selected base layer\n")
      }
      
      custom_aoi_data$n_pus    <- nrow(pu_clipped)
      custom_aoi_data$coverage <- round(coverage_pct, 1)
      
      # Store in reactive values
      custom_aoi_data$uploaded <- TRUE
      custom_aoi_data$boundary <- boundary
      custom_aoi_data$planning_units <- pu_clipped
      current_data(list(
        planning_units = pu_clipped,
        boundary_matrix = NULL,
        error = FALSE,
        scope_id = "custom"
      ))
      custom_aoi_data$boundary_matrix <- NULL  # Will be calculated if BLM > 0
      
      # Clip locked layers to AOI boundary - done once at upload, reused at render
      aoi_wgs84 <- st_transform(boundary, 4326)
      aoi_union <- st_union(aoi_wgs84)
      # PA/REFS: dissolved boundaries - exact intersection
      .intersect_to_aoi <- function(lyr) {
        if (is.null(lyr)) return(NULL)
        tryCatch({
          lyr <- st_make_valid(lyr)
          clipped <- st_intersection(lyr, aoi_union)
          if (nrow(clipped) == 0) return(NULL)
          clipped
        }, error = function(e) { cat('Clip error:', e$message, '\n'); NULL })
      }
      # Agriculture: individual PUs - bbox crop for speed
      .crop_to_aoi <- function(lyr) {
        if (is.null(lyr)) return(NULL)
        tryCatch({
          cropped <- st_crop(lyr, aoi_union)
          if (nrow(cropped) == 0) return(NULL)
          cropped
        }, error = function(e) { cat('Crop error:', e$message, '\n'); NULL })
      }
      results$locked_display <- list(
        pa   = .intersect_to_aoi(locked_layers$pa),
        refs = .intersect_to_aoi(locked_layers$refs),
        agri = .crop_to_aoi(locked_layers$agri)
      )
      cat(sprintf('Locked layers clipped to AOI - PA: %d, REFS: %d, Agri: %d features\n',
                  ifelse(is.null(results$locked_display$pa),   0, nrow(results$locked_display$pa)),
                  ifelse(is.null(results$locked_display$refs),  0, nrow(results$locked_display$refs)),
                  ifelse(is.null(results$locked_display$agri),  0, nrow(results$locked_display$agri))
      ))
      
      # Zoom preview map to uploaded AOI and display boundary
      boundary_wgs84_display <- st_transform(boundary, 4326)
      bbox <- st_bbox(boundary_wgs84_display)
      leafletProxy("preview_map") %>%
        clearGroup("aoi_boundary") %>%
        addPolygons(
          data = boundary_wgs84_display,
          color = "#2E7D32",
          weight = 3,
          fillOpacity = 0.1,
          fillColor = "#2E7D32",
          opacity = 1,
          group = "aoi_boundary"
        ) %>%
        fitBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"])
        )
      
      showNotification(
        paste0("AOI loaded: ", nrow(pu_clipped), " planning units"),
        type = "message",
        duration = 10
      )
      
    }, error = function(e) {
      custom_aoi_data$uploaded <- FALSE
      showNotification(
        paste0("Error loading boundary: ", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # AOI status output - handles both initial prompt and post-upload status
  output$aoi_status <- renderUI({
    if (custom_aoi_data$uploaded) {
      
      coverage <- custom_aoi_data$coverage
      
      # Coverage indicator
      if (!is.na(coverage) && coverage < 90) {
        coverage_html <- paste0(
          "<span style='color: #E65100; font-weight: bold;'>", round(coverage, 0), 
          "% AOI covered</span><br/>"
        )
      } else if (!is.na(coverage)) {
        coverage_html <- paste0(
          "<span style='color: #2E7D32;'>", round(coverage, 0), "% AOI covered</span><br/>"
        )
      } else {
        coverage_html <- ""
      }
      
      HTML(paste0(
        "<span style='color: #2E7D32; font-weight: bold;'>Ready</span><br/>",
        "<span style='color: #2E7D32;'>",
        format(custom_aoi_data$n_pus, big.mark = ","), " planning units<br/>",
        format(round(sum(custom_aoi_data$planning_units$Area_km2, na.rm = TRUE), 0), big.mark = ","), " km&sup2;",
        "</span><br/>",
        coverage_html
      ))
    } else {
      HTML("<small style='color: #000000;'>Upload a boundary file (KML, KMZ, Shapefile, GeoPackage, GeoJSON)</small>")
    }
  })
  
  # ---------------------------------------------------------------------------
  # LAYER WEIGHTS (scope-aware; custom AOI uses its base layer's weights)
  # ---------------------------------------------------------------------------
  effective_scope_id <- reactive({
    if (input$scope == "Custom Area of Interest") {
      req(input$aoi_base_layer)
      input$aoi_base_layer
    } else {
      available_scopes[[input$scope]]$id
    }
  })

  components_metadata <- reactive({
    load_component_metadata(effective_scope_id())
  })

  default_weights <- function(meta) as.list(setNames(meta$default_weight, meta$field_name))

  # Slider input ids for the layer weights (prefixed to avoid clashes with
  # other inputs)
  cw_id <- function(field) paste0("cw_", field)

  # Current layer weights; modified = FALSE means default costs are used
  component_weights <- reactiveValues(
    weights  = NULL,
    modified = FALSE
  )

  # Reset weights to the scope defaults whenever the effective scope changes
  observe({
    meta <- components_metadata()
    if (!is.null(meta)) {
      component_weights$weights  <- default_weights(meta)
      component_weights$modified <- FALSE
      cat("Initialised", nrow(meta), "layer weights for", effective_scope_id(), "\n")
    }
  })

  # Layer weighting modal: one tab per zone
  component_tab <- function(comps, colour) {
    div(
      style = "max-height: 450px; overflow-y: auto;",
      lapply(seq_len(nrow(comps)), function(i) {
        comp <- comps[i, ]
        div(
          style = "padding: 10px; border-bottom: 1px solid #eee;",
          h5(comp$label, style = paste0("margin-top: 0; color: ", colour, ";")),
          p(comp$layer_id, style = "font-size: 11px; color: #666; margin: 5px 0;"),
          sliderInput(
            inputId = cw_id(comp$field_name),
            label   = NULL,
            min = 0, max = 5, step = 0.1,
            value = component_weights$weights[[comp$field_name]] %||% comp$default_weight,
            width = "100%"
          )
        )
      })
    )
  }

  observeEvent(input$customize_components, {

    meta <- components_metadata()

    if (is.null(meta)) {
      showNotification(
        paste0("Layer weights file not found: weights/", effective_scope_id(), "_weights.csv"),
        type = "error",
        duration = 5
      )
      return()
    }

    bio_components  <- meta[meta$category == "bio", ]
    refs_components <- meta[meta$category == "refs", ]
    agri_components <- meta[meta$category == "agri", ]

    showModal(modalDialog(
      title = "Customize Cost Component Weights",
      size = "l",

      p("Adjust the weight of each input layer within its zone's cost. Each zone cost is",
        "the weighted mean of its layers (0-100 scale); higher weight = greater influence."),
      p(strong(paste(nrow(meta), "layer weights:")),
        nrow(bio_components), "biodiversity,",
        nrow(refs_components), "REFS,",
        nrow(agri_components), "agriculture"),

      tabsetPanel(
        id = "component_tabs",
        tabPanel("Biodiversity", br(), component_tab(bio_components,  "#2E7D32")),
        tabPanel("REFS",         br(), component_tab(refs_components, "#1976D2")),
        tabPanel("Agriculture",  br(), component_tab(agri_components, "#FF9800"))
      ),

      footer = tagList(
        actionButton("reset_weights", "Reset to Defaults", class = "btn-warning"),
        modalButton("Cancel"),
        actionButton("apply_weights", "Apply Weights", class = "btn-success")
      )
    ))
  })

  # Apply layer weights
  observeEvent(input$apply_weights, {

    meta <- components_metadata()
    req(meta)

    new_weights <- default_weights(meta)
    for (field in meta$field_name) {
      val <- input[[cw_id(field)]]
      if (!is.null(val)) new_weights[[field]] <- val
    }

    # Only flag as modified if any weight differs from the default, so an
    # unchanged Apply still uses the workflow costs exactly
    is_default <- isTRUE(all.equal(unlist(new_weights[meta$field_name]),
                                   meta$default_weight, check.attributes = FALSE))

    component_weights$weights  <- new_weights
    component_weights$modified <- !is_default

    removeModal()

    showNotification(
      if (is_default) "Weights unchanged: using default costs"
      else paste("Layer weights updated:", length(new_weights), "layers configured"),
      type = "message",
      duration = 3
    )

    cat("Applied layer weights (modified =", !is_default, ")\n")
  })

  # Reset weights to defaults
  observeEvent(input$reset_weights, {

    meta <- components_metadata()
    req(meta)

    component_weights$weights  <- default_weights(meta)
    component_weights$modified <- FALSE

    for (i in seq_len(nrow(meta))) {
      updateSliderInput(session, cw_id(meta$field_name[i]), value = meta$default_weight[i])
    }

    showNotification("Weights reset to defaults", type = "message", duration = 3)
    cat("Reset layer weights to defaults\n")
  })

  # Scope info display
  output$scope_info <- renderUI({
    scope_config <- available_scopes[[input$scope]]
    if (scope_config$id == "custom") {
      if (custom_aoi_data$uploaded) {
        HTML(paste0("Custom AOI: ", custom_aoi_data$n_pus, " Planning Units"))
      } else {
        HTML("Custom AOI: Upload boundary to begin")
      }
    } else {
      HTML(paste0("<strong>Resolution:</strong> ", scope_config$resolution, " Ha<br/>",
                  "<strong>Scale:</strong> ", ifelse(scope_config$id == "national", "Country-wide", "Provincial")))
    }
  })
  
  # Available percentage display
  output$target_available <- renderText({
    available <- 100 - input$target_conservation - input$target_refs - input$target_agriculture
    paste0("Available: ", available, "%")
  })
  
  # Zone weights as applied (normalised to a mean of 1)
  current_zone_weights <- reactive({
    c(input$zone_weight_bio, input$zone_weight_refs, input$zone_weight_agri)
  })

  output$weight_total <- renderUI({
    mult <- zone_multipliers(current_zone_weights())
    if (is.null(mult)) {
      return(HTML("<strong style='color:#D32F2F;'>At least one zone weight must be above 0</strong>"))
    }
    colour <- if (all(abs(mult - 1) < 1e-9)) "#2E7D32" else "#E65100"
    HTML(paste0(
      "<strong style='color:", colour, ";'>Cost multipliers applied</strong><br/>",
      sprintf("Conservation x%.2f | REFS x%.2f | Agriculture x%.2f", mult["bio"], mult["refs"], mult["agri"])
    ))
  })
  
  # Load data when scope changes
  observeEvent(input$scope, {
    
    if (input$scope == "Custom Area of Interest") {
      # Don't load data for custom AOI until file uploaded
      current_data(NULL)
      return()
    }
    
    cat("Scope changed to:", input$scope, "\n")
    
    scope_config <- available_scopes[[input$scope]]
    
    tryCatch({
      cat("Loading data for:", scope_config$id, "\n")
      
      # Load planning units
      if (!file.exists(scope_config$data_path)) {
        stop("Planning units not found (run scripts/app_prep/prepare_v2_planning_units.R): ",
             scope_config$data_path)
      }
      pu <- st_read(scope_config$data_path, quiet = TRUE)
      cat("  [OK] Loaded", nrow(pu), "planning units\n")

      # Calculate Area_km2 if it doesn't exist
      if (!"Area_km2" %in% colnames(pu)) {
        cat("  Calculating Area_km2 from geometry...\n")
        pu$Area_km2 <- as.numeric(st_area(pu)) / 1e6  # m2 to km2
      }

      # Precomputed boundary matrix (06c); must match the planning units
      boundary_matrix <- NULL
      if (!is.null(scope_config$boundary_path) && file.exists(scope_config$boundary_path)) {
        boundary_matrix <- readRDS(scope_config$boundary_path)
        if (nrow(boundary_matrix) != nrow(pu)) {
          cat(sprintf("  [WARN] Boundary matrix has %d rows but there are %d PUs; ignoring it (re-run 06c and prepare_v2_planning_units.R)\n",
                      nrow(boundary_matrix), nrow(pu)))
          boundary_matrix <- NULL
        } else {
          cat("  [OK] Loaded precomputed boundary matrix\n")
        }
      } else {
        cat("  [WARN] No precomputed boundary matrix; it will be computed at solve time if BLM > 0\n")
      }
      
      # Store data
      current_data(list(
        planning_units = pu,
        boundary_matrix = boundary_matrix,
        error = FALSE,
        scope_id = scope_config$id
      ))
      
      # Clip locked layers to scope boundary - skip for national (already full extent)
      if (scope_config$id != 'national' &&
          !is.null(scope_config$boundary_spatial_path) &&
          file.exists(scope_config$boundary_spatial_path)) {
        scope_boundary <- st_read(scope_config$boundary_spatial_path, quiet = TRUE)
        scope_boundary <- st_transform(scope_boundary, 4326)
        scope_union    <- st_union(scope_boundary)
        # PA/REFS: dissolved boundaries - use st_intersection for exact clip
        .intersect_to_scope <- function(lyr) {
          if (is.null(lyr)) return(NULL)
          tryCatch({
            lyr <- st_make_valid(lyr)
            clipped <- st_intersection(lyr, scope_union)
            if (nrow(clipped) == 0) return(NULL)
            clipped
          }, error = function(e) { cat('Clip error:', e$message, '\n'); NULL })
        }
        # Agriculture: individual PUs - use st_crop (bbox) for fast clipping
        .crop_to_scope <- function(lyr) {
          if (is.null(lyr)) return(NULL)
          tryCatch({
            cropped <- st_crop(lyr, scope_union)
            if (nrow(cropped) == 0) return(NULL)
            cropped
          }, error = function(e) { cat('Crop error:', e$message, '\n'); NULL })
        }
        results$locked_display <- list(
          pa   = .intersect_to_scope(locked_layers$pa),
          refs = .intersect_to_scope(locked_layers$refs),
          agri = .crop_to_scope(locked_layers$agri)
        )
        cat(sprintf('Locked layers clipped to %s - PA: %d, REFS: %d, Agri: %d\n',
                    scope_config$id,
                    ifelse(is.null(results$locked_display$pa),   0, nrow(results$locked_display$pa)),
                    ifelse(is.null(results$locked_display$refs),  0, nrow(results$locked_display$refs)),
                    ifelse(is.null(results$locked_display$agri),  0, nrow(results$locked_display$agri))
        ))
      } else if (scope_config$id == 'national') {
        # National scope - use full locked layers as-is, no clipping needed
        results$locked_display <- locked_layers
        cat('National scope - using full locked layers\n')
      }
      
    }, error = function(e) {
      cat("Error loading data:", e$message, "\n")
      current_data(list(error = TRUE, message = e$message))
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # =============================================================================
  # ULTRA-SIMPLE PREVIEW MAP - GUARANTEED TO WORK
  # =============================================================================
  
  output$preview_map <- renderLeaflet({
    
    data <- current_data()
    scope_config <- available_scopes[[input$scope]]
    
    # Just show a simple centered map with marker
    if (is.null(data) || data$error) {
      # No data - show empty map
      leaflet() %>%
        addProviderTiles("CartoDB.Positron", group = "Street") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        addLayersControl(
          baseGroups = c("Street", "Satellite", "Topographic"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(
          lng = scope_config$center["lng"],
          lat = scope_config$center["lat"],
          zoom = scope_config$zoom
        )
    } else {
      # Data loaded - show map with marker at center
      pu <- data$planning_units
      total_area <- sum(pu$Area_km2, na.rm = TRUE)
      n_pus <- nrow(pu)
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron", group = "Street") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        addLayersControl(
          baseGroups = c("Street", "Satellite", "Topographic"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(
          lng = scope_config$center["lng"],
          lat = scope_config$center["lat"],
          zoom = scope_config$zoom
        ) 
    }
  })
  
  observe({
    req(input$scope)
    
    scope_config <- available_scopes[[input$scope]]
    
    # Skip if no boundary file defined (e.g. Custom AOI before upload)
    if (is.null(scope_config$boundary_spatial_path) || 
        !file.exists(scope_config$boundary_spatial_path)) {
      leafletProxy("preview_map") %>% clearShapes()
      return()
    }
    
    boundary <- st_transform(st_read(scope_config$boundary_spatial_path, quiet = TRUE), 4326)
    
    leafletProxy("preview_map") %>%
      clearShapes() %>%
      addPolygons(
        data = boundary,
        color = "black",
        weight = 3,
        fillOpacity = 0,
        opacity = 1
      )
  })
  
  # Scenario value boxes - live updating
  output$scenario_pu_count <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = "-",
        subtitle = "Planning Units",
        icon = icon("th"),
        color = "navy"
      )
    } else {
      valueBox(
        value = format(nrow(data$planning_units), big.mark = ","),
        subtitle = "Planning Units",
        icon = icon("th"),
        color = "navy"
      )
    }
  })
  
  output$scenario_total_area <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = "-",
        subtitle = "Total Area (km\u00b2)",
        icon = icon("map"),
        color = "navy"
      )
    } else {
      total_area <- sum(data$planning_units$Area_km2, na.rm = TRUE)
      valueBox(
        value = format(round(total_area, 0), big.mark = ","),
        subtitle = "Total Area (km\u00b2)",
        icon = icon("map"),
        color = "navy"
      )
    }
  })
  
  output$scenario_available_area <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = "-",
        subtitle = "Available (%)",
        icon = icon("circle"),
        color = "navy"
      )
    } else {
      available_pct <- 100 - input$target_conservation - input$target_refs - input$target_agriculture
      total_area <- sum(data$planning_units$Area_km2, na.rm = TRUE)
      available_area <- total_area * (available_pct / 100)
      
      valueBox(
        value = paste0(available_pct, "%"),
        subtitle = paste0("Available (", format(round(available_area, 0), big.mark = ","), " km\u00b2)"),
        icon = icon("circle"),
        color = "navy"
      )
    }
  })
  
  output$scenario_conservation_target <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = paste0(input$target_conservation, "%"),
        subtitle = "Conservation Target",
        icon = icon("leaf"),
        color = "green"
      )
    } else {
      total_area <- sum(data$planning_units$Area_km2, na.rm = TRUE)
      target_area <- total_area * (input$target_conservation / 100)
      
      valueBox(
        value = paste0(input$target_conservation, "%"),
        subtitle = paste0("Conservation (", format(round(target_area, 0), big.mark = ","), " km\u00b2)"),
        icon = icon("leaf"),
        color = "green"
      )
    }
  })
  
  output$scenario_refs_target <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = paste0(input$target_refs, "%"),
        subtitle = "REFS Target",
        icon = icon("bolt"),
        color = "blue"
      )
    } else {
      total_area <- sum(data$planning_units$Area_km2, na.rm = TRUE)
      target_area <- total_area * (input$target_refs / 100)
      
      valueBox(
        value = paste0(input$target_refs, "%"),
        subtitle = paste0("REFS (", format(round(target_area, 0), big.mark = ","), " km\u00b2)"),
        icon = icon("bolt"),
        color = "blue"
      )
    }
  })
  
  output$scenario_agriculture_target <- renderValueBox({
    data <- current_data()
    
    if (is.null(data) || data$error) {
      valueBox(
        value = paste0(input$target_agriculture, "%"),
        subtitle = "Agriculture Target",
        icon = icon("seedling"),
        color = "orange"
      )
    } else {
      total_area <- sum(data$planning_units$Area_km2, na.rm = TRUE)
      target_area <- total_area * (input$target_agriculture / 100)
      
      valueBox(
        value = paste0(input$target_agriculture, "%"),
        subtitle = paste0("Agriculture (", format(round(target_area, 0), big.mark = ","), " km\u00b2)"),
        icon = icon("seedling"),
        color = "orange"
      )
    }
  })
  
  # RUN OPTIMIZATION
  output$solve_status <- renderText({
    if (is.null(results$solution)) {
      "Ready to run"
    } else {
      paste0("Last run: ", format(Sys.time(), "%H:%M:%S"))
    }
  })
  
  observeEvent(input$run_optimization, {
    
    # Switch to results tab
    runjs('$("a[data-value=\'results\']").tab("show");')
    
    # Scroll to top of page
    runjs('window.scrollTo({top: 0, behavior: "smooth"});')
    
    # Check for custom AOI
    if (input$scope == "Custom Area of Interest") {
      if (!custom_aoi_data$uploaded) {
        showNotification("Please upload a boundary file first", type = "error")
        return()
      }
      
      # Create data structure for custom AOI
      data <- list(
        planning_units = custom_aoi_data$planning_units,
        boundary_matrix = custom_aoi_data$boundary_matrix,
        error = FALSE
      )
      
      showNotification(
        paste0("Running optimization on ", nrow(data$planning_units), " planning units"),
        type = "message"
      )
    } else {
      # Regular scope - load current data
      data <- current_data()
      
      if (is.null(data) || data$error) {
        showNotification("Please load valid data first", type = "error")
        return()
      }
    }
    
    # Show initial status
    output$solve_status <- renderText("Preparing optimization...")
    
    tryCatch({
      
      # Planning units, minus any empty geometries. The precomputed boundary
      # matrix is subset to match, so rows stay aligned.
      pu   <- data$planning_units
      keep <- !st_is_empty(pu)
      pu_fixed <- pu[keep, ]
      bm <- data$boundary_matrix
      if (!is.null(bm) && !all(keep)) {
        cat(sprintf("Dropped %d empty geometries; subsetting boundary matrix\n", sum(!keep)))
        bm <- bm[keep, keep]
      }

      # User inputs
      target_conservation <- input$target_conservation / 100
      target_refs         <- input$target_refs / 100
      target_agriculture  <- input$target_agriculture / 100
      blm_value           <- as.numeric(input$blm)

      mult <- zone_multipliers(current_zone_weights())
      if (is.null(mult)) stop("At least one zone weight must be above 0")

      output$solve_status <- renderText("Calculating costs...")

      # --- Zone costs (0-100 scale, no rescaling) ---
      # Default: workflow costs as in 07. Custom layer weights: weighted mean
      # of the per-layer columns. Zone weights then scale each zone's cost.
      use_custom <- isTRUE(component_weights$modified)
      cat(if (use_custom) "Applying custom layer weights" else "Using workflow costs",
          "to", nrow(pu_fixed), "planning units\n")

      zone_costs <- build_zone_costs(
        st_drop_geometry(pu_fixed),
        meta    = components_metadata(),
        weights = component_weights$weights,
        custom  = use_custom
      )
      pu_fixed$weighted_cost_bio  <- zone_costs$bio  * mult[["bio"]]
      pu_fixed$weighted_cost_refs <- zone_costs$refs * mult[["refs"]]
      pu_fixed$weighted_cost_agri <- zone_costs$agri * mult[["agri"]]

      cat(sprintf("Zone multipliers: bio %.2f | refs %.2f | agri %.2f\n",
                  mult[["bio"]], mult[["refs"]], mult[["agri"]]))
      for (col in c("weighted_cost_bio", "weighted_cost_refs", "weighted_cost_agri")) {
        cat(sprintf("  %-18s range: %.2f - %.2f\n", col,
                    min(pu_fixed[[col]], na.rm = TRUE), max(pu_fixed[[col]], na.rm = TRUE)))
      }

      # Zone feature columns (area), as in 07
      pu_fixed$feat_z1 <- pu_fixed$Area_km2
      pu_fixed$feat_z2 <- pu_fixed$Area_km2
      pu_fixed$feat_z3 <- pu_fixed$Area_km2

      # Absolute area targets
      total_area <- sum(pu_fixed$Area_km2, na.rm = TRUE)
      targets <- matrix(c(
        total_area * target_conservation,
        total_area * target_refs,
        total_area * target_agriculture
      ), nrow = 1, ncol = 3)

      # Geometry is dropped for the solve (faster) and merged back afterwards
      pu_geom    <- st_geometry(pu_fixed)
      pu_no_geom <- st_drop_geometry(pu_fixed)

      output$solve_status <- renderText("Building optimization problem...")

      p <- problem(
        x = pu_no_geom,
        features = zones(
          "feat_z1",
          "feat_z2",
          "feat_z3",
          zone_names = ZONE_NAMES
        ),
        cost_column = c("weighted_cost_bio", "weighted_cost_refs", "weighted_cost_agri")
      ) %>%
        add_min_set_objective() %>%
        add_absolute_targets(targets) %>%
        add_binary_decisions()

      # Locks: single combined matrix, as in 07 / 08
      lock_mat <- build_lock_matrix(pu_no_geom, input$locked_areas)
      if (any(lock_mat)) {
        cat(sprintf("Locked PUs: Conservation %d | REFS %d | Agriculture %d\n",
                    sum(lock_mat[, 1]), sum(lock_mat[, 2]), sum(lock_mat[, 3])))
        p <- p %>% add_locked_in_constraints(lock_mat)
      }

      # Boundary penalties if BLM > 0
      if (blm_value > 0) {

        if (!is.null(bm)) {
          cat("Using precomputed boundary matrix\n")
          showNotification(
            "Boundary clustering enabled (using precomputed matrix)",
            duration = 5,
            type = "message"
          )
        } else {
          cat("Computing boundary matrix (this will be slow)...\n")
          showNotification(
            "Computing boundary matrix - this may take 30-60 seconds",
            duration = 10,
            type = "warning"
          )
          bm <- boundary_matrix(pu_fixed)  # needs geometry
        }

        if (!inherits(bm, "dgCMatrix")) bm <- as(bm, "generalMatrix")

        p <- p %>% add_boundary_penalties(
          penalty = blm_value,
          data = bm
        )
      }

      # Gurobi, with the same settings as 07
      p <- p %>% add_gurobi_solver(
        gap        = GUROBI_GAP,
        time_limit = GUROBI_TIMELIMIT,
        threads    = GUROBI_THREADS,
        verbose    = TRUE
      )

      output$solve_status <- renderText(
        paste0("Running Gurobi optimization...\n",
               "Planning units: ", format(nrow(pu_no_geom), big.mark = ","))
      )

      start_time <- Sys.time()
      solution <- solve(p)
      end_time <- Sys.time()
      solve_time <- as.numeric(difftime(end_time, start_time, units = "mins"))

      output$solve_status <- renderText("Processing results...")

      # Merge geometry back into the solution
      solution <- st_sf(solution, geometry = pu_geom)

      # Zone assignment: 0 = Available, 1 = Conservation, 2 = REFS, 3 = Agriculture
      solution$zone_assignment <- 0

      conservation_mask <- !is.na(solution$solution_1_Conservation) & solution$solution_1_Conservation == 1
      refs_mask         <- !is.na(solution$solution_1_REFS)         & solution$solution_1_REFS == 1
      agri_mask         <- !is.na(solution$solution_1_Agriculture)  & solution$solution_1_Agriculture == 1

      solution$zone_assignment[conservation_mask] <- 1
      solution$zone_assignment[refs_mask]         <- 2
      solution$zone_assignment[agri_mask]         <- 3

      cat(sprintf("Zone assignment: Available %d | Conservation %d | REFS %d | Agriculture %d | NA %d\n",
                  sum(solution$zone_assignment == 0, na.rm = TRUE),
                  sum(solution$zone_assignment == 1, na.rm = TRUE),
                  sum(solution$zone_assignment == 2, na.rm = TRUE),
                  sum(solution$zone_assignment == 3, na.rm = TRUE),
                  sum(is.na(solution$zone_assignment))))

      # Lock check: locked PUs not assigned to their locked zone (expect 0)
      if (any(lock_mat)) {
        n_bad <- sapply(1:3, function(z) sum(lock_mat[, z] & solution$zone_assignment != z))
        cat(sprintf("Lock check (locked PUs outside locked zone): PA=%d | REFS=%d | Agri=%d\n",
                    n_bad[1], n_bad[2], n_bad[3]))
        if (any(n_bad > 0)) cat("[WARN] Some locked PUs were not assigned to their locked zone\n")
      }

      # Statistics
      n_conservation <- sum(solution$zone_assignment == 1, na.rm = TRUE)
      n_refs         <- sum(solution$zone_assignment == 2, na.rm = TRUE)
      n_agriculture  <- sum(solution$zone_assignment == 3, na.rm = TRUE)
      n_available    <- sum(solution$zone_assignment == 0, na.rm = TRUE)

      area_conservation <- sum(solution$Area_km2[solution$zone_assignment == 1], na.rm = TRUE)
      area_refs         <- sum(solution$Area_km2[solution$zone_assignment == 2], na.rm = TRUE)
      area_agriculture  <- sum(solution$Area_km2[solution$zone_assignment == 3], na.rm = TRUE)
      area_available    <- sum(solution$Area_km2[solution$zone_assignment == 0], na.rm = TRUE)

      total_area <- sum(solution$Area_km2, na.rm = TRUE)

      # Filter to valid geometries
      output$solve_status <- renderText("Preparing map data...")
      geom_types <- st_geometry_type(solution)
      valid_geoms <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
      
      # Replace NA values in logical vector with FALSE
      valid_geoms[is.na(valid_geoms)] <- FALSE
      
      solution_clean <- solution[valid_geoms, ]
      
      cat("Geometry filtering:\n")
      cat("  Total features:", nrow(solution), "\n")
      cat("  Valid geometries:", sum(valid_geoms), "\n")
      cat("  Features after filter:", nrow(solution_clean), "\n")
      
      # Precompute display geometry - done once here so renderLeaflet is instant
      results$solution <- solution_clean
      n_pus <- nrow(solution_clean)
      
      if (n_pus > 50000) {
        # National scale: dissolve by zone first, then simplify the dissolved result
        # Far faster than simplifying 100k individual PUs
        cat('Large solve detected (', n_pus, 'PUs) - dissolving by zone for display...\n')
        map_geom <- st_transform(solution_clean, 3857)
        map_geom <- map_geom |>
          dplyr::group_by(zone_assignment) |>
          dplyr::summarise(Area_km2 = sum(Area_km2, na.rm = TRUE), .groups = 'drop')
        map_geom <- st_simplify(map_geom, dTolerance = 500, preserveTopology = TRUE)
        map_geom <- st_make_valid(map_geom)
        
        # Diagnostic
        geom_types <- st_geometry_type(map_geom)
        cat("Geometry types after dissolve:", paste(geom_types, collapse=", "), "\n")
        # Force all geometries to MULTIPOLYGON, handling any GEOMETRYCOLLECTION
        map_geom <- st_transform(map_geom, 4326)
        geom_fixed <- lapply(st_geometry(map_geom), function(g) {
          g <- st_make_valid(st_sfc(g, crs = 4326))[[1]]
          type <- st_geometry_type(st_sfc(g))
          if (type == "GEOMETRYCOLLECTION") {
            parts <- st_collection_extract(st_sfc(g, crs = 4326), "POLYGON")
            if (length(parts) > 0) st_cast(parts, "MULTIPOLYGON")[[1]]
            else st_geometrycollection()
          } else {
            st_cast(st_sfc(g), "MULTIPOLYGON")[[1]]
          }
        })
        st_geometry(map_geom) <- st_sfc(geom_fixed, crs = 4326)
        cat('Zone dissolve complete:', nrow(map_geom), 'zone polygons\n')
      } else {
        # Provincial scale: simplify individual PUs
        map_geom <- st_transform(solution_clean, 3857)
        map_geom <- st_simplify(map_geom, dTolerance = 50, preserveTopology = TRUE)
        map_geom <- st_make_valid(map_geom)
        map_geom <- st_transform(map_geom, 4326)
      }
      
      # Set solution_map LAST: this triggers renderLeaflet, so all data must be ready above
      results$solution_map <- map_geom
      results$solve_time <- solve_time
      results$stats <- list(
        n_conservation = n_conservation,
        n_refs = n_refs,
        n_agriculture = n_agriculture,
        n_available = n_available,
        area_conservation = area_conservation,
        area_refs = area_refs,
        area_agriculture = area_agriculture,
        area_available = area_available,
        pct_conservation = (area_conservation / total_area) * 100,
        pct_refs = (area_refs / total_area) * 100,
        pct_agriculture = (area_agriculture / total_area) * 100,
        pct_available = (area_available / total_area) * 100,
        total_area = total_area,
        target_pct = 100 * c(target_conservation, target_refs, target_agriculture)
      )
      
      # Update status
      output$solve_status <- renderText(paste0(
        "Optimization complete\n",
        "Solve time: ", round(solve_time, 2), " minutes\n",
        "Conservation: ", format(n_conservation, big.mark = ","), " PUs (", 
        round(results$stats$pct_conservation, 1), "%)\n",
        "REFS: ", format(n_refs, big.mark = ","), " PUs (", 
        round(results$stats$pct_refs, 1), "%)\n",
        "Agriculture: ", format(n_agriculture, big.mark = ","), " PUs (",
        round(results$stats$pct_agriculture, 1), "%)"
      ))
      
      showNotification(
        paste0("Optimization complete (", round(solve_time, 1), " min)"),
        type = "message",
        duration = 10
      )
      
    }, error = function(e) {
      output$solve_status <- renderText(paste0("Error: ", e$message))
      showNotification(
        paste0("Optimization failed: ", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Solution map with sampling for performance
  output$solution_map <- renderLeaflet({
    
    if (is.null(results$solution_map)) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron", group = "Street") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        addLayersControl(
          baseGroups = c("Street", "Satellite", "Topographic"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        setView(lng = 25, lat = -29, zoom = 5)
    } else {
      
      tryCatch({
        # Display data already pre-processed in solve block (dissolved + simplified for large solves)
        display_data <- results$solution_map
        
        # Remove rows with NA zone_assignment BEFORE transformation
        display_data <- display_data[!is.na(display_data$zone_assignment), ]
        
        # Check if we have any data left
        if (nrow(display_data) == 0) {
          cat("ERROR: No valid rows after filtering!\n")
          return(
            leaflet() %>%
              addProviderTiles("CartoDB.Positron", group = "Street") %>%
              addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
              addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
              addLayersControl(
                baseGroups = c("Street", "Satellite", "Topographic"),
                options = layersControlOptions(collapsed = FALSE)
              ) %>%
              setView(lng = 25, lat = -29, zoom = 5)
          )
        }
        
        # Remove Z dimension if present (causes leaflet issues)
        display_data <- st_zm(display_data, drop = TRUE, what = "ZM")
        
        # Ensure zone_assignment is numeric and within valid range
        display_data$zone_assignment <- as.numeric(as.character(display_data$zone_assignment))
        display_data <- display_data[display_data$zone_assignment %in% c(0, 1, 2, 3), ]
        
        # Add zone labels
        display_data$zone_label <- c("Available", "Conservation", "Renewables", "Agriculture")[display_data$zone_assignment + 1]
        
        # Replace any remaining NAs in critical fields with safe defaults
        # If dissolved-by-zone view, pu_id won't exist
        if (!"pu_id" %in% names(display_data)) {
          display_data$pu_id <- NA_character_   # dissolved view
        } else {
          display_data$pu_id <- as.character(display_data$pu_id)
          display_data$pu_id[is.na(display_data$pu_id)] <- "Unknown"  # PU view only
        }
        display_data$Area_km2[is.na(display_data$Area_km2)] <- 0
        
        # CRITICAL: Remove any rows where geometry is NULL or empty
        geom_valid <- !st_is_empty(display_data)
        geom_valid[is.na(geom_valid)] <- FALSE  # Treat NA as invalid
        display_data <- display_data[geom_valid, ]
        
        # Color palette - use character domain to match zone_assignment
        display_data$zone_char <- as.character(display_data$zone_assignment)
        
        # CRITICAL: Ensure Area_km2 can be rounded without error
        display_data$Area_km2 <- as.numeric(display_data$Area_km2)
        display_data$Area_km2[is.na(display_data$Area_km2)] <- 0
        
        # Popup strings (PU view or dissolved-by-zone view)
        display_data$popup_text <- ifelse(
          is.na(display_data$pu_id) | display_data$pu_id == "Unknown",
          paste0(
            "<strong>Zone:</strong> ", display_data$zone_label, "<br/>",
            "<strong>Total area:</strong> ", round(display_data$Area_km2, 0), " km&sup2;"
          ),
          paste0(
            "<strong>Planning Unit:</strong> ", display_data$pu_id, "<br/>",
            "<strong>Zone:</strong> ", display_data$zone_label, "<br/>",
            "<strong>Area:</strong> ", round(display_data$Area_km2, 2), " km&sup2;"
          )
        )
        
        # Fill colours by zone
        zone_color_map <- c("0" = "#CCCCCC", "1" = "#2E7D32", "2" = "#1976D2", "3" = "#FF9800")
        display_data$fill_color <- zone_color_map[display_data$zone_char]
        
        # Extract vectors - ensure they match row count
        fill_colors_vec <- as.character(display_data$fill_color)
        border_colors_vec <- as.character(display_data$fill_color)
        popup_vec <- as.character(display_data$popup_text)
        
        leaflet(display_data) %>%
          addProviderTiles("CartoDB.Positron", group = "Street") %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
          addPolygons(
            fillColor = fill_colors_vec,
            fillOpacity = 0.8,
            color = border_colors_vec,
            weight = 0.3,
            opacity = 0.8,
            popup = popup_vec,
            group = "Solution Zones"
          ) %>%
          { lyr <- results$locked_display$pa
          if (!is.null(lyr) && nrow(lyr) > 0) {
            lyr <- st_collection_extract(st_make_valid(lyr), "POLYGON")
            lyr <- st_cast(lyr, "MULTIPOLYGON")
            addPolygons(., data = lyr,
                        fillColor = "#FFD700", fillOpacity = 0.5,
                        color = "#B8860B", weight = 1, opacity = 0.8,
                        popup = "Protected Area", group = "Protected Areas")
          }  else . } %>%
          { lyr <- results$locked_display$refs
          if (!is.null(lyr) && nrow(lyr) > 0) {
            lyr <- st_collection_extract(st_make_valid(lyr), "POLYGON")
            lyr <- st_cast(lyr, "MULTIPOLYGON")
            addPolygons(., data = lyr,
                        fillColor = "#00BCD4", fillOpacity = 0.5,
                        color = "#006064", weight = 1, opacity = 0.8,
                        popup = "REFS Facility", group = "REFS Facilities")
          }  else . } %>%
          { lyr <- results$locked_display$agri
          if (!is.null(lyr) && nrow(lyr) > 0) {
            lyr <- st_collection_extract(st_make_valid(lyr), "POLYGON")
            lyr <- st_cast(lyr, "MULTIPOLYGON")
            addPolygons(., data = lyr,
                        fillColor = "#FF7043", fillOpacity = 0.5,
                        color = "#BF360C", weight = 1, opacity = 0.8,
                        popup = "Agriculture Zone", group = "Agriculture Zones")
          }  else . } %>%
          addLayersControl(
            baseGroups    = c("Street", "Satellite", "Topographic"),
            overlayGroups = c("Solution Zones", "Protected Areas", "REFS Facilities", "Agriculture Zones"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          hideGroup("Protected Areas") %>%
          hideGroup("REFS Facilities") %>%
          hideGroup("Agriculture Zones") %>%
          showGroup("Solution Zones") %>%
          addLegend(
            position = "bottomright",
            colors = c("#CCCCCC", "#2E7D32", "#1976D2", "#FF9800",
                       "#FFD700", "#00BCD4", "#FF7043"),
            labels = c("Available", "Conservation", "Renewables", "Agriculture",
                       "Protected Areas (locked)", "REFS Facilities (locked)", "Agriculture Zones (locked)"),
            title = "Land Use Zone",
            opacity = 1
          )
      }, error = function(e) {
        cat("ERROR in map rendering:\n")
        cat("  Message:", e$message, "\n")
        cat("  Call:", deparse(e$call), "\n")
        # Return empty map on error
        leaflet() %>%
          addProviderTiles("CartoDB.Positron", group = "Street") %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
          addLayersControl(
            baseGroups = c("Street", "Satellite", "Topographic"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          setView(lng = 25, lat = -29, zoom = 5)
      })
    }
  })
  
  # Results table - FIXED VERSION
  output$results_table <- renderDT({
    if (is.null(results$solution) || is.null(results$stats)) {
      data.frame(Message = "Run optimization to see results")
    } else {
      # Ensure all values are numeric (not NA)
      n_cons <- ifelse(is.na(results$stats$n_conservation), 0, results$stats$n_conservation)
      n_refs <- ifelse(is.na(results$stats$n_refs), 0, results$stats$n_refs)
      n_agri <- ifelse(is.na(results$stats$n_agriculture), 0, results$stats$n_agriculture)
      n_avail <- ifelse(is.na(results$stats$n_available), 0, results$stats$n_available)
      
      area_cons <- ifelse(is.na(results$stats$area_conservation), 0, results$stats$area_conservation)
      area_refs <- ifelse(is.na(results$stats$area_refs), 0, results$stats$area_refs)
      area_agri <- ifelse(is.na(results$stats$area_agriculture), 0, results$stats$area_agriculture)
      area_avail <- ifelse(is.na(results$stats$area_available), 0, results$stats$area_available)
      
      pct_cons <- ifelse(is.na(results$stats$pct_conservation), 0, results$stats$pct_conservation)
      pct_refs <- ifelse(is.na(results$stats$pct_refs), 0, results$stats$pct_refs)
      pct_agri <- ifelse(is.na(results$stats$pct_agriculture), 0, results$stats$pct_agriculture)
      pct_avail <- ifelse(is.na(results$stats$pct_available), 0, results$stats$pct_available)
      
      data.frame(
        `Land Use` = c("Conservation", "Renewable Energy", "Agriculture", "Available"),
        `Planning Units` = c(
          format(n_cons, big.mark = ","),
          format(n_refs, big.mark = ","),
          format(n_agri, big.mark = ","),
          format(n_avail, big.mark = ",")
        ),
        "Area (km\u00b2)" = c(
          format(round(area_cons, 0), big.mark = ","),
          format(round(area_refs, 0), big.mark = ","),
          format(round(area_agri, 0), big.mark = ","),
          format(round(area_avail, 0), big.mark = ",")
        ),
        `Percent` = c(
          paste0(round(pct_cons, 1), "%"),
          paste0(round(pct_refs, 1), "%"),
          paste0(round(pct_agri, 1), "%"),
          paste0(round(pct_avail, 1), "%")
        ),
        check.names = FALSE
      )
    }
  }, options = list(
    dom = 't',
    ordering = FALSE,
    columnDefs = list(
      list(className = 'dt-center', targets = 1:3)
    )
  ))
  
  # Value boxes
  # 1. Target Achievement
  output$stat_cost <- renderValueBox({
    if (!is.null(results$stats)) {
      # Targets are minimum areas, so a target is met when the allocated
      # share is at least the target used in the solve (locks and whole
      # planning units can push it above). Small tolerance for rounding.
      s <- results$stats
      all_met <- s$pct_conservation >= s$target_pct[1] - 0.01 &&
                 s$pct_refs         >= s$target_pct[2] - 0.01 &&
                 s$pct_agriculture  >= s$target_pct[3] - 0.01
      
      valueBox(
        value = if (all_met) "All met" else "Not met",
        subtitle = "Target Achievement",
        icon = icon("check-circle"),
        color = if(all_met) "green" else "yellow"
      )
    } else {
      valueBox(
        value = "-",
        subtitle = "Target Achievement",
        icon = icon("check-circle"),
        color = "light-blue"
      )
    }
  })
  
  # 2. Solve Time
  output$stat_time <- renderValueBox({
    valueBox(
      value = if (!is.null(results$solve_time)) paste0(round(results$solve_time, 2), " min") else "-",
      subtitle = "Solve Time",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  # 3. New Scenario Button - Centered and Taller
  output$stat_gap <- renderValueBox({
    valueBox(
      value = tags$div(
        style = "display: flex; justify-content: center; align-items: center; height: 100px;",
        actionButton("return_to_scenario", "Re-run New Scenario", 
                     icon = icon("plus-circle"),
                     class = "btn-success",
                     style = "font-size: 20px; padding: 20px 40px; width: 80%;")
      ),
      subtitle = "",
      icon = NULL,
      color = "orange"
    )
  })
  
  # Add observer to handle button click
  observeEvent(input$return_to_scenario, {
    runjs('$("a[data-value=\'scenario\']").tab("show");')
    runjs('window.scrollTo({top: 0, behavior: "smooth"});')
  })
  
  # Download shapefile
  output$download_shapefile <- downloadHandler(
    filename = function() {
      paste0("solution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(results$solution)
      
      # Create temp directory
      temp_dir <- tempfile(pattern = "shp_")
      dir.create(temp_dir)
      
      # Copy solution
      solution_shp <- results$solution
      
      # --- Ensure valid geometry ---
      solution_shp <- st_make_valid(solution_shp)
      
      # --- Ensure we have a clean zone code field ---
      if ("zone_assignment" %in% names(solution_shp)) {
        solution_shp$zone <- solution_shp$zone_assignment
      } else if (!"zone" %in% names(solution_shp)) {
        zc <- grep("zone", names(solution_shp), value = TRUE, ignore.case = TRUE)
        if (length(zc) > 0) solution_shp$zone <- solution_shp[[zc[1]]]
      }
      
      # --- Add a friendly label for non-GIS users ---
      solution_shp$zone_lbl <- dplyr::case_when(
        solution_shp$zone == 0 ~ "Available",
        solution_shp$zone == 1 ~ "Conservation",
        solution_shp$zone == 2 ~ "REFS",
        solution_shp$zone == 3 ~ "Agriculture",
        TRUE ~ NA_character_
      )
      
      # --- Keep only essential fields ---
      # Avoids duplicate/truncated name collisions in shapefile 10-char limit
      essential_cols <- c("zone", "zone_lbl", "Area_km2")
      keep_cols <- essential_cols[essential_cols %in% names(solution_shp)]
      solution_shp <- solution_shp[, keep_cols]
      cat("Fields in output shapefile:", paste(names(solution_shp), collapse = ", "), "\n")
      
      shp_path <- file.path(temp_dir, "solution.shp")
      
      # Write shapefile
      sf::st_write(
        solution_shp,
        shp_path,
        driver = "ESRI Shapefile",
        delete_layer = TRUE,
        quiet = TRUE
      )
      
      # Zip with paths relative to temp_dir (no setwd(), which would change
      # the working directory for every session sharing this R process)
      shp_files <- list.files(temp_dir, full.names = FALSE)
      zip::zip(zipfile = file, files = shp_files, root = temp_dir)
      
      unlink(temp_dir, recursive = TRUE)
    }
  )
  
  # Download GeoPackage
  output$download_gpkg <- downloadHandler(
    
    filename = function() {
      paste0("solution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gpkg")
    },
    
    content = function(file) {
      
      req(results$solution)
      
      solution_gpkg <- st_make_valid(results$solution)
      
      # --- Add a human-readable zone label ---
      # zone_assignment: 0=Available, 1=Conservation, 2=Renewables/REFS, 3=Agriculture
      if (!"zone_assignment" %in% names(solution_gpkg) && "zone" %in% names(solution_gpkg)) {
        solution_gpkg$zone_assignment <- solution_gpkg$zone
      }
      
      solution_gpkg$zone_label <- dplyr::case_when(
        solution_gpkg$zone_assignment == 0 ~ "Available",
        solution_gpkg$zone_assignment == 1 ~ "Conservation",
        solution_gpkg$zone_assignment == 2 ~ "Renewables",   # change to "REFS" if you prefer
        solution_gpkg$zone_assignment == 3 ~ "Agriculture",
        TRUE ~ NA_character_
      )
      
      sf::st_write(
        solution_gpkg,
        file,
        driver = "GPKG",
        delete_dsn = TRUE,
        quiet = TRUE
      )
    }
  )
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("solution_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Create summary dataframe
      summary_df <- data.frame(
        Zone = c("Conservation", "REFS", "Agriculture", "Available"),
        Planning_Units = c(
          results$stats$n_conservation,
          results$stats$n_refs,
          results$stats$n_agriculture,
          results$stats$n_available
        ),
        Area_km2 = c(
          results$stats$area_conservation,
          results$stats$area_refs,
          results$stats$area_agriculture,
          results$stats$area_available
        ),
        Percent = c(
          results$stats$pct_conservation,
          results$stats$pct_refs,
          results$stats$pct_agriculture,
          results$stats$pct_available
        )
      )
      
      write.csv(summary_df, file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
