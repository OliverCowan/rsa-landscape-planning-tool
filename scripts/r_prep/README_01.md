# Data preparation scripts

These scripts turn a collection of spatial input layers into the planning units that the optimisation (`scripts/solve/`) and the Shiny app use.

In plain terms, they:

1. Put every input layer on a common scale of 0 to 100, separately for each land-use zone (Conservation, Renewable Energy, Agriculture).
2. Combine those layers, using weights, into one "cost" surface per zone.
3. Build a grid of hexagonal planning units and attach the zone costs to each hexagon.
4. Flag hexagons that are already Protected Areas, renewable energy facilities, or cultivated land.
5. Work out which hexagons share a border, so the optimisation can favour compact rather than scattered solutions.

## Before you start

The scripts assume two things are already in place:

- **Processed layers.** Every input layer saved as a GeoTIFF in `data/processed/{province}/`, already lined up with the national 90 m template grid and clipped to each province. To add a single new layer from its raw download, use `add_new_cost_layer.R`, which does this and then runs the reclassification for you.
- **The layer registry**, `layer_config_template.csv` in the repository root. One row per layer, recording how it should be rescaled, which zones it feeds, whether it counts as a cost or a benefit, which provinces it applies to, and its default weight.

The full list of input layers, their sources, and how each is rescaled is documented in the Technical Reference Document (`docs/technical_reference/`). Most source data is not redistributed here; see the main README.

## Run order

| Step | Script | What it does |
|---|---|---|
| 1 | `02_reclassification.R` | Rescales every layer to 0 to 100 for each zone it feeds |
| 2 | `03a_generate_weights_configs.R` | Creates a weights file per province from the layer registry |
| 3 | `03b_composite_cost.R` | Combines layers into one weighted cost surface per zone |
| 4 | `06a_planning_unit_generation.R` | Builds the hexagon grids and attaches zone costs |
| 5 | `06b_lock_layer_extraction.R` | Flags hexagons already under a land use (at least 33% cover) |
| 6 | `06c_boundary_matrix_precomputation.R` | Records which hexagons share borders, and by how much |
| Check | `03c_na_check.R` | Optional quality check for data gaps inside provinces (run after step 4) |
| Utility | `add_new_cost_layer.R` | Adds one new layer from raw data, then hands over to step 1 |

The numbers are not consecutive because the `04` and `05` scripts produce figures and live in `scripts/figures/`.

Every script covers the national extent and all nine provinces. Most can be limited to a single province by editing a setting at the top.

## What to re-run after a change

| If you change... | Re-run from... |
|---|---|
| A layer's rescaling in the registry, or add a layer | `02` (or `add_new_cost_layer.R`), then everything below |
| Default weights in the registry | `03a`, then everything below |
| A province's weights file (`weights/{province}_weights.csv`) | `03b`, then everything below |
| Planning unit size or boundaries | `06a`, `06b`, `06c`, then the solve |
| Protected Area, facility or cultivation layers | `06b`, then the solve |

Two things worth knowing:

- **`03a` overwrites the weights files.** If you have edited a province's weights by hand, go straight to `03b` instead.
- **`06c` only needs re-running if the hexagons themselves change.** New costs or lock flags do not affect it, and it is the slowest step (5 to 30 minutes per province).

## Outputs

All outputs go to `data/`, which is not tracked in this repository because of file size:

- `data/reclassified/` rescaled layers
- `data/composite/` combined cost surfaces per zone
- `data/planning_units/` hexagon grids with costs and lock flags, plus border matrices

The weights files (`weights/`) are small and are tracked, because hand edits there are not recoverable from the layer registry alone.

## Requirements

R with the packages `terra`, `sf`, `dplyr`, `readr`, `tidyr`, `classInt`, `Matrix`, `prioritizr` and `here`. Scripts locate files relative to the repository root using `here`, so open the project from the repository root (or its `.Rproj` file) before running them.
