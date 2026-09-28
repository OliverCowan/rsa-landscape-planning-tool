# Figure scripts

These scripts produce the figures that describe the tool's inputs: how each input layer is weighted, and what the resulting cost surfaces look like. They do not run any optimisation.

## Scripts

| Script | Figure | Used for |
|---|---|---|
| `04a_heatmap_publication.R` | Grid of layer weights by zone, green for benefit layers and red for cost layers | Technical Reference Document, Figure 1 (national version) |
| `04b_heatmap_dark.R` | As 04a, dark theme | Presentations |
| `04c_bubble_publication.R` | The same weights as a bubble plot, bubble size showing weight | Alternative to 04a |
| `04d_bubble_dark.R` | As 04c, dark theme | Presentations |
| `05a_composite_maps.R` | One panel per zone: the nine provincial cost surfaces plus the national one | Technical Reference Document, Figures 2a to 2c |
| `05b_individual_composite_maps.R` | Each cost surface as its own map (10 scopes x 3 zones) | Reports and slides |
| `05c_layer_facet_maps.R` | Every input layer feeding each cost surface, as small maps | Appendix material |

Each script covers the national extent and all nine provinces.

## When to re-run

- The `04` scripts read only the layer registry (`layer_config_template.csv`) and the weights files (`weights/`). They are quick, and should be re-run whenever weights change.
- `05a` and `05b` read the combined cost surfaces, so re-run them after `03b_composite_cost.R`.
- `05c` reads the rescaled layers, so re-run it after `02_reclassification.R`.

## Outputs

Figures are saved to `outputs/figures/`, which is not tracked in this repository.

## Requirements

R with `readr`, `dplyr`, `tidyr`, `ggplot2`, `terra`, `cowplot` and `here`. Open the project from the repository root before running.
