# Optimisation scripts

These scripts solve the land allocation problem outside the Shiny app. They build and solve the same problem the app does, but in batch: for example, all provinces at once, or the same province under several settings for calibration or sensitivity testing.

In plain terms, the optimisation assigns each hexagonal planning unit to one of three zones (Conservation, Renewable Energy, Agriculture) or leaves it unallocated. It meets an area target for each zone while keeping the total "cost" as low as possible, where cost reflects how unsuitable a hexagon is for the zone it is given. Existing Protected Areas, renewable energy facilities and cultivated land can be locked into their current zone.

## Before you start

- The data preparation scripts in `scripts/r_prep/` must have been run, so that each province has planning units with costs and lock flags, plus a boundary matrix.
- The optimisation uses the **Gurobi** solver, which needs a licence. Gurobi is free for academic institutions and registered non-profits. See the main README for setup.

## Scripts

| Script | What it does |
|---|---|
| `07_prioritizr_solve.R` | Solves the allocation for each province (and nationally), and saves the solution, a summary table and maps |
| `08_blm_elbow_curves.R` | Compares solutions run at different Boundary Length Modifier (BLM) values, to choose a sensible default |

### 07: solving a scenario

Settings are at the top of the script:

- **Targets**: the share of each province allocated to each zone. The values in the script are an example only; they are not recommended targets.
- **BLM**: how strongly compact, clumped solutions are favoured over scattered ones. 0 means no preference; the default of 0.001 comes from the calibration below.
- **Locks**: whether existing Protected Areas, facilities and cultivated land are fixed in place.
- **Scenario tag**: a label added to output file names so different runs do not overwrite each other.

Selection frequency (how often each hexagon is chosen across many near-optimal solutions) is optional and slow, so it is switched off by default.

After each solve, the script checks that every locked hexagon ended up in its locked zone and reports any that did not. All three counts should be zero.

### 08: choosing the BLM

A higher BLM gives more compact solutions but at a higher cost. `08` compares a set of BLM values for each province and measures, for each one, the number and size of patches in each zone and the extra cost relative to no BLM. The recommended value sits at the "elbow": the point beyond which extra compactness costs disproportionately more. This analysis produces the figures in Appendix A of the Technical Reference Document.

To run it:

1. Run `07` once for each BLM value listed in `08` (0, 0.001, 0.005 and 0.01 by default), keeping targets and lock settings the same, and setting the scenario tag to `sprintf("blm%g", BLM)`.
2. Run `08`.

## Outputs

Outputs go to `outputs/solutions/` (solutions, summaries, maps) and `outputs/blm_analysis/` (calibration metrics and figures). These folders are not tracked in the repository.

## Requirements

R with `sf`, `terra`, `dplyr`, `readr`, `tidyr`, `prioritizr`, `gurobi`, `ggplot2`, `patchwork`, `ggnewscale` and `here`, plus a licensed Gurobi installation. Open the project from the repository root before running.
