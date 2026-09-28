# App data preparation

These scripts package the outputs of the data preparation workflow (`scripts/r_prep/`) into the files the Shiny app reads. They add nothing new to the method: the app uses the same costs and locked areas as the offline optimisation (`scripts/solve/`).

| Order | Script | What it does |
|---|---|---|
| 1 | `prepare_v2_planning_units.R` | Copies the workflow planning units (costs and lock flags) and adds each input layer's value per hexagon, so users can change individual layer weights in the app |
| 2 | `prepare_locked_layers.R` | Makes lightweight map overlays of the locked Protected Areas, facilities and cultivated land, for display only |

Run both after the data preparation workflow, and again whenever costs, weights or lock flags change.

All costs stay on the same 0 to 100 scale as the rest of the workflow. When a user sets custom layer weights, the app rebuilds each zone's cost as a plain weighted average of the layer values, so with the default weights it reproduces the default costs. `validate_province()` in the first script checks this.

Outputs go to `data/app/`, which is not tracked in this repository.
