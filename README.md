# RSA Landscape Planning Tool

A three-zone systematic conservation planning tool for South Africa, developed by the [Endangered Wildlife Trust (EWT)](https://www.ewt.org.za) Conservation Planning and Science Unit.

## Overview

The RSA Landscape Planning Tool (RSA LPT) uses integer linear programming (via [prioritizr](https://prioritizr.net/) and [Gurobi](https://www.gurobi.com/)) to identify priority areas across three land-use zones:

- **Conservation** — areas of high biodiversity value for protection
- **Renewable Energy (REFS)** — areas suitable for renewable energy development
- **Agriculture** — areas prioritised for continued agricultural use

The tool is built as an R Shiny web application and supports user-defined areas of interest (AOI), custom cost surfaces, and a biodiversity offset identification workflow.

## Dependencies

### R packages

- prioritizr 8.0.3 *(pinned — v8.1+ renames the `pu_id` field to `id`, which breaks this tool's downstream code; do not upgrade without also updating the affected scripts)*
- terra, sf
- shiny, leaflet
- tidyverse
- renv (package management)

### External

- Gurobi 13.0.1 (requires academic or commercial licence)
- ArcGIS Pro 3.6.1 (preprocessing scripts)

## Getting started

### 1. Clone the repo
```bash
git clone https://github.com/OliverCowan/rsa-landscape-planning-tool.git
cd rsa-landscape-planning-tool
```

### 2. Restore R environment
```r
install.packages("renv")
renv::restore()
```

### 3. Configure Gurobi
Place your `gurobi.lic` file in your home directory (`~/`). See `scripts/gurobi/` for setup notes.

### 4. Run the app
```r
shiny::runApp("R/")
```

## Data

Large spatial input layers are sourced from national and provincial custodians (including SANBI, DFFE, ESKOM, and DALRRD) and are not redistributed in this repository due to size and licensing restrictions. The full input layer inventory, including sources and reclassification methods, is documented in the Technical Reference Document (`docs/technical_reference/`).

A small sample dataset for testing is provided in `data/sample/`. For access to the full compiled dataset, contact the author.

## Project structure
rsa-landscape-planning-tool/
├── R/                        # Shiny app source
│   ├── modules/              # Shiny modules (AOI, solve, results)
│   └── utils/                # Helper functions (cost surface, BLM)
├── data/                     # Data inputs (mostly git-ignored)
│   └── sample/               # Small test dataset (tracked)
├── scripts/                  # Preprocessing and analysis scripts
│   ├── arcpy/                # ArcGIS Pro preprocessing (Python)
│   ├── r_prep/               # Cost surface and BLM matrix generation
│   │   └── offset_workflow/  # Biodiversity offset pipeline (3-script)
│   ├── gurobi/               # Gurobi licence and config notes
│   └── vps/                  # VPS utility scripts
├── deployment/               # Server deployment config
│   ├── nginx/                # nginx configuration
│   ├── ssl/                  # SSL setup notes
│   └── app_configs/          # App-level server config
├── docs/                     # Documentation and publications
│   ├── technical_reference/  # Technical reference document
│   ├── user_guide/           # User guide
│   ├── mee_paper/            # MEE manuscript
│   └── storymap/             # ArcGIS StoryMap assets
├── outputs/                  # Solve outputs (git-ignored)
│   └── sample_output/        # Sample output for reference (tracked)
└── tests/                    # Unit tests and fixtures
## Funding and acknowledgements

Developed under the [JRS Biodiversity Foundation](https://www.jrs.org/) 30×30 grant (extended to November 2026), supporting South Africa's commitments under the Kunming-Montreal Global Biodiversity Framework.

## Contact

Oliver Cowan — Conservation Planning and Science Unit, Endangered Wildlife Trust  
oliverc@ewt.org
