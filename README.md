# Dutch municipal environmental vulnerability: complete private research archive

Independent research project by Joshua Aguele. This package contains the complete thesis document, all saved R scripts from the uploaded RStudio project, and its saved analysis tables and outputs. Two related manuscripts are under journal consideration. **Keep this repository private** while checking the material for public release.

## Where the work is

- `report/Complete_Thesis_Dutch_Municipal_Environmental_Vulnerability.docx`: full project report as provided.
- `scripts/04_full_environmental_analysis.R`: complete environmental workflow, copied byte-for-byte from the original project's `04_buurtdashboard_clean.R`. It covers workbook import, cleaning, municipal aggregation, data audit, PCA, clustering, multiple regression, spatial models, and robustness checks.
- `scripts/original/`: all eight R scripts found at the root of the original project, preserving their filenames. `00_setup.R` through `03_climate_matadata.R` and `05_merge_analysis_data.R` document earlier exploratory setup and CBS/socioeconomic work. `04_buurtdashboard.R` and `04_buurtdashboard_backup.R` are incomplete or alternative versions; `04_buurtdashboard_clean.R` is the most complete version. These scripts are preserved, not claimed to form a single clean pipeline.
- `scripts/legacy/`: the five earlier copies found in the original project's `scripts/` directory.
- `tables/`, `outputs/data_audit/`, `outputs/multivariate/`: saved data documentation, results, figures, models, and session information from the original project. They have not been regenerated in this package.

## Running the principal analysis

The original raw workbook is stored separately in the private backup and in `data_raw.zip`. Extract `Downloadbuurtdashboard_maa26-2.xlsx` into a local `data_raw/` folder next to this README. Run from the project root in a fresh RStudio session:

```r
install.packages(c("tidyverse", "readxl", "janitor", "sf", "spdep", "spatialreg", "fpc"))
source("scripts/04_full_environmental_analysis.R")
```

The script expects a specific workbook version (87-by-9 Metadata sheet), writes output into `tables/` and `outputs/`, and may retrieve CBS municipality boundaries through the PDOK endpoint in the code when the cached local GeoPackage is absent. Packages and upstream services may change. **No clean-session rerun has yet been verified**; the saved output files demonstrate past work, not a verified reproduction of this copy.

## Publication check

Before making this repository public: rerun the main script, compare the generated values with the report and both submitted manuscripts, check source-data redistribution rights, and decide whether the combined report and saved outputs should be disclosed while the manuscripts are under review. Raw source files, CBS boundary data, and RStudio session files were deliberately excluded from this copy. The original backup remains the authoritative complete archive including raw inputs.

This project report is independent research; it is not an awarded university degree thesis. There is no verified article or Zenodo DOI in this package.
