# OParl Augsburg – Spatial Analysis

Pipeline to fetch Augsburg council papers (OParl), extract location mentions via fuzzy matching, and visualize them on an interactive map.

## Key Output

- `docs/report.html`: rendered R Markdown report with the interactive Leaflet map (filtered to ≥85% match score). Open directly in a browser.

## Quickstart (R)

1. Install required R packages (one time):

```r
install.packages(c("tidyverse", "sf", "leaflet", "knitr", "httr", "jsonlite", "dplyr", "pdftools", "yaml", "htmlwidgets", "stringdist"))
```

1. Run the full pipeline (from project root):

```r
Rscript analysis/run_all.r
```

1. View results:
	- Interactive map: `figures/interactive_map.html`
	- Full report: `docs/report.html`

## Inputs & Config

- `config.yaml`: city/API settings and limits (max papers/pages, etc.).
- Data folders under `data/` are created/used by the pipeline (PDFs, gazetteer, extracted locations).

## Scripts (R)

- `R/01_gazetteer.r`: build street gazetteer from OSM (Overpass).
- `R/02_fetch_papers.r`: download OParl papers + metadata.
- `R/03_extract_locations.r`: extract PDF text and fuzzy-match to streets.
- `R/04_visualize.r`: create interactive map (85%+ match filter).
- `analysis/run_all.r`: orchestrates all steps.

## Notes

- Rendering `docs/report.Rmd` in RStudio also produces `docs/report.html`
- Large data files and other generated HTML/PDFs remain ignored per `.gitignore`.

