#!/usr/bin/env Rscript
# ==============================================================================
# 04_visualize_map.R
# ==============================================================================
#
# Erstellt interaktive Leaflet-Karte mit PDF-Links
# Output: figures/interactive_map.html
#
# ==============================================================================

cat(rep("=", 70), "\n", sep = "")
cat("VISUALIZE MAP - Karte erstellen\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

# Pakete laden
suppressPackageStartupMessages({
  library(dplyr)
  library(leaflet)
  library(htmlwidgets)
  library(yaml)
})

# ==============================================================================
# SCRIPT-VERZEICHNIS ERMITTELN
# ==============================================================================

# Bestimme Projekt-Root relativ zum aktuellen Script

get_project_root <- function() {
  # Strategie 1: RStudio Project (beste Methode!)
  if (exists(".rs.getProjectDirectory")) {
    proj_dir <- try(.rs.getProjectDirectory(), silent = TRUE)
    if (!inherits(proj_dir, "try-error") && !is.null(proj_dir) && proj_dir != "") {
      return(proj_dir)
    }
  }

  # Strategie 2: Aktuelles Arbeitsverzeichnis (wenn .Rproj geöffnet)
  wd <- getwd()
  if (file.exists(file.path(wd, "geomodelierung.Rproj"))) {
    return(wd)
  }

  # Strategie 3: Via Rscript --file=
  if (exists(".this.dir")) {
    # RStudio (Source on Save)
    script_dir <- .this.dir
  } else {
    script_path <- tryCatch(
      {
        args <- commandArgs(trailingOnly = FALSE)
        file_index <- grep("--file=", args)
        if (length(file_index) > 0) {
          gsub("--file=", "", args[file_index])
        } else {
          file <- sys.frame(1)$ofile
          if (!is.null(file)) file else NA
        }
      },
      error = function(e) NA
    )

    if (!is.na(script_path)) {
      script_dir <- dirname(normalizePath(script_path))
      return(dirname(script_dir))  # Gehe von R/ zu Projekt-Root
    }
  }

  # Fallback
  return(wd)
}

project_root <- get_project_root()
setwd(project_root)

cat(sprintf("📂 Projekt-Verzeichnis: %s\n", getwd()))
cat(sprintf("🔍 Geöffnete .Rproj: geomodelierung.Rproj\n\n"))


# ==============================================================================
# CONFIG LADEN
# ==============================================================================

config_file <- file.path(project_root, "config.yaml")

if (!file.exists(config_file)) {
  stop(sprintf("FEHLER: %s nicht gefunden!\n\nBitte erstellen", config_file))
}

config <- read_yaml(config_file)

# Validierung: Alle erforderlichen Config-Werte prüfen
required_fields <- c("city")
missing_fields <- setdiff(required_fields, names(config))

if (length(missing_fields) > 0) {
  stop(sprintf("FEHLER: config.yaml fehlen folgende Felder:\n  - %s\n\nBitte hinzufügen.", paste(missing_fields, collapse = "\n  - ")))
}

city_name <- config$city

# Output-Ordner erstellen
dir.create("figures", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Daten laden
# ------------------------------------------------------------------------------

cat("1. Locations laden...\n")
if (!file.exists("data/extracted_locations.csv")) {
  stop("Locations nicht gefunden! Bitte 03_extract_locations.R ausführen.")
}

locations <- read.csv("data/extracted_locations.csv",
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat(sprintf("✓ %d Locations\n\n", nrow(locations)))

if (nrow(locations) == 0) {
  stop("Keine Locations zum Visualisieren!")
}

# ------------------------------------------------------------------------------
# Karte erstellen
# ------------------------------------------------------------------------------

cat("2. Karte erstellen...\n")

# Center berechnen
center_lat <- median(locations$latitude, na.rm = TRUE)
center_lon <- median(locations$longitude, na.rm = TRUE)

# Popup erstellen
locations <- locations %>%
  mutate(
    popup_text = sprintf(
      "<div style='font-family: Arial, sans-serif; max-width: 300px;'>
        <h4 style='margin: 0 0 10px 0; color: #d32f2f;'>%s</h4>
        <p style='margin: 5px 0;'><strong>Paper:</strong><br>%s</p>
        <p style='margin: 5px 0;'><strong>Datum:</strong> %s</p>
        <p style='margin: 5px 0;'><strong>Match:</strong> %.0f%%</p>
        <a href='%s' target='_blank' style='display: inline-block; margin-top: 10px; padding: 5px 10px; background: #1976d2; color: white; text-decoration: none; border-radius: 3px;'>
          📄 PDF öffnen
        </a>
      </div>",
      street_name,
      substr(paper_title, 1, 100),
      ifelse(is.na(paper_date), "k.A.", paper_date),
      match_score * 100,
      pdf_url
    )
  )

# Leaflet Karte
map <- leaflet(locations) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Hell") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dunkel") %>%
  setView(lng = center_lon, lat = center_lat, zoom = 13) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~popup_text,
    radius = 8,
    color = "#d32f2f",
    fillColor = "#f44336",
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    clusterOptions = markerClusterOptions(
      maxClusterRadius = 50,
      spiderfyOnMaxZoom = TRUE
    )
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Hell", "Dunkel"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addControl(
    html = sprintf(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2);'>
        <h3 style='margin: 0 0 5px 0;'>%s</h3>
        <p style='margin: 0; font-size: 14px;'>%d Locations aus %d Papers</p>
      </div>",
      city_name,
      nrow(locations),
      length(unique(locations$paper_id))
    ),
    position = "topright"
  )

cat("✓ Karte erstellt\n\n")

# ------------------------------------------------------------------------------
# Speichern (mit Pandoc-Fallback)
# ------------------------------------------------------------------------------

output_file <- "figures/interactive_map.html"

# Prüfe, ob Pandoc verfügbar ist (für selfcontained=TRUE erforderlich)
has_pandoc <- nzchar(Sys.which("pandoc")) || nzchar(Sys.getenv("RSTUDIO_PANDOC"))
selfcontained_flag <- has_pandoc

if (!has_pandoc) {
  cat("⚠️  Pandoc nicht gefunden. Speichere ohne selfcontained (Assets im Ordner figures/interactive_map_files).\n")
}

saveWidget(map, output_file, selfcontained = selfcontained_flag)

cat(sprintf("✓ Gespeichert: %s\n\n", output_file))

# Öffnen im Standard-Browser
browseURL(output_file)
# RStudio-Viewer (falls verfügbar)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::viewer(output_file)
}