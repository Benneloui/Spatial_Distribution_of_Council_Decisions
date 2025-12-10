#!/usr/bin/env Rscript
# ==============================================================================
# 01_setup_gazetteer.R
# ==============================================================================
#
# Erstellt Straßenverzeichnis (Gazetteer) für eine Stadt über Overpass API
# Output: data/gazetteer/streets.csv mit Straßennamen + Koordinaten
#
# ==============================================================================

cat(rep("=", 70), "\n", sep = "")
cat("GAZETTEER SETUP - Straßennamen laden\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

# Pakete laden
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
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
required_fields <- c("city", "country", "max_streets")
missing_fields <- setdiff(required_fields, names(config))

if (length(missing_fields) > 0) {
  stop(sprintf("FEHLER: config.yaml fehlen folgende Felder:\n  - %s\n\nBitte hinzufügen.", paste(missing_fields, collapse = "\n  - ")))
}

city_name <- config$city
country <- config$country
max_streets <- as.integer(config$max_streets)

# Validierung: Werte prüfen
if (is.na(max_streets) || max_streets < 1) {
  stop("FEHLER: max_streets muss eine positive Zahl sein (z.B. 100)")
}

if (nchar(city_name) == 0 || nchar(country) == 0) {
  stop("FEHLER: city und country dürfen nicht leer sein")
}

# Output-Ordner erstellen
dir.create("data/gazetteer", recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Stadt: %s\n", city_name))
cat(sprintf("Land: %s\n", country))
cat(sprintf("Max Straßen: %d\n\n", max_streets))

# ------------------------------------------------------------------------------
# Bounding Box holen
# ------------------------------------------------------------------------------

cat("Suche Bounding Box für", city_name, "...\n")

nominatim_url <- "https://nominatim.openstreetmap.org/search"
response <- GET(
  nominatim_url,
  query = list(
    q = paste(city_name, country, sep = ", "),
    format = "json",
    limit = 1
  ),
  add_headers(`User-Agent` = "OParl-Mini-Pipeline/1.0"),
  timeout(30)
)

if (status_code(response) != 200) {
  stop(sprintf("FEHLER: Nominatim API antwortet mit Status %d\nÜberprüfe Stadt: %s, Land: %s",
               status_code(response), city_name, country))
}

city_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

if (length(city_data) == 0) {
  stop(sprintf("FEHLER: Stadt '%s' in '%s' nicht gefunden!\n\nBitte config.yaml überprüfen.", city_name, country))
}

# Extrahiere Bounding Box
bbox <- NULL

if (is.data.frame(city_data)) {
  # city_data ist ein data.frame
  if ("boundingbox" %in% names(city_data) && nrow(city_data) > 0) {
    # boundingbox ist eine Spalte
    bb_value <- city_data$boundingbox[[1]]  # Erste Zeile

    # bb_value kann Liste oder Vektor sein
    if (is.list(bb_value)) {
      bbox <- as.numeric(unlist(bb_value))
    } else {
      bbox <- as.numeric(bb_value)
    }
  }
} else if (is.list(city_data)) {
  # city_data ist eine Liste
  first_result <- city_data[[1]]

  if (is.list(first_result) && "boundingbox" %in% names(first_result)) {
    bbox <- as.numeric(unlist(first_result$boundingbox))
  }
}

if (is.null(bbox) || length(bbox) != 4) {
  # Debug-Output
  cat("\nDEBUG: city_data Struktur:\n")
  str(city_data)
  cat("\n")
  stop(sprintf("FEHLER: Könnte Bounding Box für '%s' nicht extrahieren.\n\nSiehe DEBUG-Output oben für Details.", city_name))
}

cat(sprintf("✓ Bounding Box gefunden: [%.4f, %.4f, %.4f, %.4f]\n\n",
            bbox[1], bbox[2], bbox[3], bbox[4]))

# Sys.sleep(1) für Rate Limiting
Sys.sleep(1)

# ------------------------------------------------------------------------------
# Straßen von OpenStreetMap laden
# ------------------------------------------------------------------------------

cat("Lade Straßennamen von OpenStreetMap...\n")

# Overpass Query mit korrekter Formatierung
# Bounding Box Format: (south, west, north, east)
overpass_query <- sprintf('[out:json][timeout:60];
(
  way["highway"]["name"](%.6f,%.6f,%.6f,%.6f);
);
out center;', bbox[1], bbox[3], bbox[2], bbox[4])

# Debug: Zeige Query
cat("\nOverpass Query:\n")
cat(overpass_query)
cat("\n\n")

overpass_url <- "https://overpass-api.de/api/interpreter"

Sys.sleep(1)  # Rate limiting

response <- POST(
  overpass_url,
  body = overpass_query,
  add_headers(`User-Agent` = "OParl-Mini-Pipeline/1.0"),
  encode = "raw",
  timeout(60)
)

if (status_code(response) != 200) {
  cat(sprintf("\nFEHLER: Overpass API antwortet mit Status %d\n", status_code(response)))
  cat("Antwort:\n")
  cat(content(response, "text"))
  cat("\n")
  stop(sprintf("Overpass API Fehler - Status %d", status_code(response)))
}

data <- fromJSON(content(response, "text", encoding = "UTF-8"))

if (is.null(data$elements) || length(data$elements) == 0) {
  stop(sprintf("Keine Straßen für %s gefunden. Bitte Overpass API Status prüfen.", city_name))
}

cat(sprintf("✓ Gefunden: %d Straßensegmente\n", nrow(data$elements)))

# ------------------------------------------------------------------------------
# Daten verarbeiten
# ------------------------------------------------------------------------------

streets_list <- list()

for (i in 1:nrow(data$elements)) {
  element <- data$elements[i, ]

  if (!is.null(element$tags) && !is.null(element$tags$name) &&
      !is.null(element$center) && !is.null(element$center$lat)) {

    streets_list[[length(streets_list) + 1]] <- data.frame(
      street_name = element$tags$name,
      latitude = element$center$lat,
      longitude = element$center$lon,
      stringsAsFactors = FALSE
    )
  }
}

if (length(streets_list) == 0) {
  stop(sprintf("Keine verwertbaren Straßendaten für %s gefunden.", city_name))
}

streets_df <- bind_rows(streets_list)

# Duplikate entfernen
streets_df <- streets_df %>%
  group_by(street_name) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(street_name)

# Auf max_streets begrenzen
if (nrow(streets_df) > max_streets) {
  cat(sprintf("Begrenze auf Top %d Straßen\n", max_streets))
  streets_df <- streets_df[1:max_streets, ]
}

cat(sprintf("✓ Verarbeitet: %d eindeutige Straßen\n\n", nrow(streets_df)))

# ------------------------------------------------------------------------------
# Speichern
# ------------------------------------------------------------------------------

output_file <- "data/gazetteer/streets.csv"
write.csv(streets_df, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("✓ Gespeichert: %s\n", output_file))
cat(sprintf("  %d Straßen mit Koordinaten\n\n", nrow(streets_df)))

# Preview
cat("Preview (erste 5 Straßen):\n")
print(head(streets_df, 5))

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("✓ GAZETTEER FÜR '%s' ERSTELLT\n", toupper(city_name)))
cat(rep("=", 70), "\n", sep = "")
