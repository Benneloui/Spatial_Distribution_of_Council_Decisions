#!/usr/bin/env Rscript
# ==============================================================================
# run_all.R - Komplette Pipeline ausführen
# ==============================================================================
#
# Führt alle Schritte nacheinander aus:
# 1. Gazetteer erstellen
# 2. Papers herunterladen
# 3. Locations extrahieren
# 4. Karte visualisieren
#
# Usage: Rscript run_all.R
#
# ==============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat(strrep("=", 80), "\n")
cat("  OPARL MINI-PIPELINE - VOLLSTÄNDIGER DURCHLAUF\n")
cat(strrep("=", 80), "\n")
cat(strrep("=", 80), "\n")
cat("\n")

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


# ------------------------------------------------------------------------------
# Prüfe Pakete
# ------------------------------------------------------------------------------

required_packages <- c(
  "yaml", "httr", "jsonlite", "dplyr",
  "pdftools", "stringdist", "leaflet", "htmlwidgets"
)

# ------------------------------------------------------------------------------
# Prüfe und installiere Pakete automatisch
# ------------------------------------------------------------------------------

required_packages <- c(
  "yaml", "httr", "jsonlite", "dplyr",
  "pdftools", "stringdist", "leaflet", "htmlwidgets"
)

cat("Prüfe Pakete...\n")

# Funktion zum Installieren fehlender Pakete
install_if_missing <- function(packages) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    cat(sprintf("\n⚙️  Installiere fehlende Pakete: %s\n", paste(missing_packages, collapse = ", ")))
    cat("   (Dies kann einige Minuten dauern...)\n\n")

    # Installiere fehlende Pakete
    install.packages(missing_packages, repos = "https://cloud.r-project.org")

    # Überprüfe ob Installation erfolgreich war
    still_missing <- missing_packages[!sapply(missing_packages, requireNamespace, quietly = TRUE)]

    if (length(still_missing) > 0) {
      cat(sprintf("\n✗ FEHLER: Diese Pakete konnten nicht installiert werden:\n"))
      cat(paste("  -", still_missing, collapse = "\n"))
      stop("Paketinstallation fehlgeschlagen!")
    }

    cat("\n✓ Alle Pakete erfolgreich installiert\n\n")
  } else {
    cat("✓ Alle benötigten Pakete sind bereits installiert\n\n")
  }
}

# Installiere fehlende Pakete
install_if_missing(required_packages)

# ------------------------------------------------------------------------------
# Schritt 1: Gazetteer
# ------------------------------------------------------------------------------

cat(strrep("=", 80), "\n")
cat("SCHRITT 1/4: GAZETTEER ERSTELLEN\n")
cat(strrep("=", 80), "\n\n")

tryCatch({
  source("R/01_gazetteer.R", encoding = "UTF-8")
  cat("\n✓ Gazetteer erfolgreich erstellt\n\n")
}, error = function(e) {
  cat(sprintf("\n✗ FEHLER in Schritt 1: %s\n", e$message))
  stop("Pipeline abgebrochen")
})

Sys.sleep(2)

# ------------------------------------------------------------------------------
# Schritt 2: Papers laden
# ------------------------------------------------------------------------------

cat(strrep("=", 80), "\n")
cat("SCHRITT 2/4: PAPERS HERUNTERLADEN\n")
cat(strrep("=", 80), "\n\n")

tryCatch({
  source("R/02_fetch_papers.R", encoding = "UTF-8")
  cat("\n✓ Papers erfolgreich heruntergeladen\n\n")
}, error = function(e) {
  cat(sprintf("\n✗ FEHLER in Schritt 2: %s\n", e$message))
  stop("Pipeline abgebrochen")
})

Sys.sleep(2)

# ------------------------------------------------------------------------------
# Schritt 3: Locations extrahieren
# ------------------------------------------------------------------------------

cat(strrep("=", 80), "\n")
cat("SCHRITT 3/4: LOCATIONS EXTRAHIEREN\n")
cat(strrep("=", 80), "\n\n")

tryCatch({
  source("R/03_extract_locations.R", encoding = "UTF-8")
  cat("\n✓ Locations erfolgreich extrahiert\n\n")
}, error = function(e) {
  cat(sprintf("\n✗ FEHLER in Schritt 3: %s\n", e$message))
  stop("Pipeline abgebrochen")
})

Sys.sleep(2)

# ------------------------------------------------------------------------------
# Schritt 4: Karte erstellen
# ------------------------------------------------------------------------------

cat(strrep("=", 80), "\n")
cat("SCHRITT 4/4: KARTE VISUALISIEREN\n")
cat(strrep("=", 80), "\n\n")

tryCatch({
  source("R/04_visualize.R", encoding = "UTF-8")
  cat("\n✓ Karte erfolgreich erstellt\n\n")
}, error = function(e) {
  cat(sprintf("\n✗ FEHLER in Schritt 4: %s\n", e$message))
  stop("Pipeline abgebrochen")
})

# ------------------------------------------------------------------------------
# Zusammenfassung
# ------------------------------------------------------------------------------

end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("\n")
cat(strrep("=", 80), "\n")
cat(strrep("=", 80), "\n")
cat("  ✓ PIPELINE ERFOLGREICH ABGESCHLOSSEN\n")
cat(strrep("=", 80), "\n")
cat(strrep("=", 80), "\n")
cat("\n")
cat(sprintf("Dauer: %.1f Minuten\n\n", as.numeric(duration)))
