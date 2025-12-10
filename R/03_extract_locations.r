#!/usr/bin/env Rscript
# ==============================================================================
# 03_extract_locations.R
# ==============================================================================
#
# Extrahiert Straßennamen aus PDFs via Fuzzy Matching
# Output: data/extracted_locations.csv
#
# ==============================================================================

cat(rep("=", 70), "\n", sep = "")
cat("EXTRACT LOCATIONS - Straßen in PDFs finden\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

# Pakete laden
suppressPackageStartupMessages({
  library(pdftools)
  library(stringdist)
  library(dplyr)
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

# ------------------------------------------------------------------------------
# Daten laden
# ------------------------------------------------------------------------------

cat("1. Gazetteer laden...\n")
if (!file.exists("data/gazetteer/streets.csv")) {
  stop("Gazetteer nicht gefunden! Bitte 01_setup_gazetteer.R ausführen.")
}

gazetteer <- read.csv("data/gazetteer/streets.csv",
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat(sprintf("✓ %d Straßen\n\n", nrow(gazetteer)))

cat("2. Papers laden...\n")
if (!file.exists("data/papers_metadata.csv")) {
  stop("Papers nicht gefunden! Bitte 02_fetch_papers.R ausführen.")
}

papers <- read.csv("data/papers_metadata.csv",
                   stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat(sprintf("✓ %d Papers\n\n", nrow(papers)))

# ------------------------------------------------------------------------------
# Location Extraktion
# ------------------------------------------------------------------------------

cat("3. PDFs analysieren...\n\n")

all_locations <- data.frame()

for (i in 1:nrow(papers)) {
  paper <- papers[i, ]

  cat(sprintf("[%d/%d] %s\n", i, nrow(papers),
              substr(paper$paper_title, 1, 60)))

  if (!file.exists(paper$pdf_path)) {
    cat("  ✗ PDF nicht gefunden\n")
    next
  }

  tryCatch({
    # PDF Text extrahieren
    pdf_text <- pdf_text(paper$pdf_path)
    full_text <- paste(pdf_text, collapse = " ")

    # Straßennamen suchen
    found_streets <- data.frame()

    for (j in 1:nrow(gazetteer)) {
      street <- gazetteer[j, ]
      street_name <- street$street_name

      # Fuzzy Matching
      text_words <- unlist(strsplit(full_text, "\\s+"))

      distances <- stringdist(
        tolower(street_name),
        tolower(text_words),
        method = "lv"
      )

      min_dist <- min(distances, na.rm = TRUE)
      direct_match <- grepl(street_name, full_text, ignore.case = TRUE)

      if (min_dist <= 3 || direct_match) {
        match_score <- max(0, 1 - (min_dist / nchar(street_name)))

        found_streets <- rbind(found_streets, data.frame(
          paper_id = paper$paper_id,
          paper_title = paper$paper_title,
          paper_date = paper$paper_date,
          pdf_url = paper$pdf_url,
          pdf_path = paper$pdf_path,
          street_name = street_name,
          latitude = street$latitude,
          longitude = street$longitude,
          match_score = round(match_score, 2),
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(found_streets) > 0) {
      cat(sprintf("  ✓ Gefunden: %d Straßen\n", nrow(found_streets)))
      all_locations <- rbind(all_locations, found_streets)
    } else {
      cat("  → Keine Straßen\n")
    }

  }, error = function(e) {
    cat(sprintf("  ✗ Fehler: %s\n", e$message))
  })
}

cat("\n")

# ------------------------------------------------------------------------------
# Speichern
# ------------------------------------------------------------------------------

if (nrow(all_locations) > 0) {
  # Duplikate entfernen
  all_locations <- all_locations %>%
    group_by(paper_id, street_name) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(paper_id, desc(match_score))

  write.csv(all_locations, "data/extracted_locations.csv",
            row.names = FALSE, fileEncoding = "UTF-8")

  cat(sprintf("✓ Gespeichert: data/extracted_locations.csv\n"))
  cat(sprintf("  %d Locations\n\n", nrow(all_locations)))

  # Statistik
  cat("Statistik:\n")
  cat(sprintf("  Papers mit Locations: %d\n",
              length(unique(all_locations$paper_id))))
  cat(sprintf("  Eindeutige Straßen: %d\n",
              length(unique(all_locations$street_name))))
  cat(sprintf("  Ø Match Score: %.2f\n\n",
              mean(all_locations$match_score)))

  # Top Straßen
  top_streets <- all_locations %>%
    count(street_name, sort = TRUE) %>%
    head(5)

  cat("Top 5 Straßen:\n")
  print(top_streets)

} else {
  cat("⚠ Keine Locations gefunden!\n")
}

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("✓ EXTRACT LOCATIONS ABGESCHLOSSEN\n")
cat(rep("=", 70), "\n", sep = "")
