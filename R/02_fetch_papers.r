#!/usr/bin/env Rscript
# ==============================================================================
# 02_fetch_papers.R
# ==============================================================================
#
# Lädt Papers von OParl API und filtert kleine PDFs (<10 Seiten)
# Output: data/pdfs/*.pdf + data/papers_metadata.csv
#
# ==============================================================================

cat(rep("=", 70), "\n", sep = "")
cat("FETCH PAPERS - PDFs von OParl laden\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

# Pakete laden
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(pdftools)
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
required_fields <- c("oparl_api", "max_papers", "max_pages_per_pdf")
missing_fields <- setdiff(required_fields, names(config))

if (length(missing_fields) > 0) {
  stop(sprintf("FEHLER: config.yaml fehlen folgende Felder:\n  - %s\n\nBitte hinzufügen.", paste(missing_fields, collapse = "\n  - ")))
}

system_url <- config$oparl_api
max_papers <- as.integer(config$max_papers)
max_pages_per_pdf <- as.integer(config$max_pages_per_pdf)

# Validierung: Werte prüfen
if (is.na(max_papers) || max_papers < 1) {
  stop("FEHLER: max_papers muss eine positive Zahl sein (z.B. 50)")
}

if (is.na(max_pages_per_pdf) || max_pages_per_pdf < 1) {
  stop("FEHLER: max_pages_per_pdf muss eine positive Zahl sein (z.B. 50)")
}

if (nchar(system_url) == 0) {
  stop("FEHLER: oparl_api darf nicht leer sein")
}

# Output-Ordner erstellen
dir.create("data/pdfs", recursive = TRUE, showWarnings = FALSE)

cat(sprintf("OParl Endpoint: %s\n", system_url))
cat(sprintf("Max Papers: %d\n", max_papers))
cat(sprintf("Max Seiten: %d\n\n", max_pages_per_pdf))

# ------------------------------------------------------------------------------
# OParl API Navigation
# ------------------------------------------------------------------------------

cat("1. System laden...\n")
response <- GET(system_url, timeout(30))
if (status_code(response) != 200) stop("System API Fehler")
system_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
cat("✓ System:", system_data$name, "\n\n")

cat("2. Body laden...\n")
body_url <- system_data$body
if (is.list(body_url)) body_url <- body_url[[1]]

response <- GET(body_url, timeout(30))
body_data <- fromJSON(content(response, "text", encoding = "UTF-8"))

# Falls Array von Bodies
if (is.data.frame(body_data$data)) {
  body_id <- body_data$data$id[1]
  response <- GET(body_id, timeout(30))
  body_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
}

cat("✓ Body:", body_data$name, "\n\n")

# Papers Endpoint
papers_url <- body_data$paper
cat("3. Papers laden...\n")

response <- GET(papers_url, timeout(30))
papers_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
papers_list <- papers_data$data

if (is.null(papers_list) || nrow(papers_list) == 0) {
  stop("Keine Papers gefunden!")
}

cat(sprintf("✓ Gefunden: %d Papers\n\n", nrow(papers_list)))

# Auf max_papers begrenzen
if (nrow(papers_list) > max_papers) {
  papers_list <- papers_list[1:max_papers, ]
}

# ------------------------------------------------------------------------------
# PDFs herunterladen
# ------------------------------------------------------------------------------

cat("4. PDFs herunterladen...\n\n")

papers_metadata <- data.frame()
downloaded <- 0
skipped <- 0

for (i in 1:nrow(papers_list)) {
  paper <- papers_list[i, ]

  paper_name <- if (!is.null(paper$name)) paper$name else "Unbekannt"
  cat(sprintf("[%d/%d] %s\n", i, nrow(papers_list), substr(paper_name, 1, 60)))

  # PDF URL extrahieren
  pdf_url <- NA
  if (!is.null(paper$mainFile) && is.list(paper$mainFile)) {
    pdf_url <- paper$mainFile$accessUrl
  }

  if (is.na(pdf_url) || is.null(pdf_url)) {
    cat("  → Kein PDF\n")
    skipped <- skipped + 1
    next
  }

  tryCatch({
    temp_pdf <- tempfile(fileext = ".pdf")

    GET(pdf_url, write_disk(temp_pdf, overwrite = TRUE), timeout(30))

    # Seitenzahl prüfen
    pdf_info <- pdf_info(temp_pdf)
    page_count <- pdf_info$pages

    if (page_count > max_pages_per_pdf) {
      cat(sprintf("  → Zu groß: %d Seiten\n", page_count))
      unlink(temp_pdf)
      skipped <- skipped + 1
      next
    }

    # Speichern
    paper_id <- basename(paper$id)
    pdf_filename <- sprintf("%s.pdf", paper_id)
    pdf_path <- file.path("data/pdfs", pdf_filename)

    file.copy(temp_pdf, pdf_path, overwrite = TRUE)
    unlink(temp_pdf)

    cat(sprintf("  ✓ Gespeichert: %d Seiten\n", page_count))

    papers_metadata <- rbind(papers_metadata, data.frame(
      paper_id = paper_id,
      paper_title = paper_name,
      paper_date = if (!is.null(paper$date)) paper$date else NA,
      pdf_url = pdf_url,
      pdf_path = pdf_path,
      page_count = page_count,
      stringsAsFactors = FALSE
    ))

    downloaded <- downloaded + 1

  }, error = function(e) {
    cat(sprintf("  ✗ Fehler: %s\n", e$message))
    skipped <- skipped + 1
  })

  Sys.sleep(1)
}

cat("\n")
cat(sprintf("✓ Heruntergeladen: %d PDFs\n", downloaded))
cat(sprintf("→ Übersprungen: %d\n\n", skipped))

# ------------------------------------------------------------------------------
# Metadaten speichern
# ------------------------------------------------------------------------------

if (nrow(papers_metadata) > 0) {
  write.csv(papers_metadata, "data/papers_metadata.csv",
            row.names = FALSE, fileEncoding = "UTF-8")

  cat("✓ Metadaten: data/papers_metadata.csv\n")
  cat(sprintf("  %d Papers\n\n", nrow(papers_metadata)))

  cat("Preview:\n")
  print(head(papers_metadata[, c("paper_id", "paper_title", "page_count")], 3))
} else {
  cat("⚠ Keine PDFs heruntergeladen!\n")
}

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("✓ FETCH PAPERS ABGESCHLOSSEN\n")
cat(rep("=", 70), "\n", sep = "")
