



## Benötigte R-Pakete

```r
install.packages(c(
  "httr",         # HTTP requests (OParl API)
  "jsonlite",     # JSON parsing
  "dplyr",        # Data manipulation
  "pdftools",     # PDF text extraction
  "stringdist",   # Fuzzy string matching
  "leaflet",      # Interactive maps
  "htmlwidgets"   # Save HTML widgets
))
```


## Skript-Übersicht

### 01_setup_gazetteer.R
**Was:** Lädt Straßennamen + Koordinaten von OpenStreetMap
**Input:** `config.txt` (Stadt-Name)
**Output:** `data/gazetteer/streets.csv`
**Dauer:** ~30 Sekunden

**Beispiel-Output:**
```csv
street_name,latitude,longitude
Maximilianstraße,48.3693,10.8983
Königsplatz,48.3686,10.8970
```

---

### 02_fetch_papers.R
**Was:** Lädt kleine PDFs (<10 Seiten) von OParl API
**Input:** `config.txt` (OParl Endpoint)
**Output:** `data/pdfs/*.pdf` + `data/papers_metadata.csv`
**Dauer:** ~2-5 Minuten (je nach Papers)

**Beispiel-Output:**
```csv
paper_id,paper_title,paper_date,pdf_url,pdf_path,page_count
12345,Bebauungsplan Innenstadt,2024-03-15,https://...,data/pdfs/12345.pdf,8
```

---

### 03_extract_locations.R
**Was:** Findet Straßennamen in PDFs (Fuzzy Match)
**Input:** `data/pdfs/*.pdf` + `data/gazetteer/streets.csv`
**Output:** `data/extracted_locations.csv`
**Dauer:** ~1-3 Minuten

**Beispiel-Output:**
```csv
paper_id,paper_title,street_name,latitude,longitude,match_score
12345,Bebauungsplan...,Maximilianstraße,48.3693,10.8983,0.95
```

---

### 04_visualize_map.R
**Was:** Erstellt interaktive Leaflet-Karte
**Input:** `data/extracted_locations.csv`
**Output:** `figures/interactive_map.html`
**Dauer:** ~10 Sekunden


## Für andere Städte anpassen

1. Bearbeite `config.txt`:
   ```
   city: Köln
   oparl_endpoint: https://ratsinformation.stadt-koeln.de/oparl/system
   ```