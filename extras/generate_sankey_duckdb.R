# Example Script: Generate Line of Therapy (LOT) Cohorts & Plotly Sankey
# Database: test_data/omop_1959_1786811115.duckdb

library(DrugUtilisation)
library(CDMConnector)
library(omopgenerics)
library(duckdb)
library(htmlwidgets)

# Connect to test_data DuckDB instance
con <- dbConnect(duckdb(), "test_data/omop_1959_1786811115.duckdb")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

cdm <- cdmFromCon(con, cdmSchema = "cdm", writeSchema = "main")

# 1. Base Multiple Myeloma Cohort
cdm$base_mm <- newCohortTable(
  tbl(con, Id(schema = "study_operations", table = "myeloma_cohort")),
  cohortSetRef = dplyr::tibble(cohort_definition_id = 1, cohort_name = "multiple_myeloma")
)

# 2. Treatment Regimen Cohort from episode table
cdm <- generateEpisodeCohortSet(
  cdm = cdm,
  name = "tx_episodes",
  episodeConceptId = 32531
)

# 3. Generate LOT Cohorts using DrugUtilisation native pipeline
cdm <- generateTherapyLineCohortSet(
  cdm = cdm,
  name = "lot_mm",
  cohort = "base_mm",
  treatmentCohortName = "tx_episodes",
  gapEra = 60
)

# 4. Generate Plotly Sankey Widget & Save to HTML
sankey <- plotTherapyLineSankey(
  cdm$lot_mm,
  coverageThreshold = 0.80,
  maxLines = 4,
  minLinePatients = 5,
  includeEndOfFollowUp = TRUE
)

outputPath <- file.path(tempdir(), "mm_sankey_duckdb.html")
saveWidget(sankey, file = outputPath, selfcontained = TRUE)
cat("Sankey HTML saved to:", outputPath, "\n")
