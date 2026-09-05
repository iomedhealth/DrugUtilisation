# Example Script: Generate Line of Therapy (LOT) Cohorts & Plotly Sankey
# Database: test_data/omop_1959_1786811115.duckdb

library(DrugUtilisation)
library(CDMConnector)
library(omopgenerics)
library(OmopHelpers)
library(CohortConstructor)
library(duckdb)
library(htmlwidgets)

# Connect to test_data DuckDB instance
con <- dbConnect(duckdb(), "test_data/omop_1959_1786811115.duckdb")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

cdm <- cdmFromCon(con, cdmSchema = "cdm", writeSchema = "main")

# 1. Base Multiple Myeloma Cohort (Concept Set 7343) using OmopHelpers & CohortConstructor
mmCodelist <- OmopHelpers::getCodelistFromConceptSet(
  conceptSetId = 7343,
  con = con,
  cdmSchema = "cdm"
)

cdm$base_mm <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = mmCodelist,
  name = "base_mm"
)

# 2. Extract Treatment Regimen Codelists (Concept Set IDs 7347-7378, 7483-7487, 11571-11573) using OmopHelpers
regimenIds <- c(7347:7358, 7360:7378, 7483, 7485:7487, 11571:11573)
rawCodelists <- lapply(regimenIds, function(id) {
  OmopHelpers::getCodelistFromConceptSet(conceptSetId = id, con = con, cdmSchema = "cdm")
})

mergedRaw <- unlist(rawCodelists, recursive = FALSE)
processedCodelists <- OmopHelpers::process_codelists(mergedRaw)
regimenCodelist <- omopgenerics::newCodelist(processedCodelists)

# 3. Generate Episode Cohort Set
cdm <- generateEpisodeCohortSet(
  cdm = cdm,
  name = "tx_episodes",
  conceptSet = regimenCodelist,
  episodeConceptId = 32531
)

# 4. Generate LOT Cohorts using DrugUtilisation native pipeline
cdm <- generateTherapyLineCohortSet(
  cdm = cdm,
  name = "lot_mm",
  cohort = "base_mm",
  treatmentCohortName = "tx_episodes",
  gapEra = 180
)

# 5. Generate Plotly Sankey Widget & Save to HTML
sankey <- plotTherapyLineSankey(
  cdm$lot_mm,
  coverageThreshold = 0.80,
  maxLines = 4,
  minLinePatients = 5,
  includeEndOfFollowUp = TRUE
)

outputPath <- file.path("extras", "mm_sankey_duckdb.html")
saveWidget(sankey, file = outputPath, selfcontained = TRUE)
cat("Sankey HTML saved to:", outputPath, "\n")
