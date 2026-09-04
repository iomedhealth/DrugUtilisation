# Example Script: Generate Line of Therapy (LOT) Cohorts & Plotly Sankey
# Database: test_data/omop_1959_1786811115.duckdb

library(DrugUtilisation)
library(CDMConnector)
library(omopgenerics)
library(dplyr)
library(duckdb)
library(htmlwidgets)

# Connect to test_data DuckDB instance
con <- dbConnect(duckdb(), "test_data/omop_1959_1786811115.duckdb")
cdm <- cdmFromCon(con, cdmSchema = "cdm", writeSchema = "main")

obs_p <- tbl(con, Id(schema = "cdm", table = "observation_period")) |>
  select(subject_id = person_id, obs_start = observation_period_start_date, obs_end = observation_period_end_date) |>
  collect()

# 1. Base Multiple Myeloma Cohort
base_mm_df <- tbl(con, Id(schema = "study_operations", table = "myeloma_cohort")) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) |>
  collect() |>
  inner_join(obs_p, by = "subject_id") |>
  filter(cohort_start_date >= obs_start, cohort_start_date <= obs_end) |>
  mutate(cohort_end_date = pmin(cohort_end_date, obs_end)) |>
  filter(cohort_start_date <= cohort_end_date) |>
  select(-obs_start, -obs_end)

base_set <- tibble(cohort_definition_id = 1, cohort_name = "multiple_myeloma")
cdm <- insertTable(cdm, name = "base_mm", table = base_mm_df)
cdm$base_mm <- newCohortTable(cdm$base_mm, cohortSetRef = base_set)

# 2. Extract Treatment Regimen Episodes from CDM episode table
ep_df <- tbl(con, Id(schema = "cdm", table = "episode")) |>
  filter(episode_concept_id == 32531) |>
  inner_join(
    tbl(con, Id(schema = "cdm", table = "concept")) |>
      select(episode_object_concept_id = concept_id, concept_name),
    by = "episode_object_concept_id"
  ) |>
  select(
    subject_id = person_id,
    cohort_start_date = episode_start_date,
    cohort_end_date = episode_end_date,
    raw_name = concept_name
  ) |>
  collect() |>
  filter(!is.na(cohort_start_date))

ep_df <- ep_df |>
  mutate(
    cohort_end_date = coalesce(cohort_end_date, cohort_start_date),
    regimen_name = case_when(
      grepl("Dara.*VRd|Daratumumab.*Bortezomib.*Lenalidomide", raw_name, ignore.case = TRUE) ~ "Dara-VRd",
      grepl("Dara.*VTd", raw_name, ignore.case = TRUE) ~ "Dara-VTd",
      grepl("Dara.*Rd|Daratumumab.*Lenalidomide", raw_name, ignore.case = TRUE) ~ "Dara-Rd",
      grepl("Dara.*VMP", raw_name, ignore.case = TRUE) ~ "Dara-VMP",
      grepl("Dara.*Vd", raw_name, ignore.case = TRUE) ~ "Dara-Vd",
      grepl("Isa.*Kd", raw_name, ignore.case = TRUE) ~ "Isa-Kd",
      grepl("Isa.*Pd", raw_name, ignore.case = TRUE) ~ "Isa-Pd",
      grepl("VRd", raw_name, ignore.case = TRUE) ~ "VRd",
      grepl("VTd", raw_name, ignore.case = TRUE) ~ "VTd",
      grepl("VMP", raw_name, ignore.case = TRUE) ~ "VMP",
      grepl("KRd", raw_name, ignore.case = TRUE) ~ "KRd",
      grepl("KCd", raw_name, ignore.case = TRUE) ~ "KCd",
      grepl("Kd", raw_name, ignore.case = TRUE) ~ "Kd",
      grepl("PVd", raw_name, ignore.case = TRUE) ~ "PVd",
      grepl("PCd", raw_name, ignore.case = TRUE) ~ "PCd",
      grepl("Pd", raw_name, ignore.case = TRUE) ~ "Pd",
      grepl("Rd|Lenalidomide and Dexamethasone", raw_name, ignore.case = TRUE) ~ "Rd",
      grepl("Vd|Bortezomib and Dexamethasone", raw_name, ignore.case = TRUE) ~ "Vd",
      grepl("MP|Melphalan and Prednisone", raw_name, ignore.case = TRUE) ~ "MP",
      grepl("CP|Cyclophosphamide and Prednisone", raw_name, ignore.case = TRUE) ~ "CP",
      TRUE ~ raw_name
    )
  )

# Extract sequential regimen episodes per patient
patient_lot <- ep_df |>
  group_by(subject_id) |>
  arrange(cohort_start_date, cohort_end_date) |>
  group_split() |>
  purrr::map_dfr(\(sub) {
    n <- nrow(sub)
    if (n == 0) return(NULL)
    line_num <- 1L
    line_nums <- integer(n)
    line_nums[1] <- 1L
    last_regimen <- sub$regimen_name[1]
    last_end <- sub$cohort_end_date[1]

    if (n > 1) {
      for (i in 2:n) {
        c_reg <- sub$regimen_name[i]
        c_start <- sub$cohort_start_date[i]
        c_end <- sub$cohort_end_date[i]

        gap <- as.numeric(c_start - last_end)
        if (c_reg != last_regimen && !is.na(gap) && gap > 60) {
          line_num <- line_num + 1L
          last_regimen <- c_reg
        }
        line_nums[i] <- line_num
        if (!is.na(c_end) && !is.na(last_end) && c_end > last_end) last_end <- c_end
      }
    }

    sub$line_number <- line_nums

    sub |>
      group_by(subject_id, line_number, regimen_name) |>
      summarise(
        cohort_start_date = min(cohort_start_date),
        cohort_end_date = max(cohort_end_date),
        .groups = "drop"
      )
  })

patient_lot <- patient_lot |>
  inner_join(obs_p, by = "subject_id") |>
  filter(cohort_start_date >= obs_start, cohort_start_date <= obs_end) |>
  mutate(cohort_end_date = pmin(cohort_end_date, obs_end)) |>
  filter(cohort_start_date <= cohort_end_date) |>
  select(-obs_start, -obs_end)

# 3. Map cohort settings
cohortSet <- patient_lot |>
  mutate(
    clean_regimen = tolower(gsub("[^a-zA-Z0-9]+", "_", regimen_name)),
    clean_regimen = gsub("^_|_$", "", clean_regimen),
    cohort_name = paste0("line_", line_number, "_", clean_regimen)
  ) |>
  distinct(line_number, regimen_name, cohort_name) |>
  group_by(cohort_name) |>
  filter(row_number() == 1) |>
  ungroup() |>
  arrange(line_number, regimen_name) |>
  mutate(cohort_definition_id = row_number()) |>
  select(cohort_definition_id, cohort_name, line_number, regimen_name)

lotCohortTable <- patient_lot |>
  inner_join(cohortSet, by = c("line_number", "regimen_name")) |>
  select(cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)

cdm <- insertTable(cdm, name = "lot_mm", table = lotCohortTable)
cdm$lot_mm <- newCohortTable(cdm$lot_mm, cohortSetRef = cohortSet)

cat("LOT Cohort created! Unique patients:", length(unique(lotCohortTable$subject_id)), "\n")

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
