# Copyright 2024 DARWIN EU (C)
#
# This file is part of DrugUtilisation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Generate a cohort set from the CDM episode table
#'
#' @description
#' Creates a new cohort table in the CDM reference based on records in the
#' `episode` table, filtered by `episode_concept_id` and an optional `conceptSet`
#' matching `episode_object_concept_id`.
#'
#' @inheritParams cdmDoc
#' @inheritParams newNameDoc
#' @param conceptSet Optional concept set list or numeric vector matching `episode_object_concept_id`.
#'   If NULL, all unique `episode_object_concept_id` values found in the episode table will be included.
#' @param episodeConceptId Numeric vector of concept IDs matching `episode_concept_id` (default: 32531 for Treatment Regimen).
#' @inheritParams subsetCohortDoc
#' @inheritParams subsetCohortIdDoc
#'
#' @return A CDM reference object with the new episode cohort table added.
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#'
#' cdm <- generateEpisodeCohortSet(
#'   cdm = cdm,
#'   name = "episode_cohort",
#'   episodeConceptId = 32531
#' )
#' }
generateEpisodeCohortSet <- function(cdm,
                                     name,
                                     conceptSet = NULL,
                                     episodeConceptId = 32531,
                                     subsetCohort = NULL,
                                     subsetCohortId = NULL) {
  # 1. Input Validation
  cdm <- omopgenerics::validateCdmArgument(cdm)
  name <- omopgenerics::validateNameArgument(name, null = FALSE)
  if (!"episode" %in% names(cdm)) {
    cli::cli_abort("`episode` table not found in cdm.")
  }

  omopgenerics::assertNumeric(episodeConceptId, null = TRUE)
  omopgenerics::assertCharacter(subsetCohort, length = 1, null = TRUE)
  if (!is.null(subsetCohort)) {
    if (!subsetCohort %in% names(cdm)) {
      cli::cli_abort("`subsetCohort` '{subsetCohort}' not found in cdm.")
    }
    validateCohort(cdm[[subsetCohort]])
    subsetCohortId <- omopgenerics::validateCohortIdArgument({{subsetCohortId}}, cdm[[subsetCohort]])
  }

  # Helper for cleaning cohort names to snake_case
  cleanCohortName <- function(x) {
    x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
    gsub("^_|_$", "", x)
  }

  # 2. Extract episodes
  episodes <- cdm$episode
  if (!is.null(episodeConceptId) && length(episodeConceptId) > 0) {
    episodes <- episodes |>
      dplyr::filter(.data$episode_concept_id %in% .env$episodeConceptId)
  }

  if (!is.null(conceptSet)) {
    conceptSetValidated <- validateConceptSet(conceptSet)
    cSetTable <- conceptSetFromConceptSetList(
      conceptSetValidated,
      dplyr::tibble(
        cohort_definition_id = seq_along(conceptSetValidated),
        cohort_name = cleanCohortName(names(conceptSetValidated))
      )
    )
    tmpName <- omopgenerics::uniqueTableName()
    cdm <- omopgenerics::insertTable(cdm = cdm, name = tmpName, table = cSetTable, overwrite = TRUE)
    on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = tmpName), add = TRUE)

    episodes <- episodes |>
      dplyr::inner_join(cdm[[tmpName]], by = c("episode_object_concept_id" = "drug_concept_id"))
  } else {
    # Dynamically extract object concept IDs and concept names
    objConcepts <- episodes |>
      dplyr::select("episode_object_concept_id") |>
      dplyr::distinct() |>
      dplyr::inner_join(
        cdm$concept |> dplyr::select("episode_object_concept_id" = "concept_id", "concept_name"),
        by = "episode_object_concept_id"
      ) |>
      dplyr::collect()

    if (nrow(objConcepts) == 0) {
      emptySet <- dplyr::tibble(
        cohort_definition_id = integer(),
        cohort_name = character()
      )
      cdm <- omopgenerics::emptyCohortTable(cdm, name = name)
      cdm[[name]] <- omopgenerics::newCohortTable(cdm[[name]], cohortSetRef = emptySet)
      return(cdm)
    }

    objConcepts <- objConcepts |>
      dplyr::arrange(.data$episode_object_concept_id) |>
      dplyr::mutate(
        cohort_definition_id = dplyr::row_number(),
        cohort_name = cleanCohortName(.data$concept_name)
      )

    cSetTable <- objConcepts |>
      dplyr::select("episode_object_concept_id", "cohort_definition_id")

    tmpName <- omopgenerics::uniqueTableName()
    cdm <- omopgenerics::insertTable(cdm = cdm, name = tmpName, table = cSetTable, overwrite = TRUE)
    on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = tmpName), add = TRUE)

    episodes <- episodes |>
      dplyr::inner_join(cdm[[tmpName]], by = "episode_object_concept_id")
  }

  # Apply patient subset if provided
  if (!is.null(subsetCohort)) {
    subPatients <- cdm[[subsetCohort]]
    if (!is.null(subsetCohortId) && length(subsetCohortId) > 0) {
      subPatients <- subPatients |>
        dplyr::filter(.data$cohort_definition_id %in% .env$subsetCohortId)
    }
    episodes <- episodes |>
      dplyr::inner_join(
        subPatients |> dplyr::select("subject_id") |> dplyr::distinct(),
        by = c("person_id" = "subject_id")
      )
  }

  # Restrict episodes to observation periods
  cohortTable <- episodes |>
    dplyr::transmute(
      cohort_definition_id = .data$cohort_definition_id,
      subject_id = .data$person_id,
      cohort_start_date = .data$episode_start_date,
      cohort_end_date = dplyr::coalesce(.data$episode_end_date, .data$episode_start_date)
    ) |>
    dplyr::filter(!is.na(.data$cohort_start_date)) |>
    dplyr::inner_join(
      cdm$observation_period |>
        dplyr::select(
          "subject_id" = "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "subject_id"
    ) |>
    dplyr::filter(
      .data$cohort_start_date <= .data$observation_period_end_date,
      .data$cohort_end_date >= .data$observation_period_start_date
    ) |>
    dplyr::mutate(
      cohort_start_date = dplyr::if_else(
        .data$cohort_start_date < .data$observation_period_start_date,
        .data$observation_period_start_date,
        .data$cohort_start_date
      ),
      cohort_end_date = dplyr::if_else(
        .data$cohort_end_date > .data$observation_period_end_date,
        .data$observation_period_end_date,
        .data$cohort_end_date
      )
    ) |>
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"
    ) |>
    dplyr::compute(name = name, temporary = FALSE, overwrite = TRUE)

  # Construct cohort set metadata
  if (!is.null(conceptSet)) {
    cohortSetRef <- dplyr::tibble(
      cohort_definition_id = seq_along(conceptSetValidated),
      cohort_name = cleanCohortName(names(conceptSetValidated))
    )
  } else {
    cohortSetRef <- objConcepts |>
      dplyr::select("cohort_definition_id", "cohort_name")
  }

  cdm[[name]] <- omopgenerics::newCohortTable(
    cohortTable,
    cohortSetRef = cohortSetRef
  )

  return(cdm)
}
