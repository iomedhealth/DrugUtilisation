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

#' Summarise proportion Of patients covered
#'
#' @description Gives the proportion of patients still in observation who are
#' in the cohort on any given day following their first cohort entry. This is
#' known as the “proportion of patients covered” (PPC) method for assessing
#' treatment persistence.
#'
#' @inheritParams cohortDoc
#' @inheritParams cohortIdDoc
#' @inheritParams strataDoc
#' @param followUpDays Number of days to follow up individuals for. If NULL the
#' maximum amount of days from an individuals first cohort start date to their
#' last cohort end date will be used
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation(numberIndividuals = 100)
#'
#' result <- cdm$cohort1 |>
#'   summariseProportionOfPatientsCovered(followUpDays = 365)
#'
#' tidy(result)
#' }
#'
summariseProportionOfPatientsCovered <- function(cohort,
                                                 cohortId = NULL,
                                                 strata = list(),
                                                 followUpDays = NULL) {
  # check input
  omopgenerics::assertNumeric(followUpDays, min = 1, length = 1, null = TRUE)
  strata <- validateStrata(strata, cohort, call = call)
  cohort <- validateCohort(cohort)
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cohort)

  cohortTableName <- omopgenerics::tableName(cohort)
  cohortTableName[is.na(cohortTableName)] <- "temp"

  cdm <- omopgenerics::cdmReference(cohort)

  analysisSettings <- dplyr::tibble(
    "result_id" = 1L,
    "result_type" = "summarise_proportion_of_patients_covered",
    package_name = "DrugUtilisation",
    package_version = pkgVersion(),
    cohort_table_name = cohortTableName
  )
  if (omopgenerics::isTableEmpty(cohort)) {
    cli::cli_warn("No records found in cohort table")
    return(omopgenerics::emptySummarisedResult(settings = analysisSettings))
  }

  if (is.null(followUpDays)) {
    cli::cli_inform("Setting followUpDays to maximum time from first cohort entry to last cohort exit per cohort")
    maxDays <- cohort |>
      dplyr::group_by(.data$cohort_definition_id, .data$subject_id) |>
      dplyr::summarise(
        first_cohort_start_date = min(.data$cohort_start_date, na.rm = TRUE),
        last_cohort_end_date = max(.data$cohort_end_date, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(days = as.integer(clock::date_count_between(
        start = .data$first_cohort_start_date,
        end = .data$last_cohort_end_date,
        precision = "day"
      ))) |>
      dplyr::group_by(.data$cohort_definition_id) |>
      dplyr::summarise(max_days = as.integer(max(.data$days, na.rm = TRUE))) |>
      dplyr::collect()
  } else {
    maxDays <- omopgenerics::settings(cohort) |>
      dplyr::mutate(max_days = .env$followUpDays) |>
      dplyr::select("cohort_definition_id", "max_days")
  }

  ppc <- purrr::map(cohortId, \(x) {
    workingMaxDays <- maxDays |>
      dplyr::filter(.data$cohort_definition_id == .env$x) |>
      dplyr::pull()
    getPPC(cohort, cohortId = x, strata = strata, days = workingMaxDays)
  }) |>
    dplyr::bind_rows()

  if (nrow(ppc) == 0) {
    cli::cli_inform(c(
      "i" =
        "No results found for any cohort, returning an empty summarised result"
    ))
    return(omopgenerics::emptySummarisedResult(settings = analysisSettings))
  }

  ppc <- ppc |>
    dplyr::mutate(
      !!!calculatePPC(ppc$outcome_count, ppc$denominator_count, 0.05),
      outcome_count = as.character(.data$outcome_count),
      denominator_count = as.character(.data$denominator_count)
    ) |>
    tidyr::pivot_longer(
      c("outcome_count", "denominator_count", "ppc", "ppc_lower", "ppc_upper"),
      names_to = "estimate_name",
      values_to = "estimate_value"
    ) |>
    dplyr::mutate(estimate_type = dplyr::if_else(
      stringr::str_starts(.data$estimate_name, "ppc"),
      "percentage",
      "integer"
    ))

  ppc <- ppc |>
    omopgenerics::uniteGroup(cols = "cohort_name") |>
    omopgenerics::uniteAdditional(cols = "time") |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      variable_name = "overall",
      variable_level = "overall",
      estimate_value = as.character(.data$estimate_value)
    ) |>
    dplyr::select(omopgenerics::resultColumns())


  ppc <- omopgenerics::newSummarisedResult(ppc, settings = analysisSettings)

  ppc
}

getPPC <- function(cohort, cohortId, strata, days) {
  workingCohortName <- omopgenerics::settings(cohort) |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    dplyr::pull("cohort_name")
  cli::cli_inform(glue::glue("Getting PPC for cohort {workingCohortName}"))

  cli::cli_inform("Collecting cohort into memory")
  workingCohort <- cohort |>
    dplyr::filter(.data$cohort_definition_id == .env$cohortId) |>
    PatientProfiles::addFutureObservationQuery(
      futureObservationName = "observation_end_date",
      futureObservationType = "date"
    ) |>
    dplyr::collect()

  if (nrow(workingCohort) == 0) {
    cli::cli_inform(c("i" = "No records found for {workingCohortName}"))
    return(NULL)
  }

  workingCohort <- workingCohort |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(min_cohort_start_date = min(.data$cohort_start_date,
      na.rm = TRUE
    )) |>
    dplyr::ungroup()

  cli::cli_inform(glue::glue("Getting PPC over {days} days following first cohort entry"))
  # Calculate counts from interval start/end events. This avoids rebuilding and
  # scanning the complete cohort once for every day of follow-up.
  result <- c(list(character()), strata) |>
    purrr::map(\(workingStrata) {
      getPPCCounts(
        workingCohort = workingCohort,
        workingStrata = workingStrata,
        days = days
      )
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(cohort_name = .env$workingCohortName)

  result
}

getPPCCounts <- function(workingCohort, workingStrata, days) {
  intervals <- workingCohort |>
    omopgenerics::uniteStrata(cols = workingStrata) |>
    dplyr::mutate(
      start_day = as.integer(.data$cohort_start_date - .data$min_cohort_start_date),
      end_day = as.integer(.data$cohort_end_date - .data$min_cohort_start_date),
      observation_end_day = as.integer(
        .data$observation_end_date - .data$min_cohort_start_date
      )
    )

  levels <- intervals |>
    dplyr::select("strata_name", "strata_level") |>
    dplyr::distinct()

  # prepare denominator interval
  denominatorIntervals <- intervals |>
    dplyr::group_by(.data$subject_id, .data$strata_name, .data$strata_level) |>
    dplyr::summarise(
      end_day = max(.data$observation_end_day, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(!"subject_id") |>
    dplyr::mutate(
      end_day = dplyr::if_else(.data$end_day > .env$days, .env$days, .data$end_day),
      start_day = 0L
    )

  # prepare outcome interval
  outcomeIntervals <- intervals |>
    dplyr::select("strata_name", "strata_level", "start_day", "end_day") |>
    dplyr::mutate(
      end_day = dplyr::if_else(.data$end_day > .env$days, .env$days, .data$end_day)
    ) |>
    dplyr::filter(.data$start_day <= .data$end_day)

  denominatorCounts <- countPPCIntervals(denominatorIntervals, levels, days) |>
    dplyr::rename(denominator_count = "count")
  outcomeCounts <- countPPCIntervals(outcomeIntervals, levels, days) |>
    dplyr::rename(outcome_count = "count")

  tidyr::expand_grid(levels, time = 0:days) |>
    dplyr::left_join(denominatorCounts, by = c("strata_name", "strata_level", "time")) |>
    dplyr::left_join(outcomeCounts, by = c("strata_name", "strata_level", "time"))
}

countPPCIntervals <- function(intervals, levels, days) {
  events <- dplyr::bind_rows(
    intervals |>
      dplyr::select("strata_name", "strata_level", "time" = "start_day") |>
      dplyr::mutate(change = 1L),
    intervals |>
      dplyr::select("strata_name", "strata_level", "time" = "end_day") |>
      dplyr::mutate(time = .data$time + 1L, change = -1L)
  ) |>
    dplyr::filter(.data$time <= .env$days) |>
    dplyr::group_by(.data$strata_name, .data$strata_level, .data$time) |>
    dplyr::summarise(change = sum(.data$change), .groups = "drop")

  tidyr::expand_grid(levels, time = 0:days) |>
    dplyr::left_join(events, by = c("strata_name", "strata_level", "time")) |>
    dplyr::mutate(change = dplyr::coalesce(.data$change, 0L)) |>
    dplyr::group_by(.data$strata_name, .data$strata_level) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::mutate(count = cumsum(.data$change)) |>
    dplyr::ungroup() |>
    dplyr::select(!"change")
}

calculatePPC <- function(num, den, alpha) {
  p <- num / den
  q <- 1 - p
  z <- stats::qnorm(1 - alpha / 2)
  t1 <- (num + z^2 / 2) / (den + z^2)
  t2 <- z * sqrt(den) / (den + z^2) * sqrt(p * q + z^2 / (4 * den))
  upper <- t1 + t2
  upper[upper > 1] <- 1
  lower <- t1 - t2
  lower[lower < 0] <- 0
  list(ppc = p, ppc_lower = lower, ppc_upper = upper) |>
    purrr::map(\(x) sprintf("%.2f", 100 * x))
}
