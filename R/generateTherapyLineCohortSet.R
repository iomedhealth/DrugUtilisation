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

#' Generate Line of Therapy (LOT) Subcohorts
#'
#' @description
#' Generates sequential Line of Therapy (LOT) subcohorts for patients in a base
#' patient cohort based on treatment exposure records. Handles multi-drug regimen
#' identification, landmark gap windows, and maintenance modalities according to
#' OHDSI / DARWIN-EU standards.
#'
#' @details
#' The algorithm identifies sequential therapy lines for each patient in `cohort`:
#' \itemize{
#'   \item \strong{Follow-Up Intersect}: Only treatment records starting within patient cohort follow-up are considered.
#'   \item \strong{Line Start & Landmark Window}: The first exposure starts Line 1. Exposures starting within
#'         `gapEra` days from line start form the combination regimen for that line.
#'   \item \strong{Maintenance Modalities}: Exposures matching `maintenanceModalities` or starting within `gapEra`
#'         days from the preceding exposure end date are subsumed into the current line without advancing line index.
#'   \item \strong{Line Transition}: An exposure starting after the gap window triggers transition to the next line.
#'   \item \strong{Regimen Inference}: Multi-drug combinations in each line are matched against `regimenRules`
#'         or concatenated alphabetically into regimen labels (e.g. `Dara-VRd`, `Isa-Kd`, `VRd`).
#' }
#'
#' @param cdm A CDM reference object created with \code{CDMConnector}.
#' @param name Name of the output cohort table created in the CDM.
#' @param cohort Name of the base patient cohort table in `cdm`.
#' @param cohortId Cohort definition ID(s) in `cohort` to restrict to. If \code{NULL}, all cohorts in `cohort` are included.
#' @param treatmentCohortName Name of the cohort table in `cdm` containing treatment exposures.
#' @param treatmentCohortId Cohort definition ID(s) in `treatmentCohortName` to restrict to. If \code{NULL}, all cohorts are included.
#' @param regimenRules List or data frame defining multi-drug combination rules. If a named list, names represent regimen labels
#'   and values are character vectors of drug/concept names (e.g. \code{list("Dara-VRd" = c("daratumumab", "bortezomib", "lenalidomide", "dexamethasone"))}).
#' @param gapEra Number of days for the landmark window and line transition gap (default: \code{60}).
#' @param maintenanceModalities Character vector of drug or procedure labels that do not advance line number (e.g. \code{c("ASCT (Transplant)", "lenalidomide")}).
#'
#' @return A \code{cohort_table} object created in \code{cdm} with subcohorts for each line number and regimen combination.
#'   Metadata attributes stored in \code{omopgenerics::settings(cdm[[name]])} include \code{cohort_definition_id}, \code{cohort_name},
#'   \code{line_number}, and \code{regimen_name}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(DrugUtilisation)
#'
#' cdm <- mockDrugUtilisation()
#'
#' cdm <- generateTherapyLineCohortSet(
#'   cdm = cdm,
#'   name = "lot_cohorts",
#'   cohort = "cohort1",
#'   treatmentCohortName = "cohort2",
#'   gapEra = 60
#' )
#' }
generateTherapyLineCohortSet <- function(cdm,
                                        name,
                                        cohort,
                                        cohortId = NULL,
                                        treatmentCohortName,
                                        treatmentCohortId = NULL,
                                        regimenRules = NULL,
                                        gapEra = 60,
                                        maintenanceModalities = character()) {
  # 1. Validation & Input Assertion
  cdm <- omopgenerics::validateCdmArgument(cdm)
  name <- omopgenerics::validateNameArgument(name, null = FALSE)
  omopgenerics::assertCharacter(cohort, length = 1)
  if (!cohort %in% names(cdm)) {
    cli::cli_abort("Cohort table '{cohort}' not found in cdm.")
  }
  validateCohort(cdm[[cohort]])

  baseSettings <- omopgenerics::settings(cdm[[cohort]])
  if (nrow(baseSettings) == 0) {
    emptySet <- dplyr::tibble(
      cohort_definition_id = integer(),
      cohort_name = character(),
      line_number = integer(),
      regimen_name = character()
    )
    cdm <- omopgenerics::emptyCohortTable(cdm, name = name)
    cdm[[name]] <- omopgenerics::newCohortTable(
      cdm[[name]],
      cohortSetRef = emptySet
    )
    return(cdm)
  }
  cohortId <- omopgenerics::validateCohortIdArgument({{cohortId}}, cdm[[cohort]])

  omopgenerics::assertCharacter(treatmentCohortName, length = 1)
  if (!treatmentCohortName %in% names(cdm)) {
    cli::cli_abort("Treatment cohort table '{treatmentCohortName}' not found in cdm.")
  }
  validateCohort(cdm[[treatmentCohortName]])

  treatSettings <- omopgenerics::settings(cdm[[treatmentCohortName]])
  if (nrow(treatSettings) == 0) {
    emptySet <- dplyr::tibble(
      cohort_definition_id = integer(),
      cohort_name = character(),
      line_number = integer(),
      regimen_name = character()
    )
    cdm <- omopgenerics::emptyCohortTable(cdm, name = name)
    cdm[[name]] <- omopgenerics::newCohortTable(
      cdm[[name]],
      cohortSetRef = emptySet
    )
    return(cdm)
  }
  treatmentCohortId <- omopgenerics::validateCohortIdArgument({{treatmentCohortId}}, cdm[[treatmentCohortName]])

  omopgenerics::assertNumeric(gapEra, integerish = TRUE, min = 0, length = 1)
  omopgenerics::assertCharacter(maintenanceModalities)

  parsedRules <- normalizeRegimenRules(regimenRules)

  # 2. Get Treatment Exposures intersecting Base Cohort
  baseCohort <- cdm[[cohort]]
  if (length(cohortId) > 0) {
    baseCohort <- baseCohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)
  }

  treatCohort <- cdm[[treatmentCohortName]]
  if (length(treatmentCohortId) > 0) {
    treatCohort <- treatCohort |>
      dplyr::filter(.data$cohort_definition_id %in% .env$treatmentCohortId)
  }

  treatSettingsSummary <- treatSettings |>
    dplyr::select("cohort_definition_id", "cohort_name")

  exposures <- baseCohort |>
    dplyr::select(
      "subject_id",
      "base_start" = "cohort_start_date",
      "base_end" = "cohort_end_date"
    ) |>
    dplyr::inner_join(
      treatCohort |>
        dplyr::select(
          "cohort_definition_id",
          "subject_id",
          "treatment_start" = "cohort_start_date",
          "treatment_end" = "cohort_end_date"
        ),
      by = "subject_id"
    ) |>
    dplyr::filter(
      .data$treatment_start >= .data$base_start,
      .data$treatment_start <= .data$base_end
    ) |>
    dplyr::collect() |>
    dplyr::inner_join(treatSettingsSummary, by = "cohort_definition_id")

  if (nrow(exposures) == 0) {
    emptySet <- dplyr::tibble(
      cohort_definition_id = integer(),
      cohort_name = character(),
      line_number = integer(),
      regimen_name = character()
    )
    cdm <- omopgenerics::emptyCohortTable(cdm, name = name)
    cdm[[name]] <- omopgenerics::newCohortTable(
      cdm[[name]],
      cohortSetRef = emptySet
    )
    return(cdm)
  }

  # 3. Process Lines of Therapy per subject
  lotResults <- exposures |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::group_split() |>
    purrr::map_dfr(\(df) processPatientLOT(df, gapEra, maintenanceModalities, parsedRules))

  if (nrow(lotResults) == 0) {
    emptySet <- dplyr::tibble(
      cohort_definition_id = integer(),
      cohort_name = character(),
      line_number = integer(),
      regimen_name = character()
    )
    cdm <- omopgenerics::emptyCohortTable(cdm, name = name)
    cdm[[name]] <- omopgenerics::newCohortTable(
      cdm[[name]],
      cohortSetRef = emptySet
    )
    return(cdm)
  }

  # 4. Map (line_number, regimen_name) to cohort_definition_id and cohort_name
  cohortSet <- lotResults |>
    dplyr::mutate(
      clean_regimen = tolower(gsub("[^a-zA-Z0-9]+", "_", .data$regimen_name)),
      clean_regimen = gsub("^_|_$", "", .data$clean_regimen),
      cohort_name = paste0("line_", .data$line_number, "_", .data$clean_regimen)
    ) |>
    dplyr::distinct(.data$line_number, .data$regimen_name, .data$cohort_name) |>
    dplyr::group_by(.data$cohort_name) |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$line_number, .data$regimen_name) |>
    dplyr::mutate(cohort_definition_id = dplyr::row_number()) |>
    dplyr::select("cohort_definition_id", "cohort_name", "line_number", "regimen_name")

  lotCohortTable <- lotResults |>
    dplyr::inner_join(cohortSet, by = c("line_number", "regimen_name")) |>
    dplyr::select(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    )

  # 5. Register cohort table in cdm
  cdm <- omopgenerics::insertTable(
    cdm = cdm,
    name = name,
    table = lotCohortTable
  )

  cdm[[name]] <- omopgenerics::newCohortTable(
    cdm[[name]],
    cohortSetRef = cohortSet
  )

  return(cdm)
}

#' Generate a Plotly Sankey Diagram for Line of Therapy Transitions
#'
#' @description
#' Constructs an interactive Plotly Sankey diagram visualizing patient transitions
#' across sequential Lines of Therapy (Line 1 -> Line 2 -> Line 3, etc.) generated by \code{generateTherapyLineCohortSet()}.
#'
#' @details
#' The visualization includes:
#' \itemize{
#'   \item \strong{Cumulative Patient Volume Coverage}: Dynamically selects top regimens at each line step covering
#'         at least \code{coverageThreshold} (default 80\%) of patient volume, grouping minor regimens into \code{"Line L: Other Regimens"}.
#'   \item \strong{Terminal Follow-Up Streams}: Displays \code{"Line L: End of Follow-Up"} nodes for patients who drop out or reach end of study.
#'   \item \strong{Strict Step Alignment}: Nodes and target streams are strictly column-aligned by therapy line step.
#' }
#'
#' @param cohort A LOT cohort table generated by \code{generateTherapyLineCohortSet()}.
#' @param coverageThreshold Numeric between 0 and 1 (default: \code{0.80}). Cumulative percentage
#'   of patient volume per line step to explicitly represent before grouping into \code{'Other Regimens'}.
#' @param maxLines Integer (default: \code{4}). Maximum number of therapy line steps to display. If \code{NULL}, all lines are displayed.
#' @param minLinePatients Minimum total patients in a therapy line step required to display the step column (default: \code{1}).
#' @param includeEndOfFollowUp Logical (default: \code{TRUE}). Whether to include terminal nodes for patients who drop out or end follow-up after Line L.
#'
#' @return A \code{plotly} sankey diagram object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#' # ...
#' plotTherapyLineSankey(cdm$lot_cohorts)
#' }
plotTherapyLineSankey <- function(cohort,
                                  coverageThreshold = 0.80,
                                  maxLines = 4,
                                  minLinePatients = 1,
                                  includeEndOfFollowUp = TRUE) {
  validateCohort(cohort)
  omopgenerics::assertNumeric(coverageThreshold, min = 0, max = 1, length = 1)
  omopgenerics::assertNumeric(maxLines, min = 1, integerish = TRUE, length = 1, null = TRUE)
  omopgenerics::assertNumeric(minLinePatients, min = 1, integerish = TRUE, length = 1)
  omopgenerics::assertLogical(includeEndOfFollowUp, length = 1)

  settings <- omopgenerics::settings(cohort)
  if (!all(c("line_number", "regimen_name") %in% colnames(settings))) {
    cli::cli_abort("Cohort settings must contain 'line_number' and 'regimen_name'.")
  }

  df <- cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::collect() |>
    dplyr::inner_join(settings, by = "cohort_definition_id") |>
    dplyr::select("subject_id", "line_number", "regimen_name") |>
    dplyr::arrange(.data$subject_id, .data$line_number)

  if (nrow(df) == 0) {
    cli::cli_abort("No data in LOT cohort table to generate Sankey diagram.")
  }

  if (!is.null(maxLines)) {
    df <- df |> dplyr::filter(.data$line_number <= .env$maxLines)
  }

  line_counts <- df |>
    dplyr::group_by(.data$line_number) |>
    dplyr::summarise(n = dplyr::n_distinct(.data$subject_id), .groups = "drop") |>
    dplyr::filter(.data$n >= .env$minLinePatients)

  if (nrow(line_counts) == 0) {
    cli::cli_abort("No therapy lines meet the minimum patient threshold ({minLinePatients}).")
  }

  valid_lines <- line_counts$line_number
  df <- df |> dplyr::filter(.data$line_number %in% .env$valid_lines)

  # Dynamic 80% coverage threshold per line step
  df_mapped <- df |>
    dplyr::group_by(.data$line_number, .data$regimen_name) |>
    dplyr::summarise(count = dplyr::n_distinct(.data$subject_id), .groups = "drop_last") |>
    dplyr::arrange(.data$line_number, dplyr::desc(.data$count)) |>
    dplyr::mutate(
      total = sum(.data$count),
      prop = .data$count / .data$total,
      cum_prop = cumsum(.data$prop),
      lag_cum_prop = dplyr::lag(.data$cum_prop, default = 0),
      keep = .data$lag_cum_prop < .env$coverageThreshold | dplyr::row_number() == 1,
      mapped_regimen = dplyr::if_else(.data$keep, .data$regimen_name, "Other Regimens")
    ) |>
    dplyr::ungroup() |>
    dplyr::select("line_number", "regimen_name", "mapped_regimen")

  df <- df |>
    dplyr::inner_join(df_mapped, by = c("line_number", "regimen_name"))

  max_line_in_data <- max(df$line_number)

  transitions <- df |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      next_line_number = dplyr::lead(.data$line_number),
      next_mapped_regimen = dplyr::lead(.data$mapped_regimen)
    ) |>
    dplyr::ungroup()

  active_links <- transitions |>
    dplyr::filter(!is.na(.data$next_mapped_regimen)) |>
    dplyr::mutate(
      source_label = paste0("Line ", .data$line_number, ": ", .data$mapped_regimen),
      target_label = paste0("Line ", .data$next_line_number, ": ", .data$next_mapped_regimen)
    ) |>
    dplyr::group_by(.data$source_label, .data$target_label) |>
    dplyr::summarise(value = dplyr::n(), .groups = "drop")

  if (includeEndOfFollowUp) {
    terminal_links <- transitions |>
      dplyr::filter(is.na(.data$next_mapped_regimen) & .data$line_number < max_line_in_data) |>
      dplyr::mutate(
        source_label = paste0("Line ", .data$line_number, ": ", .data$mapped_regimen),
        target_label = paste0("Line ", .data$line_number, ": End of Follow-Up")
      ) |>
      dplyr::group_by(.data$source_label, .data$target_label) |>
      dplyr::summarise(value = dplyr::n(), .groups = "drop")

    all_links <- dplyr::bind_rows(active_links, terminal_links)
  } else {
    all_links <- active_links
  }

  if (nrow(all_links) == 0) {
    cli::cli_abort("No transitions found between lines in LOT cohort.")
  }

  node_labels <- unique(c(all_links$source_label, all_links$target_label))
  node_dict <- setNames(seq_along(node_labels) - 1L, node_labels)

  # Compute node x and y positions based on line number step
  node_lines <- purrr::map_dbl(node_labels, \(lbl) {
    l_num <- as.numeric(sub("^Line ([0-9]+):.*", "\\1", lbl))
    if (grepl("End of Follow-Up", lbl, fixed = TRUE)) {
      l_num <- l_num + 1
    }
    l_num
  })

  max_line <- max(node_lines, na.rm = TRUE)
  min_line <- min(node_lines, na.rm = TRUE)

  node_x <- numeric(length(node_labels))
  node_y <- numeric(length(node_labels))

  for (l in unique(node_lines)) {
    indices <- which(node_lines == l)
    k <- length(indices)
    x_val <- if (max_line > min_line) (l - min_line) / (max_line - min_line) else 0.5
    x_val <- max(0.001, min(0.999, x_val))
    node_x[indices] <- x_val

    if (k == 1) {
      node_y[indices] <- 0.5
    } else {
      node_y[indices] <- seq(0.05, 0.95, length.out = k)
    }
  }

  colors_palette <- c(
    "#2b5c8f", "#5c2d91", "#27ae60", "#f39c12", "#8e44ad",
    "#16a085", "#2980b9", "#c0392b", "#1f77b4", "#7f8c8d",
    "#e74c3c", "#34495e", "#d35400", "#b03a2e", "#e67e22"
  )

  node_colors <- purrr::map_chr(node_labels, \(lbl) {
    if (grepl("End of Follow-Up", lbl, fixed = TRUE)) return("#d5dbdb")
    if (grepl("Other Regimens", lbl, fixed = TRUE)) return("#95a5a6")
    idx <- (sum(utf8ToInt(lbl)) %% length(colors_palette)) + 1L
    colors_palette[idx]
  })

  all_links <- all_links |>
    dplyr::mutate(
      source = unname(node_dict[.data$source_label]),
      target = unname(node_dict[.data$target_label])
    )

  plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",
    orientation = "h",
    node = list(
      label = node_labels,
      x = node_x,
      y = node_y,
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = all_links$source,
      target = all_links$target,
      value = all_links$value,
      color = "rgba(200, 200, 200, 0.4)"
    )
  )
}

normalizeRegimenRules <- function(regimenRules) {
  if (is.null(regimenRules)) return(NULL)
  if (is.data.frame(regimenRules)) {
    rules <- list()
    for (i in seq_len(nrow(regimenRules))) {
      nm <- as.character(regimenRules$regimen_name[i])
      drg <- regimenRules$drugs[[i]]
      if (is.character(drg) && length(drg) == 1) {
        drg <- unlist(strsplit(drg, "[,+%]"))
      }
      rules[[nm]] <- tolower(trimws(drg))
    }
    return(rules)
  }
  if (is.list(regimenRules)) {
    return(purrr::map(regimenRules, \(x) tolower(trimws(x))))
  }
  return(NULL)
}

processPatientLOT <- function(df, gapEra, maintenanceModalities, parsedRules) {
  df <- df |>
    dplyr::arrange(.data$treatment_start, .data$treatment_end)

  n <- nrow(df)
  line_nums <- integer(n)
  curr_line <- 1L
  line_start <- df$treatment_start[1]
  line_max_end <- df$treatment_end[1]

  line_nums[1] <- 1L

  if (n > 1) {
    for (i in 2:n) {
      t_start <- df$treatment_start[i]
      t_end <- df$treatment_end[i]
      drg_nm <- df$cohort_name[i]

      in_landmark <- as.numeric(t_start - line_start) <= gapEra
      within_gap <- as.numeric(t_start - line_max_end) <= gapEra
      is_maint <- tolower(drg_nm) %in% tolower(maintenanceModalities)

      if (in_landmark || within_gap || is_maint) {
        line_nums[i] <- curr_line
        if (t_end > line_max_end) {
          line_max_end <- t_end
        }
      } else {
        curr_line <- curr_line + 1L
        line_nums[i] <- curr_line
        line_start <- t_start
        line_max_end <- t_end
      }
    }
  }

  df$line_number <- line_nums

  res <- df |>
    dplyr::group_by(.data$subject_id, .data$line_number) |>
    dplyr::summarise(
      cohort_start_date = min(.data$treatment_start),
      cohort_end_date = max(.data$treatment_end),
      drugs = list(unique(tolower(trimws(.data$cohort_name)))),
      .groups = "drop"
    )

  res$regimen_name <- purrr::map_chr(res$drugs, \(d_list) {
    matchRegimenName(d_list, parsedRules)
  })

  res |>
    dplyr::select(
      "subject_id",
      "line_number",
      "regimen_name",
      "cohort_start_date",
      "cohort_end_date"
    )
}

matchRegimenName <- function(drugs, parsedRules) {
  if (!is.null(parsedRules) && length(parsedRules) > 0) {
    sorted_drugs <- sort(unique(drugs))
    for (r_name in names(parsedRules)) {
      r_drugs <- sort(unique(parsedRules[[r_name]]))
      if (length(sorted_drugs) == length(r_drugs) && all(sorted_drugs == r_drugs)) {
        return(r_name)
      }
    }
  }
  paste(sort(unique(drugs)), collapse = " + ")
}
