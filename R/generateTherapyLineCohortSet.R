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
#' cohort based on treatment exposure records. Handles multi-drug regimen
#' identification, landmark gap windows, and maintenance modalities.
#'
#' @param cdm A CDM reference object.
#' @param name Name of the output cohort table created in the CDM.
#' @param cohort Name of the base patient cohort table.
#' @param cohortId Cohort definition ID(s) in `cohort` to include. If NULL, uses all.
#' @param treatmentCohortName Name of the cohort table or concept set containing drug exposures.
#' @param treatmentCohortId Cohort definition ID(s) in `treatmentCohortName`. If NULL, uses all.
#' @param regimenRules List or dataframe defining multi-drug combination rules and hierarchy ranking.
#' @param gapEra Number of days for the landmark window and line transition gap (default: 60).
#' @param maintenanceModalities Character vector of regimens or procedure labels that do not advance line number.
#'
#' @return A cohort table in `cdm` with subcohorts for each line order and regimen combination.
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

#' Summarise Therapy Line Duration & Time to Next Line (TTNL)
#'
#' @param cohort A LOT cohort table generated by `generateTherapyLineCohortSet()`.
#'
#' @return A summary dataframe containing patient counts, TTNL (days & months),
#'   exposure duration, and treatment-free gap metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DrugUtilisation)
#' cdm <- mockDrugUtilisation()
#' # ...
#' summariseTherapyLineDuration(cdm$lot_cohorts)
#' }
summariseTherapyLineDuration <- function(cohort) {
  validateCohort(cohort)
  settings <- omopgenerics::settings(cohort)
  if (!all(c("line_number", "regimen_name") %in% colnames(settings))) {
    cli::cli_abort("Cohort settings must contain 'line_number' and 'regimen_name'.")
  }

  df <- cohort |>
    dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date") |>
    dplyr::collect() |>
    dplyr::inner_join(settings, by = "cohort_definition_id") |>
    dplyr::distinct(.data$subject_id, .data$line_number, .keep_all = TRUE) |>
    dplyr::arrange(.data$subject_id, .data$line_number)

  if (nrow(df) == 0) {
    cli::cli_abort("No data in LOT cohort table.")
  }

  transitions <- df |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      next_line_number = dplyr::lead(.data$line_number),
      next_regimen_name = dplyr::lead(.data$regimen_name),
      next_cohort_start_date = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      exposure_days = as.numeric(.data$cohort_end_date - .data$cohort_start_date) + 1,
      ttnl_days = as.numeric(.data$next_cohort_start_date - .data$cohort_start_date),
      gap_days = as.numeric(.data$next_cohort_start_date - .data$cohort_end_date)
    )

  transitions |>
    dplyr::filter(!is.na(.data$next_regimen_name)) |>
    dplyr::group_by(.data$line_number, .data$regimen_name, .data$next_regimen_name) |>
    dplyr::summarise(
      n_patients = dplyr::n(),
      median_exposure_days = stats::median(.data$exposure_days, na.rm = TRUE),
      median_ttnl_days = stats::median(.data$ttnl_days, na.rm = TRUE),
      q25_ttnl_days = stats::quantile(.data$ttnl_days, 0.25, na.rm = TRUE),
      q75_ttnl_days = stats::quantile(.data$ttnl_days, 0.75, na.rm = TRUE),
      median_ttnl_months = round(stats::median(.data$ttnl_days, na.rm = TRUE) / 30.4375, 1),
      q25_ttnl_months = round(stats::quantile(.data$ttnl_days, 0.25, na.rm = TRUE) / 30.4375, 1),
      q75_ttnl_months = round(stats::quantile(.data$ttnl_days, 0.75, na.rm = TRUE) / 30.4375, 1),
      median_gap_days = stats::median(.data$gap_days, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Generate a Plotly Sankey diagram for Line of Therapy transitions
#'
#' @param cohort A LOT cohort table generated by `generateTherapyLineCohortSet()`.
#' @param coverageThreshold Numeric between 0 and 1 (default: 0.80). Cumulative percentage
#'   of patient volume per line step to explicitly represent before grouping into 'Other Regimens'.
#' @param maxLines Integer (default: 4). Maximum number of therapy line steps to display.
#' @param minLinePatients Minimum total patients in a therapy line step required to
#'   display the step column (default: 1).
#' @param includeEndOfFollowUp Logical (default: TRUE). Whether to include terminal
#'   nodes for patients who drop out or end follow-up after Line L.
#'
#' @return A plotly sankey object.
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
    dplyr::distinct(.data$subject_id, .data$line_number, .keep_all = TRUE) |>
    dplyr::select("subject_id", "line_number", "regimen_name", "cohort_start_date", "cohort_end_date") |>
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
  min_line_in_data <- min(df$line_number)

  transitions <- df |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::mutate(
      next_line_number = dplyr::lead(.data$line_number),
      next_mapped_regimen = dplyr::lead(.data$mapped_regimen),
      next_start_date = dplyr::lead(.data$cohort_start_date)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ttnl_days = as.numeric(.data$next_start_date - .data$cohort_start_date)
    )

  active_links <- transitions |>
    dplyr::filter(!is.na(.data$next_mapped_regimen)) |>
    dplyr::mutate(
      source_label = paste0("Line ", .data$line_number, ": ", .data$mapped_regimen),
      target_label = paste0("Line ", .data$next_line_number, ": ", .data$next_mapped_regimen)
    ) |>
    dplyr::filter(.data$source_label != .data$target_label) |>
    dplyr::group_by(.data$source_label, .data$target_label) |>
    dplyr::summarise(
      value = dplyr::n(),
      med_ttnl_days = stats::median(.data$ttnl_days, na.rm = TRUE),
      med_ttnl_mos = round(stats::median(.data$ttnl_days, na.rm = TRUE) / 30.4375, 1),
      q25_ttnl_mos = round(stats::quantile(.data$ttnl_days, 0.25, na.rm = TRUE) / 30.4375, 1),
      q75_ttnl_mos = round(stats::quantile(.data$ttnl_days, 0.75, na.rm = TRUE) / 30.4375, 1),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      hover_info = paste0(
        "<b>Transition:</b> ", .data$source_label, " &#8594; ", .data$target_label, "<br>",
        "<b>Patients:</b> N = ", .data$value, "<br>",
        "<b>Median TTNL:</b> ", .data$med_ttnl_mos, " mos (", round(.data$med_ttnl_days), " days)<br>",
        "<b>IQR:</b> ", .data$q25_ttnl_mos, " - ", .data$q75_ttnl_mos, " mos"
      )
    )

  if (includeEndOfFollowUp) {
    terminal_links <- transitions |>
      dplyr::filter(is.na(.data$next_mapped_regimen) & .data$line_number < max_line_in_data) |>
      dplyr::mutate(
        source_label = paste0("Line ", .data$line_number, ": ", .data$mapped_regimen),
        target_label = paste0("Line ", .data$line_number, ": End of Follow-Up")
      ) |>
      dplyr::group_by(.data$source_label, .data$target_label) |>
      dplyr::summarise(
        value = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        hover_info = paste0(
          "<b>Terminal Stream:</b> ", .data$source_label, " &#8594; ", .data$target_label, "<br>",
          "<b>Patients:</b> N = ", .data$value
        )
      )

    eof_lines <- sort(unique(transitions$line_number[is.na(transitions$next_mapped_regimen) & transitions$line_number < max_line_in_data]))
    dummy_links <- dplyr::tibble()
    if (length(eof_lines) > 1) {
      dummy_links <- dplyr::tibble(
        source_label = paste0("Line ", eof_lines[-length(eof_lines)], ": End of Follow-Up"),
        target_label = paste0("Line ", eof_lines[-1], ": End of Follow-Up"),
        value = 0.000001,
        hover_info = ""
      )
    }

    all_links <- dplyr::bind_rows(active_links, terminal_links, dummy_links)
  } else {
    all_links <- active_links
  }

  if (nrow(all_links) == 0) {
    cli::cli_abort("No transitions found between lines in LOT cohort.")
  }

  # Calculate volume for each label to sort nodes from largest to smallest volume
  src_vols <- all_links |>
    dplyr::group_by(label = .data$source_label) |>
    dplyr::summarise(vol_src = sum(.data$value), .groups = "drop")

  tgt_vols <- all_links |>
    dplyr::group_by(label = .data$target_label) |>
    dplyr::summarise(vol_tgt = sum(.data$value), .groups = "drop")

  node_vols <- dplyr::full_join(src_vols, tgt_vols, by = "label") |>
    dplyr::mutate(
      vol_src = dplyr::coalesce(.data$vol_src, 0),
      vol_tgt = dplyr::coalesce(.data$vol_tgt, 0),
      vol = pmax(.data$vol_src, .data$vol_tgt)
    )

  unique_labels <- unique(c(all_links$source_label, all_links$target_label))

  node_info <- dplyr::tibble(label = unique_labels) |>
    dplyr::left_join(node_vols, by = "label") |>
    dplyr::mutate(
      vol = dplyr::coalesce(.data$vol, 0),
      orig_line = as.numeric(sub("^Line ([0-9]+):.*", "\\1", .data$label)),
      is_terminal = grepl("End of Follow-Up", .data$label, fixed = TRUE),
      is_other = grepl("Other Regimens", .data$label, fixed = TRUE),
      col_step = dplyr::if_else(.data$is_terminal, .data$orig_line + 1, .data$orig_line)
    ) |>
    dplyr::arrange(
      .data$col_step,
      .data$is_terminal,     # Terminal nodes at bottom
      .data$is_other,        # 'Other Regimens' near bottom
      dplyr::desc(.data$vol) # Largest volume at top
    )

  node_labels <- node_info$label
  node_dict <- setNames(seq_along(node_labels) - 1L, node_labels)

  # Column x coordinates for active regimens and terminal target nodes
  if (max_line_in_data > min_line_in_data) {
    node_x <- (node_info$col_step - min_line_in_data) / (max_line_in_data - min_line_in_data)
    node_x <- pmax(0.001, pmin(0.999, node_x))
  } else {
    node_x <- rep(0.5, length(node_labels))
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
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = all_links$source,
      target = all_links$target,
      value = all_links$value,
      color = "rgba(200, 200, 200, 0.4)",
      customdata = all_links$hover_info,
      hovertemplate = "%{customdata}<extra></extra>"
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
