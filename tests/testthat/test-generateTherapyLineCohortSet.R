test_that("generateTherapyLineCohortSet input checks", {
  skip_on_cran()
  cdm <- mockDrugUtilisation() |> copyCdm()

  expect_error(generateTherapyLineCohortSet())
  expect_error(generateTherapyLineCohortSet(cdm = cdm))
  expect_error(generateTherapyLineCohortSet(cdm, name = "lot", cohort = "non_existent", treatmentCohortName = "cohort2"))
  expect_error(generateTherapyLineCohortSet(cdm, name = "lot", cohort = "cohort1", treatmentCohortName = "non_existent"))
  expect_error(generateTherapyLineCohortSet(cdm, name = "lot", cohort = "cohort1", treatmentCohortName = "cohort2", gapEra = -10))

  dropCreatedTables(cdm = cdm)
})

test_that("generateTherapyLineCohortSet empty cohort handling", {
  skip_on_cran()
  cdm <- mockDrugUtilisation() |> copyCdm()

  # Create an empty treatment cohort
  cdm <- omopgenerics::emptyCohortTable(cdm, name = "empty_treat")

  cdm <- generateTherapyLineCohortSet(
    cdm = cdm,
    name = "lot_empty",
    cohort = "cohort1",
    treatmentCohortName = "empty_treat"
  )

  expect_true("cohort_table" %in% class(cdm$lot_empty))
  expect_equal(dplyr::tally(cdm$lot_empty) |> dplyr::pull(), 0)

  dropCreatedTables(cdm = cdm)
})

test_that("generateTherapyLineCohortSet Multiple Myeloma LOT & Regimen classification", {
  skip_on_cran()

  cdm <- mockDrugUtilisation() |> copyCdm()

  obs_dates <- cdm$observation_period |>
    dplyr::filter(.data$person_id %in% c(3, 5)) |>
    dplyr::group_by(.data$person_id) |>
    dplyr::summarise(
      start = min(.data$observation_period_start_date),
      end = max(.data$observation_period_end_date),
      .groups = "drop"
    ) |>
    dplyr::collect()

  obs_p1 <- obs_dates |> dplyr::filter(.data$person_id == 3)
  obs_p2 <- obs_dates |> dplyr::filter(.data$person_id == 5)

  treatments <- dplyr::tibble(
    cohort_definition_id = c(1, 2, 3, 4, 5, 6, 4, 1, 5, 6),
    subject_id = c(3, 3, 3, 3, 3, 3, 3, 5, 5, 5),
    cohort_start_date = c(
      obs_p1$start + 10, obs_p1$start + 20, obs_p1$start + 30, obs_p1$start + 30,
      obs_p1$start + 120, obs_p1$start + 130, obs_p1$start + 130,
      obs_p2$start + 10, obs_p2$start + 120, obs_p2$start + 130
    ),
    cohort_end_date = c(
      obs_p1$start + 50, obs_p1$start + 50, obs_p1$start + 50, obs_p1$start + 50,
      obs_p1$start + 150, obs_p1$start + 150, obs_p1$start + 150,
      obs_p2$start + 50, obs_p2$start + 150, obs_p2$start + 150
    )
  )

  treatSet <- dplyr::tibble(
    cohort_definition_id = 1:6,
    cohort_name = c("daratumumab", "bortezomib", "lenalidomide", "dexamethasone", "isatuximab", "carfilzomib")
  )

  baseCohort <- dplyr::tibble(
    cohort_definition_id = 1,
    subject_id = c(3, 5),
    cohort_start_date = c(obs_p1$start + 5, obs_p2$start + 5),
    cohort_end_date = c(obs_p1$end - 5, obs_p2$end - 5)
  )

  cdm <- omopgenerics::insertTable(cdm, name = "base_mm", table = baseCohort)
  cdm$base_mm <- omopgenerics::newCohortTable(cdm$base_mm, .softValidation = TRUE)

  cdm <- omopgenerics::insertTable(cdm, name = "mm_treatments", table = treatments)
  cdm$mm_treatments <- omopgenerics::newCohortTable(cdm$mm_treatments, cohortSetRef = treatSet, .softValidation = TRUE)

  mmRules <- list(
    "Dara-VRd" = c("daratumumab", "bortezomib", "lenalidomide", "dexamethasone"),
    "Isa-Kd"   = c("isatuximab", "carfilzomib", "dexamethasone"),
    "Dara"     = c("daratumumab")
  )

  cdm <- generateTherapyLineCohortSet(
    cdm = cdm,
    name = "lot_mm",
    cohort = "base_mm",
    treatmentCohortName = "mm_treatments",
    regimenRules = mmRules,
    gapEra = 60
  )

  lotData <- cdm$lot_mm |> dplyr::collect()
  lotSet <- omopgenerics::settings(cdm$lot_mm)

  expect_equal(nrow(lotData), 4)
  expect_true(all(c("line_number", "regimen_name") %in% colnames(lotSet)))

  sankey <- plotTherapyLineSankey(cdm$lot_mm)
  expect_s3_class(sankey, "plotly")

  dur <- summariseTherapyLineDuration(cdm$lot_mm)
  expect_true(all(c("n_patients", "median_ttnl_days", "median_ttnl_months") %in% colnames(dur)))

  dropCreatedTables(cdm = cdm)
})
