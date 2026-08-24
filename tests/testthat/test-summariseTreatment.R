test_that("test summariseTreatment", {
  cdm <- mockDrugUtilisation() |>
    copyCdm()

  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 30), c(31, 365))
      )
  )
  expect_true(inherits(x, "summarised_result"))
  expect_identical(
    unique(x$variable_level),
    c("cohort_1", "cohort_2", "cohort_3", "untreated")
  )
  expect_true(all(x$additional_level |> unique() == c("0 to 30", "31 to 365")))

  # test concept works
  cdm <- generateDrugUtilisationCohortSet(
    cdm = cdm,
    conceptSet = list("a" = 1503327, "c" = 43135274, "b" = 2905077),
    name = "dus_cohort"
  )
  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "dus_cohort",
        window = list(c(0, Inf))
      )
  )
  expect_true(inherits(x, "summarised_result"))
  expect_true(all(
    x |> dplyr::filter(group_level == "cohort_1") |> dplyr::pull("variable_level") ==
      c("a", "a", "b", "b", "c", "c", "untreated", "untreated")
  ))
  expect_true(all(x$additional_level |> unique() == c("0 to inf")))

  # test order in cohort works
  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        treatmentCohortId = c(3, 2),
        window = list(c(0, 30), c(31, 365))
      )
  )
  expect_true(inherits(x, "summarised_result"))
  expect_identical(
    unique(x$variable_level),
    c("cohort_2", "cohort_3", "untreated")
  )
  expect_true(all(x$additional_level |> unique() == c("0 to 30", "31 to 365")))

  # test suppress
  x_sup <- omopgenerics::suppress(x, minCellCount = 100)
  expect_true(all(
    x_sup |>
      dplyr::filter(estimate_value != "0") |>
      dplyr::pull("estimate_value") == "-"
  ))

  dropCreatedTables(cdm = cdm)
})

test_that("summariseTreatment reports empty selected cohorts", {
  target <- dplyr::tibble(
    cohort_definition_id = integer(),
    subject_id = integer(),
    cohort_start_date = as.Date(character()),
    cohort_end_date = as.Date(character())
  )
  attr(target, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1:2,
    cohort_name = c("target1", "target2")
  )
  treatment <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-01")
  )
  attr(treatment, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1L,
    cohort_name = "treat1"
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2019-01-01"),
    observation_period_end_date = as.Date("2021-01-01"),
    period_type_concept_id = 0L
  )
  cdm <- mockDrugUtilisation(
    cohort1 = target,
    cohort2 = treatment,
    observation_period = observation_period
  ) |>
    copyCdm()

  result <- cdm$cohort1 |>
    summariseTreatment(treatmentCohortName = "cohort2", window = list(c(0, 0)))

  counts <- result |>
    dplyr::filter(.data$estimate_name == "count") |>
    dplyr::arrange(.data$group_level, .data$variable_level) |>
    dplyr::select(
      "group_name", "group_level", "strata_name", "strata_level",
      "variable_level", "estimate_value"
    )

  expect_identical(unique(counts$group_name), "cohort_name")
  expect_identical(unique(counts$group_level), c("target1", "target2"))
  expect_identical(unique(counts$strata_name), "overall")
  expect_identical(unique(counts$strata_level), "overall")
  expect_identical(unique(counts$variable_level), c("treat1", "untreated"))
  expect_identical(counts$estimate_value, rep("0", 4))

  dropCreatedTables(cdm = cdm)
})

test_that("test addTreatment", {
  cdm <- mockDrugUtilisation() |>
    copyCdm()

  expect_no_error(
    x <- cdm$cohort1 |>
      addTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 30), c(31, 365)),
        mutuallyExclusive = FALSE
      )
  )

  dropCreatedTables(cdm = cdm)
})

test_that("test inObservation argument", {
  expect_identical(formals(summariseTreatment)$inObservation, TRUE)
  expect_identical(formals(summariseIndication)$inObservation, TRUE)
  expect_identical(formals(summariseTreatment)$restrictIncident, FALSE)
  expect_identical(formals(summariseIndication)$restrictIncident, FALSE)

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = dplyr::tibble(
        person_id = 1:2L,
        gender_concept_id = 0L,
        year_of_birth = 1990L,
        race_concept_id = 0L,
        ethnicity_concept_id = 0L
      ),
      observation_period = dplyr::tibble(
        observation_period_id = 1:2L,
        person_id = 1:2L,
        observation_period_start_date = as.Date("2020-01-01"),
        observation_period_end_date = as.Date("2020-01-01") + c(0L, 100L),
        period_type_concept_id = 0L
      )
    ),
    cdmName = "test",
    cohortTables = list(
      cohort1 = dplyr::tibble(
        cohort_definition_id = 1L,
        subject_id = c(1L, 2L),
        cohort_start_date = as.Date("2020-01-01"),
        cohort_end_date = as.Date("2020-01-01")
      ),
      cohort2 = dplyr::tibble(
        cohort_definition_id = c(1L, 1L, 2L),
        subject_id = c(1L, 2L, 2L),
        cohort_start_date = as.Date("2020-01-01") + c(0L, 10L, 20L),
        cohort_end_date = cohort_start_date
      )
    )
  ) |>
    copyCdm()

  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 0), c(5, 365)),
        mutuallyExclusive = FALSE,
        inObservation = FALSE
      ) |>
      omopgenerics::tidy()
  )
  expect_identical(unique(x$in_observation), "FALSE")
  expect_identical(unique(x$mutually_exclusive), "FALSE")
  expect_identical(x$count, c(1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L))
  expect_identical(x$percentage, c(50, 0, 50, 0, 50, 50, 0, 50))

  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 0), c(1, 365)),
        mutuallyExclusive = FALSE,
        inObservation = TRUE
      ) |>
      omopgenerics::tidy()
  )
  expect_identical(unique(x$in_observation), "TRUE")
  expect_identical(unique(x$mutually_exclusive), "FALSE")
  expect_identical(x$count, c(1L, 0L, 1L, 1L, 1L, 0L))
  expect_identical(x$percentage, c(50, 0, 50, 100, 100, 0))

  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 0), c(1, 365)),
        mutuallyExclusive = TRUE,
        inObservation = FALSE
      ) |>
      omopgenerics::tidy()
  )
  expect_identical(unique(x$in_observation), "FALSE")
  expect_identical(unique(x$mutually_exclusive), "TRUE")
  expect_identical(x$count, c(1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L))
  expect_identical(x$percentage, c(50, 0, 0, 50, 0, 0, 0, 50, 0, 50))

  expect_no_error(
    x <- cdm$cohort1 |>
      summariseTreatment(
        treatmentCohortName = "cohort2",
        window = list(c(0, 0), c(1, 365)),
        mutuallyExclusive = TRUE,
        inObservation = TRUE
      ) |>
      omopgenerics::tidy()
  )
  expect_identical(unique(x$in_observation), "TRUE")
  expect_identical(unique(x$mutually_exclusive), "TRUE")
  expect_identical(x$count, c(1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L))
  expect_identical(x$percentage, c(50, 0, 0, 50, 0, 0, 100, 0))

  dropCreatedTables(cdm = cdm)
})

test_that("restrictIncident only includes target cohorts starting during follow-up", {
  target <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:2,
    cohort_start_date = as.Date(c("2020-01-10", "2020-01-10")),
    cohort_end_date = as.Date(c("2020-01-31", "2020-01-31"))
  )
  treatment <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L, 2L, 2L),
    cohort_start_date = as.Date(c("2020-01-01", "2020-01-16", "2020-02-01")),
    cohort_end_date = as.Date(c("2020-01-15", "2020-01-20", "2020-02-05"))
  )
  cdm <- mockDrugUtilisation(
    cohort1 = target,
    cohort2 = treatment,
    observation_period = dplyr::tibble(
      observation_period_id = 1L,
      person_id = 1:2,
      observation_period_start_date = as.Date("2019-01-01"),
      observation_period_end_date = as.Date("2021-01-01"),
      period_type_concept_id = 0L
    )
  ) |>
    copyCdm()

  prevalent <- cdm$cohort1 |>
    summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30)),
      censorDate = "cohort_end_date",
      mutuallyExclusive = FALSE,
      restrictIncident = FALSE
    ) |>
    omopgenerics::tidy()
  incident <- cdm$cohort1 |>
    summariseTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30)),
      censorDate = "cohort_end_date",
      mutuallyExclusive = FALSE,
      restrictIncident = TRUE
    ) |>
    omopgenerics::tidy()

  expect_identical(
    prevalent |>
      dplyr::filter(.data$variable_level == "cohort_1") |>
      dplyr::pull("count"),
    2L
  )
  expect_identical(
    incident |>
      dplyr::filter(.data$variable_level == "cohort_1") |>
      dplyr::pull("count"),
    1L
  )
  expect_identical(unique(prevalent$restrict_incident), "FALSE")
  expect_identical(unique(incident$restrict_incident), "TRUE")

  indication <- cdm$cohort1 |>
    summariseIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(c(0, 30)),
      censorDate = "cohort_end_date",
      mutuallyExclusive = FALSE,
      restrictIncident = TRUE
    ) |>
    omopgenerics::tidy()
  expect_identical(
    indication |>
      dplyr::filter(.data$variable_level == "cohort_1") |>
      dplyr::pull("count"),
    1L
  )
  expect_identical(unique(indication$restrict_incident), "TRUE")

  addedTreatment <- cdm$cohort1 |>
    addTreatment(
      treatmentCohortName = "cohort2",
      window = list(c(0, 30)),
      censorDate = "cohort_end_date",
      restrictIncident = TRUE
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$subject_id)
  expect_identical(
    addedTreatment$medication_0_to_30,
    c("untreated", "cohort_1")
  )

  addedIndication <- cdm$cohort1 |>
    addIndication(
      indicationCohortName = "cohort2",
      indicationWindow = list(c(0, 30)),
      censorDate = "cohort_end_date",
      restrictIncident = TRUE
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$subject_id)
  expect_identical(addedIndication$indication_0_to_30, c("none", "cohort_1"))

  dropCreatedTables(cdm = cdm)
})
