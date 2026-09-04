test_that("generateEpisodeCohortSet input checks and functionality", {
  skip_on_cran()
  cdm <- mockDrugUtilisation() |> copyCdm()

  # Get observation periods for persons in mock data
  obs <- cdm$observation_period |>
    dplyr::filter(.data$person_id %in% c(1, 2, 3)) |>
    dplyr::collect()

  if (nrow(obs) > 0) {
    p1 <- obs |> dplyr::filter(.data$person_id == 1) |> dplyr::pull(.data$observation_period_start_date) |> min()
    p2 <- obs |> dplyr::filter(.data$person_id == 2) |> dplyr::pull(.data$observation_period_start_date) |> min()
    p3 <- obs |> dplyr::filter(.data$person_id == 3) |> dplyr::pull(.data$observation_period_start_date) |> min()

    episodes <- dplyr::tibble(
      episode_id = 1:3,
      person_id = c(1, 2, 3),
      episode_concept_id = c(32531, 32531, 100),
      episode_object_concept_id = c(1125360, 1503297, 1125360),
      episode_start_date = c(p1 + 5, p2 + 5, p3 + 5),
      episode_end_date = c(p1 + 20, p2 + 20, p3 + 20)
    )

    cdm <- omopgenerics::insertTable(cdm = cdm, name = "episode", table = episodes)

    cdm <- generateEpisodeCohortSet(
      cdm = cdm,
      name = "ep_cohort",
      episodeConceptId = 32531
    )

    expect_true("cohort_table" %in% class(cdm$ep_cohort))
    expect_equal(dplyr::tally(cdm$ep_cohort) |> dplyr::pull(), 2)

    settings_df <- omopgenerics::settings(cdm$ep_cohort)
    expect_true("cohort_name" %in% colnames(settings_df))
  }

  dropCreatedTables(cdm = cdm)
})
