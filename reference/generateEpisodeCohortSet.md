# Generate a cohort set from the CDM episode table

Creates a new cohort table in the CDM reference based on records in the
`episode` table, filtered by `episode_concept_id` and an optional
`conceptSet` matching `episode_object_concept_id`.

## Usage

``` r
generateEpisodeCohortSet(
  cdm,
  name,
  conceptSet = NULL,
  episodeConceptId = 32531,
  subsetCohort = NULL,
  subsetCohortId = NULL
)
```

## Arguments

- cdm:

  A `cdm_reference` object.

- name:

  Name of the new cohort table, it must be a length 1 character vector.

- conceptSet:

  Optional concept set list or numeric vector matching
  `episode_object_concept_id`. If NULL, all unique
  `episode_object_concept_id` values found in the episode table will be
  included.

- episodeConceptId:

  Numeric vector of concept IDs matching `episode_concept_id` (default:
  32531 for Treatment Regimen).

- subsetCohort:

  Cohort table to subset.

- subsetCohortId:

  Cohort definition IDs to use from `subsetCohort`.

## Value

A CDM reference object with the new episode cohort table added.

## Examples

``` r
# \donttest{
library(DrugUtilisation)
episode <- dplyr::tibble(
  episode_id = 1,
  person_id = 1,
  episode_concept_id = 32531,
  episode_object_concept_id = 1125360,
  episode_start_date = as.Date("2020-01-01"),
  episode_end_date = as.Date("2020-01-30")
)
cdm <- mockDrugUtilisation(episode = episode)
#> Warning: episode table not included in cdm because:
#> Error in `newOmopTable()`: ! episode_type_concept_id is not present in table
#> episode

cdm <- generateEpisodeCohortSet(
  cdm = cdm,
  name = "episode_cohort",
  episodeConceptId = 32531
)
#> Error in generateEpisodeCohortSet(cdm = cdm, name = "episode_cohort",     episodeConceptId = 32531): `episode` table not found in cdm.
# }
```
