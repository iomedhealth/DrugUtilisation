# Generate Line of Therapy (LOT) Subcohorts

Generates sequential Line of Therapy (LOT) subcohorts for patients in a
base cohort based on treatment exposure records. Handles multi-drug
regimen identification, landmark gap windows, and maintenance
modalities.

## Usage

``` r
generateTherapyLineCohortSet(
  cdm,
  name,
  cohort,
  cohortId = NULL,
  treatmentCohortName,
  treatmentCohortId = NULL,
  regimenRules = NULL,
  gapEra = 60,
  maintenanceModalities = character()
)
```

## Arguments

- cdm:

  A CDM reference object.

- name:

  Name of the output cohort table created in the CDM.

- cohort:

  Name of the base patient cohort table.

- cohortId:

  Cohort definition ID(s) in `cohort` to include. If NULL, uses all.

- treatmentCohortName:

  Name of the cohort table or concept set containing drug exposures.

- treatmentCohortId:

  Cohort definition ID(s) in `treatmentCohortName`. If NULL, uses all.

- regimenRules:

  List or dataframe defining multi-drug combination rules and hierarchy
  ranking.

- gapEra:

  Number of days for the landmark window and line transition gap
  (default: 60).

- maintenanceModalities:

  Character vector of regimens or procedure labels that do not advance
  line number.

## Value

A cohort table in `cdm` with subcohorts for each line order and regimen
combination.

## Examples

``` r
# \donttest{
library(DrugUtilisation)

cdm <- mockDrugUtilisation()

cdm <- generateTherapyLineCohortSet(
  cdm = cdm,
  name = "lot_cohorts",
  cohort = "cohort1",
  treatmentCohortName = "cohort2",
  gapEra = 60
)
# }
```
