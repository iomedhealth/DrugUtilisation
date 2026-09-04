# Generate Line of Therapy (LOT) Subcohorts

Generates sequential Line of Therapy (LOT) subcohorts for patients in a
base patient cohort based on treatment exposure records. Handles
multi-drug regimen identification, landmark gap windows, and maintenance
modalities according to OHDSI / DARWIN-EU standards.

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

  A CDM reference object created with `CDMConnector`.

- name:

  Name of the output cohort table created in the CDM.

- cohort:

  Name of the base patient cohort table in `cdm`.

- cohortId:

  Cohort definition ID(s) in `cohort` to restrict to. If `NULL`, all
  cohorts in `cohort` are included.

- treatmentCohortName:

  Name of the cohort table in `cdm` containing treatment exposures.

- treatmentCohortId:

  Cohort definition ID(s) in `treatmentCohortName` to restrict to. If
  `NULL`, all cohorts are included.

- regimenRules:

  List or data frame defining multi-drug combination rules. If a named
  list, names represent regimen labels and values are character vectors
  of drug/concept names (e.g.
  `list("Dara-VRd" = c("daratumumab", "bortezomib", "lenalidomide", "dexamethasone"))`).

- gapEra:

  Number of days for the landmark window and line transition gap
  (default: `60`).

- maintenanceModalities:

  Character vector of drug or procedure labels that do not advance line
  number (e.g. `c("ASCT (Transplant)", "lenalidomide")`).

## Value

A `cohort_table` object created in `cdm` with subcohorts for each line
number and regimen combination. Metadata attributes stored in
`omopgenerics::settings(cdm[[name]])` include `cohort_definition_id`,
`cohort_name`, `line_number`, and `regimen_name`.

## Details

The algorithm identifies sequential therapy lines for each patient in
`cohort`:

- **Follow-Up Intersect**: Only treatment records starting within
  patient cohort follow-up are considered.

- **Line Start & Landmark Window**: The first exposure starts Line 1.
  Exposures starting within `gapEra` days from line start form the
  combination regimen for that line.

- **Maintenance Modalities**: Exposures matching `maintenanceModalities`
  or starting within `gapEra` days from the preceding exposure end date
  are subsumed into the current line without advancing line index.

- **Line Transition**: An exposure starting after the gap window
  triggers transition to the next line.

- **Regimen Inference**: Multi-drug combinations in each line are
  matched against `regimenRules` or concatenated alphabetically into
  regimen labels (e.g. `Dara-VRd`, `Isa-Kd`, `VRd`).

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
