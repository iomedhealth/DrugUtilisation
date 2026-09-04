# Package index

### Generate a set of drug cohorts

Generate a set of drug cohorts using given concepts or vocabulary
hierarchies

- [`generateDrugUtilisationCohortSet()`](https://iomedhealth.github.io/DrugUtilisation/reference/generateDrugUtilisationCohortSet.md)
  : Generate a set of drug cohorts based on given concepts
- [`generateIngredientCohortSet()`](https://iomedhealth.github.io/DrugUtilisation/reference/generateIngredientCohortSet.md)
  : Generate a set of drug cohorts based on drug ingredients
- [`generateAtcCohortSet()`](https://iomedhealth.github.io/DrugUtilisation/reference/generateAtcCohortSet.md)
  : Generate a set of drug cohorts based on ATC classification
- [`erafyCohort()`](https://iomedhealth.github.io/DrugUtilisation/reference/erafyCohort.md)
  : Erafy a cohort_table collapsing records separated gapEra days or
  less.
- [`cohortGapEra()`](https://iomedhealth.github.io/DrugUtilisation/reference/cohortGapEra.md)
  : Get the gapEra used to create a cohort

### Generate and visualize Line of Therapy (LOT) cohorts

Generate sequential Line of Therapy (LOT) subcohorts and visualize
treatment transitions with Sankey diagrams

- [`generateTherapyLineCohortSet()`](https://iomedhealth.github.io/DrugUtilisation/reference/generateTherapyLineCohortSet.md)
  : Generate Line of Therapy (LOT) Subcohorts
- [`plotTherapyLineSankey()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotTherapyLineSankey.md)
  : Generate a Plotly Sankey diagram for Line of Therapy transitions
- [`summariseTherapyLineDuration()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseTherapyLineDuration.md)
  : Summarise Therapy Line Duration & Time to Next Line (TTNL)

### Apply inclusion criteria to drug cohorts

Apply inclusion criteria that filter drug cohort entries based on
specified rules.

- [`requireDrugInDateRange()`](https://iomedhealth.github.io/DrugUtilisation/reference/requireDrugInDateRange.md)
  : Restrict cohort to only cohort records within a certain date range
- [`requireIsFirstDrugEntry()`](https://iomedhealth.github.io/DrugUtilisation/reference/requireIsFirstDrugEntry.md)
  : Restrict cohort to only the first cohort record per subject
- [`requireObservationBeforeDrug()`](https://iomedhealth.github.io/DrugUtilisation/reference/requireObservationBeforeDrug.md)
  : Restrict cohort to only cohort records with the given amount of
  prior observation time in the database
- [`requirePriorDrugWashout()`](https://iomedhealth.github.io/DrugUtilisation/reference/requirePriorDrugWashout.md)
  : Restrict cohort to only cohort records with a given amount of time
  since the last cohort record ended

### Identify and summarise indications for patients in drug cohorts

Indications identified based on their presence in indication cohorts or
OMOP CDM clinical tabes.

- [`addIndication()`](https://iomedhealth.github.io/DrugUtilisation/reference/addIndication.md)
  : Add a variable indicating individuals indications
- [`summariseIndication()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseIndication.md)
  : Summarise the indications of individuals in a drug cohort
- [`tableIndication()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableIndication.md)
  : Create a table showing indication results
- [`plotIndication()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotIndication.md)
  : Generate a plot visualisation (ggplot2) from the output of
  summariseIndication

### Drug use functions

Drug use functions are used to summarise and obtain the drug use
information

- [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  : Add new columns with drug use related information

- [`summariseDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseDrugUtilisation.md)
  : This function is used to summarise the dose utilisation table over
  multiple cohorts.

- [`tableDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableDrugUtilisation.md)
  : Format a drug_utilisation object into a visual table.

- [`plotDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotDrugUtilisation.md)
  :

  Plot the results of `summariseDrugUtilisation`

### Drug use individual functions

Drug use functions can be used to add a single estimates

- [`addNumberExposures()`](https://iomedhealth.github.io/DrugUtilisation/reference/addNumberExposures.md)
  :

  To add a new column with the number of exposures. To add multiple
  columns use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addNumberEras()`](https://iomedhealth.github.io/DrugUtilisation/reference/addNumberEras.md)
  :

  To add a new column with the number of eras. To add multiple columns
  use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addTimeToExposure()`](https://iomedhealth.github.io/DrugUtilisation/reference/addTimeToExposure.md)
  :

  To add a new column with the time to exposure. To add multiple columns
  use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addDaysExposed()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDaysExposed.md)
  :

  To add a new column with the days exposed. To add multiple columns use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addDaysPrescribed()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDaysPrescribed.md)
  :

  To add a new column with the days prescribed. To add multiple columns
  use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addInitialExposureDuration()`](https://iomedhealth.github.io/DrugUtilisation/reference/addInitialExposureDuration.md)
  :

  To add a new column with the duration of the first exposure. To add
  multiple columns use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addInitialQuantity()`](https://iomedhealth.github.io/DrugUtilisation/reference/addInitialQuantity.md)
  :

  To add a new column with the initial quantity. To add multiple columns
  use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addCumulativeQuantity()`](https://iomedhealth.github.io/DrugUtilisation/reference/addCumulativeQuantity.md)
  :

  To add a new column with the cumulative quantity. To add multiple
  columns use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addInitialDailyDose()`](https://iomedhealth.github.io/DrugUtilisation/reference/addInitialDailyDose.md)
  :

  To add a new column with the initial daily dose. To add multiple
  columns use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

- [`addCumulativeDose()`](https://iomedhealth.github.io/DrugUtilisation/reference/addCumulativeDose.md)
  :

  To add a new column with the cumulative dose. To add multiple columns
  use
  [`addDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugUtilisation.md)
  for efficiency.

### Summarise treatment persistence using proportion of patients covered (PPC)

Summarise the proportion of patients in the drug cohort over time.

- [`summariseProportionOfPatientsCovered()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseProportionOfPatientsCovered.md)
  : Summarise proportion Of patients covered
- [`tableProportionOfPatientsCovered()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableProportionOfPatientsCovered.md)
  : Create a table with proportion of patients covered results
- [`plotProportionOfPatientsCovered()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotProportionOfPatientsCovered.md)
  : Plot proportion of patients covered

### Summarise treatment persistence using survival analysis

Summarise the percistence of a drug cohort over time.

- [`summariseDiscontinuationAsSurvival()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseDiscontinuationAsSurvival.md)
  : Summarise discontinuation as a survival analysis
- [`tableDiscontinuationAsSurvival()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableDiscontinuationAsSurvival.md)
  : Create a table with discontinuation as survival results
- [`plotDiscontinuationAsSurvival()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotDiscontinuationAsSurvival.md)
  : Plot discontinuation

### Summarise treatments during certain windows

Summarise the use of different treatments during certain windows

- [`addTreatment()`](https://iomedhealth.github.io/DrugUtilisation/reference/addTreatment.md)
  : Add a variable indicating individuals medications
- [`summariseTreatment()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseTreatment.md)
  : This function is used to summarise treatments received
- [`tableTreatment()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableTreatment.md)
  : Format a summarised_treatment result into a visual table.
- [`plotTreatment()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotTreatment.md)
  : Generate a custom ggplot2 from a summarised_result object generated
  with summariseTreatment function.

### Summarise treatment restart or switch during certain time

Summarise the restart of a treatment, or switch to another, during
certain time

- [`addDrugRestart()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDrugRestart.md)
  : Add drug restart information as a column per follow-up period of
  interest.
- [`summariseDrugRestart()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseDrugRestart.md)
  : Summarise the drug restart for each follow-up period of interest.
- [`tableDrugRestart()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableDrugRestart.md)
  : Format a drug_restart object into a visual table.
- [`plotDrugRestart()`](https://iomedhealth.github.io/DrugUtilisation/reference/plotDrugRestart.md)
  : Generate a custom ggplot2 from a summarised_result object generated
  with summariseDrugRestart() function.

### Daily dose documentation

Functions to assess coverage for the diferent ingredients and document
how daily dose is calculated

- [`patternsWithFormula`](https://iomedhealth.github.io/DrugUtilisation/reference/patternsWithFormula.md)
  : Patterns valid to compute daily dose with the associated formula.

- [`patternTable()`](https://iomedhealth.github.io/DrugUtilisation/reference/patternTable.md)
  : Function to create a tibble with the patterns from current drug
  strength table

- [`addDailyDose()`](https://iomedhealth.github.io/DrugUtilisation/reference/addDailyDose.md)
  :

  Add daily dose to `drug_exposure` like table

- [`summariseDoseCoverage()`](https://iomedhealth.github.io/DrugUtilisation/reference/summariseDoseCoverage.md)
  : Check coverage of daily dose computation in a sample of the cdm for
  selected concept sets and ingredient

- [`tableDoseCoverage()`](https://iomedhealth.github.io/DrugUtilisation/reference/tableDoseCoverage.md)
  : Format a dose_coverage object into a visual table.

### Complementary functions

Complementary functions

- [`benchmarkDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/benchmarkDrugUtilisation.md)
  : Run benchmark of drug utilisation cohort generation
- [`mockDrugUtilisation()`](https://iomedhealth.github.io/DrugUtilisation/reference/mockDrugUtilisation.md)
  : It creates a mock database for testing DrugUtilisation package
