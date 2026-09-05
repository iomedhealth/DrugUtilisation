# Example Script: Generate GEM-PETHEMA 2025 Guideline Line of Therapy (LOT) Sankey
# Database: test_data/omop_1959_1786811115.duckdb

library(DrugUtilisation)
library(CDMConnector)
library(omopgenerics)
library(OmopHelpers)
library(CohortConstructor)
library(duckdb)
library(htmlwidgets)
library(dplyr)
library(plotly)

# Connect to test_data DuckDB instance
con <- dbConnect(duckdb(), "test_data/omop_1959_1786811115.duckdb")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

cdm <- cdmFromCon(con, cdmSchema = "cdm", writeSchema = "main")

# 1. Base Multiple Myeloma Cohort (Concept Set 7343) using OmopHelpers & CohortConstructor
mmCodelist <- OmopHelpers::getCodelistFromConceptSet(
  conceptSetId = 7343,
  con = con,
  cdmSchema = "cdm"
)

cdm$base_mm <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = mmCodelist,
  name = "base_mm"
)

# 2. Extract Treatment Regimen Codelists (Concept Set IDs 7347-7378, 7483-7487, 11571-11573) using OmopHelpers
regimenIds <- c(7347:7358, 7360:7378, 7483, 7485:7487, 11571:11573)
rawCodelists <- lapply(regimenIds, function(id) {
  OmopHelpers::getCodelistFromConceptSet(conceptSetId = id, con = con, cdmSchema = "cdm")
})

mergedRaw <- unlist(rawCodelists, recursive = FALSE)
processedCodelists <- OmopHelpers::process_codelists(mergedRaw)
regimenCodelist <- omopgenerics::newCodelist(processedCodelists)

# 3. Generate Episode Cohort Set
cdm <- generateEpisodeCohortSet(
  cdm = cdm,
  name = "tx_episodes",
  conceptSet = regimenCodelist,
  episodeConceptId = 32531
)

# 4. Define Regimen Rules (Hierarchy & Resolution) and Generate LOT Cohorts
mmRules <- list(
  "dara_rvd"    = c("dara_rvd", "rvd", "daratumumab"),
  "dara_rd"     = c("dara_rd", "rd", "daratumumab"),
  "dara_vd"     = c("dara_vd", "vd", "daratumumab"),
  "dara_vmp"    = c("dara_vmp", "vmp", "daratumumab"),
  "dara_cybord" = c("dara_cybord", "cybord", "daratumumab"),
  "isa_kd"      = c("isa_kd", "kd", "isatuximab"),
  "isa_pd"      = c("isa_pd", "pd", "isatuximab"),
  "krd"         = c("krd", "carfilzomib"),
  "vtd"         = c("vtd", "bortezomib"),
  "rvd"         = c("rvd", "bortezomib", "lenalidomide"),
  "pvd"         = c("pvd", "pomalidomide"),
  "pcd"         = c("pcd", "pomalidomide"),
  "vmp"         = c("vmp", "bortezomib"),
  "kd"          = c("kd", "carfilzomib"),
  "rd"          = c("rd", "lenalidomide"),
  "vd"          = c("vd", "bortezomib"),
  "mp"          = c("mp", "melphalan")
)

cdm <- generateTherapyLineCohortSet(
  cdm = cdm,
  name = "lot_mm",
  cohort = "base_mm",
  treatmentCohortName = "tx_episodes",
  regimenRules = mmRules,
  gapEra = 180
)

# 5. Map LOT Cohorts to GEM-PETHEMA Guidelines (2.ª edición 2025) Categories
map_gem_pethema <- function(line_number, regimen) {
  reg <- tolower(trimws(regimen))

  if (line_number == 1) {
    if (reg %in% c("dara_rvd", "rvd", "vtd", "dara_vtd")) return("Line 1: Dara-VRd / VRd (TE ± ASCT)")
    if (reg %in% c("dara_rd")) return("Line 1: Dara-Rd (TNE Continuous)")
    if (reg %in% c("dara_vmp", "vmp")) return("Line 1: Dara-VMP / VMP (TNE Fixed)")
    if (reg %in% c("rd", "vd", "cp", "mp", "pd")) return("Line 1: Frail Doublets (Vd / CP / Rd)")
    return("Line 1: Other Regimens")
  }

  if (line_number == 2) {
    if (reg %in% c("isa_kd", "dara_kd", "kd")) return("Line 2: Isa-Kd / Dara-Kd (Len-Refractory)")
    if (reg %in% c("pvd", "pcd")) return("Line 2: PVd (Len-Refractory)")
    if (reg %in% c("cilta_cel", "ide_cel", "car_t")) return("Line 2: Cilta-cel / CAR-T")
    if (reg %in% c("dara_rd", "krd")) return("Line 2: Dara-Rd / KRd (Len-Sensitive)")
    if (reg %in% c("dara_vd", "vd")) return("Line 2: Dara-Vd / Vd-based (Len-Sensitive)")
    return("Line 2: Other Regimens")
  }

  if (line_number == 3) {
    if (reg %in% c("isa_pd", "dara_pd", "pd")) return("Line 3: Isa-Pd / Dara-Pd (Anti-CD38 + IMiD)")
    if (reg %in% c("kd", "kcd", "svd")) return("Line 3: Kd / KCd / SVd (Proteasome / Selinexor)")
    if (reg %in% c("ide_cel", "cilta_cel", "car_t")) return("Line 3: Ide-cel / CAR-T")
    return("Line 3: Other Regimens")
  }

  if (line_number == 4) {
    if (reg %in% c("teclistamab", "elranatamab", "talquetamab")) return("Line 4: Bispecifics (Teclistamab / Elranatamab / Talquetamab)")
    if (reg %in% c("ide_cel", "cilta_cel", "car_t")) return("Line 4: CAR-T (Ide-cel / Cilta-cel)")
    if (reg %in% c("selinexor", "melflufen", "pepaxto_dex", "dtpace", "pace", "bumel")) return("Line 4: Novel Chemotherapy (Selinexor / Melflufen / PACE)")
    return("Line 4: Other Regimens")
  }

  return(paste0("Line ", line_number, ": Other Regimens"))
}

lotSettings <- omopgenerics::settings(cdm$lot_mm)
lotData <- cdm$lot_mm |>
  dplyr::collect() |>
  dplyr::inner_join(lotSettings, by = "cohort_definition_id") |>
  dplyr::filter(line_number <= 4)

lotMapped <- lotData |>
  dplyr::rowwise() |>
  dplyr::mutate(gem_group = map_gem_pethema(line_number, regimen_name)) |>
  dplyr::ungroup()

# 6. Build GEM-PETHEMA Sankey Diagram Data
transitions <- lotMapped |>
  dplyr::group_by(subject_id) |>
  dplyr::arrange(line_number) |>
  dplyr::mutate(
    next_line = dplyr::lead(line_number),
    next_gem_group = dplyr::lead(gem_group)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(next_gem_group) & next_line == line_number + 1)

active_links <- transitions |>
  dplyr::group_by(source_label = gem_group, target_label = next_gem_group) |>
  dplyr::summarise(value = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(
    hover_info = paste0("<b>Transition:</b> ", source_label, " &#8594; ", target_label, "<br><b>Patients:</b> N = ", value)
  )

unique_labels <- c(
  "Line 1: Dara-VRd / VRd (TE ± ASCT)",
  "Line 1: Dara-Rd (TNE Continuous)",
  "Line 1: Dara-VMP / VMP (TNE Fixed)",
  "Line 1: Frail Doublets (Vd / CP / Rd)",
  "Line 1: Other Regimens",
  "Line 2: Isa-Kd / Dara-Kd (Len-Refractory)",
  "Line 2: PVd (Len-Refractory)",
  "Line 2: Cilta-cel / CAR-T",
  "Line 2: Dara-Rd / KRd (Len-Sensitive)",
  "Line 2: Dara-Vd / Vd-based (Len-Sensitive)",
  "Line 2: Other Regimens",
  "Line 3: Isa-Pd / Dara-Pd (Anti-CD38 + IMiD)",
  "Line 3: Kd / KCd / SVd (Proteasome / Selinexor)",
  "Line 3: Ide-cel / CAR-T",
  "Line 3: Other Regimens",
  "Line 4: Bispecifics (Teclistamab / Elranatamab / Talquetamab)",
  "Line 4: CAR-T (Ide-cel / Cilta-cel)",
  "Line 4: Novel Chemotherapy (Selinexor / Melflufen / PACE)",
  "Line 4: Other Regimens"
)

# Filter unique labels present in active_links
present_labels <- unique(c(active_links$source_label, active_links$target_label))
node_labels <- intersect(unique_labels, present_labels)
node_dict <- setNames(seq_along(node_labels) - 1L, node_labels)

node_info <- dplyr::tibble(label = node_labels) |>
  dplyr::mutate(
    line_num = as.numeric(sub("^Line ([0-9]+):.*", "\\1", label))
  )

node_x <- (node_info$line_num - 1) / 3

color_map <- c(
  "Line 1: Dara-VRd / VRd (TE ± ASCT)" = "#2b5c8f",
  "Line 1: Dara-Rd (TNE Continuous)" = "#5c2d91",
  "Line 1: Dara-VMP / VMP (TNE Fixed)" = "#27ae60",
  "Line 1: Frail Doublets (Vd / CP / Rd)" = "#1f77b4",
  "Line 1: Other Regimens" = "#7f8c8d",

  "Line 2: Isa-Kd / Dara-Kd (Len-Refractory)" = "#8e44ad",
  "Line 2: PVd (Len-Refractory)" = "#d35400",
  "Line 2: Cilta-cel / CAR-T" = "#c0392b",
  "Line 2: Dara-Rd / KRd (Len-Sensitive)" = "#27ae60",
  "Line 2: Dara-Vd / Vd-based (Len-Sensitive)" = "#2980b9",
  "Line 2: Other Regimens" = "#95a5a6",

  "Line 3: Isa-Pd / Dara-Pd (Anti-CD38 + IMiD)" = "#8e44ad",
  "Line 3: Kd / KCd / SVd (Proteasome / Selinexor)" = "#e67e22",
  "Line 3: Ide-cel / CAR-T" = "#c0392b",
  "Line 3: Other Regimens" = "#bdc3c7",

  "Line 4: Bispecifics (Teclistamab / Elranatamab / Talquetamab)" = "#f39c12",
  "Line 4: CAR-T (Ide-cel / Cilta-cel)" = "#e74c3c",
  "Line 4: Novel Chemotherapy (Selinexor / Melflufen / PACE)" = "#34495e",
  "Line 4: Other Regimens" = "#7f8c8d"
)

node_colors <- unname(color_map[node_labels])
node_colors[is.na(node_colors)] <- "#7f8c8d"

all_links <- active_links |>
  dplyr::mutate(
    source = unname(node_dict[source_label]),
    target = unname(node_dict[target_label])
  )

sankey <- plotly::plot_ly(
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
) |>
  plotly::layout(
    title = list(
      text = "<b>GEM-PETHEMA Guidelines (2.ª edición 2025) Real-World Sequence</b>",
      font = list(size = 18, color = "#5c2d91")
    ),
    font = list(size = 12),
    margin = list(l = 20, r = 20, t = 50, b = 20)
  )

outputPath <- file.path("extras", "mm_sankey_gem_pethema.html")
saveWidget(sankey, file = outputPath, selfcontained = TRUE)
cat("GEM-PETHEMA Sankey HTML saved to:", outputPath, "\n")