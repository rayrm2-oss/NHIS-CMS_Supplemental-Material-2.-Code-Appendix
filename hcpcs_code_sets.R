#!/usr/bin/env Rscript
# Shared HCPCS/CPT code sets for the analysis.
#


hcpcs_prev_procedure <- c(
  "17000", "17003", "17004",
  "96567", "96573", "96574"
)

hcpcs_prev_destruction <- c("17000", "17003", "17004")
hcpcs_prev_pdt_procedure <- c("96567", "96573", "96574")
hcpcs_prev_pdt_supply <- c("J7308", "J7345")
hcpcs_prev_cost_burden <- unique(c(hcpcs_prev_procedure, hcpcs_prev_pdt_supply))

hcpcs_ct_malignant_excision <- c(
  "11600", "11601", "11602", "11603", "11604", "11606",
  "11620", "11621", "11622", "11623", "11624", "11626",
  "11640", "11641", "11642", "11643", "11644", "11646"
)

hcpcs_ct_malignant_destruction <- c(
  "17260", "17261", "17262", "17263", "17264", "17266",
  "17270", "17271", "17272", "17273", "17274", "17276",
  "17280", "17281", "17282", "17283", "17284", "17286"
)

hcpcs_ct_mohs <- c("17311", "17312", "17313", "17314", "17315")

hcpcs_ct_chemo <- c("96401", "96405", "96413")
hcpcs_ct_hyperthermia <- c("77600")

hcpcs_ct_radiation_srt_ebt <- c(
  "0394T",
  "77261", "77262", "77263",
  "77280", "77285", "77290",
  "77300", "77332", "77333", "77334", "77336",
  "77401", "77427",
  "G6001", "G6003", "G6007"
)

hcpcs_ct_ebt_specific <- c("0394T")

hcpcs_ct_surgical_non_mohs <- unique(c(
  hcpcs_ct_malignant_excision,
  hcpcs_ct_malignant_destruction
))

hcpcs_ct_any <- unique(c(
  hcpcs_ct_surgical_non_mohs,
  hcpcs_ct_mohs,
  hcpcs_ct_chemo,
  hcpcs_ct_hyperthermia,
  hcpcs_ct_radiation_srt_ebt
))

hcpcs_code_groups <- list(
  prevention_procedure = hcpcs_prev_procedure,
  prevention_destruction = hcpcs_prev_destruction,
  prevention_pdt_procedure = hcpcs_prev_pdt_procedure,
  prevention_pdt_supply_cost_only = hcpcs_prev_pdt_supply,
  prevention_cost_burden = hcpcs_prev_cost_burden,
  treatment_any_mohs_ebt = hcpcs_ct_any,
  treatment_surgical_non_mohs = hcpcs_ct_surgical_non_mohs,
  treatment_mohs = hcpcs_ct_mohs,
  treatment_radiation_srt_ebt = hcpcs_ct_radiation_srt_ebt,
  treatment_ebt_specific = hcpcs_ct_ebt_specific,
  treatment_chemo = hcpcs_ct_chemo,
  treatment_hyperthermia = hcpcs_ct_hyperthermia
)

hcpcs_group_lookup <- tibble::tibble(
  group = rep(names(hcpcs_code_groups), lengths(hcpcs_code_groups)),
  hcpcs_cd = unlist(hcpcs_code_groups, use.names = FALSE)
) |>
  dplyr::distinct(group, hcpcs_cd)
