#!/usr/bin/env Rscript
# Audits code-set decisions for the Mohs/eBT-expanded analysis.

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(janitor)
  library(tibble)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  file.path(getwd(), "scripts")
}
project_root <- normalizePath(file.path(script_dir, ".."))
path <- function(...) file.path(project_root, ...)

source(path("scripts", "hcpcs_code_sets_mohs_ebt.R"))

dir.create(path("03_analysis"), showWarnings = FALSE, recursive = TRUE)

log_file <- path("log_04a_hcpcs_code_set_audit_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 04a_hcpcs_code_set_audit_mohs_ebt.R started", as.character(Sys.time()), "\n")

cms_files <- list.files(
  path("02_data_clean"),
  pattern = "^clean_Medicare_Physician_Other_Practitioners_by_Provider_and_Service_20(19|20|21|22|23)[.]rds$",
  full.names = TRUE
)
if (length(cms_files) == 0) stop("No cleaned CMS files found.")

all_included_codes <- sort(unique(hcpcs_group_lookup$hcpcs_cd))

by_year <- map_dfr(cms_files, function(file) {
  year <- as.integer(str_match(basename(file), "(20[0-9]{2})[.]rds$")[, 2])
  read_rds(file) |>
    clean_names() |>
    filter(hcpcs_cd %in% all_included_codes) |>
    mutate(
      year = year,
      total_pay_unstd = tot_srvcs * avg_mdcr_pymt_amt
    ) |>
    group_by(year, hcpcs_cd, hcpcs_desc, hcpcs_drug_ind) |>
    summarise(
      rows = n(),
      benes = sum(tot_benes, na.rm = TRUE),
      services = sum(tot_srvcs, na.rm = TRUE),
      total_pay_unstd = sum(total_pay_unstd, na.rm = TRUE),
      .groups = "drop"
    )
})

by_group_year <- by_year |>
  inner_join(hcpcs_group_lookup, by = "hcpcs_cd", relationship = "many-to-many") |>
  group_by(group, year) |>
  summarise(
    codes_present = paste(sort(unique(hcpcs_cd)), collapse = ", "),
    rows = sum(rows),
    benes = sum(benes),
    services = sum(services),
    total_pay_unstd = sum(total_pay_unstd),
    .groups = "drop"
  )

by_group_total <- by_group_year |>
  group_by(group) |>
  summarise(
    codes_present = paste(sort(unique(unlist(strsplit(codes_present, ", ")))), collapse = ", "),
    rows = sum(rows),
    benes = sum(benes),
    services = sum(services),
    total_pay_unstd = sum(total_pay_unstd),
    .groups = "drop"
  )

prevention_review <- tibble(
  code_set = c(
    "Retained prevention procedure codes",
    "Added prevention cost-burden-only PDT supply codes",
    "Reviewed but not added to prevention",
    "Reviewed but not added to prevention",
    "Reviewed but not added to prevention"
  ),
  codes = c(
    paste(hcpcs_prev_procedure, collapse = ", "),
    paste(hcpcs_prev_pdt_supply, collapse = ", "),
    "11102-11107",
    "17110-17111",
    "96910, 96912, 96920, 96922"
  ),
  rationale = c(
    "Procedure codes directly describing destruction of premalignant/precancer growths or PDT application for premalignant lesions.",
    "Aminolevulinic acid PDT drug/supply codes were present in CMS data and are included only in prevention cost burden to avoid double-counting service volume.",
    "Biopsy codes are diagnostic, not prevention/treatment allocation codes for this study.",
    "Benign lesion destruction codes are not specific to premalignant skin cancer prevention.",
    "Phototherapy/laser codes were retained as psoriasis treatment comparators, not skin cancer prevention."
  )
)

write_csv(by_year, path("03_analysis", "hcpcs_code_audit_mohs_ebt_by_year.csv"))
write_csv(by_group_year, path("03_analysis", "hcpcs_code_audit_mohs_ebt_by_group_year.csv"))
write_csv(by_group_total, path("03_analysis", "hcpcs_code_audit_mohs_ebt_by_group_total.csv"))
write_csv(prevention_review, path("03_analysis", "prevention_code_review_mohs_ebt.csv"))

cat("Codes included:", paste(all_included_codes, collapse = ", "), "\n")
cat("Saved HCPCS audit outputs to 03_analysis.\n")
cat("=== finished", as.character(Sys.time()), "\n")
