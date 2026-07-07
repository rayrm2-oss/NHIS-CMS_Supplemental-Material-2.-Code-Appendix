#!/usr/bin/env Rscript
# 03_transform.R
# Creates analytic dataset using codes.

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(janitor)
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

log_file <- path("log_03_transform_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 03_transform_mohs_ebt.R started", as.character(Sys.time()), "\n")

extract_year <- function(file) {
  as.integer(str_match(basename(file), "(20[0-9]{2})[.]rds$")[, 2])
}

yes_indicator <- function(x) as.integer(!is.na(x) & as.numeric(x) == 1)

cms_paths <- list.files(
  path("02_data_clean"),
  pattern = "^clean_Medicare_Physician_Other_Practitioners_by_Provider_and_Service_20(19|20|21|22|23)[.]rds$",
  full.names = TRUE
)
if (length(cms_paths) == 0) stop("No cleaned CMS files found.")

cms_keep_cols <- c(
  "rndrng_npi", "rndrng_prvdr_zip5", "rndrng_prvdr_type",
  "hcpcs_cd", "hcpcs_desc", "hcpcs_drug_ind",
  "tot_benes", "tot_srvcs", "avg_mdcr_pymt_amt", "avg_mdcr_stdzd_amt"
)

cms_dfs <- map_dfr(cms_paths, function(file) {
  read_rds(file) |>
    clean_names() |>
    select(any_of(cms_keep_cols)) |>
    mutate(year = extract_year(file))
})

xwalk <- read_csv(
  path("05_docs", "zip_to_nchs.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  transmute(
    zip_chr = str_pad(ZIP, 5, pad = "0"),
    nchs_eqiv = as.integer(readr::parse_number(NCHS_Adj))
  ) |>
  filter(nchs_eqiv %in% 1:4)

cms_summ <- cms_dfs |>
  mutate(
    zip_chr = str_pad(as.character(rndrng_prvdr_zip5), 5, pad = "0"),
    prev_flag = hcpcs_cd %in% hcpcs_prev_procedure,
    treat_flag = hcpcs_cd %in% hcpcs_ct_any,
    mohs_flag = hcpcs_cd %in% hcpcs_ct_mohs,
    ebt_flag = hcpcs_cd %in% hcpcs_ct_ebt_specific,
    radiation_flag = hcpcs_cd %in% hcpcs_ct_radiation_srt_ebt,
    pdt_supply_flag = hcpcs_cd %in% hcpcs_prev_pdt_supply
  ) |>
  left_join(xwalk, by = "zip_chr") |>
  filter(!is.na(nchs_eqiv)) |>
  group_by(rndrng_npi, year, nchs_eqiv) |>
  summarise(
    total_benes = sum(tot_benes, na.rm = TRUE),
    prev_benes = sum(tot_benes[prev_flag], na.rm = TRUE),
    prev_pdt_supply_benes = sum(tot_benes[pdt_supply_flag], na.rm = TRUE),
    treat_benes = sum(tot_benes[treat_flag], na.rm = TRUE),
    mohs_benes = sum(tot_benes[mohs_flag], na.rm = TRUE),
    radiation_srt_ebt_benes = sum(tot_benes[radiation_flag], na.rm = TRUE),
    ebt_specific_benes = sum(tot_benes[ebt_flag], na.rm = TRUE),
    prop_prevcare_byprovider = prev_benes / total_benes,
    prop_treat_byprovider = treat_benes / total_benes,
    prop_mohs_byprovider = mohs_benes / total_benes,
    prop_radiation_srt_ebt_byprovider = radiation_srt_ebt_benes / total_benes,
    .groups = "drop"
  )

nhis_paths <- list.files(
  path("02_data_clean"),
  pattern = "^clean_adult[0-9]{2}[.]rds$",
  full.names = TRUE
)
if (length(nhis_paths) == 0) stop("No cleaned NHIS files found.")

nhis_dfs <- map_dfr(nhis_paths, read_rds)

cat("=== NHIS filtering summary ===\n")
total_nhis_rows <- nrow(nhis_dfs)
medicare_rows <- nhis_dfs |> filter(medicare_a %in% c(1, 2)) |> nrow()
cat("Total NHIS respondents:", total_nhis_rows, "\n")
cat("Medicare-insured respondents:", medicare_rows, "\n")
cat("Excluded non-Medicare records:", total_nhis_rows - medicare_rows, "\n")

nhis_agg <- nhis_dfs |>
  mutate(
    year = as.integer(srvy_yr),
    nchs_eqiv = as.integer(urbrrl),
    melancan_flag = yes_indicator(melancan_a),
    sknnmcan_flag = yes_indicator(sknnmcan_a),
    skndkcan_flag = yes_indicator(skndkcan_a),
    skincancer_any = as.integer(melancan_flag == 1 | sknnmcan_flag == 1 | skndkcan_flag == 1)
  ) |>
  filter(medicare_a %in% c(1, 2), nchs_eqiv %in% 1:4) |>
  group_by(year, nchs_eqiv) |>
  summarise(
    total_resp = n(),
    skin_cases = sum(skincancer_any == 1, na.rm = TRUE),
    skin_prev = skin_cases / total_resp,
    .groups = "drop"
  )

analytic <- cms_summ |>
  left_join(nhis_agg, by = c("year", "nchs_eqiv")) |>
  filter(!is.na(skin_prev)) |>
  mutate(
    prevcare_high = factor(if_else(prop_prevcare_byprovider > 0, "Y", "N")),
    treat_high = factor(if_else(prop_treat_byprovider > 0, "Y", "N")),
    mohs_high = factor(if_else(prop_mohs_byprovider > 0, "Y", "N")),
    radiation_srt_ebt_high = factor(if_else(prop_radiation_srt_ebt_byprovider > 0, "Y", "N")),
    nchs_eqiv = factor(nchs_eqiv, levels = 1:4, ordered = TRUE)
  )

out_rds <- path("03_analysis", "analytic_dataset_mohs_ebt.rds")
write_rds(analytic, out_rds, compress = "xz")

n_counts <- tibble(
  step = c("03_transform_mohs_ebt"),
  file = c("analytic_dataset_mohs_ebt.rds"),
  n_rows = c(nrow(analytic))
)
write_csv(n_counts, path("03_analysis", "n_counts_mohs_ebt.csv"))

summary_by_nchs <- analytic |>
  group_by(nchs_eqiv) |>
  summarise(
    providers_year_rows = n(),
    total_benes = sum(total_benes),
    prev_benes = sum(prev_benes),
    treat_benes = sum(treat_benes),
    mohs_benes = sum(mohs_benes),
    radiation_srt_ebt_benes = sum(radiation_srt_ebt_benes),
    ebt_specific_benes = sum(ebt_specific_benes),
    mean_prop_prevcare = mean(prop_prevcare_byprovider),
    mean_prop_treat = mean(prop_treat_byprovider),
    .groups = "drop"
  )
write_csv(summary_by_nchs, path("03_analysis", "analytic_dataset_mohs_ebt_summary_by_nchs.csv"))

cat("Rows in analytic data:", nrow(analytic), "\n")
cat("Saved:", out_rds, "\n")
cat("=== finished", as.character(Sys.time()), "\n")
