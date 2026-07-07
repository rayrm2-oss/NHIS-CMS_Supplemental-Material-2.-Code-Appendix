#!/usr/bin/env Rscript
# 04d_cms_provider_table.R
# Builds the CMS provider table.

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(tibble)
  library(glue)
  library(scales)
  library(gt)
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
dir.create(path("04_figures"), showWarnings = FALSE, recursive = TRUE)

log_file <- path("log_04d_cms_provider_table_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 04d_cms_provider_table_mohs_ebt.R started", as.character(Sys.time()), "\n")

var_hcpcs <- "hcpcs_cd"
var_benes <- "tot_benes"
var_srvcs <- "tot_srvcs"
var_pay_std <- "avg_mdcr_stdzd_amt"
var_pay_raw <- "avg_mdcr_pymt_amt"
var_charge <- "avg_sbmtd_chrg"
var_zip <- "rndrng_prvdr_zip5"

cms_files <- list.files(
  path("02_data_clean"),
  pattern = "^clean_Medicare_Physician_Other_Practitioners_by_Provider_and_Service_20(19|20|21|22|23)[.]rds$",
  full.names = TRUE
)
if (length(cms_files) == 0) stop("No cleaned CMS files found.")

safe_sum <- function(x) sum(x, na.rm = TRUE)
safe_mean <- function(x) round(mean(x, na.rm = TRUE), 2)
calc_percent <- function(pay, charge) ifelse(charge == 0, NA_real_, pay / charge * 100)

safe_pvalue <- function(tab) {
  exp <- suppressWarnings(chisq.test(tab)$expected)
  if (all(exp >= 5)) {
    chisq.test(tab)$p.value
  } else {
    fisher.test(tab, simulate.p.value = TRUE, B = 10000)$p.value
  }
}

safe_kw_p <- function(v, g) {
  ok <- !is.na(v) & !is.na(g)
  if (sum(ok) == 0 || dplyr::n_distinct(g[ok]) < 2) return(NA_real_)
  kruskal.test(v[ok] ~ g[ok])$p.value
}

calc_or_ci <- function(a, b, c, d) {
  cells <- as.numeric(c(a, b, c, d))
  if (any(cells == 0)) cells <- cells + 0.5
  a <- cells[1]; b <- cells[2]; c <- cells[3]; d <- cells[4]
  or <- (a * d) / (b * c)
  se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  c(
    or = or,
    lci = exp(log(or) - 1.96 * se),
    uci = exp(log(or) + 1.96 * se)
  )
}

parse_nchs <- function(x) {
  v <- readr::parse_number(
    x,
    na = c("", "NA", "N/A", "#VALUE!", ".", "NULL", "NaN", "INF", "-INF")
  )
  v <- as.integer(v)
  v[!v %in% 1:4] <- NA_integer_
  v
}

cms <- map_dfr(cms_files, function(file) {
  read_rds(file) |>
    clean_names() |>
    mutate(rndrng_prvdr_state_fips = as.character(rndrng_prvdr_state_fips))
})

xwalk <- read_csv(
  path("05_docs", "zip_to_nchs.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  mutate(
    ZIP = str_pad(ZIP, 5, pad = "0"),
    NCHS_Adj = parse_nchs(NCHS_Adj)
  ) |>
  filter(!is.na(NCHS_Adj)) |>
  transmute(
    !!var_zip := ZIP,
    nchs_eqiv = factor(
      NCHS_Adj,
      levels = 1:4,
      ordered = TRUE,
      labels = c(
        "Large central metro",
        "Large fringe metro",
        "Medium and small metro",
        "Nonmetropolitan"
      )
    )
  )

cms <- cms |>
  mutate(!!var_zip := str_pad(.data[[var_zip]], 5, pad = "0")) |>
  left_join(xwalk, by = var_zip) |>
  filter(!is.na(nchs_eqiv)) |>
  filter(.data[[var_benes]] >= 5)

ref_level <- "Large central metro"

get_cat_denoms <- function(cat_vec) {
  cms |>
    filter(.data[[var_hcpcs]] %in% cat_vec) |>
    group_by(nchs_eqiv) |>
    summarise(cat_den = safe_sum(.data[[var_benes]]), .groups = "drop")
}

format_or <- function(x) sprintf("%.2f (%.2f-%.2f)", x["or"], x["lci"], x["uci"])

count_row <- function(h_vec, label, denom_vec, section, basis_note) {
  cond <- cms[[var_hcpcs]] %in% h_vec

  num_tbl <- cms |>
    filter(cond) |>
    group_by(nchs_eqiv) |>
    summarise(n = safe_sum(.data[[var_benes]]), .groups = "drop")

  long_tbl <- num_tbl |>
    left_join(get_cat_denoms(denom_vec), by = "nchs_eqiv") |>
    mutate(
      pct = ifelse(cat_den > 0, 100 * n / cat_den, NA_real_),
      cell_txt = ifelse(is.na(pct), comma(n), sprintf("%s (%.1f%%)", comma(n), pct))
    )

  case_tbl <- cms |>
    mutate(is_case = cond) |>
    group_by(nchs_eqiv, is_case) |>
    summarise(bene_sum = safe_sum(.data[[var_benes]]), .groups = "drop") |>
    pivot_wider(
      names_from = is_case,
      values_from = bene_sum,
      values_fill = 0,
      names_prefix = "case_"
    ) |>
    rename(n = case_TRUE, non_cases = case_FALSE)

  ref_row <- filter(case_tbl, nchs_eqiv == ref_level)
  c_ref <- ref_row$n
  d_ref <- ref_row$non_cases

  or_tbl <- case_tbl |>
    rowwise() |>
    mutate(or_ci = if (nchs_eqiv == ref_level) {
      "1.00 (Reference)"
    } else {
      format_or(calc_or_ci(n, non_cases, c_ref, d_ref))
    }) |>
    ungroup()

  wide_cells <- long_tbl |>
    select(nchs_eqiv, cell_txt) |>
    pivot_wider(names_from = nchs_eqiv, values_from = cell_txt)
  wide_or <- or_tbl |>
    select(nchs_eqiv, or_ci) |>
    pivot_wider(names_from = nchs_eqiv, values_from = or_ci, names_glue = "{nchs_eqiv} OR")

  overall_n <- safe_sum(cms[[var_benes]][cond])
  overall_cat_den <- cms |>
    filter(.data[[var_hcpcs]] %in% denom_vec) |>
    summarise(n = safe_sum(.data[[var_benes]])) |>
    pull(n)

  tibble(
    section = section,
    row_lbl = label,
    code_set = paste(sort(unique(h_vec)), collapse = ", "),
    payment_basis = basis_note,
    Overall = sprintf("%s (%.1f%%)", comma(overall_n), 100 * overall_n / overall_cat_den),
    !!!wide_cells,
    !!!wide_or,
    `P value` = safe_pvalue(table(cond, cms$nchs_eqiv))
  )
}

mean_row <- function(h_vec, label, payment_basis, metric_fun = function(pay, charge) pay) {
  sub <- cms |>
    filter(.data[[var_hcpcs]] %in% h_vec) |>
    mutate(metric = metric_fun(.data[[var_pay_std]], .data[[var_charge]]))

  by_nchs <- sub |>
    group_by(nchs_eqiv) |>
    summarise(val = safe_mean(metric), .groups = "drop") |>
    mutate(val = sprintf("%.2f", val)) |>
    pivot_wider(names_from = nchs_eqiv, values_from = val)

  tibble(
    section = "Cost basis",
    row_lbl = label,
    code_set = paste(sort(unique(h_vec)), collapse = ", "),
    payment_basis = payment_basis,
    Overall = sprintf("%.2f", safe_mean(sub$metric)),
    !!!by_nchs,
    `P value` = safe_kw_p(sub$metric, sub$nchs_eqiv)
  )
}

cost_burden_row <- function(h_vec, label, payment_basis) {
  df <- cms |>
    filter(.data[[var_hcpcs]] %in% h_vec) |>
    mutate(total_pay_unstd = .data[[var_srvcs]] * .data[[var_pay_raw]])

  by_nchs <- df |>
    group_by(nchs_eqiv) |>
    summarise(pay = sum(total_pay_unstd, na.rm = TRUE), .groups = "drop") |>
    mutate(pay_fmt = dollar(pay)) |>
    select(nchs_eqiv, pay_fmt) |>
    pivot_wider(names_from = nchs_eqiv, values_from = pay_fmt)

  tibble(
    section = "Total cost burden",
    row_lbl = label,
    code_set = paste(sort(unique(h_vec)), collapse = ", "),
    payment_basis = payment_basis,
    Overall = dollar(sum(df$total_pay_unstd, na.rm = TRUE)),
    !!!by_nchs,
    `P value` = NA_real_
  )
}

table3 <- bind_rows(
  mean_row(
    hcpcs_prev_procedure,
    "[Prevention Cost Basis (std)] Avg Medicare Payment - Precancer/PDT procedures",
    "Geographically standardized avg Medicare payment; procedures only"
  ),
  mean_row(
    hcpcs_ct_any,
    "[Treatment Cost Basis (std)] Avg Medicare Payment - Any expanded skin cancer treatment",
    "Geographically standardized avg Medicare payment"
  ),
  mean_row(
    hcpcs_ct_mohs,
    "[Treatment Cost Basis (std)] Avg Medicare Payment - Mohs surgery",
    "Geographically standardized avg Medicare payment"
  ),
  mean_row(
    hcpcs_ct_surgical_non_mohs,
    "[Treatment Cost Basis (std)] Avg Medicare Payment - Surgical excision/destruction, non-Mohs",
    "Geographically standardized avg Medicare payment"
  ),
  mean_row(
    hcpcs_ct_radiation_srt_ebt,
    "[Treatment Cost Basis (std)] Avg Medicare Payment - Radiation/SRT/eBT-associated",
    "Geographically standardized avg Medicare payment"
  ),
  mean_row(
    hcpcs_ct_any,
    "[Percent Collection (unstd)] Avg Payment / Charge - Any expanded treatment",
    "Unstandardized avg Medicare payment / submitted charge",
    metric_fun = calc_percent
  ),
  cost_burden_row(
    hcpcs_prev_procedure,
    "[Total Cost Burden (unstd)] Preventive procedures - All HCPCS",
    "Unstandardized avg Medicare payment x total services; procedures only"
  ),
  cost_burden_row(
    hcpcs_prev_pdt_supply,
    "[Total Cost Burden (unstd)] PDT drug/supply adjuncts - J codes",
    "Unstandardized avg Medicare payment x total services; cost burden only"
  ),
  cost_burden_row(
    hcpcs_prev_cost_burden,
    "[Total Cost Burden (unstd)] Preventive procedures + PDT drug/supply adjuncts",
    "Unstandardized avg Medicare payment x total services"
  ),
  cost_burden_row(
    hcpcs_ct_any,
    "[Total Cost Burden (unstd)] Expanded cancer treatments - All HCPCS",
    "Unstandardized avg Medicare payment x total services"
  ),
  cost_burden_row(
    hcpcs_ct_mohs,
    "[Total Cost Burden (unstd)] Mohs surgery",
    "Unstandardized avg Medicare payment x total services"
  ),
  cost_burden_row(
    hcpcs_ct_surgical_non_mohs,
    "[Total Cost Burden (unstd)] Surgical excision/destruction, non-Mohs",
    "Unstandardized avg Medicare payment x total services"
  ),
  cost_burden_row(
    hcpcs_ct_radiation_srt_ebt,
    "[Total Cost Burden (unstd)] Radiation/SRT/eBT-associated",
    "Unstandardized avg Medicare payment x total services"
  ),
  cost_burden_row(
    hcpcs_ct_ebt_specific,
    "[Total Cost Burden (unstd)] eBT-specific code 0394T",
    "Unstandardized avg Medicare payment x total services"
  ),
  count_row(
    hcpcs_prev_procedure,
    "[Preventive Procedure Volume] Destruction/PDT of precancer skin growth",
    denom_vec = hcpcs_prev_procedure,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; procedure codes only"
  ),
  count_row(
    hcpcs_prev_destruction,
    "[Preventive Procedure Volume] Destruction of precancer skin growth",
    denom_vec = hcpcs_prev_procedure,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; procedure codes only"
  ),
  count_row(
    hcpcs_prev_pdt_procedure,
    "[Preventive Procedure Volume] PDT procedures for precancer skin growth",
    denom_vec = hcpcs_prev_procedure,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; procedure codes only"
  ),
  count_row(
    hcpcs_ct_any,
    "[Cancer Treatment Service Volume] Any expanded skin cancer treatment",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_mohs,
    "[Cancer Treatment Service Volume] Mohs surgery",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_surgical_non_mohs,
    "[Cancer Treatment Service Volume] Surgical excision/destruction, non-Mohs",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_radiation_srt_ebt,
    "[Cancer Treatment Service Volume] Radiation/SRT/eBT-associated",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_ebt_specific,
    "[Cancer Treatment Service Volume] eBT-specific code 0394T",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_chemo,
    "[Cancer Treatment Service Volume] Chemotherapy",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  ),
  count_row(
    hcpcs_ct_hyperthermia,
    "[Cancer Treatment Service Volume] Hyperthermia",
    denom_vec = hcpcs_ct_any,
    section = "Service volume",
    basis_note = "Beneficiary-service volume; ORs are crude by NCHS category"
  )
)

out_csv <- path("03_analysis", "table3_cms_provider_mohs_ebt.csv")
write_csv(table3, out_csv, na = "")

metadata <- tibble(
  cms_files = paste(basename(cms_files), collapse = "; "),
  cms_rows_after_zip_join_and_small_cell_suppression = nrow(cms),
  prevention_service_volume_codes = paste(hcpcs_prev_procedure, collapse = ", "),
  prevention_cost_burden_additional_codes = paste(hcpcs_prev_pdt_supply, collapse = ", "),
  expanded_treatment_codes = paste(sort(hcpcs_ct_any), collapse = ", "),
  cost_burden_formula = "sum(tot_srvcs * avg_mdcr_pymt_amt)",
  mean_payment_formula = "mean(avg_mdcr_stdzd_amt)"
)
write_csv(metadata, path("03_analysis", "table3_cms_provider_mohs_ebt_metadata.csv"))

gt_tbl <- table3 |>
  gt(rowname_col = "row_lbl", groupname_col = "section") |>
  sub_missing(columns = everything(), missing_text = " ") |>
  cols_align(
    columns = c(
      Overall, `Large central metro`, `Large fringe metro`,
      `Medium and small metro`, Nonmetropolitan
    ),
    align = "right"
  ) |>
  fmt_scientific(columns = `P value`, decimals = 2) |>
  cols_label(
    code_set = "HCPCS/CPT codes",
    payment_basis = "Payment/denominator basis",
    Overall = "Overall",
    `Large central metro` = "NCHS Large central metro",
    `Large fringe metro` = "NCHS Large fringe metro",
    `Medium and small metro` = "NCHS Medium + small metro",
    Nonmetropolitan = "NCHS Non-metropolitan",
    `Large central metro OR` = "LCM OR",
    `Large fringe metro OR` = "LFM OR",
    `Medium and small metro OR` = "MSM OR",
    `Nonmetropolitan OR` = "Non-metro OR",
    `P value` = md("*P*")
  ) |>
  tab_source_note(md("Mean payment rows use geographically standardized Medicare payments. Total cost burden rows use unstandardized average Medicare payment multiplied by total services. PDT drug/supply J codes are included in prevention cost burden but not prevention procedure-volume ORs.")) |>
  tab_options(table.font.names = "Arial", data_row.padding = px(2))

out_html <- path("04_figures", "table3_cms_provider_mohs_ebt.html")
gtsave(gt_tbl, out_html)

cat("CMS rows after ZIP join and small-cell suppression:", nrow(cms), "\n")
cat("Saved CSV:", out_csv, "\n")
cat("Saved HTML:", out_html, "\n")
cat("=== finished", as.character(Sys.time()), "\n")
