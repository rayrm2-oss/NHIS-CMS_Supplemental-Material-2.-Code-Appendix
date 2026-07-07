#!/usr/bin/env Rscript
# 04c_skincancer_weighted.R
# Builds NHIS skin outcome table:


options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(survey)
  library(glue)
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

dir.create(path("03_analysis"), showWarnings = FALSE, recursive = TRUE)
dir.create(path("04_figures"), showWarnings = FALSE, recursive = TRUE)

log_file <- path("log_04c_skincancer_weighted_corrected.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 04c_skincancer_weighted_corrected.R started", as.character(Sys.time()), "\n")

nhis_files <- list.files(
  path("02_data_clean"),
  pattern = "^clean_adult[0-9]{2}[.]rds$",
  full.names = TRUE
)
if (length(nhis_files) == 0) stop("No clean_adultYY.rds files found in 02_data_clean.")

extract_nhis_year <- function(file) {
  yy <- stringr::str_match(basename(file), "^clean_adult([0-9]{2})[.]rds$")[, 2]
  as.integer(yy) + 2000L
}

parsed_years <- tibble(
  file = basename(nhis_files),
  year = map_int(nhis_files, extract_nhis_year)
)
if (any(is.na(parsed_years$year))) stop("Could not parse one or more NHIS years.")

yes_indicator <- function(x) as.integer(!is.na(x) & as.numeric(x) == 1)
yes_no_response <- function(x) {
  x <- as.numeric(x)
  case_when(
    x == 1 ~ 1L,
    x == 2 ~ 0L,
    TRUE ~ NA_integer_
  )
}
clean_age <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(97, 98, 99)] <- NA_real_
  x
}

nhis <- map_dfr(nhis_files, function(file) {
  read_rds(file) %>% mutate(year = extract_nhis_year(file))
})

nhis <- nhis %>%
  filter(medicare_a %in% c(1, 2)) %>%
  mutate(
    nchs_eqiv = factor(
      urbrrl,
      levels = 1:4,
      labels = c(
        "Large central metro",
        "Large fringe metro",
        "Medium and small metro",
        "Nonmetropolitan"
      ),
      ordered = TRUE
    ),
    melancan_a = yes_indicator(melancan_a),
    sknnmcan_a = yes_indicator(sknnmcan_a),
    skndkcan_a = yes_indicator(skndkcan_a),
    psorev_a = yes_no_response(psorev_a),
    melanagetc_a = clean_age(melanagetc_a),
    sknnmagetc_a = clean_age(sknnmagetc_a),
    skndkagetc_a = clean_age(skndkagetc_a),
    skincancer_any = as.integer(melancan_a == 1 | sknnmcan_a == 1 | skndkcan_a == 1),
    age_skin_dx_any = pmin(
      if_else(melancan_a == 1, melanagetc_a, NA_real_),
      if_else(sknnmcan_a == 1, sknnmagetc_a, NA_real_),
      if_else(skndkcan_a == 1, skndkagetc_a, NA_real_),
      na.rm = TRUE
    ),
    age_skin_dx_any = if_else(is.infinite(age_skin_dx_any), NA_real_, age_skin_dx_any)
  )

n_years <- n_distinct(nhis$year)
if (n_years != 5L) warning(glue("Expected 5 survey years, found {n_years}."))
nhis <- nhis %>% mutate(weight_adj = wtfa_a / n_years)

nhis_design <- svydesign(
  ids = ~ppsu,
  strata = ~pstrat,
  weights = ~weight_adj,
  data = nhis,
  nest = TRUE
)

format_count_pct <- function(n, pct) {
  glue("{format(round(n), big.mark = ',')} ({sprintf('%.1f', pct)}%)")
}

make_binary_row <- function(var, pretty, denominator = c("all", "skin_cases"), denominator_label = NULL) {
  denominator <- match.arg(denominator)
  design <- if (denominator == "skin_cases") {
    subset(nhis_design, skincancer_any == 1)
  } else {
    nhis_design
  }
  if (is.null(denominator_label)) {
    denominator_label <- if_else(
      denominator == "all",
      "All Medicare respondents",
      "Respondents with any skin cancer history"
    )
  }

  form_var_group <- as.formula(paste0("~", var, " + nchs_eqiv"))
  form_var_only <- as.formula(paste0("~", var))
  wt_tab <- svytable(form_var_group, design)
  overall_tab <- svytable(form_var_only, design)
  yes_row <- "1"

  row_total <- as.numeric(overall_tab[yes_row])
  overall_pct <- 100 * row_total / sum(overall_tab)

  cells <- map_chr(colnames(wt_tab), function(col) {
    nchs_val <- as.numeric(wt_tab[yes_row, col])
    nchs_pct <- 100 * nchs_val / sum(wt_tab[, col])
    format_count_pct(nchs_val, nchs_pct)
  })

  p_val <- tryCatch(
    formatC(svychisq(form_var_group, design)$p.value, format = "e", digits = 2),
    error = function(e) NA_character_
  )

  tibble(
    row_lbl = pretty,
    denominator = denominator_label,
    Overall = format_count_pct(row_total, overall_pct),
    !!!set_names(cells, colnames(wt_tab)),
    `P value` = p_val
  )
}

median_value <- function(var, design) {
  form <- as.formula(paste0("~", var))
  value <- tryCatch(
    as.numeric(svyquantile(form, design, quantiles = 0.5, na.rm = TRUE, ci = FALSE))[1],
    error = function(e) NA_real_
  )
  value
}

rank_p_value <- function(var, design) {
  form <- as.formula(paste0(var, " ~ nchs_eqiv"))
  tryCatch(
    {
      p <- svyranktest(form, design, test = "KruskalWallis")$p.value
      formatC(as.numeric(p)[1], format = "e", digits = 2)
    },
    error = function(e) NA_character_
  )
}

make_age_row <- function(var, pretty, condition_expr, denominator_label) {
  design <- subset(nhis_design, eval(condition_expr) & !is.na(get(var)))
  overall_med <- median_value(var, design)
  cells <- map_chr(levels(nhis$nchs_eqiv), function(level) {
    sub_design <- subset(design, nchs_eqiv == level)
    med <- median_value(var, sub_design)
    if (is.na(med)) "" else sprintf("%.0f", med)
  })

  tibble(
    row_lbl = pretty,
    denominator = denominator_label,
    Overall = if (is.na(overall_med)) "" else sprintf("%.0f", overall_med),
    !!!set_names(cells, levels(nhis$nchs_eqiv)),
    `P value` = rank_p_value(var, design)
  )
}

rows_binary <- bind_rows(
  make_binary_row("skincancer_any", "[Skin Cancer History] Any", "all"),
  make_binary_row("melancan_a", "[Skin Cancer Type] Melanoma", "skin_cases"),
  make_binary_row("sknnmcan_a", "[Skin Cancer Type] Non-Melanoma", "skin_cases"),
  make_binary_row("skndkcan_a", "[Skin Cancer Type] Unknown Type", "skin_cases"),
  make_binary_row(
    "psorev_a",
    "[Psoriasis] Psoriasis Dx",
    "all",
    denominator_label = "Medicare respondents with nonmissing psoriasis response"
  )
)

rows_age <- bind_rows(
  make_age_row(
    "age_skin_dx_any",
    "[Skin Cancer Age] Any",
    quote(skincancer_any == 1),
    "Respondents with any skin cancer history and nonmissing age at diagnosis"
  ),
  make_age_row(
    "melanagetc_a",
    "[Skin Cancer Age] Melanoma",
    quote(melancan_a == 1),
    "Respondents with melanoma history and nonmissing age at diagnosis"
  ),
  make_age_row(
    "sknnmagetc_a",
    "[Skin Cancer Age] Non-Melanoma",
    quote(sknnmcan_a == 1),
    "Respondents with non-melanoma skin cancer history and nonmissing age at diagnosis"
  ),
  make_age_row(
    "skndkagetc_a",
    "[Skin Cancer Age] Unknown Type",
    quote(skndkcan_a == 1),
    "Respondents with unknown-type skin cancer history and nonmissing age at diagnosis"
  )
)

table2 <- bind_rows(rows_binary, rows_age)

out_csv <- path("03_analysis", "table2_skincancer_weighted_corrected.csv")
write_csv(table2, out_csv)

metadata <- parsed_years %>%
  mutate(
    n_years_used_for_weight_adjustment = n_years,
    unweighted_medicare_n = nrow(nhis),
    unweighted_skin_cancer_history_n = sum(nhis$skincancer_any == 1, na.rm = TRUE)
  )
write_csv(metadata, path("03_analysis", "table2_skincancer_weighted_corrected_metadata.csv"))

gt_tbl <- table2 %>%
  select(-denominator) %>%
  gt(rowname_col = "row_lbl") %>%
  tab_stubhead("Characteristic") %>%
  fmt_missing(columns = everything(), missing_text = " ") %>%
  cols_label(
    Overall = md("Overall<br>Average annual weighted n (%) or median age"),
    `Large central metro` = md("LCM<br>Average annual weighted n (%) or median age"),
    `Large fringe metro` = md("LFM<br>Average annual weighted n (%) or median age"),
    `Medium and small metro` = md("MSM<br>Average annual weighted n (%) or median age"),
    Nonmetropolitan = md("Non-metro<br>Average annual weighted n (%) or median age"),
    `P value` = md("*P*")
  ) %>%
  tab_source_note(md("Any skin cancer and psoriasis percentages use all Medicare respondents as the denominator. Skin cancer type percentages use respondents with any skin cancer history as the denominator.")) %>%
  tab_options(table.font.names = "Arial", data_row.padding = px(2))

out_html <- path("04_figures", "table2_skincancer_weighted_corrected.html")
gtsave(gt_tbl, out_html)

cat("Parsed NHIS years:", paste(sort(unique(nhis$year)), collapse = ", "), "\n")
cat("Unweighted Medicare respondents:", nrow(nhis), "\n")
cat("Unweighted skin cancer history cases:", sum(nhis$skincancer_any == 1, na.rm = TRUE), "\n")
cat("Saved CSV:", out_csv, "\n")
cat("Saved HTML:", out_html, "\n")
cat("=== finished", as.character(Sys.time()), "\n")
