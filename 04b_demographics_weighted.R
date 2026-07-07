#!/usr/bin/env Rscript
# 04b_demographics_weighted.R
# Builds  NHIS demographic table with average annual pooled weights.

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

log_file <- path("log_04b_demographics_weighted_corrected.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 04b_demographics_weighted_corrected.R started", as.character(Sys.time()), "\n")

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

nhis <- map_dfr(nhis_files, function(file) {
  read_rds(file) %>% mutate(year = extract_nhis_year(file))
})

nhis <- nhis %>% filter(medicare_a %in% c(1, 2))
n_years <- n_distinct(nhis$year)
if (n_years != 5L) warning(glue("Expected 5 survey years, found {n_years}."))

nhis <- nhis %>% mutate(weight_adj = wtfa_a / n_years)

nhis <- nhis %>%
  mutate(
    age_band = cut(
      agep_a,
      breaks = c(-Inf, 64, 69, 74, 79, 84, Inf),
      labels = c("<65yo", "65-69yo", "70-74yo", "75-79yo", "80-84yo", "85+yo"),
      right = TRUE,
      ordered_result = TRUE
    ),
    sex_cat = recode_factor(sex_a, `1` = "Male", `2` = "Female"),
    race_eth = case_when(
      hispallp_a == 1 ~ "Hispanic",
      hispallp_a == 2 ~ "Non-Hispanic White",
      hispallp_a == 3 ~ "Non-Hispanic Black",
      hispallp_a == 4 ~ "Non-Hispanic Asian",
      hispallp_a %in% c(5, 6) ~ "Non-Hispanic AIAN",
      hispallp_a == 7 ~ "Other/Multiple Races",
      TRUE ~ "Other/Not Reported"
    ),
    so_cat = recode_factor(
      orient_a,
      `1` = "Gay/Lesbian",
      `2` = "Straight",
      `3` = "Bisexual",
      `4` = "Something else",
      .default = "Unknown"
    ),
    inc_cat = recode_factor(
      ratcat_a,
      `01` = "0.00 - 0.49", `02` = "0.50 - 0.74",
      `03` = "0.75 - 0.99", `04` = "1.00 - 1.24",
      `05` = "1.25 - 1.49", `06` = "1.50 - 1.74",
      `07` = "1.75 - 1.99", `08` = "2.00 - 2.49",
      `09` = "2.50 - 2.99", `10` = "3.00 - 3.49",
      `11` = "3.50 - 3.99", `12` = "4.00 - 4.49",
      `13` = "4.50 - 4.99", `14` = "5.00+",
      `98` = "Not Ascertained",
      .default = "Unknown"
    ),
    region_cat = recode_factor(
      region,
      `1` = "Northeast",
      `2` = "Midwest",
      `3` = "South",
      `4` = "West"
    ),
    edu_cat = recode_factor(
      educp_a,
      `00` = "No education / Kindergarten only",
      `01` = "Grade 1-11",
      `02` = "12th grade, no diploma",
      `03` = "GED or equivalent",
      `04` = "High school graduate",
      `05` = "Some college, no degree",
      `06` = "Assoc degree: occupational",
      `07` = "Assoc degree: academic",
      `08` = "Bachelor's degree",
      `09` = "Master's degree",
      `10` = "Doctoral or professional degree",
      .default = "Unknown"
    ),
    nchs_eqiv = factor(
      urbrrl,
      levels = 1:4,
      labels = c(
        "Large central metro",
        "Large fringe metro",
        "Medium & small metro",
        "Non-metro"
      ),
      ordered = TRUE
    )
  )

nhis_design <- svydesign(
  ids = ~ppsu,
  strata = ~pstrat,
  weights = ~weight_adj,
  data = nhis,
  nest = TRUE
)

make_rows <- function(pretty_name, var) {
  formula_var <- as.formula(paste0("~", var, " + nchs_eqiv"))
  formula_one <- as.formula(paste0("~", var))

  wt_tab <- svytable(formula_var, nhis_design)
  overall_tab <- svytable(formula_one, nhis_design)

  rows <- map_dfr(rownames(overall_tab), function(level) {
    row_total <- as.numeric(overall_tab[level])
    overall_pct <- 100 * row_total / sum(overall_tab)

    cells <- map_chr(colnames(wt_tab), function(col) {
      nchs_val <- as.numeric(wt_tab[level, col])
      nchs_pct <- 100 * nchs_val / sum(wt_tab[, col])
      glue("{format(round(nchs_val), big.mark = ',')} ({sprintf('%.1f', nchs_pct)}%)")
    })

    tibble(
      row_lbl = glue("[{pretty_name}] {level}"),
      Overall = glue("{format(round(row_total), big.mark = ',')} ({sprintf('%.1f', overall_pct)}%)"),
      !!!set_names(cells, colnames(wt_tab))
    )
  })

  p_val <- tryCatch(
    formatC(svychisq(formula_var, nhis_design)$p.value, format = "e", digits = 2),
    error = function(e) NA_character_
  )
  rows$`P value` <- p_val
  rows
}

var_map <- list(
  age_band = "Age",
  sex_cat = "Sex",
  race_eth = "R/E",
  so_cat = "S/O",
  inc_cat = "Income Ratio",
  region_cat = "Region",
  edu_cat = "Education"
)

table_rows <- imap_dfr(var_map, make_rows)

out_csv <- path("03_analysis", "table1_nhis_demographics_weighted_corrected.csv")
write_csv(table_rows, out_csv)

metadata <- parsed_years %>%
  mutate(
    n_years_used_for_weight_adjustment = n_years,
    unweighted_medicare_n = nrow(nhis)
  )
write_csv(metadata, path("03_analysis", "table1_nhis_demographics_weighted_corrected_metadata.csv"))

gt_tbl <- table_rows %>%
  gt(rowname_col = "row_lbl") %>%
  tab_stubhead("Characteristic") %>%
  cols_label(
    Overall = md("Overall<br>Average annual weighted n (%)"),
    `Large central metro` = md("LCM<br>Average annual weighted n (%)"),
    `Large fringe metro` = md("LFM<br>Average annual weighted n (%)"),
    `Medium & small metro` = md("MSM<br>Average annual weighted n (%)"),
    `Non-metro` = md("Non-metro<br>Average annual weighted n (%)"),
    `P value` = md("*P*")
  ) %>%
  tab_options(table.font.names = "Arial", data_row.padding = px(2))

out_html <- path("04_figures", "table1_nhis_demographics_weighted_corrected.html")
gtsave(gt_tbl, out_html)

cat("Parsed NHIS years:", paste(sort(unique(nhis$year)), collapse = ", "), "\n")
cat("Unweighted Medicare respondents:", nrow(nhis), "\n")
cat("Saved CSV:", out_csv, "\n")
cat("Saved HTML:", out_html, "\n")
cat("=== finished", as.character(Sys.time()), "\n")
