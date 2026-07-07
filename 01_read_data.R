#!/usr/bin/env Rscript
# 01_read_data.R — read raw CMS & NHIS CSVs (2019-2023) → .rds
# ------------------------------------------------------------------
debug_mode <- FALSE                  # set TRUE for <1 % sample during dev
options(stringsAsFactors = FALSE)
set.seed(42)

#---  packages ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(here)
  library(fs)
})

#---  logging ----
log_path <- here("log_01_read_data.txt")
sink(log_path, split = TRUE)

cat("=== 01_read_data.R started at", Sys.time(), "===\n")

#---  helper: fast fread wrapper that returns tibble --------------
read_csv_fast <- function(path, n_max = Inf) {
  data.table::fread(path, nThread = 4, nrows = n_max) |>
    as_tibble(.name_repair = "minimal")
}

#---  discover raw files ------------------------------------------
cms_paths <- dir_ls(here("01_data_raw"), regexp = "(?i)_provider_and_service_.*\\.csv$")
nhis_paths <- dir_ls(here("01_data_raw"), regexp = "adult.*\\.csv$")

stopifnot(length(cms_paths)  > 0, length(nhis_paths) > 0)

#---  optional down-sample for quick dev ---------------------------
sample_nmax <- if (debug_mode) 5000 else Inf

#---  read + save --------------------------------------------------
for (fp in cms_paths) {
  yr <- str_extract(fp, "\\d{4}")
  out <- read_csv_fast(fp, n_max = sample_nmax)
  write_rds(out, here("01_data_raw", gsub(".csv$", ".rds", basename(fp))), compress = "xz")
  cat("CMS", yr, "rows:", nrow(out), "\n")
}

for (fp in nhis_paths) {
  yr <- paste0("20", str_extract(fp, "(?<=adult)\\d{2}"))
  out <- read_csv_fast(fp, n_max = sample_nmax)
  write_rds(out, here("01_data_raw", gsub(".csv$", ".rds", basename(fp))), compress = "xz")
  cat("NHIS", yr, "rows:", nrow(out), "\n")
}

#---  n-counts tracker ---------------------------------------------
n_counts <- tibble(
  step   = "01_read_data",
  file   = basename(c(cms_paths, nhis_paths)),
  n_rows = map_int(cms_paths  %>% c(nhis_paths), ~ nrow(read_rds(here("01_data_raw", gsub(".csv$", ".rds", basename(.x))))))
)
write_csv(n_counts, here("n_counts.csv"))

cat("=== finished at", Sys.time(), "===\n")
sink()
