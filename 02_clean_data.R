#!/usr/bin/env Rscript
# 02_clean_data.R — audit & basic cleaning; writes clean_{year}.rds
# ------------------------------------------------------------------
debug_mode <- FALSE
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(tidyverse); library(janitor); library(here)
})

sink(here("log_02_clean_data.txt"), split = TRUE)
cat("=== 02_clean_data.R started", Sys.time(), "\n")

#--- read exceptions template --------------------------------------
exceptions <- yaml::read_yaml(here("05_docs/exceptions.yml"))

handle_missing <- function(df, var, method = c("median_impute", "listwise_delete", "drop")) {
  method <- match.arg(method)
  if (method == "median_impute" && is.numeric(df[[var]])) {
   med <- median(df[[var]], na.rm = TRUE)
    df[[var]][is.na(df[[var]])] <- med
 } else if (method == "listwise_delete") {
    df <- df %>% filter(!is.na(.data[[var]]))
  } else if (method == "drop") {
    df[[var]] <- NULL
  }
  df
}

cms_files <- dir_ls(here("01_data_raw"), regexp = "(?i)_Provider_and_Service_.*\\.rds$")
nhis_files <- dir_ls(here("01_data_raw"), regexp = "(?i)adult.*\\.rds$")

for (fp in c(cms_files, nhis_files)) {
  df  <- read_rds(fp)
  nm  <- basename(fp) %>% path_ext_remove()
  
  #--- NA audit -----------------------------------------------------
  na_summary <- df %>% summarize(across(everything(), ~ sum(is.na(.x)))) %>% pivot_longer(everything())
  write_csv(na_summary, here("02_data_clean", glue::glue("na_{nm}.csv")))
  
  #--- simple cleaning (trim & remove zero-row cols) ---------------
  df <- df %>% janitor::clean_names() %>% remove_empty("cols")
  
  #--- apply missing-handling rules --------------------------------
  for (var in names(exceptions$missing_handling)) {
    if (var %in% names(df)) {
      df <- handle_missing(df, var, exceptions$missing_handling[[var]]$method)
    }
  }
  
  #--- save cleaned rds --------------------------------------------
  write_rds(df, here("02_data_clean", glue::glue("clean_{nm}.rds")), compress = "xz")
  cat("Cleaned", nm, "rows:", nrow(df), "\n")
}

#--- update n-counts ----------------------------------------------
old <- read_csv(here("n_counts.csv"))
add <- tibble(
  step = "02_clean_data",
  file = basename(c(cms_files, nhis_files)),
  n_rows = map_int(c(cms_files, nhis_files), ~ nrow(read_rds(here("02_data_clean", glue::glue("clean_{path_ext_remove(basename(.x))}.rds")))))
)
write_csv(bind_rows(old, add), here("n_counts.csv"))

cat("=== finished", Sys.time(), "\n")
sink()
