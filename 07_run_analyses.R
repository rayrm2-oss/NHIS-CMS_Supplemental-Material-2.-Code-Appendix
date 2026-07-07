#!/usr/bin/env Rscript
# Runs the full analysis workflow.

scripts <- c(
  "04b_demographics_weighted.R",
  "04c_skincancer_weighted.R",
  "04a_hcpcs_code_set_audit.R",
  "03_transform.R",
  "04d_cms_provider_table.R",
  "05_compare_models.R",
  "06_exploratory_analysis.R",
  "06b_exploratory_analysis_mod.R"
)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1])))
} else {
  file.path(getwd(), "scripts")
}

project_root <- normalizePath(file.path(script_dir, ".."))
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

for (script in scripts) {
  script_path <- file.path("scripts", script)
  message("Running ", script_path)
  status <- system2("Rscript", script_path)
  if (!identical(status, 0L)) {
    stop("Workflow script failed: ", script)
  }
}

message("All analysis scripts completed.")
