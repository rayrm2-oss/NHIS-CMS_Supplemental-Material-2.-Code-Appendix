#!/usr/bin/env Rscript
# Runs the full corrected + Mohs/eBT-expanded analysis workflow.

scripts <- c(
  "04b_demographics_weighted_corrected.R",
  "04c_skincancer_weighted_corrected.R",
  "04a_hcpcs_code_set_audit_mohs_ebt.R",
  "03_transform_mohs_ebt.R",
  "04d_cms_provider_table_mohs_ebt.R",
  "05_compare_models_mohs_ebt.R",
  "06_exploratory_analysis_mohs_ebt.R",
  "06b_exploratory_analysis_mod_mohs_ebt.R"
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

message("All corrected + Mohs/eBT-expanded analysis scripts completed.")
