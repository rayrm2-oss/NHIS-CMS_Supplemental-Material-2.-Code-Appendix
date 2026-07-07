#!/usr/bin/env Rscript
# 05_compare_models_mohs_ebt.R
# Compares model behavior using analytic_dataset_mohs_ebt.rds.

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(broom)
  library(ggplot2)
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

log_file <- path("log_05_compare_models_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 05_compare_models_mohs_ebt.R started", as.character(Sys.time()), "\n")

analytic <- read_rds(path("03_analysis", "analytic_dataset_mohs_ebt.rds")) |>
  filter(
    !is.na(skin_prev),
    between(prop_prevcare_byprovider, 0, 1),
    between(prop_treat_byprovider, 0, 1)
  )

mod_lm_prev <- lm(
  skin_prev ~ prop_prevcare_byprovider + factor(nchs_eqiv) + year,
  data = analytic
)
mod_lm_treat <- lm(
  skin_prev ~ prop_treat_byprovider + factor(nchs_eqiv) + year,
  data = analytic
)
mod_lm_mohs <- lm(
  skin_prev ~ prop_mohs_byprovider + factor(nchs_eqiv) + year,
  data = analytic
)

results <- bind_rows(
  tidy(mod_lm_prev) |> mutate(model = "lm_skin_prev_vs_prevention"),
  tidy(mod_lm_treat) |> mutate(model = "lm_skin_prev_vs_expanded_treatment"),
  tidy(mod_lm_mohs) |> mutate(model = "lm_skin_prev_vs_mohs")
)

if (requireNamespace("betareg", quietly = TRUE)) {
  n <- nrow(analytic)
  analytic_beta <- analytic |>
    mutate(skin_prev_beta = (skin_prev * (n - 1) + 0.5) / n)

  mod_beta_prev <- betareg::betareg(
    skin_prev_beta ~ prop_prevcare_byprovider + factor(nchs_eqiv) + year,
    data = analytic_beta
  )
  mod_beta_treat <- betareg::betareg(
    skin_prev_beta ~ prop_treat_byprovider + factor(nchs_eqiv) + year,
    data = analytic_beta
  )

  results <- bind_rows(
    results,
    tidy(mod_beta_prev) |> mutate(model = "betareg_skin_prev_vs_prevention"),
    tidy(mod_beta_treat) |> mutate(model = "betareg_skin_prev_vs_expanded_treatment")
  )
} else {
  cat("Package betareg is not installed; beta regression comparison skipped.\n")
}

write_csv(results, path("03_analysis", "model_comparison_mohs_ebt.csv"))

ggplot(analytic, aes(x = prop_treat_byprovider, y = skin_prev)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", color = "darkred") +
  facet_wrap(~ nchs_eqiv) +
  labs(
    title = "Skin Cancer History Proportion vs Expanded Treatment Share",
    x = "Expanded treatment beneficiary-service proportion",
    y = "NHIS skin cancer history proportion"
  ) +
  theme_minimal()
ggsave(path("03_analysis", "scatter_expanded_treatment_vs_skinprev_mohs_ebt.png"), width = 8, height = 5)

cat("Saved model comparison and scatter plot.\n")
cat("=== finished", as.character(Sys.time()), "\n")
