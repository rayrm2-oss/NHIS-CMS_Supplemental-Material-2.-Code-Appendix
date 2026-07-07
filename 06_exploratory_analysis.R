#!/usr/bin/env Rscript
# 06_exploratory_analysis.R

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

log_file <- path("log_06_exploratory_analysis_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 06_exploratory_analysis_mohs_ebt.R started", as.character(Sys.time()), "\n")

analytic <- read_rds(path("03_analysis", "analytic_dataset_mohs_ebt.rds")) |>
  filter(
    !is.na(skin_prev),
    between(prop_prevcare_byprovider, 0, 1),
    between(prop_treat_byprovider, 0, 1)
  )

mod1 <- lm(skin_prev ~ prop_prevcare_byprovider + factor(nchs_eqiv) + year, data = analytic)
mod2 <- lm(prop_treat_byprovider ~ factor(nchs_eqiv) + year, data = analytic)
mod3 <- lm(skin_prev ~ prop_treat_byprovider + factor(nchs_eqiv) + year, data = analytic)
mod4 <- lm(skin_prev ~ prop_mohs_byprovider + factor(nchs_eqiv) + year, data = analytic)
mod5 <- lm(skin_prev ~ factor(nchs_eqiv) * year, data = analytic)

models <- list(
  mod1_skinprev_vs_prevention = tidy(mod1),
  mod2_expanded_treatment_by_nchs = tidy(mod2),
  mod3_skinprev_vs_expanded_treatment = tidy(mod3),
  mod4_skinprev_vs_mohs = tidy(mod4),
  mod5_skinprev_by_nchs_time = tidy(mod5)
)
purrr::iwalk(models, ~ write_csv(.x, path("03_analysis", paste0("summary_", .y, "_mohs_ebt.csv"))))

plot_and_save <- function(plot, filename, width = 8, height = 5) {
  ggsave(path("03_analysis", filename), plot = plot, width = width, height = height)
}

plot_and_save(
  ggplot(analytic, aes(prop_prevcare_byprovider, skin_prev)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", color = "blue") +
    facet_wrap(~ nchs_eqiv) +
    labs(
      title = "Skin Cancer History vs Preventive Procedure Share",
      x = "Preventive procedure beneficiary-service proportion",
      y = "NHIS skin cancer history proportion"
    ) +
    theme_minimal(),
  "plot1_prevcare_vs_skinprev_mohs_ebt.png"
)

plot_and_save(
  ggplot(analytic, aes(factor(nchs_eqiv), prop_treat_byprovider)) +
    geom_boxplot() +
    labs(
      title = "Expanded Treatment Share by NCHS Region",
      x = "NCHS category",
      y = "Expanded treatment beneficiary-service proportion"
    ) +
    theme_minimal(),
  "plot2_expanded_treatment_by_region_mohs_ebt.png"
)

plot_and_save(
  ggplot(analytic, aes(prop_treat_byprovider, skin_prev)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", color = "darkred") +
    facet_wrap(~ nchs_eqiv) +
    labs(
      title = "Skin Cancer History vs Expanded Treatment Share",
      x = "Expanded treatment beneficiary-service proportion",
      y = "NHIS skin cancer history proportion"
    ) +
    theme_minimal(),
  "plot3_expanded_treatment_vs_skinprev_mohs_ebt.png"
)

plot_and_save(
  ggplot(analytic, aes(prop_prevcare_byprovider, prop_treat_byprovider)) +
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm") +
    labs(
      title = paste("Prevention vs Expanded Treatment Share; correlation =", round(cor(analytic$prop_prevcare_byprovider, analytic$prop_treat_byprovider), 3)),
      x = "Preventive procedure beneficiary-service proportion",
      y = "Expanded treatment beneficiary-service proportion"
    ) +
    theme_minimal(),
  "plot4_prev_vs_expanded_treatment_mohs_ebt.png"
)

plot_and_save(
  ggplot(analytic, aes(year, skin_prev, color = factor(nchs_eqiv))) +
    geom_point(alpha = 0.35) +
    geom_smooth(method = "lm") +
    labs(
      title = "Skin Cancer History Proportion Over Time by NCHS Region",
      x = "Year",
      y = "NHIS skin cancer history proportion"
    ) +
    theme_minimal(),
  "plot5_skinprev_over_time_mohs_ebt.png"
)

cat("Saved exploratory model summaries and plots.\n")
cat("=== finished", as.character(Sys.time()), "\n")
