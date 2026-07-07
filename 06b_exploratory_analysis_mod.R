#!/usr/bin/env Rscript
# 06b_exploratory_analysis_mod.R
# Aggregated means and normal-approximation CIs for analysis.

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
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

log_file <- path("log_06b_exploratory_analysis_mod_mohs_ebt.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("=== 06b_exploratory_analysis_mod_mohs_ebt.R started", as.character(Sys.time()), "\n")

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  tibble(mean = m, lower = m - 1.96 * se, upper = m + 1.96 * se, n = n)
}

analytic <- read_rds(path("03_analysis", "analytic_dataset_mohs_ebt.rds")) |>
  filter(
    !is.na(skin_prev),
    between(prop_prevcare_byprovider, 0, 1),
    between(prop_treat_byprovider, 0, 1)
  )

agg_data <- analytic |>
  group_by(nchs_eqiv) |>
  summarise(
    skin_prev_ci = list(mean_ci(skin_prev)),
    prevcare_ci = list(mean_ci(prop_prevcare_byprovider)),
    expanded_treat_ci = list(mean_ci(prop_treat_byprovider)),
    mohs_ci = list(mean_ci(prop_mohs_byprovider)),
    radiation_srt_ebt_ci = list(mean_ci(prop_radiation_srt_ebt_byprovider)),
    .groups = "drop"
  ) |>
  mutate(
    skin_prev_ci = purrr::map(skin_prev_ci, ~ rename(.x, skin_mean = mean, skin_lower = lower, skin_upper = upper, skin_n = n)),
    prevcare_ci = purrr::map(prevcare_ci, ~ rename(.x, prevcare_mean = mean, prevcare_lower = lower, prevcare_upper = upper, prevcare_n = n)),
    expanded_treat_ci = purrr::map(expanded_treat_ci, ~ rename(.x, treat_mean = mean, treat_lower = lower, treat_upper = upper, treat_n = n)),
    mohs_ci = purrr::map(mohs_ci, ~ rename(.x, mohs_mean = mean, mohs_lower = lower, mohs_upper = upper, mohs_n = n)),
    radiation_srt_ebt_ci = purrr::map(radiation_srt_ebt_ci, ~ rename(.x, radiation_mean = mean, radiation_lower = lower, radiation_upper = upper, radiation_n = n))
  ) |>
  unnest(c(skin_prev_ci, prevcare_ci, expanded_treat_ci, mohs_ci, radiation_srt_ebt_ci))

write_csv(agg_data, path("03_analysis", "06b_exploratory_analysis_mod_summary_agg_data_mohs_ebt.csv"))

ggplot(agg_data, aes(x = prevcare_mean, y = skin_mean, label = nchs_eqiv)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = skin_lower, ymax = skin_upper), width = 0.002) +
  geom_errorbarh(aes(xmin = prevcare_lower, xmax = prevcare_upper), height = 0.002) +
  geom_text(nudge_y = 0.001) +
  labs(
    title = "Aggregated CI: Skin Cancer History vs Prevention",
    x = "Mean preventive procedure share",
    y = "Mean skin cancer history proportion"
  ) +
  theme_minimal()
ggsave(path("03_analysis", "06b_plot1_ci_prevcare_vs_skinprev_mohs_ebt.png"), width = 7, height = 5)

ggplot(agg_data, aes(x = treat_mean, y = skin_mean, label = nchs_eqiv)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = skin_lower, ymax = skin_upper), width = 0.002) +
  geom_errorbarh(aes(xmin = treat_lower, xmax = treat_upper), height = 0.002) +
  geom_text(nudge_y = 0.001) +
  labs(
    title = "Aggregated CI: Skin Cancer History vs Expanded Treatment",
    x = "Mean expanded treatment share",
    y = "Mean skin cancer history proportion"
  ) +
  theme_minimal()
ggsave(path("03_analysis", "06b_plot2_ci_expanded_treat_vs_skinprev_mohs_ebt.png"), width = 7, height = 5)

ggplot(agg_data, aes(x = factor(nchs_eqiv), y = treat_mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = treat_lower, ymax = treat_upper), width = 0.2) +
  labs(
    title = "Aggregated CI: Expanded Treatment Share by Region",
    x = "NCHS category",
    y = "Mean expanded treatment share"
  ) +
  theme_minimal()
ggsave(path("03_analysis", "06b_plot3_ci_expanded_treat_by_region_mohs_ebt.png"), width = 7, height = 5)

cat("Saved aggregated summary and CI plots.\n")
cat("=== finished", as.character(Sys.time()), "\n")
