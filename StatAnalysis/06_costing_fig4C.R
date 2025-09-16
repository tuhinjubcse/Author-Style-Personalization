# 06_costing_fig4C.R — summarize costs for Fig.4C
suppressPackageStartupMessages({ library(readr); library(dplyr); library(stringr) })

paths <- c("ai_costs.csv", "data/processed/ai_costs.csv", "../ai_costs.csv", "/mnt/data/ai_costs.csv")
cost_path <- NULL; for (p in paths) if (file.exists(p)) { cost_path <- p; break }
if (is.null(cost_path)) stop("ai_costs.csv not found in any known location.")

costs_raw <- readr::read_csv(cost_path, show_col_types = FALSE)
costs <- costs_raw %>%
  rename_with(~str_replace_all(., "\"", "")) %>%
  rename(author_from_figure = dplyr::any_of(c("author_from_figure","author"))) %>%
  mutate(across(ends_with("_usd"), suppressWarnings(~as.numeric(.x))))

summ_cost <- costs %>%
  summarise(
    n = n(),
    fine_tune_min = min(fine_tune_cost_usd, na.rm=TRUE),
    fine_tune_med = median(fine_tune_cost_usd, na.rm=TRUE),
    fine_tune_max = max(fine_tune_cost_usd, na.rm=TRUE),
    total_min = min(total_ai_training_cost_usd, na.rm=TRUE),
    total_med = median(total_ai_training_cost_usd, na.rm=TRUE),
    total_max = max(total_ai_training_cost_usd, na.rm=TRUE)
  )
readr::write_csv(summ_cost, "fig4c_cost_summary.csv")
message("06_costing_fig4C.R wrote: fig4c_cost_summary.csv")
