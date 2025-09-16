# 07_power_analysis.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(stringr); library(ggplot2)
  library(emmeans); library(lmtest); library(sandwich); library(clubSandwich)
})

# ------------------------ helpers reused from analysis ------------------------
fit_glm_cr2 <- function(df, rhs_formula) {
  mod <- glm(rhs_formula, family = binomial(link = "logit"), data = df)
  vc  <- clubSandwich::vcovCR(mod, cluster = df$judge_id, type = "CR2")
  list(model = mod, vcov = vc)
}

get_contrasts_cr <- function(mod_list,
                             hypothesis = c("H1", "H2"),
                             baseline_lvls = c("GPT4o_baseline","Claude_baseline","Gemini_baseline")) {
  hypothesis <- match.arg(hypothesis)
  emm <- emmeans(mod_list$model, ~ writer_type | judge_type, vcov. = mod_list$vcov)
  if (hypothesis == "H1") {
    wts <- c(Human = -1, setNames(rep(1/length(baseline_lvls), length(baseline_lvls)), baseline_lvls))
  } else {
    wts <- c(Human = -1, GPT4o_finetuned = 1)
  }
  wts <- wts[names(wts) %in% emm@grid$writer_type]
  contrast(emm, method = list(test = wts), by = "judge_type") %>%
    as.data.frame() %>% mutate(hypothesis = hypothesis)
}

# ------------------------ load or rebuild trial data --------------------------
load_trial_data <- function() {
  if (file.exists("trial_data.csv")) {
    td <- readr::read_csv("trial_data.csv", show_col_types = FALSE)
  } else {
    # Minimal rebuild from final_data.csv (mirrors your 01 script)
    stopifnot(file.exists("final_data.csv"))
    raw <- readr::read_csv("final_data.csv", show_col_types = FALSE)
    
    convert_to_analysis_format <- function(data) {
      data %>%
        mutate(unique_row_id = row_number(),
               trial_id = paste0(judge, "_", unique_row_id, "_", metric)) %>%
        pivot_longer(cols = c("excerpt1_type","excerpt2_type"),
                     names_to = "excerpt_position", values_to = "excerpt_type") %>%
        mutate(excerpt_model = case_when(
          excerpt_position == "excerpt1_type" ~ excerpt1_model,
          excerpt_position == "excerpt2_type" ~ excerpt2_model,
          TRUE ~ NA_character_
        ),
        excerpt_label = if_else(excerpt_position == "excerpt1_type", "Excerpt1", "Excerpt2"),
        choice = case_when(pref_source == excerpt_label ~ 1,
                           !is.na(pref_source) ~ 0, TRUE ~ NA_real_)) %>%
        mutate(writer_type = case_when(
          excerpt_type == "Human" ~ "Human",
          excerpt_type == "AI" & excerpt_model %in% c("GPT4o","GPT4") & condition == "fewshot" ~ "GPT4o_baseline",
          excerpt_type == "AI" & excerpt_model %in% c("Claude3.5Sonnet","Claude35Sonnet") & condition == "fewshot" ~ "Claude_baseline",
          excerpt_type == "AI" & excerpt_model %in% c("Gemini_1.5_Pro","Gemini1_5_Pro") & condition == "fewshot" ~ "Gemini_baseline",
          excerpt_type == "AI" & excerpt_model %in% c("GPT4_Finetuned","GPT4o_Finetuned") ~ "GPT4o_finetuned",
          TRUE ~ paste0(excerpt_type, "_", excerpt_model)
        ),
        setting = case_when(condition == "fewshot" ~ "Few_shot",
                            condition == "finetuned" ~ "Fine_tuned",
                            TRUE ~ condition),
        judge_type = case_when(panel == "expert" ~ "Expert",
                               panel == "lay" ~ "Lay", TRUE ~ panel),
        outcome = metric,
        judge_id = as.factor(judge),
        prompt_id = as.factor(target_author)) %>%
        select(trial_id, judge_id, judge_type, prompt_id,
               writer_type, setting, outcome, choice) %>%
        filter(!is.na(choice))
    }
    
    td <- convert_to_analysis_format(raw) %>%
      mutate(
        judge_type = factor(judge_type, levels = c("Expert","Lay")),
        writer_type = factor(writer_type,
                             levels = c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline","GPT4o_finetuned")),
        setting = factor(setting, levels = c("Few_shot","Fine_tuned")),
        outcome = factor(outcome, levels = c("style","quality"))
      ) %>%
      filter(writer_type %in% levels(writer_type))
    readr::write_csv(td, "trial_data.csv")
  }
  td
}

trial_data <- load_trial_data()

# Quick sanity echo
message("Trial rows: ", nrow(trial_data))
message("Judges (Expert/Lay): ", paste(levels(trial_data$judge_type), collapse = ", "))

# ------------------------ analysis on a given dataset -------------------------
fit_four_and_contrasts <- function(df) {
  slice_data <- function(outcome, setting, keep) {
    df %>% filter(outcome == !!outcome, setting == !!setting, writer_type %in% keep) %>% droplevels()
  }
  style_few   <- slice_data("style","Few_shot",   c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline"))
  style_fine  <- slice_data("style","Fine_tuned", c("Human","GPT4o_finetuned"))
  quality_few <- slice_data("quality","Few_shot",   c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline"))
  quality_fine<- slice_data("quality","Fine_tuned", c("Human","GPT4o_finetuned"))
  
  # Fit
  m_style_few    <- fit_glm_cr2(style_few,    choice ~ writer_type * judge_type)
  m_style_fine   <- fit_glm_cr2(style_fine,   choice ~ writer_type * judge_type)
  m_quality_few  <- fit_glm_cr2(quality_few,  choice ~ writer_type * judge_type)
  m_quality_fine <- fit_glm_cr2(quality_fine, choice ~ writer_type * judge_type)
  
  # Contrasts
  c_style <- bind_rows(
    get_contrasts_cr(m_style_few,  "H1"),
    get_contrasts_cr(m_style_fine, "H2")
  ) %>% mutate(outcome = "style")
  
  c_quality <- bind_rows(
    get_contrasts_cr(m_quality_few,  "H1"),
    get_contrasts_cr(m_quality_fine, "H2")
  ) %>% mutate(outcome = "quality")
  
  all_contr <- bind_rows(c_style, c_quality) %>%
    group_by(outcome, hypothesis) %>%
    mutate(p.holm = p.adjust(p.value, method = "holm")) %>%
    ungroup() %>%
    transmute(outcome, hypothesis, judge_type,
              estimate, SE, odds_ratio = exp(estimate),
              OR_lower = exp(estimate - 1.96*SE), OR_upper = exp(estimate + 1.96*SE),
              p.value, p.holm)
  all_contr
}

# ------------------------ resampling design -----------------------------------
# Sample judges within each audience, then sample their trials (with replacement)
resample_by_judge <- function(df, n_expert, n_lay, trials_per_judge = 20, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  one_group <- function(dg, n) {
    if (n <= 0) return(dg[0, ])
    js <- unique(dg$judge_id)
    pick <- sample(js, size = n, replace = (n > length(js)))
    bind_rows(lapply(pick, function(j) {
      dj <- dg %>% filter(judge_id == j)
      tids <- unique(dj$trial_id)
      k <- min(length(tids), trials_per_judge)
      take <- sample(tids, size = k, replace = (k > length(tids)))
      dj %>% filter(trial_id %in% take)
    }))
  }
  dexp <- df %>% filter(judge_type == "Expert")
  dlay <- df %>% filter(judge_type == "Lay")
  bind_rows(one_group(dexp, n_expert), one_group(dlay, n_lay)) %>% droplevels()
}

# ------------------------ power driver ----------------------------------------
# Returns per (outcome, hypothesis, judge_type) the rejection rate after Holm
power_simulate <- function(df, n_expert, n_lay, trials_per_judge = 20,
                           R = 1000, seed = 123) {
  set.seed(seed)
  res <- vector("list", R)
  for (r in seq_len(R)) {
    dsim <- resample_by_judge(df, n_expert, n_lay, trials_per_judge = trials_per_judge)
    # Guard against empty slices in small designs
    ok <- tryCatch({
      contr <- fit_four_and_contrasts(dsim)
      contr
    }, error = function(e) NULL)
    if (is.null(ok)) next
    res[[r]] <- ok %>%
      mutate(reject_holm = p.holm < 0.05) %>%
      select(outcome, hypothesis, judge_type, reject_holm)
  }
  out <- bind_rows(res)
  out %>%
    group_by(outcome, hypothesis, judge_type) %>%
    summarise(power = mean(reject_holm, na.rm = TRUE),
              reps   = n(), .groups = "drop") %>%
    mutate(n_expert = n_expert, n_lay = n_lay, trials_per_judge = trials_per_judge)
}

# ------------------------ canned scenarios ------------------------------------
# (A) Realized design
realized_counts <- trial_data %>%
  distinct(judge_id, judge_type) %>%
  count(judge_type) %>%
  tidyr::pivot_wider(names_from = judge_type, values_from = n, values_fill = 0)

n_exp_real <- realized_counts$Expert %||% 0
n_lay_real <- realized_counts$Lay %||% 0

# (B) Planned (update) design
n_exp_plan <- 25
n_lay_plan <- 120
t_per      <- 20

message("Realized judges — Expert: ", n_exp_real, "  Lay: ", n_lay_real)

# Run main scenarios (adjust R up to 2000+ for final SI numbers)
pow_real <- power_simulate(trial_data, n_exp_real, n_lay_real, trials_per_judge = t_per, R = 500, seed = 1001)
pow_plan <- power_simulate(trial_data, n_exp_plan, n_lay_plan, trials_per_judge = t_per, R = 500, seed = 2002)

readr::write_csv(pow_real, "si_power_realized.csv")
readr::write_csv(pow_plan, "si_power_planned.csv")

# (C) Sensitivity curves: vary judge counts
grid <- expand.grid(n_expert = c(10, 15, 20, 25, 30, 35, 40),
                    n_lay    = c(80, 100, 120, 150, 180, 200),
                    stringsAsFactors = FALSE)

pow_grid <- pmap_dfr(grid, ~power_simulate(trial_data, n_expert = ..1, n_lay = ..2,
                                           trials_per_judge = t_per, R = 300, seed = 3000 + ..1 + ..2))
readr::write_csv(pow_grid, "si_power_grid.csv")

# Simple plot for SI (optional)
p <- pow_grid %>%
  filter(hypothesis %in% c("H1","H2"), outcome %in% c("style","quality")) %>%
  mutate(group = paste0(outcome," • ",hypothesis," • ",judge_type)) %>%
  ggplot(aes(x = n_expert, y = power, group = interaction(group, n_lay))) +
  geom_line() + geom_point() +
  facet_grid(judge_type ~ outcome + hypothesis, scales = "free_y") +
  labs(x = "Number of Expert judges (Lay varied by color/line)",
       y = "Power (Holm-adjusted)",
       title = "Design power via judge-level resampling") +
  theme_minimal()
ggsave("si_power_curves.pdf", p, width = 10, height = 6)
message("Saved: si_power_realized.csv, si_power_planned.csv, si_power_grid.csv, si_power_curves.pdf")
