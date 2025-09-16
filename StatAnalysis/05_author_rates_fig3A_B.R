# 05_author_rates_fig3A_B.R — Fig.3A pooled author success; optional 3B OLS
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr)
})

td <- readr::read_csv("trial_data.csv", show_col_types = FALSE)

# Harmonize labels
td1 <- td %>%
  mutate(
    setting_clean = case_when(str_detect(str_to_lower(setting), "fine") ~ "Fine-tuned",
                              str_detect(str_to_lower(setting), "few|in[- ]?context|fs") ~ "Few-shot",
                              TRUE ~ as.character(setting)),
    judge_type_clean = case_when(str_to_lower(judge_type) %in% c("expert","experts") ~ "Expert",
                                 str_to_lower(judge_type) %in% c("lay","lay readers","layreader","lay_readers") ~ "Lay",
                                 TRUE ~ as.character(judge_type)),
    writer_type_clean = case_when(str_detect(str_to_lower(writer_type), "human") ~ "Human", TRUE ~ as.character(writer_type)),
    outcome_clean = case_when(str_to_lower(outcome) %in% c("fidelity","style","stylistic","style_fidelity") ~ "fidelity",
                              str_to_lower(outcome) %in% c("quality","prose_quality") ~ "quality",
                              TRUE ~ as.character(outcome))
  )

# Keep Fine-tuned, AI rows only, both judge groups
ft_ai <- td1 %>%
  filter(setting_clean == "Fine-tuned",
         writer_type_clean != "Human",
         judge_type_clean %in% c("Expert","Lay"),
         outcome_clean %in% c("fidelity","quality"),
         !is.na(choice))

stopifnot(nrow(ft_ai) > 0)

author_outcome <- ft_ai %>%
  group_by(prompt_id, outcome_clean) %>%
  summarise(
    n_trials = n(), ai_wins = sum(choice == 1L),
    n_expert = sum(judge_type_clean=="Expert"), k_expert = sum(choice == 1L & judge_type_clean=="Expert"),
    n_lay = sum(judge_type_clean=="Lay"),       k_lay    = sum(choice == 1L & judge_type_clean=="Lay"),
    .groups = "drop"
  ) %>%
  mutate(
    ai_win_rate_raw = ai_wins / n_trials,
    ai_win_rate_jeffreys = (ai_wins + 0.5) / (n_trials + 1),
    ci_low  = qbeta(0.025, ai_wins + 0.5, n_trials - ai_wins + 0.5),
    ci_high = qbeta(0.975, ai_wins + 0.5, n_trials - ai_wins + 0.5),
    expert_rate_raw      = ifelse(n_expert > 0, k_expert/n_expert, NA_real_),
    expert_rate_jeffreys = ifelse(n_expert > 0, (k_expert+0.5)/(n_expert+1), NA_real_),
    lay_rate_raw         = ifelse(n_lay > 0,    k_lay/n_lay, NA_real_),
    lay_rate_jeffreys    = ifelse(n_lay > 0,    (k_lay+0.5)/(n_lay+1), NA_real_),
    balanced_rate_jeffreys = ifelse(n_expert > 0 & n_lay > 0, 0.5*(expert_rate_jeffreys + lay_rate_jeffreys), NA_real_),
    tier = case_when(ci_low > 0.50 ~ "Easy (AI > Human)", ci_high < 0.50 ~ "Hard (Human > AI)", TRUE ~ "Ambiguous (≈ Parity)"),
    author = str_replace_all(prompt_id, "_", " ")
  ) %>%
  group_by(outcome_clean) %>%
  arrange(desc(ai_win_rate_jeffreys), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(planned_trials = 24L, complete_trials = n_trials == planned_trials)

fidelity_out <- author_outcome %>%
  filter(outcome_clean == "fidelity") %>%
  select(author, prompt_id, outcome=outcome_clean, rank, tier, n_trials, complete_trials,
         ai_wins, ai_win_rate_raw, ai_win_rate_jeffreys, ci_low, ci_high,
         n_expert, k_expert, expert_rate_raw, expert_rate_jeffreys,
         n_lay, k_lay, lay_rate_raw, lay_rate_jeffreys,
         balanced_rate_jeffreys)

quality_out <- author_outcome %>%
  filter(outcome_clean == "quality") %>%
  select(author, prompt_id, outcome=outcome_clean, rank, tier, n_trials, complete_trials,
         ai_wins, ai_win_rate_raw, ai_win_rate_jeffreys, ci_low, ci_high,
         n_expert, k_expert, expert_rate_raw, expert_rate_jeffreys,
         n_lay, k_lay, lay_rate_raw, lay_rate_jeffreys,
         balanced_rate_jeffreys)

write_csv(fidelity_out, "panel3a_author_success_pooled_fidelity.csv")
write_csv(quality_out,  "panel3a_author_success_pooled_quality.csv")
message("Wrote panel3a_author_success_pooled_{fidelity,quality}.csv")

# ---------- Optional: Fig.3B OLS if pairs CSVs exist ----------
safe_read <- function(p) if (file.exists(p)) readr::read_csv(p, show_col_types = FALSE) else NULL

analyze_pairs_fixed <- function(df, label, y_col) {
  if (is.null(df)) { message("Skip 3B ", label, ": file not found."); return(invisible(NULL)) }
  tok_col <- if ("tokens_millions" %in% names(df)) "tokens_millions"
             else if ("tokens" %in% names(df)) "tokens" else stop("No token column.")
  df2 <- df %>% transmute(tokens_millions = if (tok_col=="tokens_millions") as.numeric(.data[[tok_col]]) else as.numeric(.data[[tok_col]])/1e6,
                          win_rate = as.numeric(.data[[y_col]])) %>% filter(is.finite(tokens_millions), is.finite(win_rate))
  if (nrow(df2) < 5) { message("Insufficient rows for 3B (", label, ")."); return(invisible(NULL)) }
  ols <- lm(win_rate ~ tokens_millions, data = df2)
  Vhc3 <- sandwich::vcovHC(ols, type="HC3")
  ct <- lmtest::coeftest(ols, vcov.=Vhc3)["tokens_millions", , drop=FALSE]
  message(sprintf("3B %s — slope(HC3)=%.5f  SE=%.5f  p=%.4f",
                  label, ct[1,"Estimate"], ct[1,"Std. Error"], ct[1,"Pr(>|t|)"]))
  invisible(NULL)
}

pairs_fid <- safe_read("author_tokens_fidelity_winrate_pairs.csv")
pairs_qlt <- safe_read("author_tokens_quality_winrate_pairs.csv")
if (!is.null(pairs_fid)) analyze_pairs_fixed(pairs_fid, "Fidelity", "win_rate_fidelity")
if (!is.null(pairs_qlt)) analyze_pairs_fixed(pairs_qlt, "Quality",  "win_rate_quality")
