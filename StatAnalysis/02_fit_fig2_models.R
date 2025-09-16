# 02_fit_fig2_models.R
# -------------------------------------------------------------------
# Figures 2A/B (ORs + CIs) and 2C/D (predicted probs + CIs)
# Uses GLM + CR2 robust SEs and emmeans on the response scale
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(emmeans)
  library(sandwich)      # robust vcov (not used directly here, but fine to keep)
  library(clubSandwich)  # CR2 estimator
  library(lmtest)        # coeftest()
  library(car)           # Anova()
  library(readr)
})

# ------------------------------------------------------------------
# 0) Expect trial_data_clean in memory (from 01_build_data.R)
#    (If you prefer, you can uncomment the fallback loader)
# ------------------------------------------------------------------
# if (!exists("trial_data_clean")) {
#   trial_data_clean <- readr::read_csv("trial_data.csv", show_col_types = FALSE)
#   # Ensure factors if loading from CSV
#   trial_data_clean <- trial_data_clean %>%
#     mutate(
#       judge_type = factor(judge_type, levels = c("Expert", "Lay")),
#       writer_type = factor(writer_type, levels = c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline","GPT4o_finetuned")),
#       setting = factor(setting, levels = c("Few_shot","Fine_tuned")),
#       outcome = factor(outcome, levels = c("style","quality")),
#       judge_id = factor(judge_id),
#       prompt_id = factor(prompt_id)
#     )
# }

# ------------------------------------------------------------------
# 1) Helper: fit GLM and attach CR2 covariance (cluster = judge_id)
# ------------------------------------------------------------------
fit_glm_cr2 <- function(df, rhs_formula) {
  mod <- glm(rhs_formula, family = binomial(link = "logit"), data = df)
  vc  <- clubSandwich::vcovCR(mod, cluster = df$judge_id, type = "CR2")
  list(model = mod, vcov = vc)
}

# ------------------------------------------------------------------
# 2) Build each slice exactly once (same as your original)
# ------------------------------------------------------------------
slice_data <- function(outcome, setting, keep_levels) {
  trial_data_clean %>%
    filter(outcome == !!outcome,
           setting == !!setting,
           writer_type %in% keep_levels) %>%
    droplevels()
}

# ---- STYLE -------------------------------------------------------
style_few   <- slice_data("style",   "Few_shot",
                          c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline"))
style_fine  <- slice_data("style",   "Fine_tuned",
                          c("Human","GPT4o_finetuned"))

# ---- QUALITY -----------------------------------------------------
quality_few <- slice_data("quality", "Few_shot",
                          c("Human","GPT4o_baseline","Claude_baseline","Gemini_baseline"))
quality_fine<- slice_data("quality", "Fine_tuned",
                          c("Human","GPT4o_finetuned"))

# ------------------------------------------------------------------
# 3) Fit the four GLMs with robust SEs (same calls)
# ------------------------------------------------------------------
style_few_glm    <- fit_glm_cr2(style_few,    choice ~ writer_type * judge_type)
style_fine_glm   <- fit_glm_cr2(style_fine,   choice ~ writer_type * judge_type)
quality_few_glm  <- fit_glm_cr2(quality_few,  choice ~ writer_type * judge_type)
quality_fine_glm <- fit_glm_cr2(quality_fine, choice ~ writer_type * judge_type)

# Optional console dumps (unchanged)
cat("\nSTYLE • FEW-SHOT\n");    print(coeftest(style_few_glm$model,  style_few_glm$vcov))
cat("\nSTYLE • FINE-TUNED\n");  print(coeftest(style_fine_glm$model, style_fine_glm$vcov))
cat("\nQUALITY • FEW-SHOT\n");  print(coeftest(quality_few_glm$model,  quality_few_glm$vcov))
cat("\nQUALITY • FINE-TUNED\n");print(coeftest(quality_fine_glm$model, quality_fine_glm$vcov))

# ------------------------------------------------------------------
# 4) Preregistered contrasts (H1/H2) with Holm correction (same logic)
# ------------------------------------------------------------------
get_contrasts_cr <- function(mod_list,
                             hypothesis = c("H1", "H2"),
                             baseline_lvls = c("GPT4o_baseline","Claude_baseline","Gemini_baseline")) {
  hypothesis <- match.arg(hypothesis)
  
  emm <- emmeans(mod_list$model, ~ writer_type | judge_type, vcov. = mod_list$vcov)
  
  if (hypothesis == "H1") {
    wts <- c(Human = -1,
             setNames(rep( 1 / length(baseline_lvls), length(baseline_lvls)),
                      baseline_lvls))
  } else {
    wts <- c(Human = -1, GPT4o_finetuned = 1)
  }
  
  # Drop absent levels defensively
  wts <- wts[names(wts) %in% emm@grid$writer_type]
  
  contrast(emm, method = list(test = wts), by = "judge_type") %>%
    as.data.frame() %>%
    mutate(hypothesis = hypothesis)
}

style_contr   <- bind_rows(
  get_contrasts_cr(style_few_glm,  "H1"),
  get_contrasts_cr(style_fine_glm, "H2")
) %>% mutate(outcome = "style")

quality_contr <- bind_rows(
  get_contrasts_cr(quality_few_glm,  "H1"),
  get_contrasts_cr(quality_fine_glm, "H2")
) %>% mutate(outcome = "quality")

all_contrasts <- bind_rows(style_contr, quality_contr) %>%
  group_by(outcome, hypothesis) %>%
  mutate(p.holm = p.adjust(p.value, method = "holm")) %>%
  ungroup() %>%
  mutate(
    odds_ratio = exp(estimate),
    OR_lower   = exp(estimate - 1.96 * SE),
    OR_upper   = exp(estimate + 1.96 * SE),
    sig_raw    = p.value < .05,
    sig_holm   = p.holm  < .05
  )

readr::write_csv(all_contrasts, "fig2_or_ci.csv")

# ------------------------------------------------------------------
# 5) Predicted probabilities for display panels (C/D) — FIXED
#    Use emmeans on the response scale with CR2 vcov
# ------------------------------------------------------------------
.normalize_emm_cols <- function(df) {
  # Handle name variants across emmeans versions
  prob_col  <- intersect(c("prob","response","emmean"), names(df))
  lcl_col   <- intersect(c("asymp.LCL","lower.CL","LCL"), names(df))
  ucl_col   <- intersect(c("asymp.UCL","upper.CL","UCL"), names(df))
  df %>%
    rename(
      prob       = !!prob_col[1],
      prob_lower = !!lcl_col[1],
      prob_upper = !!ucl_col[1]
    ) %>%
    mutate(
      prob       = as.numeric(prob),
      prob_lower = as.numeric(prob_lower),
      prob_upper = as.numeric(prob_upper)
    )
}

pred_from_emm <- function(mod_list, outcome_label, setting_label) {
  # EMMs for writer_type within judge_type; response = probability of choice==1
  emm <- emmeans(mod_list$model, ~ writer_type | judge_type,
                 vcov. = mod_list$vcov, type = "response")
  out <- summary(emm, type = "response") %>%
    as.data.frame() %>%
    .normalize_emm_cols() %>%
    mutate(outcome = outcome_label, setting = setting_label)
  out[, c("outcome","setting","judge_type","writer_type","prob","prob_lower","prob_upper")]
}

style_preds   <- bind_rows(
  pred_from_emm(style_few_glm,   "style",   "Few_shot"),
  pred_from_emm(style_fine_glm,  "style",   "Fine_tuned")
)
quality_preds <- bind_rows(
  pred_from_emm(quality_few_glm, "quality", "Few_shot"),
  pred_from_emm(quality_fine_glm,"quality", "Fine_tuned")
)

pred_tbl <- bind_rows(style_preds, quality_preds) %>%
  mutate(model_label = dplyr::case_when(
    writer_type == "GPT4o_baseline"  ~ "GPT-4o (In-Context)",
    writer_type == "Claude_baseline" ~ "Claude (In-Context)",
    writer_type == "Gemini_baseline" ~ "Gemini (In-Context)",
    writer_type == "GPT4o_finetuned" ~ "GPT-4o (Fine-tuned)",
    writer_type == "Human"           ~ "Human",
    TRUE ~ as.character(writer_type)
  )) %>%
  # For panels, we keep AI rows (like your earlier selection), but write everything
  arrange(outcome, setting, judge_type, writer_type)

readr::write_csv(pred_tbl, "fig2_pred_probs.csv")

# ------------------------------------------------------------------
# 6) Interaction tests (robust): keep but protect against singularities
# ------------------------------------------------------------------
interaction_terms_individual <- function(mod_list, outcome_label, setting_label) {
  ct <- lmtest::coeftest(mod_list$model, mod_list$vcov)
  keep <- grepl(":", rownames(ct))
  if (!any(keep)) {
    return(tibble(outcome = character(0), setting = character(0),
                  term = character(0), estimate = numeric(0), se = numeric(0),
                  z = numeric(0), p = numeric(0)))
  }
  tibble(
    outcome  = outcome_label,
    setting  = setting_label,
    term     = rownames(ct)[keep],
    estimate = unname(ct[keep, 1]),
    se       = unname(ct[keep, 2]),
    z        = unname(ct[keep, 3]),
    p        = unname(ct[keep, 4])
  )
}

interaction_test_joint <- function(mod_list, outcome_label, setting_label) {
  jt <- trySuppressWarnings(
    car::Anova(mod_list$model, type = "III", test = "Wald", vcov. = mod_list$vcov)
  )
  if (inherits(jt, "try-error")) {
    return(tibble(outcome = outcome_label, setting = setting_label,
                  term = NA_character_, df = NA_real_, chisq = NA_real_, p = NA_real_))
  }
  jt_df <- as.data.frame(jt)
  jt_df$term <- rownames(jt_df)
  row <- dplyr::filter(jt_df, grepl(":", term))
  if (nrow(row) == 0L) {
    return(tibble(outcome = outcome_label, setting = setting_label,
                  term = NA_character_, df = NA_real_, chisq = NA_real_, p = NA_real_))
  }
  pcol <- names(row)[grepl("Pr\\(>Chisq\\)|Pr\\(>Wald\\)", names(row))]
  tibble(
    outcome = outcome_label,
    setting = setting_label,
    term    = row$term[1],
    df      = suppressWarnings(if ("Df" %in% names(row)) as.numeric(row$Df[1]) else NA_real_),
    chisq   = suppressWarnings(if ("Chisq" %in% names(row)) as.numeric(row$Chisq[1]) else NA_real_),
    p       = suppressWarnings(if (length(pcol)) as.numeric(row[[pcol[1]]]) else NA_real_)
  )
}

trySuppressWarnings <- function(expr) {
  try(suppressWarnings(expr), silent = TRUE)
}

ints_indiv <- bind_rows(
  interaction_terms_individual(style_few_glm,    "style",   "Few_shot"),
  interaction_terms_individual(style_fine_glm,   "style",   "Fine_tuned"),
  interaction_terms_individual(quality_few_glm,  "quality", "Few_shot"),
  interaction_terms_individual(quality_fine_glm, "quality", "Fine_tuned")
)

ints_joint <- bind_rows(
  interaction_test_joint(style_few_glm,    "style",   "Few_shot"),
  interaction_test_joint(style_fine_glm,   "style",   "Fine_tuned"),
  interaction_test_joint(quality_few_glm,  "quality", "Few_shot"),
  interaction_test_joint(quality_fine_glm, "quality", "Fine_tuned")
)

readr::write_csv(ints_indiv, "fig2_interactions_individual.csv")
readr::write_csv(ints_joint, "fig2_interactions_joint.csv")

cat("\n=== ANALYSIS 1 COMPLETE (Fig. 2) ===\n")
cat("Wrote: fig2_or_ci.csv, fig2_pred_probs.csv, fig2_interactions_*.csv\n")
