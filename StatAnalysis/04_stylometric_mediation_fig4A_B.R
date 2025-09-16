# 04_stylometric_mediation_fig4A_B.R
# Fig. 4A (Stage 1: stylometrics → pangram) and Fig. 4B (Stage 2: pangram → choice)
# Robust SEs (HC3 for OLS; CR2 for GLMs), clean mappings, and consistent CSV outputs.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(sandwich)     # HC3 for Stage 1
  library(clubSandwich) # CR2 for Stage 2
  library(lmtest)
  library(car)
  library(tibble)
})

# ───────────────────────────── helpers ─────────────────────────────
clip01 <- function(p, eps = 1e-6) pmin(pmax(p, eps), 1 - eps)
logit  <- function(p) log(p / (1 - p))
zscore <- function(x) {
  m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

map_setting <- function(x) {
  y <- tolower(as.character(x))
  dplyr::case_when(
    y %in% c("few-shot","fewshot","few_shot","fs","0") ~ "Few-shot",
    y %in% c("fine-tuned","finetuned","fine_tuned","ft","1") ~ "Fine-tuned",
    str_detect(y, "^few")  ~ "Few-shot",
    str_detect(y, "fine")  ~ "Fine-tuned",
    TRUE ~ "Few-shot"
  )
}

# Writer grouping to identify in-context vs fine-tuned trials
map_writer_group <- function(writer_type, excerpt_type, setting, excerpt_model) {
  wt <- tolower(as.character(writer_type))
  et <- tolower(as.character(excerpt_type))
  st <- tolower(map_setting(setting))
  em <- tolower(as.character(excerpt_model))
  out <- rep(NA_character_, length(wt))
  out[ grepl("human", wt) | grepl("^human$", et) ] <- "Human"
  ft_hit   <- grepl("finetune|fine[-_ ]?tuned", wt) | grepl("finetune|fine[-_ ]?tuned", em) | st == "fine-tuned"
  base_hit <- grepl("baseline", wt) | grepl("^ai$", et) | st == "few-shot"
  out[ is.na(out) & ft_hit ]   <- "AI_finetuned"
  out[ is.na(out) & base_hit ] <- "AI_incontext"
  out[ is.na(out) & st == "few-shot" ]   <- "AI_incontext"
  out[ is.na(out) & st == "fine-tuned" ] <- "AI_finetuned"
  out
}

tidy_robust_lm <- function(fit, vcov_mat) {
  est <- coef(fit); se <- sqrt(diag(vcov_mat))
  keep <- names(est) != "(Intercept)"
  tibble(
    term      = names(est)[keep],
    estimate  = unname(est[keep]),
    std_error = unname(se[keep]),
    conf_low  = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
}

# Safe, robust probability predictions with CR2 variance
predict_glm_probs <- function(model, vcov_mat, newdata) {
  # Drop response from terms so 'choice' isn't required in newdata
  trms <- delete.response(terms(model))
  
  # Align factor levels to the model's xlevels/contrasts
  nd <- newdata
  if (!is.null(model$xlevels)) {
    for (nm in names(model$xlevels)) {
      if (nm %in% names(nd)) {
        nd[[nm]] <- factor(nd[[nm]], levels = model$xlevels[[nm]])
      }
    }
  }
  
  X    <- model.matrix(trms, nd, contrasts.arg = model$contrasts)
  beta <- coef(model)
  eta  <- as.numeric(X %*% beta)
  
  V      <- as.matrix(vcov_mat)
  # Guard against small negative numerical noise
  Q      <- X %*% V %*% t(X)
  diag(Q) <- pmax(diag(Q), 0)
  se_eta <- sqrt(diag(Q))
  z      <- qnorm(0.975)
  
  tibble(
    prob       = plogis(eta),
    prob_lower = plogis(eta - z * se_eta),
    prob_upper = plogis(eta + z * se_eta)
  )
}

find_first_existing <- function(paths) {
  p <- paths[file.exists(paths)]
  if (length(p)) p[1] else NA_character_
}

# ───────────────────────────── load data ──────────────────────────
# Stylometric inputs (required)
sty_path <- find_first_existing(c("stylometric_data.csv", "../stylometric_data.csv"))
if (is.na(sty_path)) stop("stylometric_data.csv not found. Please run the stylometric export first.")
sty <- readr::read_csv(sty_path, show_col_types = FALSE)

# Trial-level inputs (prefer in-memory 'trial_df'; else look for CSVs)
if (!exists("trial_df")) {
  trial_path <- find_first_existing(c("trial_df.csv","./trial_df.csv","../trial_df.csv",
                                      "trial_data.csv","./trial_data.csv","../trial_data.csv"))
  if (is.na(trial_path)) {
    stop("No trial data found. Provide 'trial_df' in memory or a 'trial_df.csv' / 'trial_data.csv' file.")
  }
  trial_df <- readr::read_csv(trial_path, show_col_types = FALSE)
}

# ───────────── Stage 1: stylometrics → pangram (Fig. 4A) ─────────
needed_sty <- c("author","writer_type","setting","writer_name",
                "pangram_score","cliche_density","readability_ease","total_adjective_count")
stopifnot(all(needed_sty %in% names(sty)))

sty1 <- sty %>%
  mutate(
    setting         = map_setting(setting),
    pangram_clipped = clip01(pangram_score),
    logit_pangram   = logit(pangram_clipped),
    complexity      = 100 - readability_ease
  ) %>%
  mutate(
    z_cliche_density        = zscore(cliche_density),
    z_complexity            = zscore(complexity),
    z_total_adjective_count = zscore(total_adjective_count)
  )

# Few-shot sample: Humans + AI_incontext
fs_subset <- sty1 %>% filter(setting == "Few-shot", writer_type %in% c("Human","AI_incontext"))

# Fine-tuned sample: humans from authors that have AI_finetuned
ft_authors <- sty1 %>%
  filter(setting == "Fine-tuned", writer_type == "AI_finetuned") %>%
  distinct(author) %>% pull(author)

ft_subset <- sty1 %>%
  filter(author %in% ft_authors, writer_type %in% c("Human","AI_finetuned"))

# Sample counts (for logs/SI)
bind_rows(
  fs_subset %>% mutate(setting_stage1 = "Few-shot"),
  ft_subset %>% mutate(setting_stage1 = "Fine-tuned")
) %>%
  count(setting_stage1, writer_type, name = "n") %>%
  write_csv("panelB_stage1_sample_counts.csv")

fit_stage1_on_subset <- function(df_subset, setting_label) {
  # Predictors were z-scored above; drop any zero-variance
  candidates <- c("z_cliche_density","z_complexity","z_total_adjective_count")
  keep_vars <- candidates[sapply(df_subset[candidates], function(x) sd(x, na.rm = TRUE) > 0)]
  if (length(keep_vars) == 0) {
    warning(sprintf("All Stage 1 predictors have zero variance in '%s'. Skipping.", setting_label))
    return(tibble(setting = setting_label, feature = character(0),
                  estimate = numeric(0), std_error = numeric(0),
                  conf_low = numeric(0), conf_high = numeric(0), n_excerpts = nrow(df_subset)))
  }
  form <- as.formula(paste("logit_pangram ~", paste(keep_vars, collapse = " + ")))
  fit  <- lm(form, data = df_subset)
  vc   <- sandwich::vcovHC(fit, type = "HC3")
  
  tidy_robust_lm(fit, vc) %>%
    mutate(
      setting = setting_label,
      feature = dplyr::case_match(
        term,
        "z_cliche_density"        ~ "Cliché density (z)",
        "z_complexity"            ~ "Complexity = 100 − Flesch (z)",
        "z_total_adjective_count" ~ "Adjectives (count, z)",
        .default = term
      ),
      n_excerpts = nrow(df_subset)
    ) %>%
    select(setting, feature, estimate, std_error, conf_low, conf_high, n_excerpts)
}

stage1_results <- bind_rows(
  fit_stage1_on_subset(fs_subset, "Few-shot"),
  fit_stage1_on_subset(ft_subset, "Fine-tuned")
)
write_csv(stage1_results, "panelB_stage1_coeffs.csv")

top_features <- stage1_results %>%
  group_by(setting) %>%
  slice_max(order_by = abs(estimate), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    feature_code = case_match(
      feature,
      "Cliché density (z)"            ~ "z_cliche_density",
      "Complexity = 100 − Flesch (z)" ~ "z_complexity",
      "Adjectives (count, z)"         ~ "z_total_adjective_count",
      .default = NA_character_
    )
  ) %>%
  select(setting, feature, feature_code, estimate, std_error, conf_low, conf_high, n_excerpts)
write_csv(top_features, "panelB_stage1_top_features.csv")

message("Stage 1 complete. Wrote: panelB_stage1_coeffs.csv, panelB_stage1_top_features.csv, panelB_stage1_sample_counts.csv")

# ───────────── Stage 2: pangram → choice (Fig. 4B; SI S5.1) ───────
# Normalize trial fields; require pangram_score (either present in trial_df or attach fallback)
norm_trials <- trial_df %>%
  mutate(
    judge_type = case_when(
      judge_type %in% c("Expert","Lay") ~ judge_type,
      tolower(judge_type) == "expert" ~ "Expert",
      tolower(judge_type) == "lay"    ~ "Lay",
      TRUE ~ as.character(judge_type)
    ),
    judge_type = factor(judge_type, levels = c("Expert","Lay")),
    setting_chr = dplyr::case_when(
      is.numeric(setting) ~ ifelse(setting == 0, "Few-shot", "Fine-tuned"),
      setting %in% c("Few_shot","fewshot","Few-shot") ~ "Few-shot",
      setting %in% c("Fine_tuned","finetuned","Fine-tuned") ~ "Fine-tuned",
      TRUE ~ as.character(setting)
    ),
    setting = factor(setting_chr, levels = c("Few-shot","Fine-tuned")),
    judge_id = if (!"judge_id" %in% names(.)) factor(judge) else factor(judge_id)
  ) %>%
  filter(!is.na(choice),
         judge_type %in% c("Expert","Lay"),
         setting %in% c("Few-shot","Fine-tuned"))

# If pangram_score is missing or mostly NA, attach fallback by writer averages from stylometrics
needs_pangram <- !("pangram_score" %in% names(norm_trials)) ||
  mean(is.na(norm_trials$pangram_score)) > 0.5

if (needs_pangram) {
  # Prepare stylometric aggregates (carry pangram_score)
  sty2 <- sty1 %>%
    select(author, writer_name, pangram_score,
           z_cliche_density, z_complexity, z_total_adjective_count)
  sty_mean_by_writer <- sty2 %>%
    group_by(author, writer_name) %>%
    summarise(
      pangram_score            = mean(pangram_score, na.rm = TRUE),
      z_cliche_density        = mean(z_cliche_density, na.rm = TRUE),
      z_complexity            = mean(z_complexity, na.rm = TRUE),
      z_total_adjective_count = mean(z_total_adjective_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  trials_for_join <- norm_trials %>%
    mutate(
      writer_name_trial = case_when(
        tolower(as.character(excerpt_type)) == "human" ~ as.character(human_author),
        TRUE ~ as.character(excerpt_model)
      )
    )
  
  norm_trials <- trials_for_join %>%
    left_join(sty_mean_by_writer,
              by = c("prompt_id" = "author", "writer_name_trial" = "writer_name"))
}

# Clean pangram to numeric and clip
norm_trials <- norm_trials %>%
  mutate(pangram_score = clip01(as.numeric(pangram_score))) %>%
  filter(!is.na(pangram_score))

fit_pangram_glm_cr2 <- function(dat, outcome_label) {
  df <- dat %>% filter(outcome == !!outcome_label)
  if (nrow(df) == 0) return(NULL)
  
  mod <- glm(
    choice ~ pangram_score * setting * judge_type,
    family = binomial(link = "logit"),
    data   = df,
    control = list(maxit = 100)
  )
  vc  <- clubSandwich::vcovCR(mod, cluster = df$judge_id, type = "CR2")
  
  # Robust coefficient table with ORs and 95% CIs
  ct  <- lmtest::coeftest(mod, vcov. = vc)
  coef_tbl <- tibble(
    term     = rownames(ct),
    estimate = unname(ct[,1]),
    SE       = unname(ct[,2]),
    z        = unname(ct[,3]),
    p.value  = unname(ct[,4])
  ) %>%
    mutate(
      odds_ratio = exp(estimate),
      OR_lower   = exp(estimate - 1.96 * SE),
      OR_upper   = exp(estimate + 1.96 * SE),
      outcome    = outcome_label,
      N          = nrow(df),
      n_judges   = dplyr::n_distinct(df$judge_id)
    ) %>%
    relocate(outcome, N, n_judges, .before = term)
  
  # Type-III robust Wald
  a3 <- suppressWarnings(car::Anova(mod, type = "III", test = "Wald", vcov. = vc))
  a3_df <- as.data.frame(a3); a3_df$Effect <- rownames(a3_df)
  pcol <- names(a3_df)[grepl("Pr\\(>Chisq\\)|Pr\\(>Wald\\)", names(a3_df))]
  if (length(pcol) == 0) pcol <- "Pr(>Chisq)"
  type3_tbl <- tibble(
    outcome = outcome_label,
    Effect  = a3_df$Effect,
    Df      = if ("Df" %in% names(a3_df)) a3_df$Df else NA_real_,
    Chisq   = if ("Chisq" %in% names(a3_df)) a3_df$Chisq else NA_real_,
    p.value = a3_df[[pcol]]
  )
  
  list(model = mod, vcov = vc, data = df,
       coef_tbl = coef_tbl, type3_tbl = type3_tbl)
}

fits <- list(
  quality = fit_pangram_glm_cr2(norm_trials, "quality"),
  style   = fit_pangram_glm_cr2(norm_trials, "style")
)

# Write coefficient and Type-III tables (SI S5.1 + Fig4B copies)
coef_out <- bind_rows(
  if (!is.null(fits$quality)) fits$quality$coef_tbl,
  if (!is.null(fits$style))   fits$style$coef_tbl
)
type3_out <- bind_rows(
  if (!is.null(fits$quality)) fits$quality$type3_tbl,
  if (!is.null(fits$style))   fits$style$type3_tbl
)

if (nrow(coef_out)) {
  write_csv(coef_out, "S5_1_pangram_glm_coeffs.csv")
  write_csv(coef_out, "fig4B_pangram_glm_coeffs.csv")
}
if (nrow(type3_out)) {
  write_csv(type3_out, "S5_1_pangram_glm_typeIII.csv")
  write_csv(type3_out, "fig4B_pangram_glm_typeIII.csv")
}

cat("\n=== S5.1 / Fig4B — Pangram GLM (CR2) COEFFICIENTS ===\n")
print(coef_out)
cat("\n=== S5.1 / Fig4B — Type-III Robust Wald (CR2) ===\n")
print(type3_out)
cat("\nWrote:\n - S5_1_pangram_glm_coeffs.csv (also fig4B_pangram_glm_coeffs.csv)\n",
    " - S5_1_pangram_glm_typeIII.csv (also fig4B_pangram_glm_typeIII.csv)\n", sep = "")

# ─────────── Optional: predicted curves by pangram bins (Fig. 4B) ────────────
make_pred_grid <- function() {
  bins <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  expand.grid(
    pangram_score = bins,
    setting    = factor(c("Few-shot","Fine-tuned"), levels = c("Few-shot","Fine-tuned")),
    judge_type = factor(c("Expert","Lay"), levels = c("Expert","Lay"))
  ) %>% as_tibble()
}

pred_blocks <- list()
for (o in names(fits)) {
  fit <- fits[[o]]
  if (is.null(fit)) next
  nd <- make_pred_grid()
  pr <- predict_glm_probs(fit$model, fit$vcov, nd)
  pred_blocks[[o]] <- dplyr::bind_cols(
    tibble(outcome = o),
    nd,
    pr
  )
}
if (length(pred_blocks)) {
  pred_curves <- dplyr::bind_rows(pred_blocks)
  readr::write_csv(pred_curves, "fig4B_pred_by_pangram_bin.csv")
  cat("\nWrote: fig4B_pred_by_pangram_bin.csv\n")
}

cat("\n=== 04_stylometric_mediation_fig4A_B.R complete ===\n")
