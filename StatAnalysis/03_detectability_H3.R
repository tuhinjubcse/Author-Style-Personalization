# 03_detectability_H3.R — H3 Pangram ↔ choice (CR2), Panel F bins (style/quality)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(sandwich)
  library(lmtest)
  library(clubSandwich)
  library(tibble)
})

# ---------- helpers ------------------------------------------------------------
vec <- function(x) as.numeric(x)           # drop matrix/list cols
nz   <- function(x) pmax(0, x)             # non-negative guard
map_setting_num <- function(x) {
  # Map 'Few_shot'/'Fine_tuned' (and common variants) → 0/1
  y <- tolower(as.character(x))
  case_when(
    y %in% c("few_shot","few-shot","fewshot","fs","0") ~ 0L,
    y %in% c("fine_tuned","fine-tuned","finetuned","ft","1") ~ 1L,
    grepl("^few", y)  ~ 0L,
    grepl("^fine", y) ~ 1L,
    TRUE ~ 0L
  )
}

# Fit GLM with CR2 clustered SEs (cluster = judge_id)
fit_h3 <- function(df) {
  m <- glm(choice ~ pangram_score * setting_num * judge_type,
           family = binomial, data = df)
  V <- clubSandwich::vcovCR(m, cluster = df$judge_id, type = "CR2")
  list(model = m, vcov = V)
}

# Build Panel-F predictions for one outcome; write CSV
by_outcome <- function(outc, outfile, bins = c(0.1, 0.3, 0.5, 0.7, 0.9)) {
  # Subset + minimal cleaning
  tmp <- trial %>%
    filter(outcome == outc) %>%
    drop_na(pangram_score, choice, judge_id)
  
  if (!"setting_num" %in% names(tmp)) {
    if ("setting" %in% names(tmp)) {
      tmp <- mutate(tmp, setting_num = map_setting_num(setting))
    } else {
      stop("Need either 'setting_num' or 'setting' in trial data.")
    }
  }
  
  # Normalize judge_type factor levels
  jt_levels <- c("Expert", "Lay")
  tmp <- tmp %>%
    mutate(
      judge_type = factor(as.character(judge_type), levels = jt_levels),
      setting_num = as.integer(setting_num)
    )
  
  # Fit
  fit <- fit_h3(tmp)
  
  # Console summary (robust)
  cat("\n==== H3 ROBUST COEFFICIENTS —", toupper(outc), "====\n")
  print(lmtest::coeftest(fit$model, fit$vcov))
  
  # Prediction grid (align factor levels to the model data)
  pred <- expand.grid(
    pangram_score = bins,
    setting_num   = c(0L, 1L),
    judge_type    = jt_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    mutate(judge_type = factor(judge_type, levels = levels(tmp$judge_type)))
  
  # Linear predictor + robust SEs (make sure we produce vectors)
  X  <- model.matrix(~ pangram_score * setting_num * judge_type, data = pred)
  lp <- vec(X %*% coef(fit$model))
  Vp <- X %*% fit$vcov %*% t(X)
  se <- sqrt(nz(vec(diag(Vp))))
  
  pred_out <- pred %>%
    mutate(
      prob       = vec(plogis(lp)),
      prob_lower = vec(plogis(lp - 1.96 * se)),
      prob_upper = vec(plogis(lp + 1.96 * se)),
      setting    = ifelse(setting_num == 0L, "Few-shot", "Fine-tuned"),
      condition  = paste0(ifelse(setting_num == 0L, "In-Context", "Fine-tuned"),
                          " (", as.character(judge_type), ")"),
      bin_label  = cut(pangram_score,
                       breaks = c(0, .2, .4, .6, .8, 1),
                       labels = c("0.0–0.2","0.2–0.4","0.4–0.6","0.6–0.8","0.8–1.0"),
                       include.lowest = TRUE, right = TRUE)
    ) %>%
    # Create outcome column as a literal value, not a column rename
    mutate(outcome = outc, .before = 1) %>%
    select(
      outcome, bin = pangram_score, bin_label, judge_type, setting_num, setting,
      condition, prob, prob_lower, prob_upper
    )
  
  # Final guard: forbid list/matrix columns
  stopifnot(!any(vapply(pred_out, function(z) is.list(z) || is.matrix(z), logical(1))))
  
  readr::write_csv(pred_out, outfile)
  message("Wrote: ", normalizePath(outfile))
}

# ---------- main --------------------------------------------------------------
# Prefer trial_data_with_pangram.csv; otherwise stop (upstream script should write it)
trial <- readr::read_csv("trial_data_with_pangram.csv", show_col_types = FALSE)

by_outcome("style",   "fig4_panelF_style.csv")
by_outcome("quality", "fig4_panelF_quality.csv")
