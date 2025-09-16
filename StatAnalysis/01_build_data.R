# 01_build_data.R

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(stringdist)
  library(tibble)
  library(tidyr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
})

# ----------------------- helpers -----------------------
clean_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[“”\"']", "") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_squish()
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# IMPORTANT: preserve uniqueness of judge IDs (no digit stripping!)
clean_judge <- function(nm) {
  nm <- as.character(nm)
  ifelse(is.na(nm) | nm == "", NA_character_, str_squish(nm))
}

extract_src  <- function(k, tag) (k[[tag]] %||% NA_character_)

# ===============================================================
#  1) PARSE JUDGMENT JSON  →  all_judgments
# ===============================================================
parse_file <- function(path) {
  meta <- str_split(basename(path), "[_.]", simplify = TRUE)
  metric    <- meta[1]; condition <- meta[2]; panel <- meta[3]
  
  fromJSON(path, simplifyVector = FALSE) |>
    imap_dfr(function(author_block, target_author) {
      imap_dfr(author_block, function(human_block, human_key) {
        human_author <- str_remove(human_key, "^written_by_")
        map_dfr(human_block, function(j) {
          winner   <- j[[1]]
          if (length(j) == 3) { judge_raw <- j[[2]]; rec <- j[[3]] }
          else                { rec <- j[[2]]; judge_raw <- rec$user }
          judge <- clean_judge(judge_raw)  # keep as-is (trim only)
          
          tibble(
            metric, condition, panel,
            target_author, human_author,
            judge_raw, judge, judgment_id = rec$id %||% NA_integer_,
            winner_label = winner,
            pref_source  = rec$Preference %||% rec$win %||% NA_character_,
            excerpt1_source = extract_src(rec$key, "Excerpt1"),
            excerpt2_source = extract_src(rec$key, "Excerpt2"),
            excerpt1_text   = rec$Excerpt1,
            excerpt2_text   = rec$Excerpt2
          )
        })
      })
    })
}

base_dir <- "../"
files <- file.path(base_dir, c(
  "quality_fewshot_expert.json",  "quality_fewshot_lay.json",
  "quality_finetuned_expert.json","quality_finetuned_lay.json",
  "style_fewshot_expert.json",    "style_fewshot_lay.json",
  "style_finetuned_expert.json",  "style_finetuned_lay.json"
))

all_judgments <- map_dfr(files, parse_file) |>
  mutate(
    excerpt1_clean = clean_text(excerpt1_text),
    excerpt2_clean = clean_text(excerpt2_text)
  )

# ===============================================================
#  2) BUILD CATALOGUE  →  excerpt_mapping
# ===============================================================
records <- fromJSON(file.path(base_dir, "Human-AI.json"), simplifyVector = FALSE)

excerpt_mapping <- map_dfr(records, function(rec) {
  ta <- rec$writer; rows <- list()
  add_row <- function(type, id, txt)
    rows <<- append(rows, list(tibble(
      target_author = ta,
      source_type   = type,
      source_id     = id,
      clean_excerpt = clean_text(txt)
    )))
  if (!is.null(rec$original))       add_row("Original", ta, rec$original)
  if (!is.null(rec$MFA))            imap(rec$MFA,           ~ add_row("Human", .y, .x))
  if (!is.null(rec$AI))             imap(rec$AI,            ~ add_row("AI",    .y, .x))
  if (!is.null(rec$GPT4_Finetuned)) add_row("AI", "GPT4_Finetuned", rec$GPT4_Finetuned)
  bind_rows(rows)
})

map1 <- excerpt_mapping |> 
  rename(excerpt1_clean = clean_excerpt,
         excerpt1_type  = source_type,
         excerpt1_model = source_id)

map2 <- excerpt_mapping |>
  rename(excerpt2_clean = clean_excerpt,
         excerpt2_type  = source_type,
         excerpt2_model = source_id)

# ===============================================================
#  3) EXACT JOIN
# ===============================================================
enriched <- all_judgments |>
  left_join(map1, by = c("target_author", "excerpt1_clean")) |>
  left_join(map2, by = c("target_author", "excerpt2_clean"))

# ===============================================================
#  4) REPAIR UNMATCHED WITH ≥97% LEVENSHTEIN RATIO (AI sides only)
# ===============================================================
repair_side <- function(df, side) {
  clean_col <- paste0(side, "_clean")
  type_col  <- paste0(side, "_type")
  model_col <- paste0(side, "_model")
  
  need_fix <- which(is.na(df[[model_col]]) & df[[paste0(side, "_source")]] == "AI")
  if (length(need_fix) == 0) return(df)
  
  for (i in need_fix) {
    ta  <- df$target_author[i]
    txt <- df[[clean_col]][i]
    pool <- excerpt_mapping |>
      filter(target_author == ta, source_type == "AI")
    if (nrow(pool) == 0) next
    sim <- stringsim(txt, pool$clean_excerpt, method = "lv")
    best <- which.max(sim)
    if (sim[best] >= 0.97) {
      df[[type_col]][i]  <- pool$source_type[best]
      df[[model_col]][i] <- pool$source_id[best]
    }
  }
  df
}

enriched <- enriched |> repair_side("excerpt1") |> repair_side("excerpt2")

# ===============================================================
#  5) FINAL WIDE DATA FOR MODELING  → final_data.csv
# ===============================================================
analysis_df <- enriched |>
  select(metric, condition, panel,
         target_author, human_author,
         judge, judgment_id,
         winner_label, pref_source,
         excerpt1_type, excerpt1_model,
         excerpt2_type, excerpt2_model)

write.csv(analysis_df, "final_data.csv", row.names = FALSE, quote = FALSE)

# ===============================================================
#  6) LONG TRIAL DATA (UNCHANGED STRUCTURE)
# ===============================================================
raw_data <- read.csv("final_data.csv", stringsAsFactors = FALSE)

convert_to_analysis_format <- function(data) {
  data_with_id <- data %>% mutate(unique_row_id = row_number())
  
  long_data <- data_with_id %>%
    mutate(trial_id = paste0(judge, "_", unique_row_id, "_", metric)) %>%
    pivot_longer(
      cols = c("excerpt1_type", "excerpt2_type"),
      names_to = "excerpt_position",
      values_to = "excerpt_type"
    ) %>%
    mutate(
      excerpt_model = case_when(
        excerpt_position == "excerpt1_type" ~ excerpt1_model,
        excerpt_position == "excerpt2_type" ~ excerpt2_model,
        TRUE ~ NA_character_
      ),
      excerpt_label = case_when(
        excerpt_position == "excerpt1_type" ~ "Excerpt1",
        excerpt_position == "excerpt2_type" ~ "Excerpt2",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      choice = case_when(
        pref_source == excerpt_label ~ 1,
        pref_source != excerpt_label & !is.na(pref_source) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      writer_type = case_when(
        excerpt_type == "Human" ~ "Human",
        excerpt_type == "AI" & excerpt_model == "GPT4o"            & condition == "fewshot"   ~ "GPT4o_baseline",
        excerpt_type == "AI" & excerpt_model == "Claude3.5Sonnet"  & condition == "fewshot"   ~ "Claude_baseline",
        excerpt_type == "AI" & excerpt_model == "Gemini_1.5_Pro"   & condition == "fewshot"   ~ "Gemini_baseline",
        excerpt_type == "AI" & excerpt_model == "GPT4_Finetuned"                               ~ "GPT4o_finetuned",
        excerpt_type == "AI" & excerpt_model == "GPT4"             & condition == "fewshot"   ~ "GPT4o_baseline",
        TRUE ~ paste0(excerpt_type, "_", excerpt_model)
      ),
      setting = case_when(
        condition == "fewshot"   ~ "Few_shot",
        condition == "finetuned" ~ "Fine_tuned",
        TRUE ~ condition
      ),
      judge_type = case_when(
        panel == "expert" ~ "Expert",
        panel == "lay"    ~ "Lay",
        TRUE ~ panel
      ),
      outcome   = metric,
      judge_id  = judge,               # use preserved unique ID
      prompt_id = target_author,
      excerpt_id = paste0(target_author, "_", excerpt_type, "_", excerpt_model,
                          "_", unique_row_id, "_", excerpt_position)
    ) %>%
    select(trial_id, unique_row_id, judge_id, judge_type, prompt_id, excerpt_id,
           writer_type, setting, outcome, choice, excerpt_model, excerpt_type,
           human_author, excerpt_position, pref_source, excerpt_label) %>%
    filter(!is.na(choice))
  
  long_data
}

trial_data <- convert_to_analysis_format(raw_data)

cat("=== CONVERSION RESULTS ===\n")
cat("Original rows:", nrow(raw_data), "\n")
cat("Expected converted rows:", nrow(raw_data) * 2, "\n")
cat("Actual converted rows:", nrow(trial_data), "\n")
cat("Missing rows:", (nrow(raw_data) * 2) - nrow(trial_data), "\n\n")
print(utils::head(trial_data, 10))

validate_conversion <- function(data) {
  cat("=== DATA VALIDATION REPORT ===\n\n")
  cat("Data dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")
  cat("=== FACTOR LEVELS ===\n")
  cat("Judge types:", paste(unique(data$judge_type), collapse = ", "), "\n")
  cat("Writer types:", paste(unique(data$writer_type), collapse = ", "), "\n")
  cat("Settings:", paste(unique(data$setting), collapse = ", "), "\n")
  cat("Outcomes:", paste(unique(data$outcome), collapse = ", "), "\n\n")
  cat("=== CHOICE DISTRIBUTION ===\n")
  print(table(data$choice)); cat("\n")
  cat("=== CONDITION BALANCE ===\n")
  print(table(data$writer_type, data$judge_type, data$outcome))
  cat("\n=== MISSING VALUES ===\n")
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  print(missing_counts[missing_counts > 0])
  cat("\n=== TRIAL STRUCTURE ===\n")
  trials_per_judge <- data %>%
    group_by(judge_id, outcome) %>%
    summarise(n_trials = n_distinct(trial_id), .groups = "drop")
  cat("Trials per judge (should be consistent):\n")
  print(summary(trials_per_judge$n_trials))
  cat("\nUnique trial IDs:", length(unique(data$trial_id)), "\n")
  cat("Expected unique trial IDs:", nrow(raw_data), "\n")
  invisible(NULL)
}

validate_conversion(trial_data)

trial_data_clean <- trial_data %>%
  mutate(
    judge_type = factor(judge_type, levels = c("Expert", "Lay")),
    writer_type = factor(
      writer_type,
      levels = c("Human", "GPT4o_baseline", "Claude_baseline", "Gemini_baseline", "GPT4o_finetuned")
    ),
    setting = factor(setting, levels = c("Few_shot", "Fine_tuned")),
    outcome = factor(outcome, levels = c("style", "quality")),
    judge_id = factor(judge_id),
    prompt_id = factor(prompt_id)
  ) %>%
  filter(writer_type %in% c("Human", "GPT4o_baseline", "Claude_baseline", "Gemini_baseline", "GPT4o_finetuned"))

cat("=== FINAL CLEANED DATA ===\n")
cat("Rows:", nrow(trial_data_clean), "\n")
cat("Expected rows:", nrow(raw_data) * 2, "\n")
cat("Data completeness:", round(nrow(trial_data_clean) / (nrow(raw_data) * 2) * 100, 2), "%\n")
cat("Writer types:", paste(levels(trial_data_clean$writer_type), collapse = ", "), "\n")

# ---- quick cluster sanity check (non-fatal) ---------------------
cluster_check <- trial_data_clean %>%
  distinct(judge_type, judge_id) %>%
  count(judge_type, name = "n_clusters")
print(cluster_check)
if (any(cluster_check$n_clusters <= 2)) {
  warning("Very few unique judge_id in at least one panel. ",
          "Did anonymization collapse distinct judges into the same ID?")
}

# ===============================================================
#  7) PANGRAM JOIN → trial_df
# ===============================================================
hjson <- fromJSON(file.path(base_dir, "Human-AI.json"), simplifyVector = FALSE)

pangram_df <- map_dfr(hjson, function(rec) {
  ta <- rec$writer; rows <- list()
  if (!is.null(rec$MFA_pangram)) {
    rows <- append(rows, imap(rec$MFA_pangram, function(p, writer) {
      tibble(target_author = ta, source_type = "Human", source_id = writer,
             ai_likelihood = p$ai_likelihood, pangram_pred = p$prediction)
    }))
  }
  if (!is.null(rec$AI_pangram)) {
    rows <- append(rows, imap(rec$AI_pangram, function(p, model) {
      tibble(target_author = ta, source_type = "AI", source_id = model,
             ai_likelihood = p$ai_likelihood, pangram_pred = p$prediction)
    }))
  }
  bind_rows(rows)
})

excerpt_with_pangram <- excerpt_mapping %>%
  inner_join(pangram_df, by = c("target_author", "source_type", "source_id"))

pangram_tbl <- excerpt_with_pangram %>%
  mutate(stem_id = str_c(target_author, source_type, source_id, sep = "_")) %>%
  transmute(stem_id, pangram_score = ai_likelihood)

trial_df <- trial_data_clean %>%
  mutate(
    stem_id = str_remove(excerpt_id, "_\\d+_excerpt\\d+_type$"),
    setting = if_else(setting == "Few_shot", 0L, 1L)
  ) %>%
  left_join(pangram_tbl, by = "stem_id")

cat("\n=== FINAL DISTRIBUTION ===\n")
print(table(trial_data_clean$outcome, trial_data_clean$judge_type, trial_data_clean$writer_type))

# ===============================================================
#  8) Stylometric mediation data  → stylometric_data.csv
# ===============================================================
data_med <- fromJSON(file.path(base_dir, "Human-AI_mediator_data.json"), simplifyVector = FALSE)

stylometric_data <- data.frame()

for (i in seq_along(data_med)) {
  author_obj  <- data_med[[i]]
  author_name <- author_obj$writer
  
  if ("MFA_pangram" %in% names(author_obj)) {
    for (mfa_writer in names(author_obj$MFA_pangram)) {
      pangram_info <- author_obj$MFA_pangram[[mfa_writer]]
      stylometric_data <- rbind(stylometric_data, data.frame(
        author = author_name, writer_type = "Human", setting = "Few-shot",
        writer_name = mfa_writer,
        pangram_score = ifelse(is.null(pangram_info$ai_likelihood), 0, pangram_info$ai_likelihood),
        cliche_density = ifelse(is.null(pangram_info$cliche_density), 0, pangram_info$cliche_density),
        readability_ease = ifelse(is.null(pangram_info$readability_ease), 0, pangram_info$readability_ease),
        total_adjective_count = ifelse(is.null(pangram_info$total_adjective_count), 0, pangram_info$total_adjective_count),
        num_cliches = length(pangram_info$cliches),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if ("AI_pangram" %in% names(author_obj)) {
    for (ai_model in names(author_obj$AI_pangram)) {
      pangram_info <- author_obj$AI_pangram[[ai_model]]
      if (ai_model == "GPT4_Finetuned") {
        writer_type <- "AI_finetuned"; setting <- "Fine-tuned"
      } else {
        writer_type <- "AI_incontext"; setting <- "Few-shot"
      }
      stylometric_data <- rbind(stylometric_data, data.frame(
        author = author_name, writer_type = writer_type, setting = setting,
        writer_name = ai_model,
        pangram_score = ifelse(is.null(pangram_info$ai_likelihood), 0, pangram_info$ai_likelihood),
        cliche_density = ifelse(is.null(pangram_info$cliche_density), 0, pangram_info$cliche_density),
        readability_ease = ifelse(is.null(pangram_info$readability_ease), 0, pangram_info$readability_ease),
        total_adjective_count = ifelse(is.null(pangram_info$total_adjective_count), 0, pangram_info$total_adjective_count),
        num_cliches = length(pangram_info$cliches),
        stringsAsFactors = FALSE
      ))
    }
  }
}

write.csv(stylometric_data, file = "stylometric_data.csv", row.names = FALSE, quote = FALSE)

# Save the long-format trials
readr::write_csv(trial_data,       "trial_data.csv")
readr::write_csv(trial_data_clean, "trial_data_clean.csv")

# Save the pangram-joined trials
readr::write_csv(trial_df, "trial_data_with_pangram.csv")

cat("Wrote:\n",
    " - ", normalizePath("trial_data.csv"), "\n",
    " - ", normalizePath("trial_data_clean.csv"), "\n",
    " - ", normalizePath("trial_data_with_pangram.csv"), "\n", sep = "")
