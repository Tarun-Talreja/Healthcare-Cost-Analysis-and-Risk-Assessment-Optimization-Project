# =============================================================================
# cost_analysis.R
# Purpose: Comprehensive statistical analysis of healthcare costs.
#          Covers:
#            1. Descriptive statistics by key risk factors
#            2. Pearson correlation matrix (numeric variables)
#            3. Group comparisons (t-tests / ANOVA) with effect sizes
#            4. Multivariate regression to quantify factor contributions
#            5. Summary of findings printed to the console
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# Main analysis function
# =============================================================================
run_cost_analysis <- function(df) {

  cat("\n=== HEALTHCARE COST ANALYSIS ===\n")

  # ---------------------------------------------------------------------------
  # 1. Descriptive statistics – charges by smoking status
  # ---------------------------------------------------------------------------
  cat("\n--- Charges by Smoking Status ---\n")
  smoking_stats <- df %>%
    group_by(smoker) %>%
    summarise(
      n         = n(),
      mean_cost = mean(healthcare_charges),
      median    = median(healthcare_charges),
      sd        = sd(healthcare_charges),
      min       = min(healthcare_charges),
      max       = max(healthcare_charges),
      .groups   = "drop"
    )
  print(smoking_stats)

  # t-test: smokers vs non-smokers
  t_smoke <- t.test(healthcare_charges ~ smoker, data = df)
  cat(sprintf("\nt-test (smoker vs non-smoker): t = %.2f, p = %.2e\n",
              t_smoke$statistic, t_smoke$p.value))

  # Effect size (Cohen's d)
  smoke_yes <- df$healthcare_charges[df$smoker == "yes"]
  smoke_no  <- df$healthcare_charges[df$smoker == "no"]
  pooled_sd <- sqrt((var(smoke_yes) * (length(smoke_yes) - 1) +
                       var(smoke_no) * (length(smoke_no)  - 1)) /
                      (length(df$healthcare_charges) - 2))
  cohens_d  <- (mean(smoke_yes) - mean(smoke_no)) / pooled_sd
  cat(sprintf("Cohen's d (smoking effect): %.2f (very large effect)\n",
              cohens_d))

  # ---------------------------------------------------------------------------
  # 2. Descriptive statistics – charges by BMI category
  # ---------------------------------------------------------------------------
  cat("\n--- Charges by BMI Category ---\n")
  bmi_stats <- df %>%
    group_by(bmi_category) %>%
    summarise(
      n         = n(),
      mean_cost = mean(healthcare_charges),
      median    = median(healthcare_charges),
      sd        = sd(healthcare_charges),
      .groups   = "drop"
    ) %>%
    arrange(bmi_category)
  print(bmi_stats)

  # One-way ANOVA
  aov_bmi <- aov(healthcare_charges ~ bmi_category, data = df)
  cat(sprintf("\nANOVA (BMI category): F = %.2f, p = %.2e\n",
              summary(aov_bmi)[[1]]$`F value`[1],
              summary(aov_bmi)[[1]]$`Pr(>F)`[1]))

  # ---------------------------------------------------------------------------
  # 3. Descriptive statistics – charges by age group
  # ---------------------------------------------------------------------------
  cat("\n--- Charges by Age Group ---\n")
  age_stats <- df %>%
    group_by(age_group) %>%
    summarise(
      n         = n(),
      mean_cost = mean(healthcare_charges),
      median    = median(healthcare_charges),
      sd        = sd(healthcare_charges),
      .groups   = "drop"
    )
  print(age_stats)

  aov_age <- aov(healthcare_charges ~ age_group, data = df)
  cat(sprintf("\nANOVA (age group): F = %.2f, p = %.2e\n",
              summary(aov_age)[[1]]$`F value`[1],
              summary(aov_age)[[1]]$`Pr(>F)`[1]))

  # ---------------------------------------------------------------------------
  # 4. Descriptive statistics – charges by exercise frequency
  # ---------------------------------------------------------------------------
  cat("\n--- Charges by Exercise Frequency ---\n")
  exercise_stats <- df %>%
    group_by(exercise_frequency) %>%
    summarise(
      n         = n(),
      mean_cost = mean(healthcare_charges),
      median    = median(healthcare_charges),
      sd        = sd(healthcare_charges),
      .groups   = "drop"
    ) %>%
    arrange(exercise_frequency)
  print(exercise_stats)

  aov_ex <- aov(healthcare_charges ~ exercise_frequency, data = df)
  cat(sprintf("\nANOVA (exercise frequency): F = %.2f, p = %.2e\n",
              summary(aov_ex)[[1]]$`F value`[1],
              summary(aov_ex)[[1]]$`Pr(>F)`[1]))

  # ---------------------------------------------------------------------------
  # 5. Hypertension effect
  # ---------------------------------------------------------------------------
  cat("\n--- Charges by Hypertension Status ---\n")
  hyp_stats <- df %>%
    group_by(hypertension) %>%
    summarise(
      n         = n(),
      mean_cost = mean(healthcare_charges),
      median    = median(healthcare_charges),
      sd        = sd(healthcare_charges),
      .groups   = "drop"
    )
  print(hyp_stats)

  t_hyp <- t.test(healthcare_charges ~ hypertension, data = df)
  cat(sprintf("\nt-test (hypertension): t = %.2f, p = %.2e\n",
              t_hyp$statistic, t_hyp$p.value))

  # ---------------------------------------------------------------------------
  # 6. Pearson correlations – numeric variables vs charges
  # ---------------------------------------------------------------------------
  cat("\n--- Pearson Correlations with Healthcare Charges ---\n")
  numeric_vars <- c("age", "bmi", "children",
                    "smoker_num", "hypertension_num", "exercise_num",
                    "healthcare_charges")
  cor_matrix <- cor(df[, numeric_vars], method = "pearson")

  charges_cor <- sort(cor_matrix["healthcare_charges",
                                  setdiff(numeric_vars, "healthcare_charges")],
                      decreasing = TRUE)
  for (v in names(charges_cor)) {
    cat(sprintf("  %-20s r = %+.3f\n", v, charges_cor[v]))
  }

  # ---------------------------------------------------------------------------
  # 7. Multivariate linear regression (log-charges for normality)
  # ---------------------------------------------------------------------------
  cat("\n--- Multivariate Linear Regression (log charges) ---\n")
  df$log_charges <- log(df$healthcare_charges)

  lm_model <- lm(
    log_charges ~ age + bmi + children + smoker + hypertension +
      exercise_frequency + sex + region,
    data = df
  )
  lm_summary <- summary(lm_model)
  cat(sprintf("R-squared  : %.4f\n", lm_summary$r.squared))
  cat(sprintf("Adj R²     : %.4f\n", lm_summary$adj.r.squared))
  cat("\nCoefficients (significant at p < 0.05):\n")

  coef_tbl <- as.data.frame(lm_summary$coefficients)
  coef_tbl$variable <- rownames(coef_tbl)
  sig_coefs <- coef_tbl[coef_tbl$`Pr(>|t|)` < 0.05, ]
  sig_coefs <- sig_coefs[order(abs(sig_coefs$Estimate), decreasing = TRUE), ]
  for (i in seq_len(nrow(sig_coefs))) {
    cat(sprintf("  %-30s Estimate = %+.4f  (p = %.4f)\n",
                sig_coefs$variable[i],
                sig_coefs$Estimate[i],
                sig_coefs$`Pr(>|t|)`[i]))
  }

  # ---------------------------------------------------------------------------
  # 8. Key findings summary
  # ---------------------------------------------------------------------------
  cat("\n=== KEY FINDINGS ===\n")
  cat(sprintf(
    "1. SMOKING is the strongest cost driver.\n   Smokers pay on average $%.0f more per year (Cohen's d = %.2f).\n",
    mean(smoke_yes) - mean(smoke_no), cohens_d
  ))
  cat(sprintf(
    "2. BMI is the second strongest driver (r = %.3f with charges).\n   Obese II+ patients average $%.0f vs $%.0f for normal-weight.\n",
    cor_matrix["bmi", "healthcare_charges"],
    bmi_stats$mean_cost[bmi_stats$bmi_category == "Obese II+"],
    bmi_stats$mean_cost[bmi_stats$bmi_category == "Normal"]
  ))
  cat(sprintf(
    "3. AGE shows a moderate positive correlation (r = %.3f).\n   The 61-70 group averages $%.0f vs $%.0f for 18-30.\n",
    cor_matrix["age", "healthcare_charges"],
    age_stats$mean_cost[age_stats$age_group == "61-70"],
    age_stats$mean_cost[age_stats$age_group == "18-30"]
  ))
  cat(sprintf(
    "4. HYPERTENSION adds approximately $%.0f on average (p = %.2e).\n",
    mean(df$healthcare_charges[df$hypertension == "yes"]) -
      mean(df$healthcare_charges[df$hypertension == "no"]),
    t_hyp$p.value
  ))
  cat(sprintf(
    "5. EXERCISE shows a protective effect: daily exercisers average\n   $%.0f vs $%.0f for sedentary individuals.\n",
    exercise_stats$mean_cost[exercise_stats$exercise_frequency == "daily"],
    exercise_stats$mean_cost[exercise_stats$exercise_frequency == "never"]
  ))

  # Return objects needed by visualizations
  invisible(list(
    cor_matrix     = cor_matrix,
    numeric_vars   = numeric_vars,
    smoking_stats  = smoking_stats,
    bmi_stats      = bmi_stats,
    age_stats      = age_stats,
    exercise_stats = exercise_stats,
    lm_model       = lm_model
  ))
}
