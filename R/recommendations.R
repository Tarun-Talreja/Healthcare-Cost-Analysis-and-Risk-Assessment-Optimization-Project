# =============================================================================
# recommendations.R
# Purpose: Generate data-driven recommendations for:
#            1. Differentiated premium pricing based on risk scores
#            2. Lifestyle incentive programme design
#            3. Personalized health intervention strategies
#            4. Population-level cost management opportunities
#
# Inputs:
#   df              - Full prepared dataset (data.frame)
#   svm_results     - Object returned by run_svm_model()
#   analysis_results- Object returned by run_cost_analysis()
#
# Outputs:
#   - Printed recommendation report (console)
#   - Saved CSV: output/premium_pricing_table.csv
#   - Saved CSV: output/lifestyle_incentive_programme.csv
#   - Returns invisible list of recommendation data.frames
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# Helper: compute risk score (0-100) for a single record
# =============================================================================
compute_risk_score <- function(age, bmi, smoker, hypertension,
                               exercise_frequency, children) {
  # Weighted additive model based on coefficient magnitudes from regression
  score <- 0

  # Age component (0-20 points)
  score <- score + (age - 18) / 52 * 20

  # BMI component (0-20 points)
  if (bmi < 18.5) {
    score <- score + 8   # underweight risk
  } else if (bmi < 25) {
    score <- score + 0   # normal
  } else if (bmi < 30) {
    score <- score + 10  # overweight
  } else if (bmi < 35) {
    score <- score + 16  # obese I
  } else {
    score <- score + 20  # obese II+
  }

  # Smoking component (0-35 points) – largest driver
  if (smoker == "yes") score <- score + 35

  # Hypertension component (0-15 points)
  if (hypertension == "yes") score <- score + 15

  # Exercise component (-10 to +8 points)
  score <- score + switch(exercise_frequency,
    "daily"  = -10,
    "weekly" =  -5,
    "rarely" =   3,
    "never"  =   8,
    0
  )

  # Children (minor positive effect: 0-3 points)
  score <- score + min(children * 0.5, 3)

  # Clamp to [0, 100]
  pmin(pmax(round(score, 1), 0), 100)
}

# =============================================================================
# Main recommendations function
# =============================================================================
run_recommendations <- function(df, svm_results, analysis_results,
                                output_dir) {

  cat("\n=== HEALTHCARE COST RECOMMENDATIONS ===\n")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # ---------------------------------------------------------------------------
  # 1. Compute risk scores for all records
  # ---------------------------------------------------------------------------
  cat("\n--- Computing individual risk scores ---\n")

  df$risk_score <- mapply(
    compute_risk_score,
    df$age, df$bmi, df$smoker, df$hypertension,
    df$exercise_frequency, df$children
  )

  # Risk tier assignment
  df$risk_tier <- cut(
    df$risk_score,
    breaks = c(-Inf, 25, 45, 65, Inf),
    labels = c("Tier 1 – Low Risk",
               "Tier 2 – Moderate Risk",
               "Tier 3 – High Risk",
               "Tier 4 – Very High Risk"),
    right  = TRUE
  )

  tier_summary <- df %>%
    group_by(risk_tier) %>%
    summarise(
      n              = n(),
      pct            = sprintf("%.1f%%", 100 * n() / nrow(df)),
      mean_score     = round(mean(risk_score), 1),
      mean_charges   = round(mean(healthcare_charges)),
      median_charges = round(median(healthcare_charges)),
      pct_smoker     = sprintf("%.1f%%", 100 * mean(smoker == "yes")),
      pct_hypert     = sprintf("%.1f%%", 100 * mean(hypertension == "yes")),
      mean_bmi       = round(mean(bmi), 1),
      .groups        = "drop"
    )

  cat("\n--- Risk Tier Distribution ---\n")
  print(as.data.frame(tier_summary))

  # ---------------------------------------------------------------------------
  # 2. Premium pricing recommendations
  # ---------------------------------------------------------------------------
  cat("\n--- Premium Pricing Recommendations ---\n")

  # Base premium (actuarial: mean charge + 15% admin margin)
  base_premium <- mean(df$healthcare_charges) * 1.15

  # Multipliers derived from relative mean charges per tier
  tier_charges  <- df %>%
    group_by(risk_tier) %>%
    summarise(mean_charges = mean(healthcare_charges), .groups = "drop")
  overall_mean  <- mean(df$healthcare_charges)
  tier_charges$multiplier <- round(tier_charges$mean_charges / overall_mean, 3)

  pricing_table <- tier_charges %>%
    mutate(
      base_premium_usd   = round(base_premium),
      adjusted_premium   = round(base_premium * multiplier),
      annual_adjustment  = round(base_premium * multiplier - base_premium),
      tier_description   = dplyr::recode(
        as.character(risk_tier),
        "Tier 1 – Low Risk"       = "Standard discount",
        "Tier 2 – Moderate Risk"  = "Base rate",
        "Tier 3 – High Risk"      = "Loaded premium",
        "Tier 4 – Very High Risk" = "High-risk premium"
      )
    )

  cat("\nPremium Pricing by Risk Tier:\n")
  print(as.data.frame(pricing_table %>%
                        select(risk_tier, multiplier,
                               adjusted_premium, tier_description)))

  # Save pricing table
  pricing_csv <- file.path(output_dir, "premium_pricing_table.csv")
  write.csv(pricing_table, pricing_csv, row.names = FALSE)
  cat(sprintf("\n  Saved: %s\n", pricing_csv))

  # ---------------------------------------------------------------------------
  # 3. Lifestyle incentive programme design
  # ---------------------------------------------------------------------------
  cat("\n--- Lifestyle Incentive Programme ---\n")

  incentive_programme <- data.frame(
    programme              = c(
      "Smoking Cessation Support",
      "Weight Management Programme",
      "Cardiac Rehabilitation (Hypertension)",
      "Exercise & Activity Rewards",
      "Preventive Screening Discount",
      "Nutrition Counselling",
      "Mental Health & Stress Reduction",
      "Annual Wellness Check Incentive"
    ),
    target_population      = c(
      "Smokers (Tier 3-4)",
      "BMI ≥ 30 (any tier)",
      "Hypertension patients (Tier 2-4)",
      "Sedentary individuals (rarely/never)",
      "All members age 40+",
      "Overweight/Obese members",
      "High-stress occupation groups",
      "All members"
    ),
    estimated_cost_saving  = c(
      "$8,000 – $15,000 / participant / year",
      "$3,000 – $6,000 / participant / year",
      "$4,000 – $7,000 / participant / year",
      "$1,500 – $3,500 / participant / year",
      "$500 – $1,200 / participant / year",
      "$800 – $2,000 / participant / year",
      "$600 – $1,800 / participant / year",
      "$300 – $800 / participant / year"
    ),
    incentive_offered      = c(
      "30% premium reduction after 12 months smoke-free",
      "20% premium reduction after BMI < 30",
      "15% premium reduction + care coordination",
      "$200 annual rebate for 150+ active days",
      "2% premium discount for on-time screenings",
      "Reimbursement of up to $500 in nutrition services",
      "Reimbursement of up to $400 in mental health apps/therapy",
      "$150 gift card for annual health check completion"
    ),
    roi_estimate           = c(
      "6:1 – 12:1",
      "3:1 – 5:1",
      "4:1 – 7:1",
      "2:1 – 4:1",
      "3:1 – 5:1",
      "2:1 – 3:1",
      "2:1 – 4:1",
      "2:1 – 3:1"
    ),
    stringsAsFactors = FALSE
  )

  cat("\nProposed Incentive Programmes:\n")
  for (i in seq_len(nrow(incentive_programme))) {
    cat(sprintf("\n  %d. %s\n", i, incentive_programme$programme[i]))
    cat(sprintf("     Target    : %s\n", incentive_programme$target_population[i]))
    cat(sprintf("     Saving    : %s\n", incentive_programme$estimated_cost_saving[i]))
    cat(sprintf("     Incentive : %s\n", incentive_programme$incentive_offered[i]))
    cat(sprintf("     ROI       : %s\n", incentive_programme$roi_estimate[i]))
  }

  # Save incentive programme
  incentive_csv <- file.path(output_dir, "lifestyle_incentive_programme.csv")
  write.csv(incentive_programme, incentive_csv, row.names = FALSE)
  cat(sprintf("\n  Saved: %s\n", incentive_csv))

  # ---------------------------------------------------------------------------
  # 4. Personalized risk profiles (segments)
  # ---------------------------------------------------------------------------
  cat("\n--- Risk Segment Profiles ---\n")

  segments <- list(
    list(
      name        = "Young Healthy Non-Smoker",
      criteria    = "Age 18-30 | BMI < 25 | Non-smoker | Active",
      n           = nrow(df %>% filter(age <= 30, bmi < 25,
                                       smoker == "no",
                                       exercise_frequency %in%
                                         c("daily","weekly"))),
      strategy    = paste(
        "Offer entry-level wellness plans.",
        "Reward continued healthy behavior with loyalty discounts.",
        "Promote preventive screenings at age 25+."
      ),
      priority    = "Low"
    ),
    list(
      name        = "Obese Sedentary Non-Smoker",
      criteria    = "BMI ≥ 30 | Exercise rarely/never | Non-smoker",
      n           = nrow(df %>% filter(bmi >= 30, smoker == "no",
                                       exercise_frequency %in%
                                         c("rarely","never"))),
      strategy    = paste(
        "Enroll in weight management programme.",
        "Provide gym membership subsidy.",
        "Schedule 6-monthly BMI and cholesterol checks.",
        "Offer premium reduction pathway linked to BMI reduction."
      ),
      priority    = "Medium-High"
    ),
    list(
      name        = "Active Smoker",
      criteria    = "Smoker | Exercise daily/weekly",
      n           = nrow(df %>% filter(smoker == "yes",
                                       exercise_frequency %in%
                                         c("daily","weekly"))),
      strategy    = paste(
        "Prioritize smoking cessation intervention.",
        "Leverage existing exercise motivation as a positive anchor.",
        "Nicotine Replacement Therapy reimbursement.",
        "Progressive premium reduction over 12/24/36 months quit."
      ),
      priority    = "High"
    ),
    list(
      name        = "Older High-Risk Patient",
      criteria    = "Age ≥ 55 | Hypertension | BMI ≥ 28",
      n           = nrow(df %>% filter(age >= 55, hypertension == "yes",
                                       bmi >= 28)),
      strategy    = paste(
        "Assign dedicated care coordinator.",
        "Monthly blood pressure monitoring programme.",
        "Medication adherence support and telehealth check-ins.",
        "Cardiac risk assessment and dietitian referral."
      ),
      priority    = "Very High"
    ),
    list(
      name        = "Smoker with Hypertension",
      criteria    = "Smoker | Hypertension",
      n           = nrow(df %>% filter(smoker == "yes",
                                       hypertension == "yes")),
      strategy    = paste(
        "Immediate dual intervention: smoking cessation + BP management.",
        "Cardiologist referral.",
        "High-tier premium loading with clear cost-reduction roadmap.",
        "Disease management programme enrolment."
      ),
      priority    = "Critical"
    )
  )

  for (seg in segments) {
    cat(sprintf("\n  [%s PRIORITY] %s\n", seg$priority, seg$name))
    cat(sprintf("  Criteria  : %s\n", seg$criteria))
    cat(sprintf("  Count     : %d (%.1f%%)\n",
                seg$n, 100 * seg$n / nrow(df)))
    cat(sprintf("  Strategy  : %s\n", seg$strategy))
  }

  # ---------------------------------------------------------------------------
  # 5. Quantified impact of SVM model deployment
  # ---------------------------------------------------------------------------
  cat("\n--- Quantified Model Deployment Impact ---\n")

  accuracy    <- svm_results$metrics$accuracy
  sensitivity <- svm_results$metrics$sensitivity
  specificity <- svm_results$metrics$specificity

  n_highrisk  <- sum(df$risk_label == "high_risk")
  n_lowrisk   <- sum(df$risk_label == "low_risk")

  # Expected correct identifications
  expected_tp <- round(n_highrisk * sensitivity)
  expected_tn <- round(n_lowrisk  * specificity)
  expected_fp <- n_lowrisk  - expected_tn
  expected_fn <- n_highrisk - expected_tp

  # Cost impact assumptions
  avg_saving_per_intervention  <- 6000   # conservative estimate
  avg_wrongful_premium_overload <- 800   # cost of false positive (overcharged)

  net_saving <- expected_tp * avg_saving_per_intervention -
    expected_fp * avg_wrongful_premium_overload

  cat(sprintf(
    "\n  SVM Model Performance on %d-record dataset:\n", nrow(df)
  ))
  cat(sprintf("  Accuracy     : %.1f%%\n", accuracy    * 100))
  cat(sprintf("  Sensitivity  : %.1f%%\n", sensitivity * 100))
  cat(sprintf("  Specificity  : %.1f%%\n", specificity * 100))
  cat(sprintf(
    "\n  Correct high-risk identifications : %d / %d\n",
    expected_tp, n_highrisk
  ))
  cat(sprintf(
    "  False positives (over-assessed)   : %d / %d\n",
    expected_fp, n_lowrisk
  ))
  cat(sprintf(
    "\n  Estimated annual net savings      : $%s\n",
    formatC(net_saving, format = "f", digits = 0, big.mark = ",")
  ))

  # ---------------------------------------------------------------------------
  # 6. Executive summary
  # ---------------------------------------------------------------------------
  cat("\n========================================\n")
  cat("       EXECUTIVE SUMMARY\n")
  cat("========================================\n")
  cat(paste0(
    "\nThis analysis of ", nrow(df), " healthcare records identified five\n",
    "key cost drivers and delivers three actionable recommendation streams:\n",
    "\n  A. PREMIUM DIFFERENTIATION\n",
    "     Four-tier risk-based pricing (multipliers: ",
    paste(round(pricing_table$multiplier, 2), collapse = " / "),
    ") aligns premiums\n",
    "     with actuarial risk and incentivizes healthy behaviour.\n",
    "\n  B. LIFESTYLE INCENTIVE PROGRAMMES\n",
    "     Eight targeted programmes with estimated ROI of 2:1 to 12:1.\n",
    "     Smoking cessation delivers the highest per-participant saving.\n",
    "\n  C. PREDICTIVE RISK MANAGEMENT\n",
    "     The deployed SVM model (", round(accuracy * 100, 1), "% accuracy, ",
    round(sensitivity * 100, 1), "% sensitivity)\n",
    "     enables proactive outreach before high-cost events occur,\n",
    "     generating an estimated net saving of $",
    formatC(net_saving, format = "f", digits = 0, big.mark = ","),
    " per cohort cycle.\n"
  ))

  # ---------------------------------------------------------------------------
  # Return all recommendation objects
  # ---------------------------------------------------------------------------
  invisible(list(
    df_with_scores     = df,
    tier_summary       = tier_summary,
    pricing_table      = pricing_table,
    incentive_programme= incentive_programme,
    segments           = segments,
    net_saving         = net_saving
  ))
}
