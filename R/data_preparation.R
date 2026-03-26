# =============================================================================
# data_preparation.R
# Purpose: Load (or generate) the healthcare dataset and perform all
#          pre-processing steps needed by downstream scripts:
#            - Type conversion and factor encoding
#            - Derived features (age groups, BMI categories, numeric flags)
#            - Missing-value checks
#            - 80/20 stratified train/test split
# =============================================================================

# ---- Package dependencies ---------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
})

# ---- Source the data generator ----------------------------------------------
source(file.path(PROJECT_ROOT, "data", "generate_sample_data.R"))

# =============================================================================
# 1. Generate / Load data
# =============================================================================
load_and_prepare_data <- function() {

  cat("\n=== DATA PREPARATION ===\n")

  # Generate synthetic dataset
  df <- generate_healthcare_data(n = 1200, seed = 42)

  # ---------------------------------------------------------------------------
  # 2. Type coercions
  # ---------------------------------------------------------------------------
  df$sex                <- factor(df$sex,
                                  levels = c("female", "male"))
  df$smoker             <- factor(df$smoker,
                                  levels = c("no", "yes"))
  df$region             <- factor(df$region,
                                  levels = c("northeast", "northwest",
                                             "southeast", "southwest"))
  df$exercise_frequency <- factor(df$exercise_frequency,
                                  levels = c("daily", "weekly",
                                             "rarely", "never"))
  df$hypertension       <- factor(df$hypertension,
                                  levels = c("no", "yes"))

  # risk_label already a factor from the generator

  # ---------------------------------------------------------------------------
  # 3. Derived features
  # ---------------------------------------------------------------------------

  # Age groups (clinical breakpoints)
  df$age_group <- cut(df$age,
                      breaks = c(17, 30, 45, 60, 71),
                      labels = c("18-30", "31-45", "46-60", "61-70"),
                      right  = TRUE)

  # BMI categories (WHO classification)
  df$bmi_category <- cut(df$bmi,
                         breaks = c(0, 18.5, 25, 30, 35, Inf),
                         labels = c("Underweight", "Normal",
                                    "Overweight", "Obese I",
                                    "Obese II+"),
                         right  = FALSE)

  # Numeric binary flags (useful for correlation analysis)
  df$smoker_num      <- as.integer(df$smoker      == "yes")
  df$hypertension_num<- as.integer(df$hypertension == "yes")
  df$exercise_num    <- as.integer(df$exercise_frequency %in%
                                     c("daily", "weekly"))

  # ---------------------------------------------------------------------------
  # 4. Missing-value audit
  # ---------------------------------------------------------------------------
  na_counts <- colSums(is.na(df))
  if (any(na_counts > 0)) {
    warning("Missing values detected:\n",
            paste(names(na_counts[na_counts > 0]),
                  na_counts[na_counts > 0], sep = ": ", collapse = "\n"))
  } else {
    cat("No missing values detected.\n")
  }

  # ---------------------------------------------------------------------------
  # 5. Summary statistics
  # ---------------------------------------------------------------------------
  cat("\n--- Summary of key variables ---\n")
  cat(sprintf("Total records          : %d\n", nrow(df)))
  cat(sprintf("Smokers                : %d (%.1f%%)\n",
              sum(df$smoker == "yes"),
              100 * mean(df$smoker == "yes")))
  cat(sprintf("Hypertension           : %d (%.1f%%)\n",
              sum(df$hypertension == "yes"),
              100 * mean(df$hypertension == "yes")))
  cat(sprintf("Mean BMI               : %.1f\n", mean(df$bmi)))
  cat(sprintf("Mean age               : %.1f years\n", mean(df$age)))
  cat(sprintf("Mean charges           : $%.0f\n",
              mean(df$healthcare_charges)))
  cat(sprintf("Median charges         : $%.0f\n",
              median(df$healthcare_charges)))
  cat(sprintf("High-risk records      : %d (%.1f%%)\n",
              sum(df$risk_label == "high_risk"),
              100 * mean(df$risk_label == "high_risk")))

  # ---------------------------------------------------------------------------
  # 6. Stratified 80/20 train/test split
  # ---------------------------------------------------------------------------
  set.seed(42)
  train_idx <- createDataPartition(df$risk_label,
                                   p    = 0.80,
                                   list = FALSE)
  train_data <- df[ train_idx, ]
  test_data  <- df[-train_idx, ]

  cat(sprintf("\nTrain set: %d records | Test set: %d records\n",
              nrow(train_data), nrow(test_data)))

  return(list(
    full_data  = df,
    train_data = train_data,
    test_data  = test_data
  ))
}
