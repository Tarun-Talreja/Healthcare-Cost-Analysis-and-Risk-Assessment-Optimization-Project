# =============================================================================
# svm_model.R
# Purpose: Train and evaluate a Support Vector Machine (SVM) classifier for
#          healthcare risk assessment.
#
# Workflow:
#   1. Feature engineering – build numeric feature matrix from prepared data
#   2. Train SVM with radial basis function (RBF) kernel on the training set
#   3. Tune hyper-parameters (cost C and gamma) via grid search + CV
#   4. Evaluate on the held-out test set:
#        - Confusion matrix
#        - Accuracy, sensitivity, specificity, F1
#   5. 10-fold cross-validation on the full dataset for unbiased estimate
#   6. Return model objects for downstream use (ROC, decision boundary plots)
#
# Target metric: ~96% accuracy, ~97% sensitivity (high-risk recall)
# =============================================================================

suppressPackageStartupMessages({
  library(e1071)       # SVM implementation
  library(caret)       # confusionMatrix, createFolds
  library(dplyr)
})

# =============================================================================
# Helper: build feature matrix
# =============================================================================
build_feature_matrix <- function(df) {
  # Numeric predictors only (SVM requires numeric input)
  # Binary encode categorical variables
  features <- data.frame(
    age             = as.numeric(df$age),
    bmi             = as.numeric(df$bmi),
    children        = as.numeric(df$children),
    smoker_num      = as.integer(df$smoker == "yes"),
    hypertension_num= as.integer(df$hypertension == "yes"),
    exercise_daily  = as.integer(df$exercise_frequency == "daily"),
    exercise_weekly = as.integer(df$exercise_frequency == "weekly"),
    exercise_rarely = as.integer(df$exercise_frequency == "rarely"),
    sex_male        = as.integer(df$sex == "male"),
    region_ne       = as.integer(df$region == "northeast"),
    region_nw       = as.integer(df$region == "northwest"),
    region_se       = as.integer(df$region == "southeast")
  )
  return(features)
}

# =============================================================================
# Main SVM training and evaluation function
# =============================================================================
run_svm_model <- function(train_data, test_data, full_data) {

  cat("\n=== SVM RISK CLASSIFICATION MODEL ===\n")

  # ---------------------------------------------------------------------------
  # 1. Build feature matrices
  # ---------------------------------------------------------------------------
  cat("\n--- Building feature matrices ---\n")

  X_train <- build_feature_matrix(train_data)
  y_train <- train_data$risk_label

  X_test  <- build_feature_matrix(test_data)
  y_test  <- test_data$risk_label

  cat(sprintf("Training features : %d x %d\n", nrow(X_train), ncol(X_train)))
  cat(sprintf("Test features     : %d x %d\n", nrow(X_test),  ncol(X_test)))
  cat(sprintf("Class distribution (train) – low_risk: %d | high_risk: %d\n",
              sum(y_train == "low_risk"), sum(y_train == "high_risk")))

  # ---------------------------------------------------------------------------
  # 2. Scale features (critical for RBF kernel SVM)
  # ---------------------------------------------------------------------------
  # We use the training set statistics to scale both train and test sets
  train_means <- colMeans(X_train)
  train_sds   <- apply(X_train, 2, sd)
  # Avoid division by zero for binary columns with zero variance
  train_sds[train_sds == 0] <- 1

  X_train_scaled <- scale(X_train, center = train_means, scale = train_sds)
  X_test_scaled  <- scale(X_test,  center = train_means, scale = train_sds)

  # ---------------------------------------------------------------------------
  # 3. Hyper-parameter tuning (grid search with 5-fold CV)
  # ---------------------------------------------------------------------------
  cat("\n--- Hyper-parameter tuning (5-fold CV grid search) ---\n")
  cat("    Searching over cost in {0.1, 1, 10, 100} and gamma in {0.01, 0.05, 0.1, 0.5}\n")

  set.seed(42)
  tune_result <- tune(
    svm,
    train.x  = X_train_scaled,
    train.y  = y_train,
    kernel   = "radial",
    ranges   = list(
      cost  = c(0.1, 1, 10, 100),
      gamma = c(0.01, 0.05, 0.1, 0.5)
    ),
    tunecontrol = tune.control(
      sampling = "cross",
      cross    = 5,
      nrepeat  = 1
    )
  )

  best_cost  <- tune_result$best.parameters$cost
  best_gamma <- tune_result$best.parameters$gamma
  cat(sprintf("Best parameters  : cost = %.2f, gamma = %.4f\n",
              best_cost, best_gamma))
  cat(sprintf("Best CV error    : %.4f (CV accuracy = %.2f%%)\n",
              tune_result$best.performance,
              100 * (1 - tune_result$best.performance)))

  # ---------------------------------------------------------------------------
  # 4. Train final SVM with best hyper-parameters
  # ---------------------------------------------------------------------------
  cat("\n--- Training final SVM model ---\n")

  set.seed(42)
  svm_model <- svm(
    x          = X_train_scaled,
    y          = y_train,
    kernel     = "radial",
    cost       = best_cost,
    gamma      = best_gamma,
    probability= TRUE,   # needed for ROC curve
    scale      = FALSE   # already scaled above
  )

  cat(sprintf("Support vectors  : %d (%.1f%% of training set)\n",
              nrow(svm_model$SV),
              100 * nrow(svm_model$SV) / nrow(X_train_scaled)))

  # ---------------------------------------------------------------------------
  # 5. Predict on test set and evaluate
  # ---------------------------------------------------------------------------
  cat("\n--- Test Set Evaluation ---\n")

  pred_test      <- predict(svm_model, X_test_scaled)
  pred_test_prob <- predict(svm_model, X_test_scaled, probability = TRUE)
  prob_high_risk <- attr(pred_test_prob, "probabilities")[, "high_risk"]

  # Confusion matrix (positive class = high_risk)
  cm <- confusionMatrix(pred_test, y_test, positive = "high_risk")
  print(cm)

  # Extract key metrics
  accuracy    <- as.numeric(cm$overall["Accuracy"])
  sensitivity <- as.numeric(cm$byClass["Sensitivity"])
  specificity <- as.numeric(cm$byClass["Specificity"])
  ppv         <- as.numeric(cm$byClass["Pos Pred Value"])
  npv         <- as.numeric(cm$byClass["Neg Pred Value"])
  f1          <- as.numeric(cm$byClass["F1"])
  kappa       <- as.numeric(cm$overall["Kappa"])

  cat(sprintf("\n--- Performance Summary ---\n"))
  cat(sprintf("  Accuracy    : %.2f%%\n",  accuracy    * 100))
  cat(sprintf("  Sensitivity : %.2f%%\n",  sensitivity * 100))
  cat(sprintf("  Specificity : %.2f%%\n",  specificity * 100))
  cat(sprintf("  PPV         : %.2f%%\n",  ppv         * 100))
  cat(sprintf("  NPV         : %.2f%%\n",  npv         * 100))
  cat(sprintf("  F1 Score    : %.4f\n",    f1))
  cat(sprintf("  Kappa       : %.4f\n",    kappa))

  # ---------------------------------------------------------------------------
  # 6. 10-fold cross-validation on the full dataset
  # ---------------------------------------------------------------------------
  cat("\n--- 10-Fold Cross-Validation (full dataset) ---\n")

  X_full <- build_feature_matrix(full_data)
  y_full <- full_data$risk_label

  full_means <- colMeans(X_full)
  full_sds   <- apply(X_full, 2, sd)
  full_sds[full_sds == 0] <- 1
  X_full_scaled <- scale(X_full, center = full_means, scale = full_sds)

  set.seed(42)
  folds     <- createFolds(y_full, k = 10, list = TRUE, returnTrain = FALSE)
  cv_acc    <- numeric(10)
  cv_sens   <- numeric(10)
  cv_spec   <- numeric(10)

  for (k in seq_along(folds)) {
    test_idx  <- folds[[k]]
    train_idx <- setdiff(seq_len(nrow(X_full_scaled)), test_idx)

    cv_model <- svm(
      x      = X_full_scaled[train_idx, ],
      y      = y_full[train_idx],
      kernel = "radial",
      cost   = best_cost,
      gamma  = best_gamma,
      scale  = FALSE
    )
    cv_pred <- predict(cv_model, X_full_scaled[test_idx, ])
    cv_cm   <- confusionMatrix(cv_pred, y_full[test_idx],
                               positive = "high_risk")
    cv_acc[k]  <- as.numeric(cv_cm$overall["Accuracy"])
    cv_sens[k] <- as.numeric(cv_cm$byClass["Sensitivity"])
    cv_spec[k] <- as.numeric(cv_cm$byClass["Specificity"])
  }

  cat(sprintf("  CV Accuracy    : %.2f%% (+/- %.2f%%)\n",
              mean(cv_acc)  * 100, sd(cv_acc)  * 100))
  cat(sprintf("  CV Sensitivity : %.2f%% (+/- %.2f%%)\n",
              mean(cv_sens) * 100, sd(cv_sens) * 100))
  cat(sprintf("  CV Specificity : %.2f%% (+/- %.2f%%)\n",
              mean(cv_spec) * 100, sd(cv_spec) * 100))

  # ---------------------------------------------------------------------------
  # 7. Feature importance (proxy via linear SVM coefficients approximation)
  #    For RBF kernel, approximate importance via permutation on CV accuracy
  # ---------------------------------------------------------------------------
  cat("\n--- Approximate Feature Importance (permutation) ---\n")

  base_acc    <- mean(cv_acc)
  feat_names  <- colnames(X_full_scaled)
  perm_drop   <- numeric(length(feat_names))
  names(perm_drop) <- feat_names

  set.seed(42)
  # Use a single fold for speed (indicative, not exhaustive)
  test_idx_imp  <- folds[[1]]
  train_idx_imp <- setdiff(seq_len(nrow(X_full_scaled)), test_idx_imp)

  imp_model <- svm(
    x      = X_full_scaled[train_idx_imp, ],
    y      = y_full[train_idx_imp],
    kernel = "radial",
    cost   = best_cost,
    gamma  = best_gamma,
    scale  = FALSE
  )

  for (j in seq_along(feat_names)) {
    X_perm         <- X_full_scaled[test_idx_imp, ]
    X_perm[, j]    <- sample(X_perm[, j])          # permute feature j
    perm_pred      <- predict(imp_model, X_perm)
    perm_acc       <- mean(perm_pred == y_full[test_idx_imp])
    # Current fold accuracy (unpermuted)
    base_perm_pred <- predict(imp_model, X_full_scaled[test_idx_imp, ])
    base_perm_acc  <- mean(base_perm_pred == y_full[test_idx_imp])
    perm_drop[j]   <- base_perm_acc - perm_acc
  }

  perm_drop_sorted <- sort(perm_drop, decreasing = TRUE)
  cat("  Feature importance (accuracy drop on permutation):\n")
  for (nm in names(perm_drop_sorted)) {
    bar <- strrep("|", max(0, round(perm_drop_sorted[nm] * 100)))
    cat(sprintf("  %-20s %s %.4f\n", nm, bar, perm_drop_sorted[nm]))
  }

  # ---------------------------------------------------------------------------
  # 8. Return all objects needed by visualizations.R
  # ---------------------------------------------------------------------------
  invisible(list(
    model           = svm_model,
    tune_result     = tune_result,
    X_train_scaled  = X_train_scaled,
    X_test_scaled   = X_test_scaled,
    y_train         = y_train,
    y_test          = y_test,
    pred_test       = pred_test,
    prob_high_risk  = prob_high_risk,
    confusion_matrix= cm,
    cv_accuracy     = cv_acc,
    cv_sensitivity  = cv_sens,
    cv_specificity  = cv_spec,
    feature_names   = feat_names,
    feature_importance = perm_drop_sorted,
    train_means     = train_means,
    train_sds       = train_sds,
    best_cost       = best_cost,
    best_gamma      = best_gamma,
    metrics = list(
      accuracy    = accuracy,
      sensitivity = sensitivity,
      specificity = specificity,
      ppv         = ppv,
      npv         = npv,
      f1          = f1,
      kappa       = kappa
    )
  ))
}
