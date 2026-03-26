# =============================================================================
# visualizations.R
# Purpose: Generate and save all project visualizations to output/plots/
#
# Plots produced:
#   01_correlation_heatmap.png      – Pearson correlation matrix (corrplot)
#   02_cost_by_smoking.png          – Charge distributions by smoking status
#   03_cost_by_bmi_category.png     – Violin + box plot of charges by BMI group
#   04_cost_by_age_group.png        – Box plots of charges by age group
#   05_cost_by_exercise.png         – Charges by exercise frequency
#   06_roc_curve.png                – ROC curve with AUC annotation
#   07_svm_decision_boundary.png    – 2D SVM decision boundary (BMI vs age)
#   08_feature_importance.png       – Permutation feature importance bar chart
#   09_cv_performance.png           – Cross-validation fold metrics
#   10_cost_heatmap_bmi_age.png     – Mean cost heatmap: BMI category x age group
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(corrplot)
  library(dplyr)
  library(tidyr)
  library(scales)     # dollar formatting
  library(e1071)      # predict for decision boundary
})

# Helper: ensure output directory exists
ensure_output_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# Helper: save a ggplot with consistent settings
save_plot <- function(p, filename, output_dir,
                      width = 10, height = 7, dpi = 150) {
  filepath <- file.path(output_dir, filename)
  ggsave(filepath, plot = p, width = width, height = height,
         dpi = dpi, bg = "white")
  cat(sprintf("  Saved: %s\n", filepath))
}

# =============================================================================
# Main visualization function
# =============================================================================
run_visualizations <- function(df, analysis_results, svm_results, output_dir) {

  cat("\n=== GENERATING VISUALIZATIONS ===\n")
  ensure_output_dir(output_dir)

  # Custom theme for all plots
  theme_healthcare <- theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      axis.title    = element_text(face = "bold"),
      legend.title  = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin   = margin(12, 12, 12, 12)
    )

  # Color palettes
  risk_colors     <- c("low_risk"  = "#2196F3", "high_risk" = "#F44336")
  smoking_colors  <- c("no" = "#4CAF50",         "yes"       = "#FF5722")
  bmi_colors      <- c("Underweight" = "#81D4FA", "Normal" = "#4CAF50",
                       "Overweight"  = "#FFC107",
                       "Obese I"     = "#FF7043",
                       "Obese II+"   = "#B71C1C")

  # -------------------------------------------------------------------------
  # Plot 01: Correlation heatmap (corrplot)
  # -------------------------------------------------------------------------
  cat("\n[01] Correlation heatmap\n")
  png(file.path(output_dir, "01_correlation_heatmap.png"),
      width = 900, height = 800, res = 150, bg = "white")

  cor_mat <- analysis_results$cor_matrix
  # Rename for readability
  colnames(cor_mat) <- rownames(cor_mat) <- c(
    "Age", "BMI", "Children", "Smoker",
    "Hypertension", "Exercise\n(active)", "Charges"
  )
  corrplot(
    cor_mat,
    method       = "color",
    type         = "upper",
    order        = "hclust",
    addCoef.col  = "black",
    number.cex   = 0.85,
    tl.col       = "black",
    tl.srt       = 45,
    tl.cex       = 0.9,
    col          = colorRampPalette(c("#1565C0", "white", "#C62828"))(200),
    title        = "Pearson Correlation Matrix – Healthcare Variables",
    mar          = c(0, 0, 2, 0),
    cl.cex       = 0.8
  )
  dev.off()
  cat(sprintf("  Saved: %s\n",
              file.path(output_dir, "01_correlation_heatmap.png")))

  # -------------------------------------------------------------------------
  # Plot 02: Cost distributions by smoking status
  # -------------------------------------------------------------------------
  cat("[02] Cost by smoking status\n")
  p02 <- ggplot(df, aes(x = smoker, y = healthcare_charges, fill = smoker)) +
    geom_violin(alpha = 0.6, trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.15, fill = "white", outlier.shape = 21,
                 outlier.size = 1.2, outlier.alpha = 0.5) +
    scale_fill_manual(values = smoking_colors) +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       breaks = seq(0, 80000, 10000)) +
    scale_x_discrete(labels = c("no" = "Non-Smoker", "yes" = "Smoker")) +
    labs(
      title    = "Healthcare Charges by Smoking Status",
      subtitle = "Smokers incur dramatically higher annual healthcare costs",
      x        = "Smoking Status",
      y        = "Annual Healthcare Charges ($)"
    ) +
    theme_healthcare +
    # Annotate mean values
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 3, fill = "gold", color = "black")
  save_plot(p02, "02_cost_by_smoking.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 03: Cost by BMI category
  # -------------------------------------------------------------------------
  cat("[03] Cost by BMI category\n")
  p03 <- ggplot(df,
                aes(x    = bmi_category,
                    y    = healthcare_charges,
                    fill = bmi_category)) +
    geom_violin(alpha = 0.65, trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.12, fill = "white", outlier.shape = 21,
                 outlier.size = 1, outlier.alpha = 0.4) +
    scale_fill_manual(values = bmi_colors) +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       breaks = seq(0, 80000, 10000)) +
    labs(
      title    = "Healthcare Charges by BMI Category",
      subtitle = "Obesity is associated with substantially higher costs",
      x        = "BMI Category (WHO Classification)",
      y        = "Annual Healthcare Charges ($)"
    ) +
    theme_healthcare +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  save_plot(p03, "03_cost_by_bmi_category.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 04: Cost by age group
  # -------------------------------------------------------------------------
  cat("[04] Cost by age group\n")
  p04 <- ggplot(df, aes(x = age_group, y = healthcare_charges,
                        fill = age_group)) +
    geom_boxplot(alpha = 0.75, outlier.shape = 21,
                 outlier.size = 1.2, outlier.alpha = 0.5) +
    scale_fill_brewer(palette = "Blues", direction = 1) +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       breaks = seq(0, 80000, 10000)) +
    labs(
      title    = "Healthcare Charges by Age Group",
      subtitle = "Costs increase steadily with age",
      x        = "Age Group (years)",
      y        = "Annual Healthcare Charges ($)",
      fill     = "Age Group"
    ) +
    theme_healthcare
  save_plot(p04, "04_cost_by_age_group.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 05: Cost by exercise frequency
  # -------------------------------------------------------------------------
  cat("[05] Cost by exercise frequency\n")
  ex_order <- c("daily", "weekly", "rarely", "never")
  ex_labels <- c("Daily", "Weekly", "Rarely", "Never")

  p05 <- df %>%
    mutate(exercise_frequency = factor(exercise_frequency,
                                       levels = ex_order,
                                       labels = ex_labels)) %>%
    ggplot(aes(x = exercise_frequency, y = healthcare_charges,
               fill = exercise_frequency)) +
    geom_violin(alpha = 0.65, trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.12, fill = "white", outlier.shape = 21,
                 outlier.size = 1, outlier.alpha = 0.4) +
    scale_fill_manual(values = c("Daily"  = "#1B5E20",
                                 "Weekly" = "#4CAF50",
                                 "Rarely" = "#FFC107",
                                 "Never"  = "#B71C1C")) +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       breaks = seq(0, 80000, 10000)) +
    labs(
      title    = "Healthcare Charges by Exercise Frequency",
      subtitle = "Regular exercise is associated with lower healthcare costs",
      x        = "Exercise Frequency",
      y        = "Annual Healthcare Charges ($)"
    ) +
    theme_healthcare
  save_plot(p05, "05_cost_by_exercise.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 06: ROC curve
  # -------------------------------------------------------------------------
  cat("[06] ROC curve\n")

  # Compute ROC manually (sort by predicted probability)
  prob   <- svm_results$prob_high_risk
  actual <- as.integer(svm_results$y_test == "high_risk")

  # Sort by descending predicted probability
  ord    <- order(prob, decreasing = TRUE)
  prob_s <- prob[ord]
  act_s  <- actual[ord]

  n_pos  <- sum(actual == 1)
  n_neg  <- sum(actual == 0)

  tpr <- cumsum(act_s == 1)  / n_pos
  fpr <- cumsum(act_s == 0)  / n_neg

  # Add origin
  tpr <- c(0, tpr)
  fpr <- c(0, fpr)

  # AUC via trapezoidal rule
  auc_val <- sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1))) / 2

  roc_df <- data.frame(fpr = fpr, tpr = tpr)

  p06 <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
    geom_ribbon(aes(ymin = 0, ymax = tpr), fill = "#1565C0", alpha = 0.15) +
    geom_line(color = "#1565C0", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "gray50", linewidth = 0.8) +
    annotate("text", x = 0.65, y = 0.20,
             label = sprintf("AUC = %.3f", auc_val),
             size = 5.5, fontface = "bold", color = "#1565C0") +
    annotate("point", x = 1 - svm_results$metrics$specificity,
             y = svm_results$metrics$sensitivity,
             shape = 21, size = 4, fill = "#F44336", color = "black") +
    annotate("text",
             x = 1 - svm_results$metrics$specificity + 0.05,
             y = svm_results$metrics$sensitivity - 0.04,
             label = sprintf("Operating\npoint\n(%.2f, %.2f)",
                             1 - svm_results$metrics$specificity,
                             svm_results$metrics$sensitivity),
             size = 3.5, color = "#F44336") +
    scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    labs(
      title    = "ROC Curve – SVM Risk Classification",
      subtitle = sprintf("Radial Kernel SVM  |  AUC = %.3f", auc_val),
      x        = "False Positive Rate (1 - Specificity)",
      y        = "True Positive Rate (Sensitivity)"
    ) +
    theme_healthcare
  save_plot(p06, "06_roc_curve.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 07: SVM decision boundary (2D projection: age vs BMI)
  # -------------------------------------------------------------------------
  cat("[07] SVM decision boundary\n")

  # Build a 2D model using only age and BMI (already standardized by svm_model.R)
  train_2d <- data.frame(
    age_s = svm_results$X_train_scaled[, "age"],
    bmi_s = svm_results$X_train_scaled[, "bmi"]
  )
  y_train_2d <- svm_results$y_train

  set.seed(42)
  svm_2d <- svm(
    x      = train_2d,
    y      = y_train_2d,
    kernel = "radial",
    cost   = svm_results$best_cost,
    gamma  = svm_results$best_gamma,
    scale  = FALSE
  )

  # Create a fine grid for the decision surface
  age_range <- seq(min(train_2d$age_s) - 0.5, max(train_2d$age_s) + 0.5,
                   length.out = 120)
  bmi_range <- seq(min(train_2d$bmi_s) - 0.5, max(train_2d$bmi_s) + 0.5,
                   length.out = 120)
  grid      <- expand.grid(age_s = age_range, bmi_s = bmi_range)
  grid$pred <- predict(svm_2d, grid)

  # Actual test points
  test_2d <- data.frame(
    age_s = svm_results$X_test_scaled[, "age"],
    bmi_s = svm_results$X_test_scaled[, "bmi"],
    label = svm_results$y_test
  )

  p07 <- ggplot() +
    geom_tile(data = grid,
              aes(x = age_s, y = bmi_s, fill = pred),
              alpha = 0.35, show.legend = FALSE) +
    scale_fill_manual(values = c("low_risk" = "#90CAF9",
                                 "high_risk" = "#EF9A9A")) +
    geom_point(data = test_2d,
               aes(x = age_s, y = bmi_s, color = label, shape = label),
               size = 1.8, alpha = 0.75) +
    scale_color_manual(values = risk_colors,
                       labels = c("low_risk" = "Low Risk",
                                  "high_risk" = "High Risk"),
                       name = "True Label") +
    scale_shape_manual(values = c("low_risk" = 16, "high_risk" = 17),
                       labels = c("low_risk" = "Low Risk",
                                  "high_risk" = "High Risk"),
                       name = "True Label") +
    labs(
      title    = "SVM Decision Boundary (2D Projection)",
      subtitle = "Features: Standardized Age vs. Standardized BMI",
      x        = "Age (standardized)",
      y        = "BMI (standardized)"
    ) +
    theme_healthcare +
    guides(color = guide_legend(override.aes = list(size = 3)))
  save_plot(p07, "07_svm_decision_boundary.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 08: Feature importance (permutation)
  # -------------------------------------------------------------------------
  cat("[08] Feature importance\n")

  fi <- svm_results$feature_importance
  fi_df <- data.frame(
    feature    = names(fi),
    importance = as.numeric(fi),
    stringsAsFactors = FALSE
  ) %>%
    arrange(importance) %>%
    mutate(feature = factor(feature, levels = feature))

  # Clean up feature names for display
  fi_df$feature_label <- dplyr::recode(
    as.character(fi_df$feature),
    "smoker_num"       = "Smoker",
    "hypertension_num" = "Hypertension",
    "bmi"              = "BMI",
    "age"              = "Age",
    "exercise_daily"   = "Exercise: Daily",
    "exercise_weekly"  = "Exercise: Weekly",
    "exercise_rarely"  = "Exercise: Rarely",
    "children"         = "Children",
    "sex_male"         = "Sex (Male)",
    "region_ne"        = "Region: NE",
    "region_nw"        = "Region: NW",
    "region_se"        = "Region: SE"
  )
  fi_df$feature_label <- factor(fi_df$feature_label,
                                levels = fi_df$feature_label)
  fi_df$color_group <- ifelse(fi_df$importance > 0.02, "high",
                              ifelse(fi_df$importance > 0.005, "medium", "low"))

  p08 <- ggplot(fi_df, aes(x = feature_label, y = importance,
                            fill = color_group)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.4f", importance)),
              hjust = -0.1, size = 3.5) +
    scale_fill_manual(values = c("high" = "#C62828",
                                 "medium" = "#E65100",
                                 "low" = "#1565C0")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    coord_flip() +
    labs(
      title    = "SVM Feature Importance (Permutation Method)",
      subtitle = "Drop in accuracy when each feature is randomly permuted",
      x        = NULL,
      y        = "Accuracy Drop (permutation importance)"
    ) +
    theme_healthcare
  save_plot(p08, "08_feature_importance.png", output_dir, height = 6)

  # -------------------------------------------------------------------------
  # Plot 09: Cross-validation performance across folds
  # -------------------------------------------------------------------------
  cat("[09] CV performance\n")

  cv_df <- data.frame(
    fold        = rep(1:10, 3),
    metric      = rep(c("Accuracy", "Sensitivity", "Specificity"), each = 10),
    value       = c(svm_results$cv_accuracy,
                    svm_results$cv_sensitivity,
                    svm_results$cv_specificity)
  )

  p09 <- ggplot(cv_df, aes(x = fold, y = value, color = metric,
                            group = metric)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = mean(value), color = metric),
               linetype = "dashed", linewidth = 0.7, alpha = 0.6,
               data = cv_df %>% group_by(metric) %>%
                 summarise(value = mean(value), .groups = "drop")) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = percent_format(),
                       limits = c(0.80, 1.01)) +
    scale_color_manual(values = c("Accuracy"    = "#1565C0",
                                  "Sensitivity" = "#C62828",
                                  "Specificity" = "#2E7D32")) +
    labs(
      title    = "10-Fold Cross-Validation Performance",
      subtitle = "SVM with radial kernel – metrics per fold",
      x        = "Fold Number",
      y        = "Metric Value",
      color    = "Metric"
    ) +
    theme_healthcare
  save_plot(p09, "09_cv_performance.png", output_dir)

  # -------------------------------------------------------------------------
  # Plot 10: Mean cost heatmap – BMI category x Age group
  # -------------------------------------------------------------------------
  cat("[10] Cost heatmap (BMI x Age)\n")

  heatmap_df <- df %>%
    group_by(bmi_category, age_group) %>%
    summarise(mean_cost = mean(healthcare_charges), .groups = "drop") %>%
    filter(!is.na(bmi_category), !is.na(age_group))

  p10 <- ggplot(heatmap_df,
                aes(x = age_group, y = bmi_category, fill = mean_cost)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = dollar(round(mean_cost, -2))),
              size = 3.5, color = "black") +
    scale_fill_gradient2(
      low      = "#E3F2FD",
      mid      = "#FF8F00",
      high     = "#B71C1C",
      midpoint = median(heatmap_df$mean_cost),
      labels   = dollar_format(prefix = "$"),
      name     = "Mean\nCharges"
    ) +
    labs(
      title    = "Mean Healthcare Charges: BMI Category × Age Group",
      subtitle = "Older patients with higher BMI incur the greatest costs",
      x        = "Age Group",
      y        = "BMI Category"
    ) +
    theme_healthcare +
    theme(axis.text.y = element_text(size = 10))
  save_plot(p10, "10_cost_heatmap_bmi_age.png", output_dir, width = 9, height = 6)

  cat("\n=== All visualizations saved to:", output_dir, "===\n")
}
