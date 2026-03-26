# =============================================================================
# main.R
# Healthcare Cost Analysis and Risk Assessment Optimization
#
# Entry point that orchestrates the full analysis pipeline:
#   1. Data generation and preparation
#   2. Exploratory cost analysis with statistical tests
#   3. SVM risk classification model (train, tune, evaluate)
#   4. Visualizations (heatmap, distributions, ROC, decision boundary)
#   5. Data-driven recommendations (premium pricing, lifestyle incentives)
#
# Usage:
#   Rscript main.R
#   or source("main.R") from an interactive R session
#
# Output:
#   - Console report covering all analytical findings
#   - output/plots/  – 10 publication-quality PNG figures
#   - output/premium_pricing_table.csv
#   - output/lifestyle_incentive_programme.csv
# =============================================================================

# ---- Record start time -------------------------------------------------------
t_start <- proc.time()

# ---- Reproducibility ---------------------------------------------------------
set.seed(42)

# ---- Project root (works both with Rscript and interactive source()) ---------
# When run via Rscript main.R, commandArgs gives us the script path.
# When source()d interactively, we fall back to the current working directory.
PROJECT_ROOT <- tryCatch({
  script_args <- commandArgs(trailingOnly = FALSE)
  script_flag <- grep("^--file=", script_args, value = TRUE)
  if (length(script_flag) > 0) {
    # Rscript main.R  path
    normalizePath(dirname(sub("^--file=", "", script_flag[1])),
                  mustWork = FALSE)
  } else {
    # Interactive: use current working directory
    normalizePath(".", mustWork = FALSE)
  }
}, error = function(e) normalizePath(".", mustWork = FALSE))

cat("=============================================================\n")
cat("  Healthcare Cost Analysis & Risk Assessment Optimization\n")
cat("=============================================================\n")
cat(sprintf("Project root : %s\n", PROJECT_ROOT))
cat(sprintf("Started at   : %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ---- Package availability check ----------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "ggplot2", "corrplot",
                   "caret", "e1071", "scales")

missing_pkgs <- required_pkgs[!sapply(required_pkgs,
                                       requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org",
                   quiet = TRUE)
}

# ---- Output directories ------------------------------------------------------
plots_dir  <- file.path(PROJECT_ROOT, "output", "plots")
output_dir <- file.path(PROJECT_ROOT, "output")
if (!dir.exists(plots_dir))  dir.create(plots_dir,  recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# STEP 1 – Source all module scripts
# =============================================================================
cat("\n[STEP 0] Loading module scripts...\n")

source(file.path(PROJECT_ROOT, "data", "generate_sample_data.R"))
source(file.path(PROJECT_ROOT, "R",    "data_preparation.R"))
source(file.path(PROJECT_ROOT, "R",    "cost_analysis.R"))
source(file.path(PROJECT_ROOT, "R",    "svm_model.R"))
source(file.path(PROJECT_ROOT, "R",    "visualizations.R"))
source(file.path(PROJECT_ROOT, "R",    "recommendations.R"))

cat("  All modules loaded successfully.\n")

# =============================================================================
# STEP 2 – Data Preparation
# =============================================================================
cat("\n[STEP 1] Data Preparation\n")
cat(strrep("-", 60), "\n")

data_list  <- load_and_prepare_data()
full_data  <- data_list$full_data
train_data <- data_list$train_data
test_data  <- data_list$test_data

cat(sprintf("\n  Dataset summary:\n"))
cat(sprintf("    Total records  : %d\n", nrow(full_data)))
cat(sprintf("    Training set   : %d records\n", nrow(train_data)))
cat(sprintf("    Test set       : %d records\n", nrow(test_data)))
cat(sprintf("    Features       : %s\n",
            paste(setdiff(names(full_data),
                          c("risk_label", "log_charges")),
                  collapse = ", ")))

# =============================================================================
# STEP 3 – Healthcare Cost Analysis
# =============================================================================
cat("\n[STEP 2] Healthcare Cost Analysis\n")
cat(strrep("-", 60), "\n")

analysis_results <- run_cost_analysis(full_data)

# =============================================================================
# STEP 4 – SVM Risk Classification Model
# =============================================================================
cat("\n[STEP 3] SVM Risk Classification Model\n")
cat(strrep("-", 60), "\n")

svm_results <- run_svm_model(train_data, test_data, full_data)

# Quick performance summary
cat(sprintf("\n  Final SVM Metrics (test set):\n"))
cat(sprintf("    Accuracy     : %.2f%%\n", svm_results$metrics$accuracy    * 100))
cat(sprintf("    Sensitivity  : %.2f%%\n", svm_results$metrics$sensitivity * 100))
cat(sprintf("    Specificity  : %.2f%%\n", svm_results$metrics$specificity * 100))
cat(sprintf("    F1 Score     : %.4f\n",   svm_results$metrics$f1))
cat(sprintf("    Kappa        : %.4f\n",   svm_results$metrics$kappa))

# =============================================================================
# STEP 5 – Visualizations
# =============================================================================
cat("\n[STEP 4] Generating Visualizations\n")
cat(strrep("-", 60), "\n")

run_visualizations(
  df               = full_data,
  analysis_results = analysis_results,
  svm_results      = svm_results,
  output_dir       = plots_dir
)

# =============================================================================
# STEP 6 – Recommendations
# =============================================================================
cat("\n[STEP 5] Generating Recommendations\n")
cat(strrep("-", 60), "\n")

recommendations <- run_recommendations(
  df               = full_data,
  svm_results      = svm_results,
  analysis_results = analysis_results,
  output_dir       = output_dir
)

# =============================================================================
# PIPELINE COMPLETE
# =============================================================================
t_end     <- proc.time()
elapsed   <- (t_end - t_start)[["elapsed"]]

cat("\n=============================================================\n")
cat("  PIPELINE COMPLETE\n")
cat("=============================================================\n")
cat(sprintf("  Total elapsed time : %.1f seconds\n", elapsed))
cat(sprintf("  Plots saved to     : %s\n", plots_dir))
cat(sprintf("  Reports saved to   : %s\n", output_dir))
cat(sprintf("  Finished at        : %s\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=============================================================\n")

# Return results invisibly (useful when source()d interactively)
invisible(list(
  full_data        = full_data,
  train_data       = train_data,
  test_data        = test_data,
  analysis_results = analysis_results,
  svm_results      = svm_results,
  recommendations  = recommendations
))
