# =============================================================================
# generate_sample_data.R
# Purpose: Generate a realistic synthetic healthcare dataset (1000+ records)
#          mimicking real insurance cost data distributions.
#
# Key design decisions:
#   - Smokers have dramatically higher charges (3-4x multiplier)
#   - Charges increase with BMI (especially obesity threshold > 30)
#   - Charges increase with age (older patients cost more)
#   - Hypertension adds a significant cost premium
#   - Exercise reduces costs modestly
#   - Regional variation is minor
# =============================================================================

generate_healthcare_data <- function(n = 1200, seed = 42) {

  set.seed(seed)

  # ---------------------------------------------------------------------------
  # Demographic variables
  # ---------------------------------------------------------------------------
  age  <- round(runif(n, min = 18, max = 70))
  sex  <- sample(c("male", "female"), n, replace = TRUE, prob = c(0.50, 0.50))

  # BMI: roughly normally distributed around 26, with a right skew
  bmi  <- round(rnorm(n, mean = 26.5, sd = 5.5), 1)
  bmi  <- pmax(bmi, 15)    # floor at 15
  bmi  <- pmin(bmi, 53)    # ceiling at 53

  # Number of children (0–4, Poisson-ish)
  children <- sample(0:4, n, replace = TRUE,
                     prob = c(0.35, 0.25, 0.22, 0.12, 0.06))

  # Region
  region <- sample(c("northeast", "northwest", "southeast", "southwest"),
                   n, replace = TRUE, prob = c(0.25, 0.25, 0.27, 0.23))

  # ---------------------------------------------------------------------------
  # Lifestyle / clinical variables
  # ---------------------------------------------------------------------------
  # ~20% of the population smokes
  smoker <- sample(c("yes", "no"), n, replace = TRUE,
                   prob = c(0.20, 0.80))

  # Exercise frequency: daily, weekly, rarely, never
  exercise_frequency <- sample(c("daily", "weekly", "rarely", "never"),
                               n, replace = TRUE,
                               prob = c(0.20, 0.35, 0.30, 0.15))

  # Hypertension: more prevalent in older, higher-BMI individuals
  # Use a logistic model to assign probability
  hyp_logit <- -4.5 + 0.05 * age + 0.08 * (bmi - 25) +
                0.8 * (smoker == "yes")
  hyp_prob  <- 1 / (1 + exp(-hyp_logit))
  hypertension <- ifelse(runif(n) < hyp_prob, "yes", "no")

  # ---------------------------------------------------------------------------
  # Healthcare charges — the response variable
  #
  # Base formula (inspired by the classic UCI insurance dataset):
  #   charges = base + age_effect + bmi_effect + smoking_effect +
  #             hypertension_effect + exercise_discount + children_effect +
  #             noise
  # ---------------------------------------------------------------------------

  # Base charge
  base_charge <- 1500

  # Age contribution: roughly $250 per year over 18
  age_effect  <- (age - 18) * 250

  # BMI contribution: quadratic – heavier individuals cost more
  bmi_effect  <- ifelse(bmi >= 30,
                        500 + (bmi - 30)^2 * 50,   # obesity premium
                        (bmi - 18.5) * 100)

  # Smoking: by far the largest driver (~$20 000 premium)
  smoking_base    <- ifelse(smoker == "yes", 20000, 0)
  # Extra interaction: smoking × obesity is particularly costly
  smoking_bmi_int <- ifelse(smoker == "yes" & bmi >= 30,
                            (bmi - 30) * 600, 0)

  # Hypertension adds ~$5 000
  hyp_effect  <- ifelse(hypertension == "yes", 5000, 0)

  # Exercise discount
  exercise_discount <- case_when(
    exercise_frequency == "daily"  ~ -1500,
    exercise_frequency == "weekly" ~ -800,
    exercise_frequency == "rarely" ~ -200,
    TRUE                           ~ 0
  )

  # Children: modest cost per child
  children_effect <- children * 400

  # Regional slight adjustments
  region_effect <- case_when(
    region == "northeast" ~  500,
    region == "northwest" ~  100,
    region == "southeast" ~ -200,
    TRUE                  ~ -100
  )

  # Lognormal noise to mimic real-world cost distributions
  noise <- rlnorm(n, meanlog = 7.0, sdlog = 0.6)

  # Assemble raw charges
  charges_raw <- base_charge + age_effect + bmi_effect +
                 smoking_base + smoking_bmi_int +
                 hyp_effect + exercise_discount +
                 children_effect + region_effect + noise

  # Floor at $500 (minimum realistic charge)
  healthcare_charges <- round(pmax(charges_raw, 500), 2)

  # ---------------------------------------------------------------------------
  # Risk label (used as SVM target)
  # "high_risk" if charges exceed the 70th percentile, else "low_risk"
  # ---------------------------------------------------------------------------
  threshold    <- quantile(healthcare_charges, 0.70)
  risk_label   <- ifelse(healthcare_charges >= threshold,
                         "high_risk", "low_risk")
  risk_label   <- factor(risk_label, levels = c("low_risk", "high_risk"))

  # ---------------------------------------------------------------------------
  # Assemble final data frame
  # ---------------------------------------------------------------------------
  healthcare_data <- data.frame(
    age                = age,
    sex                = sex,
    bmi                = bmi,
    children           = children,
    smoker             = smoker,
    region             = region,
    exercise_frequency = exercise_frequency,
    hypertension       = hypertension,
    healthcare_charges = healthcare_charges,
    risk_label         = risk_label,
    stringsAsFactors   = FALSE
  )

  cat(sprintf("Dataset generated: %d records, %d columns\n",
              nrow(healthcare_data), ncol(healthcare_data)))
  cat(sprintf("Charge range: $%.0f – $%.0f  |  Median: $%.0f\n",
              min(healthcare_charges),
              max(healthcare_charges),
              median(healthcare_charges)))
  cat(sprintf("High-risk proportion: %.1f%%\n",
              100 * mean(risk_label == "high_risk")))

  return(healthcare_data)
}
