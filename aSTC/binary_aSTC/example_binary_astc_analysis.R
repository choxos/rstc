################################################################################
##################### Binary Anchored STC Analysis Example ####################
################################################################################
# 
# This example demonstrates how to perform binary anchored STC analysis
# using the functions in binary_astc_analysis.R and binary_astc_html_reporting.R
#
# Based on NICE DSU Technical Support Document 18 methodology
#
# Author: Unanchored STC Analysis Package  
# Version: 1.0
# Last Updated: 2024
################################################################################

# Load required packages and functions
library(dplyr)
library(tidyr)

# Source the analysis and reporting functions
source("binary_astc_analysis.R")
source("binary_astc_html_reporting.R")

# Set seed for reproducibility
set.seed(42)

################################################################################
##################### Example Data Generation #################################
################################################################################

cat("Generating example data for anchored STC analysis...\n")

# Example 1: IPD from AB trial (Treatment A vs Treatment B)
# This represents individual patient data we have access to
n_ab <- 400
ipd_data <- data.frame(
  patient_id = 1:n_ab,
  treatment = rep(c("A", "B"), each = n_ab/2),
  age = c(rnorm(n_ab/2, mean = 65, sd = 10), rnorm(n_ab/2, mean = 63, sd = 10)),
  gender = c(rbinom(n_ab/2, 1, 0.6), rbinom(n_ab/2, 1, 0.65)),
  smoking = c(rbinom(n_ab/2, 1, 0.3), rbinom(n_ab/2, 1, 0.35))
)

# Generate outcome based on covariates and treatment
ipd_data$outcome_prob <- with(ipd_data, {
  linear_pred <- -1.2 + 
    0.02 * (age - 65) +           # Age effect (centered at 65)
    0.5 * gender +                # Gender effect (male = 1)
    0.3 * smoking +               # Smoking effect
    ifelse(treatment == "B", -0.8, 0)  # Treatment B effect
  
  # Add some treatment interactions
  linear_pred <- linear_pred + 
    ifelse(treatment == "B", 0.015 * (age - 65), 0)  # Age-treatment interaction
  
  1 / (1 + exp(-linear_pred))
})

ipd_data$outcome <- rbinom(n_ab, 1, ipd_data$outcome_prob)
ipd_data$outcome_prob <- NULL  # Remove probability column

# Convert gender and smoking to more intuitive coding
ipd_data$male <- ipd_data$gender
ipd_data$smoker <- ipd_data$smoking
ipd_data <- ipd_data[, c("patient_id", "treatment", "age", "male", "smoker", "outcome")]

cat("IPD Data Summary:\n")
print(ipd_data %>% 
  group_by(treatment) %>% 
  summarise(
    n = n(),
    age_mean = mean(age),
    male_prop = mean(male),
    smoker_prop = mean(smoker),
    outcome_events = sum(outcome),
    outcome_rate = mean(outcome),
    .groups = "drop"
  ))

################################################################################
##################### Example Aggregate Data ##################################
################################################################################

# Example 2: Aggregate data from AC trial (Treatment A vs Treatment C)
# This represents published summary statistics from another trial

# Example aggregate data summaries
agd_covariate_summaries <- list(
  age = list(mean = 68.5, sd = 12.2),      # Older population
  male = list(mean = 0.45, sd = NULL),     # Lower proportion of males
  smoker = list(mean = 0.25, sd = NULL)    # Lower smoking rate
)

# Outcome data for AC trial
agd_outcome_summaries <- list(
  A = list(events = 45, total = 150),      # Reference arm A
  C = list(events = 38, total = 150)       # New treatment C
)

# Create covariate mapping (maps IPD variables to AgD summaries)
covariate_mapping <- list(
  age = "age",
  male = "male", 
  smoker = "smoker"
)

# Define multiple models with different covariate adjustments
models <- list(
  "Naive" = character(0),                    # No covariates
  "Age_Adjusted" = c("age"),                 # Age only
  "Demographics" = c("age", "male"),         # Age + gender
  "Full_Model" = c("age", "male", "smoker")  # All covariates
)

cat("\nAggregate Data (AC Trial) Summary:\n")
cat("Treatment A: 45/150 events (30.0%)\n")
cat("Treatment C: 38/150 events (25.3%)\n")
cat("Mean age: 68.5 years\n")
cat("Male proportion: 45%\n")
cat("Smoking proportion: 25%\n")

cat("\nModels to be fitted:\n")
for (i in seq_along(models)) {
  model_name <- names(models)[i]
  covariates <- models[[i]]
  if (length(covariates) == 0) {
    cat("  ", model_name, ": No covariates (naive model)\n")
  } else {
    cat("  ", model_name, ": ", paste(covariates, collapse = ", "), "\n")
  }
}

################################################################################
##################### Anchored STC Analysis ###################################
################################################################################

# Run analysis with bootstrap (enhanced for debugging)
cat("\n================================================================================ \n")
cat("PERFORMING ANCHORED STC ANALYSIS\n")
cat("================================================================================ \n")

results <- analyze_anchored_stc(
  ipd_data = ipd_data,
  agd_covariate_summaries = agd_covariate_summaries,
  agd_outcome_summaries = agd_outcome_summaries,
  outcome_col = "outcome",
  treatment_col = "treatment",
  covariate_mapping = covariate_mapping,
  models = models,
  reference_arm = "A",
  ipd_comparator_arm = "B",
  agd_comparator_arm = "C",
  use_bootstrap = TRUE,  # Enable bootstrap
  n_bootstrap = 20,      # Reduced for debugging
  include_interactions = TRUE
)

# DEBUG: Check bootstrap results for first model
cat("\n================================================================================ \n")
cat("BOOTSTRAP DEBUG INFO\n")
cat("================================================================================ \n")

first_model <- names(models)[1]
bootstrap_results <- results$model_results[[first_model]]$bootstrap_cis

if (!is.null(bootstrap_results)) {
  cat("Bootstrap results found for model:", first_model, "\n")
  cat("Indirect OR CI:", bootstrap_results$indirect_effect$or_ci, "\n")
  cat("Indirect RR CI:", bootstrap_results$indirect_effect$rr_ci, "\n")
  cat("IPD OR CI:", bootstrap_results$ipd_effect$or_ci, "\n")
  cat("Successful samples:", bootstrap_results$n_successful, "out of", bootstrap_results$n_bootstrap, "\n")
} else {
  cat("ERROR: No bootstrap results found!\n")
}

# Display results
print(results)

################################################################################
##################### Results Summary ##########################################
################################################################################

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS RESULTS SUMMARY\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Extract key results from each model
model_names <- names(results$model_results)

cat("\n1. Model Comparison Summary:\n")
cat("   Model Name        | Type     | Covariates           | Indirect OR (95% CI)\n")
cat("   ", paste(rep("-", 70), collapse = ""), "\n")

for (model_name in model_names) {
  model_data <- results$model_results[[model_name]]
  indirect <- model_data$indirect_comparison
  
  # Format covariates
  if (length(model_data$covariates) == 0) {
    covariates_str <- "None"
  } else {
    covariates_str <- paste(model_data$covariates, collapse = ", ")
  }
  
  # Format type
  type_str <- ifelse(model_data$is_naive, "Naive", "Adjusted")
  
  cat("   ", sprintf("%-16s", model_name), "| ", sprintf("%-8s", type_str), 
      "| ", sprintf("%-20s", covariates_str), 
      "| ", sprintf("%.3f (%.3f, %.3f)", indirect$or, indirect$or_ci_lower, indirect$or_ci_upper), "\n")
}

cat("\n2. Aggregate Data Trial Result:\n")
agd_effect <- results$agd_effect
cat("   ", results$study_characteristics$agd_trial, "\n")
cat("   OR (95% CI):", sprintf("%.3f (%.3f, %.3f)", agd_effect$or, agd_effect$or_ci_lower, agd_effect$or_ci_upper), "\n")

cat("\n3. IPD Trial Results (", results$study_characteristics$ipd_trial, "):\n")
# Show results from the full model
full_model_data <- results$model_results[["Full_Model"]]
if (!is.null(full_model_data$enhanced_effect)) {
  effect <- full_model_data$enhanced_effect
  cat("   RR (95% CI):", effect$rr_formatted, "\n")
  cat("   OR (95% CI):", effect$or_formatted, "\n")
  cat("   RD (95% CI):", effect$rd_formatted, "\n")
  cat("   NNT (95% CI):", effect$nnt_formatted, "\n")
}

cat("\n4. Clinical Interpretation:\n")

# Use primary indirect comparison for interpretation
primary_indirect <- results$indirect_comparison

interpretation <- if (primary_indirect$or > 1.2) {
  "Treatment C appears to have higher odds of the outcome compared to Treatment B"
} else if (primary_indirect$or < 0.8) {
  "Treatment C appears to have lower odds of the outcome compared to Treatment B"  
} else {
  "No substantial difference detected between Treatment C and Treatment B"
}

significance <- if (primary_indirect$p_value < 0.05) {
  "statistically significant"
} else {
  "not statistically significant"
}

cat("   Primary result is", significance, "(p =", sprintf("%.4f", primary_indirect$p_value), ")\n")
cat("  ", interpretation, "\n")

# Model comparison
cat("\n5. Model Comparison:\n")
naive_result <- results$model_results[["Naive"]]$indirect_comparison
full_result <- results$model_results[["Full_Model"]]$indirect_comparison

or_difference <- abs(full_result$or - naive_result$or)
cat("   Naive OR:", sprintf("%.3f", naive_result$or), "\n")
cat("   Fully adjusted OR:", sprintf("%.3f", full_result$or), "\n")
cat("   Absolute difference:", sprintf("%.3f", or_difference), "\n")

if (or_difference > 0.2) {
  cat("   → Substantial difference detected - covariate adjustment is important\n")
} else {
  cat("   → Minimal difference detected - populations may be similar\n")
}

################################################################################
##################### HTML Report Generation ###############################
################################################################################

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("GENERATING HTML REPORT\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Generate comprehensive HTML report
report_path <- generate_binary_astc_html_report(
  results = results,
  project_name = "Example_Anchored_STC",
  output_dir = "reports",
  title = "Binary Anchored STC Analysis - Multiple Models Example"
)

cat("\nHTML report generated:", report_path, "\n")

################################################################################
##################### Model Validation ####################################
################################################################################

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MODEL VALIDATION\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Check model assumptions and fit for each model
for (model_name in model_names) {
  model_data <- results$model_results[[model_name]]
  
  cat("\n", model_name, "Model:\n")
  
  # Model convergence
  if (model_data$model$converged) {
    cat("   ✓ Model converged successfully\n")
  } else {
    cat("   ✗ Model failed to converge\n")
  }
  
  # Covariates
  if (length(model_data$covariates) == 0) {
    cat("   Covariates: None (naive model)\n")
  } else {
    cat("   Covariates:", paste(model_data$covariates, collapse = ", "), "\n")
  }
  
  # Model fit statistics
  reg_summary <- summary(model_data$model)
  cat("   AIC:", sprintf("%.2f", reg_summary$aic), "\n")
  cat("   Deviance:", sprintf("%.2f", reg_summary$deviance), "\n")
}

cat("\n2. Sample Sizes:\n")
cat("   IPD Trial (AB):", nrow(ipd_data), "patients\n")
cat("   AgD Trial (AC):", sum(sapply(agd_outcome_summaries, function(x) x$total)), "patients\n")

cat("\n3. Event Rates:\n")
ipd_summary <- results$data_summaries$ipd_summary
for (i in 1:nrow(ipd_summary)) {
  arm <- ipd_summary[i, ]
  cat("   IPD -", arm[[1]], ":", sprintf("%.1f%%", arm[[4]] * 100), 
      sprintf("(%d/%d)", arm[[3]], arm[[2]]), "\n")
}

agd_summary <- results$data_summaries$agd_summary  
for (i in 1:nrow(agd_summary)) {
  arm <- agd_summary[i, ]
  cat("   AgD -", arm[[1]], ":", sprintf("%.1f%%", arm[[4]] * 100), 
      sprintf("(%d/%d)", arm[[2]], arm[[3]]), "\n")
}

cat("\n4. Covariate Centering:\n")
# Check centering from first adjusted model
full_model <- results$model_results[["Full_Model"]]$model
if (!is.null(full_model$covariate_centers)) {
  for (cov in names(full_model$covariate_centers)) {
    center_val <- full_model$covariate_centers[[cov]]
    cat("  ", cov, "centered at:", sprintf("%.3f", center_val), "\n")
  }
} else {
  cat("   No covariates were centered\n")
}

################################################################################
##################### Analysis Notes #######################################
################################################################################

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS NOTES\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nAnchored STC Method:\n")
cat("• Uses individual patient data from AB trial and aggregate data from AC trial\n")
cat("• Connects trials through shared reference arm A\n")
cat("• Adjusts for population differences by centering IPD covariates on AgD means\n")
cat("• Accounts for effect modification through treatment-covariate interactions\n")
cat("• Provides indirect comparison of C vs B adjusted for population differences\n")

cat("\nKey Assumptions:\n")
cat("• Shared reference arm A has similar characteristics across trials\n")
cat("• Effect modifiers are correctly identified and measured\n")
cat("• No unmeasured confounders that differ between populations\n")
cat("• Logistic regression model is correctly specified\n")
cat("• Treatment-covariate interactions are appropriately modeled\n")

cat("\nInterpretation Guidelines:\n")
cat("• Results represent the treatment effect in the aggregate data population\n")
cat("• Confidence intervals account for uncertainty in both trials\n")
cat("• P-values test the null hypothesis of no difference between C and B\n")
cat("• Consider clinical significance in addition to statistical significance\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("Report saved to:", report_path, "\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

################################################################################
##################### Optional: Advanced Analysis ##########################
################################################################################

# Uncomment the following section for additional analyses

# cat("\n=== ADVANCED ANALYSIS (OPTIONAL) ===\n")
# 
# # Sensitivity analysis with different covariate specifications
# cat("\nSensitivity Analysis - Naive Model (No Covariates):\n")
# 
# # Fit naive model without covariates
# naive_results <- analyze_anchored_stc(
#   ipd_data = ipd_data,
#   agd_covariate_summaries = list(),  # No covariates
#   agd_outcome_summaries = ac_outcome_summaries,
#   outcome_col = "outcome",
#   treatment_col = "treatment", 
#   covariate_cols = character(0),     # No covariates
#   reference_arm = "A",
#   ipd_comparator_arm = "B",
#   agd_comparator_arm = "C",
#   include_interactions = FALSE,
#   use_robust_se = TRUE
# )
# 
# naive_indirect <- naive_results$indirect_comparison
# adjusted_indirect <- results$indirect_comparison
# 
# cat("Naive OR (95% CI):", sprintf("%.3f (%.3f, %.3f)", 
#     naive_indirect$or, naive_indirect$or_ci_lower, naive_indirect$or_ci_upper), "\n")
# cat("Adjusted OR (95% CI):", sprintf("%.3f (%.3f, %.3f)", 
#     adjusted_indirect$or, adjusted_indirect$or_ci_lower, adjusted_indirect$or_ci_upper), "\n")
# 
# # Calculate difference
# or_diff <- abs(adjusted_indirect$or - naive_indirect$or)
# cat("Absolute difference in OR:", sprintf("%.3f", or_diff), "\n")
# 
# if (or_diff > 0.1) {
#   cat("→ Substantial difference detected - covariate adjustment is important\n")
# } else {
#   cat("→ Minimal difference detected - populations may be similar\n")
# }

# End of example 