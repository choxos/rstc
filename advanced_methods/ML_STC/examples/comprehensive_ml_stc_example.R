################################################################################
##################### Comprehensive ML-STC Analysis Example ###################
################################################################################
#
# This script demonstrates all machine learning methods for STC analysis:
# 1. Tree-Based Methods: Causal Forests, BART, XGBoost/LightGBM
# 2. Meta-Learning: T-Learner, X-Learner, R-Learner, Double ML
# 3. Specialized Causal: TMLE, Causal Neural Networks
# 4. Ensemble Methods: AIPW, GANs
#
# Author: Advanced STC Methods Package
# Date: 2024

# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Source all ML STC functions
source("../utils/ml_stc_utils.R")
source("../tree_based/causal_forests_stc.R")
source("../tree_based/bart_stc.R")
source("../tree_based/xgboost_stc.R")
source("../meta_learning/meta_learners_stc.R")
source("../meta_learning/double_ml_stc.R")
source("../specialized_causal/tmle_stc.R")
source("../specialized_causal/causal_neural_networks_stc.R")
source("../ensemble_methods/aipw_stc.R")
source("../ensemble_methods/gans_causal_stc.R")

################################################################################
# Data Simulation
################################################################################

set.seed(2024)

# Sample sizes
n_trial <- 800
n_target <- 1000

# Generate realistic clinical trial data
generate_clinical_data <- function(n) {
  # Patient demographics
  age <- rnorm(n, 65, 12)
  age <- pmax(18, pmin(90, age))  # Constrain age
  
  # Comorbidities (correlated)
  diabetes <- rbinom(n, 1, plogis(-1 + 0.02 * age))
  hypertension <- rbinom(n, 1, plogis(-0.5 + 0.01 * age + 0.5 * diabetes))
  
  # Laboratory values
  creatinine <- rnorm(n, 1.1, 0.3)
  creatinine <- pmax(0.5, creatinine)
  
  # Functional status
  ejection_fraction <- rnorm(n, 45, 10)
  ejection_fraction <- pmax(15, pmin(75, ejection_fraction))
  
  # Risk score
  risk_score <- 0.1 * scale(age)[,1] + 0.3 * diabetes + 0.2 * hypertension + 
                0.15 * scale(creatinine)[,1] - 0.1 * scale(ejection_fraction)[,1]
  
  return(data.frame(
    age = age,
    diabetes = diabetes,
    hypertension = hypertension,
    creatinine = creatinine,
    ejection_fraction = ejection_fraction,
    risk_score = risk_score
  ))
}

# Generate trial data
trial_data <- generate_clinical_data(n_trial)

# Treatment assignment (non-random, based on patient characteristics)
ps_true <- plogis(-0.5 + 0.3 * trial_data$risk_score + 
                  0.2 * trial_data$diabetes - 0.1 * scale(trial_data$age)[,1])
trial_data$treatment <- rbinom(n_trial, 1, ps_true)

# Generate outcomes with heterogeneous treatment effects
base_outcome <- plogis(-1.5 + 0.4 * trial_data$risk_score + 
                       0.3 * trial_data$diabetes + 0.002 * trial_data$age)

# Heterogeneous treatment effect (stronger in high-risk patients)
treatment_effect <- -0.3 - 0.2 * trial_data$risk_score - 
                    0.15 * trial_data$diabetes * (trial_data$age > 70)

# Generate outcomes
prob_outcome <- plogis(qlogis(base_outcome) + 
                      trial_data$treatment * treatment_effect)
trial_data$outcome <- rbinom(n_trial, 1, prob_outcome)

# Generate target population
target_data <- generate_clinical_data(n_target)
target_data$age <- target_data$age + 3  # Slightly older
target_data$diabetes <- rbinom(n_target, 1, plogis(qlogis(0.25) + 0.01 * target_data$age))

# Define covariate columns
covariate_cols <- c("age", "diabetes", "hypertension", "creatinine", "ejection_fraction", "risk_score")

# Display data summary
cat("=== Data Summary ===\n")
cat("Trial Data:\n")
print(summary(trial_data))
cat("\nTarget Population:\n")
print(summary(target_data))

################################################################################
# Tree-Based Methods
################################################################################

cat("\n=== Tree-Based Methods ===\n")

# XGBoost Analysis
cat("\n--- XGBoost Analysis ---\n")
xgb_results <- tryCatch({
  xgboost_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    target_population = target_data,
    method = "xgboost",
    approach = "separate_models",
    tune_params = TRUE,
    cv_folds = 5,
    seed = 2024
  )
}, error = function(e) {
  cat("XGBoost analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(xgb_results)) {
  cat(sprintf("XGBoost ATE: %.4f\n", xgb_results$ate_estimate))
  if (!is.null(xgb_results$target_predictions)) {
    cat(sprintf("Target Population ATE: %.4f\n", xgb_results$target_predictions$ate_estimate))
  }
}

# Causal Forests (requires grf package)
cat("\n--- Causal Forests Analysis ---\n")
cf_results <- tryCatch({
  if (requireNamespace("grf", quietly = TRUE)) {
    causal_forest_stc_analysis(
      data = trial_data,
      outcome_col = "outcome",
      treatment_col = "treatment",
      covariate_cols = covariate_cols,
      target_population = target_data,
      num_trees = 1000,  # Reduced for speed
      seed = 2024
    )
  } else {
    cat("grf package not available for Causal Forests\n")
    return(NULL)
  }
}, error = function(e) {
  cat("Causal Forest analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(cf_results)) {
  cat(sprintf("Causal Forest ATE: %.4f (SE: %.4f)\n", 
              cf_results$ate_estimate, cf_results$ate_se))
  if (!is.null(cf_results$target_predictions)) {
    cat(sprintf("Target Population ATE: %.4f\n", cf_results$target_predictions$ate_estimate))
  }
}

# BART Analysis (requires BART package)
cat("\n--- BART Analysis ---\n")
bart_results <- tryCatch({
  if (requireNamespace("BART", quietly = TRUE)) {
    bart_stc_analysis(
      data = trial_data,
      outcome_col = "outcome",
      treatment_col = "treatment",
      covariate_cols = covariate_cols,
      target_population = target_data,
      n_trees = 100,  # Reduced for speed
      n_burn = 500,
      n_sim = 500,
      seed = 2024
    )
  } else {
    cat("BART package not available\n")
    return(NULL)
  }
}, error = function(e) {
  cat("BART analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(bart_results)) {
  cat(sprintf("BART ATE: %.4f (CI: [%.4f, %.4f])\n", 
              bart_results$ate_estimate, bart_results$ci_lower, bart_results$ci_upper))
}

################################################################################
# Meta-Learning Approaches
################################################################################

cat("\n=== Meta-Learning Approaches ===\n")

# T-Learner
cat("\n--- T-Learner ---\n")
t_results <- tryCatch({
  t_learner_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    base_learner = "rf",
    target_population = target_data,
    seed = 2024
  )
}, error = function(e) {
  cat("T-Learner analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(t_results)) {
  cat(sprintf("T-Learner ATE: %.4f\n", t_results$ate_estimate))
}

# X-Learner
cat("\n--- X-Learner ---\n")
x_results <- tryCatch({
  x_learner_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    base_learner = "rf",
    target_population = target_data,
    seed = 2024
  )
}, error = function(e) {
  cat("X-Learner analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(x_results)) {
  cat(sprintf("X-Learner ATE: %.4f\n", x_results$ate_estimate))
}

# R-Learner
cat("\n--- R-Learner ---\n")
r_results <- tryCatch({
  r_learner_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    base_learner = "elastic_net",
    target_population = target_data,
    seed = 2024
  )
}, error = function(e) {
  cat("R-Learner analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(r_results)) {
  cat(sprintf("R-Learner ATE: %.4f\n", r_results$ate_estimate))
}

# Double Machine Learning
cat("\n--- Double Machine Learning ---\n")
dml_results <- tryCatch({
  double_ml_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    ml_method = "superlearner",
    n_folds = 5,
    target_population = target_data,
    seed = 2024
  )
}, error = function(e) {
  cat("DML analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(dml_results)) {
  cat(sprintf("DML ATE: %.4f (SE: %.4f)\n", 
              dml_results$ate_estimate, dml_results$ate_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n", dml_results$ci_lower, dml_results$ci_upper))
}

################################################################################
# Specialized Causal Methods
################################################################################

cat("\n=== Specialized Causal Methods ===\n")

# TMLE with Super Learner
cat("\n--- TMLE Analysis ---\n")
tmle_results <- tryCatch({
  if (requireNamespace("SuperLearner", quietly = TRUE) && 
      requireNamespace("tmle", quietly = TRUE)) {
    tmle_stc_analysis(
      data = trial_data,
      outcome_col = "outcome",
      treatment_col = "treatment",
      covariate_cols = covariate_cols,
      Q_SL_library = c("SL.glm", "SL.randomForest"),
      g_SL_library = c("SL.glm", "SL.randomForest"),
      target_population = target_data,
      seed = 2024
    )
  } else {
    cat("SuperLearner or tmle package not available\n")
    return(NULL)
  }
}, error = function(e) {
  cat("TMLE analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(tmle_results)) {
  cat(sprintf("TMLE ATE: %.4f (SE: %.4f)\n", 
              tmle_results$ate_estimate, tmle_results$ate_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n", tmle_results$ci_lower, tmle_results$ci_upper))
}

# Causal Neural Networks
cat("\n--- Causal Neural Networks ---\n")
nn_results <- tryCatch({
  # Check if Python environment is available
  if (requireNamespace("reticulate", quietly = TRUE)) {
    nn_env <- setup_python_env(install_packages = FALSE)
    if (nn_env$status == "success") {
      # TarNet analysis (simplified for speed)
      tarnet_stc_analysis(
        data = trial_data,
        outcome_col = "outcome",
        treatment_col = "treatment",
        covariate_cols = covariate_cols,
        target_population = target_data,
        representation_dim = 16,
        hidden_layers = c(32, 16),
        epochs = 50,  # Reduced for speed
        seed = 2024
      )
    } else {
      cat("Python environment not available for neural networks\n")
      return(NULL)
    }
  } else {
    cat("reticulate package not available\n")
    return(NULL)
  }
}, error = function(e) {
  cat("Neural network analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(nn_results)) {
  cat(sprintf("TarNet ATE: %.4f\n", nn_results$ate_estimate))
}

################################################################################
# Advanced Ensemble Methods
################################################################################

cat("\n=== Advanced Ensemble Methods ===\n")

# AIPW with Machine Learning
cat("\n--- AIPW Analysis ---\n")
aipw_results <- tryCatch({
  aipw_stc_analysis(
    data = trial_data,
    outcome_col = "outcome",
    treatment_col = "treatment",
    covariate_cols = covariate_cols,
    ps_method = "rf",
    outcome_method = "rf",
    cross_fit = TRUE,
    n_folds = 5,
    target_population = target_data,
    seed = 2024
  )
}, error = function(e) {
  cat("AIPW analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(aipw_results)) {
  cat(sprintf("AIPW ATE: %.4f (SE: %.4f)\n", 
              aipw_results$ate_estimate, aipw_results$ate_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n", aipw_results$ci_lower, aipw_results$ci_upper))
  cat(sprintf("Propensity Score Overlap: %.3f\n", aipw_results$diagnostics$ps_overlap))
  cat(sprintf("Double Robust Score: %.3f\n", aipw_results$diagnostics$double_robust_score))
}

# GANs for Causal Inference
cat("\n--- GANs Analysis ---\n")
gan_results <- tryCatch({
  if (requireNamespace("reticulate", quietly = TRUE)) {
    gan_env <- setup_gan_env(install_packages = FALSE)
    if (gan_env$status == "success") {
      # GANITE analysis (simplified for speed)
      ganite_stc_analysis(
        data = trial_data,
        outcome_col = "outcome",
        treatment_col = "treatment",
        covariate_cols = covariate_cols,
        target_population = target_data,
        generator_dim = c(32, 16),
        discriminator_dim = c(16, 8),
        epochs = 50,  # Reduced for speed
        seed = 2024
      )
    } else {
      cat("Python environment not available for GANs\n")
      return(NULL)
    }
  } else {
    cat("reticulate package not available\n")
    return(NULL)
  }
}, error = function(e) {
  cat("GAN analysis failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(gan_results)) {
  cat(sprintf("GANITE ATE: %.4f\n", gan_results$ate_estimate))
}

################################################################################
# Results Summary
################################################################################

cat("\n=== Results Summary ===\n")

# Collect all results
results_list <- list(
  "XGBoost" = xgb_results,
  "Causal Forest" = cf_results,
  "BART" = bart_results,
  "T-Learner" = t_results,
  "X-Learner" = x_results,
  "R-Learner" = r_results,
  "Double ML" = dml_results,
  "TMLE" = tmle_results,
  "TarNet" = nn_results,
  "AIPW" = aipw_results,
  "GANITE" = gan_results
)

# Create summary table
summary_data <- data.frame(
  Method = character(),
  ATE_Estimate = numeric(),
  SE = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  Target_ATE = numeric(),
  stringsAsFactors = FALSE
)

for (method_name in names(results_list)) {
  result <- results_list[[method_name]]
  if (!is.null(result)) {
    ate <- result$ate_estimate
    se <- ifelse(is.null(result$ate_se), NA, result$ate_se)
    ci_lower <- ifelse(is.null(result$ci_lower), NA, result$ci_lower)
    ci_upper <- ifelse(is.null(result$ci_upper), NA, result$ci_upper)
    target_ate <- ifelse(is.null(result$target_predictions), NA, 
                        result$target_predictions$ate_estimate)
    
    summary_data <- rbind(summary_data, data.frame(
      Method = method_name,
      ATE_Estimate = ate,
      SE = se,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Target_ATE = target_ate,
      stringsAsFactors = FALSE
    ))
  }
}

# Display summary
if (nrow(summary_data) > 0) {
  cat("\nMethod Comparison:\n")
  print(summary_data, digits = 4, row.names = FALSE)
  
  # Basic statistics
  valid_estimates <- summary_data$ATE_Estimate[!is.na(summary_data$ATE_Estimate)]
  if (length(valid_estimates) > 1) {
    cat(sprintf("\nATE Estimate Range: [%.4f, %.4f]\n", 
                min(valid_estimates), max(valid_estimates)))
    cat(sprintf("Mean ATE Estimate: %.4f\n", mean(valid_estimates)))
    cat(sprintf("SD of ATE Estimates: %.4f\n", sd(valid_estimates)))
  }
  
  # Target population estimates
  valid_target <- summary_data$Target_ATE[!is.na(summary_data$Target_ATE)]
  if (length(valid_target) > 1) {
    cat(sprintf("\nTarget Population ATE Range: [%.4f, %.4f]\n", 
                min(valid_target), max(valid_target)))
    cat(sprintf("Mean Target ATE: %.4f\n", mean(valid_target)))
  }
} else {
  cat("No successful analyses to summarize.\n")
}

################################################################################
# Visualization
################################################################################

cat("\n=== Creating Visualizations ===\n")

tryCatch({
  # Create comparison plot if we have results
  if (nrow(summary_data) > 0) {
    
    # ATE estimates plot
    plot_data <- summary_data[!is.na(summary_data$ATE_Estimate), ]
    
    if (nrow(plot_data) > 0) {
      p1 <- ggplot(plot_data, aes(x = reorder(Method, ATE_Estimate), y = ATE_Estimate)) +
        geom_point(size = 3, color = "steelblue") +
        geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                      width = 0.2, alpha = 0.7, na.rm = TRUE) +
        coord_flip() +
        labs(title = "ATE Estimates Across ML Methods",
             x = "Method", y = "ATE Estimate") +
        theme_minimal() +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
      
      print(p1)
      
      # Save plot if possible
      tryCatch({
        ggsave("ml_stc_comparison.png", plot = p1, width = 10, height = 6, dpi = 300)
        cat("Comparison plot saved as 'ml_stc_comparison.png'\n")
      }, error = function(e) {
        cat("Could not save plot:", e$message, "\n")
      })
    }
  }
  
  # Data visualization
  p2 <- ggplot(trial_data, aes(x = risk_score, y = as.numeric(outcome), 
                               color = factor(treatment))) +
    geom_smooth(method = "loess", se = TRUE) +
    geom_point(alpha = 0.5) +
    labs(title = "Outcome by Risk Score and Treatment",
         x = "Risk Score", y = "Outcome Probability",
         color = "Treatment") +
    theme_minimal()
  
  print(p2)
  
}, error = function(e) {
  cat("Visualization failed:", e$message, "\n")
})

################################################################################
# Recommendations
################################################################################

cat("\n=== Recommendations ===\n")
cat("1. For robustness: Use doubly robust methods (AIPW, DML, TMLE)\n")
cat("2. For heterogeneity: Use Causal Forests, neural networks, or meta-learners\n")
cat("3. For interpretability: Use tree-based methods or linear meta-learners\n")
cat("4. For complex confounding: Consider neural networks or GANs\n")
cat("5. For uncertainty quantification: Use BART or TMLE\n")

cat("\n=== Analysis Complete ===\n")
cat("This comprehensive analysis demonstrates the application of advanced\n")
cat("machine learning methods to STC analysis. Choose methods based on your\n")
cat("specific research question, data characteristics, and computational constraints.\n") 