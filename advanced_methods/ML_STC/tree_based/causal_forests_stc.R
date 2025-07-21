################################################################################
##################### Causal Forests for STC Analysis #########################
################################################################################
#
# Implementation of Causal Forests for Simulated Treatment Comparison
# using the grf (Generalized Random Forests) package.
#
# Causal Forests provide:
# - Honest estimation of heterogeneous treatment effects
# - Valid confidence intervals and hypothesis tests
# - Feature importance for effect modification
# - Subgroup identification and characterization
#
# Based on:
# - Wager & Athey (2018): "Estimation and Inference of Heterogeneous Treatment Effects using Random Forests"
# - Athey et al. (2019): "Generalized random forests"
#
# Author: Advanced STC Methods Package - ML Extension
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("grf")) install.packages("grf")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")

library(grf)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Source utility functions
source("../utils/ml_stc_utils.R")

################################################################################
##################### Core Causal Forests Functions ############################
################################################################################

#' Causal Forest STC Analysis
#' 
#' Performs comprehensive causal forest analysis for simulated treatment comparison
#' including heterogeneous treatment effect estimation, variable importance,
#' and subgroup analysis.
#' 
#' @param data Data frame containing all variables
#' @param outcome_col Name of outcome column
#' @param treatment_col Name of treatment column
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional data frame specifying target population characteristics
#' @param num_trees Number of trees in the forest (default: 2000)
#' @param min_node_size Minimum node size (default: 5)
#' @param sample_fraction Fraction of observations to sample (default: 0.5)
#' @param honesty Whether to use honest estimation (default: TRUE)
#' @param seed Random seed for reproducibility
#' @return List containing comprehensive causal forest results
#' @export
causal_forest_stc_analysis <- function(data,
                                      outcome_col,
                                      treatment_col, 
                                      covariate_cols,
                                      target_population = NULL,
                                      num_trees = 2000,
                                      min_node_size = 5,
                                      sample_fraction = 0.5,
                                      honesty = TRUE,
                                      seed = 123) {
  
  cat("################################################################################\n")
  cat("##################### CAUSAL FOREST STC ANALYSIS #############################\n")
  cat("################################################################################\n\n")
  
  set.seed(seed)
  
  # Prepare data
  cat("Step 1: Data preparation...\n")
  data_prep <- prepare_ml_stc_data(data, outcome_col, treatment_col, covariate_cols)
  
  Y <- data_prep$Y
  W <- data_prep$W
  X <- data_prep$X
  
  # Validate minimum requirements for causal forests
  if (data_prep$n_treated < 10 || data_prep$n_control < 10) {
    stop("Causal forests require at least 10 observations in each treatment group")
  }
  
  cat("Step 2: Fitting causal forest...\n")
  cat("Forest parameters:\n")
  cat("  Number of trees:", num_trees, "\n")
  cat("  Minimum node size:", min_node_size, "\n")
  cat("  Sample fraction:", sample_fraction, "\n")
  cat("  Honesty:", honesty, "\n\n")
  
  # Fit causal forest
  cf <- causal_forest(
    X = X,
    Y = Y,
    W = W,
    num.trees = num_trees,
    min.node.size = min_node_size,
    sample.fraction = sample_fraction,
    honesty = honesty,
    seed = seed
  )
  
  cat("Causal forest fitted successfully!\n")
  cat("OOB prediction error:", round(cf$debiased.error, 4), "\n\n")
  
  # Step 3: Get treatment effect predictions
  cat("Step 3: Predicting individual treatment effects...\n")
  
  # Predict treatment effects with confidence intervals
  predictions <- predict(cf, estimate.variance = TRUE)
  tau_hat <- predictions$predictions
  tau_var <- predictions$variance.estimates
  tau_se <- sqrt(tau_var)
  
  # Calculate confidence intervals
  tau_ci_lower <- tau_hat - 1.96 * tau_se
  tau_ci_upper <- tau_hat + 1.96 * tau_se
  
  # Step 4: Calculate aggregate estimates
  cat("Step 4: Calculating aggregate treatment effects...\n")
  
  # Average Treatment Effect (ATE)
  ate <- average_treatment_effect(cf)
  
  cat("Average Treatment Effect (ATE):\n")
  cat("  Estimate:", round(ate[1], 4), "\n")
  cat("  Standard Error:", round(ate[2], 4), "\n")
  cat("  95% CI: (", round(ate[1] - 1.96 * ate[2], 4), ", ", 
      round(ate[1] + 1.96 * ate[2], 4), ")\n\n")
  
  # Step 5: Variable importance
  cat("Step 5: Calculating variable importance...\n")
  
  var_imp <- variable_importance(cf)
  var_imp_df <- data.frame(
    variable = data_prep$covariate_names,
    importance = var_imp
  ) %>%
    arrange(desc(importance))
  
  cat("Variable Importance (top 5):\n")
  print(head(var_imp_df, 5))
  cat("\n")
  
  # Step 6: Heterogeneity analysis
  cat("Step 6: Analyzing treatment effect heterogeneity...\n")
  
  # Test for heterogeneity
  het_test <- test_calibration(cf)
  
  cat("Heterogeneity Test:\n")
  cat("  Best linear fit coefficient:", round(het_test[1], 4), "\n")
  cat("  Standard error:", round(het_test[2], 4), "\n")
  cat("  P-value:", round(2 * (1 - pnorm(abs(het_test[1] / het_het[2]))), 4), "\n")
  
  # Differentiation test (omnibus test for heterogeneity)
  diff_test <- tryCatch({
    test_calibration(cf)
  }, error = function(e) {
    cat("Could not perform differentiation test\n")
    c(NA, NA)
  })
  
  # Step 7: Target population prediction (if provided)
  target_results <- NULL
  if (!is.null(target_population)) {
    cat("Step 7: Predicting effects for target population...\n")
    
    # Ensure target population has same covariates
    target_X <- as.matrix(target_population[, covariate_cols, drop = FALSE])
    
    target_pred <- predict(cf, target_X, estimate.variance = TRUE)
    target_tau <- target_pred$predictions
    target_se <- sqrt(target_pred$variance.estimates)
    
    # Average effect in target population
    target_ate <- mean(target_tau)
    target_ate_se <- sd(target_tau) / sqrt(length(target_tau))
    
    cat("Target Population ATE:", round(target_ate, 4), "\n")
    cat("Standard Error:", round(target_ate_se, 4), "\n")
    cat("95% CI: (", round(target_ate - 1.96 * target_ate_se, 4), ", ", 
        round(target_ate + 1.96 * target_ate_se, 4), ")\n\n")
    
    target_results <- list(
      target_effects = target_tau,
      target_se = target_se,
      target_ate = target_ate,
      target_ate_se = target_ate_se
    )
  }
  
  # Step 8: Identify treatment effect subgroups
  cat("Step 8: Identifying treatment effect subgroups...\n")
  
  # Simple quartile-based subgroups
  quartiles <- quantile(tau_hat, probs = c(0.25, 0.5, 0.75))
  subgroups <- cut(tau_hat, 
                   breaks = c(-Inf, quartiles, Inf),
                   labels = c("Low", "Medium-Low", "Medium-High", "High"))
  
  subgroup_summary <- data.frame(
    individual_id = 1:length(tau_hat),
    predicted_effect = tau_hat,
    effect_se = tau_se,
    subgroup = subgroups
  ) %>%
    group_by(subgroup) %>%
    summarise(
      n = n(),
      mean_effect = mean(predicted_effect),
      se_effect = sd(predicted_effect) / sqrt(n()),
      .groups = "drop"
    )
  
  cat("Treatment Effect Subgroups:\n")
  print(subgroup_summary)
  cat("\n")
  
  # Compile comprehensive results
  results <- list(
    # Core results
    causal_forest = cf,
    predictions = list(
      tau_hat = tau_hat,
      tau_se = tau_se,
      tau_ci_lower = tau_ci_lower,
      tau_ci_upper = tau_ci_upper
    ),
    
    # Aggregate effects
    ate = list(
      estimate = ate[1],
      se = ate[2],
      ci_lower = ate[1] - 1.96 * ate[2],
      ci_upper = ate[1] + 1.96 * ate[2]
    ),
    
    # Variable importance
    variable_importance = var_imp_df,
    
    # Heterogeneity tests
    heterogeneity = list(
      calibration_test = het_test,
      differentiation_test = diff_test
    ),
    
    # Subgroup analysis
    subgroups = list(
      assignments = subgroups,
      summary = subgroup_summary
    ),
    
    # Target population results
    target_population = target_results,
    
    # Data and parameters
    data_info = data_prep,
    parameters = list(
      num_trees = num_trees,
      min_node_size = min_node_size,
      sample_fraction = sample_fraction,
      honesty = honesty,
      seed = seed
    )
  )
  
  class(results) <- "causal_forest_stc"
  
  cat("=== CAUSAL FOREST STC ANALYSIS COMPLETE ===\n")
  cat("Average Treatment Effect:", round(results$ate$estimate, 4), "\n")
  cat("95% CI: (", round(results$ate$ci_lower, 4), ", ", round(results$ate$ci_upper, 4), ")\n")
  cat("Effect heterogeneity detected:", 
      ifelse(results$heterogeneity$calibration_test[1] > 0.1, "Yes", "No"), "\n\n")
  
  return(results)
}

################################################################################
##################### Visualization Functions ###################################
################################################################################

#' Plot Causal Forest Results
#' 
#' @param cf_results Results from causal_forest_stc_analysis
#' @param show_subgroups Whether to highlight subgroups
#' @return List of ggplot objects
#' @export
plot.causal_forest_stc <- function(cf_results, show_subgroups = TRUE) {
  
  tau_hat <- cf_results$predictions$tau_hat
  tau_se <- cf_results$predictions$tau_se
  subgroups <- cf_results$subgroups$assignments
  var_imp <- cf_results$variable_importance
  X <- cf_results$data_info$X
  
  plots <- list()
  
  # 1. Treatment effect distribution
  p1 <- ggplot(data.frame(tau = tau_hat), aes(x = tau)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = cf_results$ate$estimate, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = cf_results$ate$ci_lower, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    geom_vline(xintercept = cf_results$ate$ci_upper, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    labs(title = "Distribution of Individual Treatment Effects",
         subtitle = paste("ATE =", round(cf_results$ate$estimate, 3)),
         x = "Treatment Effect",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$distribution <- p1
  
  # 2. Variable importance
  p2 <- ggplot(head(var_imp, 10), aes(x = reorder(variable, importance), y = importance)) +
    geom_col(fill = "forestgreen", alpha = 0.7) +
    coord_flip() +
    labs(title = "Variable Importance for Treatment Effect Heterogeneity",
         x = "Variable",
         y = "Importance") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$importance <- p2
  
  # 3. Treatment effects vs. top important variable
  if (nrow(var_imp) > 0) {
    top_var <- var_imp$variable[1]
    top_var_idx <- which(cf_results$data_info$covariate_names == top_var)
    
    plot_data <- data.frame(
      tau = tau_hat,
      top_variable = X[, top_var_idx],
      subgroup = if(show_subgroups) subgroups else "All"
    )
    
    p3 <- ggplot(plot_data, aes(x = top_variable, y = tau)) +
      geom_point(aes(color = subgroup), alpha = 0.6) +
      geom_smooth(method = "loess", color = "black", linetype = "dashed") +
      geom_hline(yintercept = cf_results$ate$estimate, 
                 color = "red", linetype = "solid") +
      labs(title = paste("Treatment Effects vs.", top_var),
           x = top_var,
           y = "Treatment Effect",
           color = "Subgroup") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    plots$scatter <- p3
  }
  
  # 4. Subgroup effects
  if (show_subgroups) {
    subgroup_data <- cf_results$subgroups$summary
    
    p4 <- ggplot(subgroup_data, aes(x = subgroup, y = mean_effect)) +
      geom_col(fill = "orange", alpha = 0.7) +
      geom_errorbar(aes(ymin = mean_effect - 1.96 * se_effect,
                       ymax = mean_effect + 1.96 * se_effect),
                   width = 0.2) +
      geom_hline(yintercept = cf_results$ate$estimate, 
                 color = "red", linetype = "dashed") +
      geom_text(aes(label = paste("n =", n)), vjust = -0.5) +
      labs(title = "Average Treatment Effects by Subgroup",
           x = "Effect Subgroup",
           y = "Average Treatment Effect") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    plots$subgroups <- p4
  }
  
  # 5. Confidence intervals plot
  effect_data <- data.frame(
    individual = 1:length(tau_hat),
    effect = tau_hat,
    ci_lower = cf_results$predictions$tau_ci_lower,
    ci_upper = cf_results$predictions$tau_ci_upper
  ) %>%
    arrange(effect) %>%
    mutate(rank = row_number())
  
  # Sample for visualization if too many points
  if (nrow(effect_data) > 500) {
    sample_idx <- sample(nrow(effect_data), 500)
    effect_data <- effect_data[sample_idx, ]
  }
  
  p5 <- ggplot(effect_data, aes(x = rank, y = effect)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                 alpha = 0.3, width = 0) +
    geom_hline(yintercept = cf_results$ate$estimate, 
               color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
    labs(title = "Individual Treatment Effects with Confidence Intervals",
         subtitle = "Sorted by effect size",
         x = "Individual (ranked by effect)",
         y = "Treatment Effect") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$confidence_intervals <- p5
  
  return(plots)
}

################################################################################
##################### Specialized Analysis Functions ########################### 
################################################################################

#' Identify Best Treatment Policy Using Causal Forests
#' 
#' @param cf_results Results from causal_forest_stc_analysis
#' @param cost_ratio Cost ratio of treatment vs. control
#' @return Treatment policy recommendations
#' @export
identify_treatment_policy <- function(cf_results, cost_ratio = 1) {
  
  tau_hat <- cf_results$predictions$tau_hat
  tau_se <- cf_results$predictions$tau_se
  
  # Simple policy: treat if expected benefit > cost
  treat_policy <- tau_hat > cost_ratio
  
  # Calculate policy value
  policy_value <- mean(pmax(tau_hat - cost_ratio, 0))
  
  # Conservative policy using confidence intervals
  conservative_treat <- (tau_hat - 1.96 * tau_se) > cost_ratio
  conservative_value <- mean(pmax(tau_hat - cost_ratio, 0) * conservative_treat)
  
  cat("=== TREATMENT POLICY ANALYSIS ===\n")
  cat("Cost ratio (treatment vs control):", cost_ratio, "\n")
  cat("Proportion recommended for treatment:", round(mean(treat_policy), 3), "\n")
  cat("Expected policy value:", round(policy_value, 4), "\n")
  cat("Conservative policy (lower CI):", round(mean(conservative_treat), 3), "\n")
  cat("Conservative policy value:", round(conservative_value, 4), "\n\n")
  
  return(list(
    treatment_policy = treat_policy,
    conservative_policy = conservative_treat,
    policy_value = policy_value,
    conservative_value = conservative_value,
    cost_ratio = cost_ratio
  ))
}

#' Perform Subgroup Analysis with Statistical Testing
#' 
#' @param cf_results Results from causal_forest_stc_analysis
#' @param subgroup_var Name of variable to define subgroups
#' @param threshold Threshold for binary subgrouping (optional)
#' @return Subgroup analysis results
#' @export
subgroup_analysis <- function(cf_results, subgroup_var, threshold = NULL) {
  
  X <- cf_results$data_info$X
  var_names <- cf_results$data_info$covariate_names
  tau_hat <- cf_results$predictions$tau_hat
  tau_se <- cf_results$predictions$tau_se
  
  # Find the subgroup variable
  var_idx <- which(var_names == subgroup_var)
  if (length(var_idx) == 0) {
    stop("Subgroup variable not found in covariates")
  }
  
  subgroup_values <- X[, var_idx]
  
  # Create subgroups
  if (is.null(threshold)) {
    # Use median split for continuous variables
    if (is.numeric(subgroup_values)) {
      threshold <- median(subgroup_values)
      subgroups <- ifelse(subgroup_values > threshold, "High", "Low")
    } else {
      # For categorical variables, use factor levels
      subgroups <- as.character(subgroup_values)
    }
  } else {
    subgroups <- ifelse(subgroup_values > threshold, "High", "Low")
  }
  
  # Calculate subgroup effects
  subgroup_stats <- data.frame(
    tau = tau_hat,
    tau_se = tau_se,
    subgroup = subgroups
  ) %>%
    group_by(subgroup) %>%
    summarise(
      n = n(),
      mean_effect = mean(tau),
      se_effect = sd(tau) / sqrt(n()),
      ci_lower = mean_effect - 1.96 * se_effect,
      ci_upper = mean_effect + 1.96 * se_effect,
      .groups = "drop"
    )
  
  # Test for difference between subgroups
  if (length(unique(subgroups)) == 2) {
    group1_effects <- tau_hat[subgroups == unique(subgroups)[1]]
    group2_effects <- tau_hat[subgroups == unique(subgroups)[2]]
    
    # Two-sample t-test
    t_test <- t.test(group1_effects, group2_effects)
    
    cat("=== SUBGROUP ANALYSIS ===\n")
    cat("Subgroup variable:", subgroup_var, "\n")
    if (!is.null(threshold)) {
      cat("Threshold:", threshold, "\n")
    }
    cat("Subgroup comparison:\n")
    print(subgroup_stats)
    cat("\nDifference test (t-test):\n")
    cat("Difference:", round(t_test$estimate[1] - t_test$estimate[2], 4), "\n")
    cat("95% CI:", round(t_test$conf.int, 4), "\n")
    cat("P-value:", round(t_test$p.value, 4), "\n\n")
    
    test_results <- list(
      difference = t_test$estimate[1] - t_test$estimate[2],
      ci = t_test$conf.int,
      p_value = t_test$p.value
    )
  } else {
    test_results <- NULL
  }
  
  return(list(
    subgroup_stats = subgroup_stats,
    test_results = test_results,
    subgroup_assignments = subgroups,
    threshold = threshold
  ))
}

cat("Causal Forests STC functions loaded successfully.\n")
cat("Main function: causal_forest_stc_analysis()\n")
cat("Plotting: plot.causal_forest_stc()\n")
cat("Policy analysis: identify_treatment_policy()\n")
cat("Subgroup analysis: subgroup_analysis()\n\n") 