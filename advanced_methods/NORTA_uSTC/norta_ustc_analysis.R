################################################################################
##################### NORTA Unanchored STC Analysis Package ##################
################################################################################
#
# Implementation of Unanchored Simulated Treatment Comparison using the
# NORTA (NORmal To Anything) algorithm for generating correlated binary covariates
#
# Based on: Ren et al. (2023) - Comprehensive simulation study of unanchored 
# simulated treatment comparison approach
#
# Key Features:
# - Uses NORTA algorithm (Normal copula) for correlated binary covariate generation
# - Supports unanchored STC with bootstrap confidence intervals
# - Handles multiple binary covariates with specified correlation structure
# - Provides comprehensive validation and bias assessment
#
# Author: Advanced STC Methods Package
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("copula")) install.packages("copula")
if (!require("dplyr")) install.packages("dplyr")
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

library(copula)
library(dplyr)
library(mvtnorm)
library(ggplot2)
library(knitr)
library(kableExtra)

################################################################################
##################### Core NORTA Functions ####################################
################################################################################

#' Generate Correlated Covariates Using NORTA Algorithm
#' 
#' This function generates correlated binary covariates using the NORTA 
#' (NORmal To Anything) algorithm, also known as Normal copula method.
#' 
#' @param n_patients Number of patients to generate
#' @param covariate_type Type of covariates ("binary" currently supported)
#' @param agd_correlations Correlation matrix for AgD population
#' @param ipd_correlations Correlation matrix for IPD population  
#' @param agd_probabilities Vector of marginal probabilities for AgD population
#' @param ipd_probabilities Vector of marginal probabilities for IPD population
#' @return Data frame with simulated covariate data for both populations
#' @export
generate_norta_covariates <- function(n_patients,
                                     covariate_type = "binary",
                                     agd_correlations,
                                     ipd_correlations,
                                     agd_probabilities,
                                     ipd_probabilities) {
  
  if (covariate_type != "binary") {
    stop("Currently only binary covariates are supported")
  }
  
  n_covariates <- length(agd_probabilities)
  
  # Validate inputs
  if (length(ipd_probabilities) != n_covariates) {
    stop("AgD and IPD probability vectors must have same length")
  }
  
  if (is.matrix(agd_correlations)) {
    if (nrow(agd_correlations) != n_covariates || ncol(agd_correlations) != n_covariates) {
      stop("AgD correlation matrix dimensions must match number of covariates")
    }
  } else {
    # If scalar, convert to correlation matrix for 2 covariates
    if (n_covariates != 2) {
      stop("Scalar correlation only valid for 2 covariates")
    }
    agd_correlations <- matrix(c(1, agd_correlations, agd_correlations, 1), nrow = 2)
  }
  
  if (is.matrix(ipd_correlations)) {
    if (nrow(ipd_correlations) != n_covariates || ncol(ipd_correlations) != n_covariates) {
      stop("IPD correlation matrix dimensions must match number of covariates")
    }
  } else {
    # If scalar, convert to correlation matrix for 2 covariates
    if (n_covariates != 2) {
      stop("Scalar correlation only valid for 2 covariates")
    }
    ipd_correlations <- matrix(c(1, ipd_correlations, ipd_correlations, 1), nrow = 2)
  }
  
  cat("Generating NORTA covariates:\n")
  cat("Sample size:", n_patients, "\n")
  cat("Number of covariates:", n_covariates, "\n")
  cat("AgD probabilities:", paste(round(agd_probabilities, 3), collapse = ", "), "\n")
  cat("IPD probabilities:", paste(round(ipd_probabilities, 3), collapse = ", "), "\n\n")
  
  # Generate AgD population covariates
  agd_copula <- normalCopula(P2p(agd_correlations), dim = n_covariates, dispstr = "un")
  
  # Create margin specifications for binary variables
  agd_margins <- rep("binom", n_covariates)
  agd_margin_params <- lapply(agd_probabilities, function(p) list(size = 1, prob = p))
  
  agd_mvd <- mvdc(copula = agd_copula, 
                  margins = agd_margins,
                  paramMargins = agd_margin_params)
  
  # Generate IPD population covariates
  ipd_copula <- normalCopula(P2p(ipd_correlations), dim = n_covariates, dispstr = "un")
  ipd_margin_params <- lapply(ipd_probabilities, function(p) list(size = 1, prob = p))
  
  ipd_mvd <- mvdc(copula = ipd_copula,
                  margins = agd_margins,
                  paramMargins = ipd_margin_params)
  
  # Generate samples
  agd_samples <- rMvdc(n_patients, agd_mvd)
  ipd_samples <- rMvdc(n_patients, ipd_mvd)
  
  # Create data frames
  covariate_names <- paste0("X", 1:n_covariates)
  
  # AgD data (both treatment arms)
  agd_a <- data.frame(agd_samples)
  colnames(agd_a) <- covariate_names
  agd_a$treatment <- "A"
  agd_a$population <- "AgD"
  
  agd_b <- data.frame(agd_samples)
  colnames(agd_b) <- covariate_names
  agd_b$treatment <- "B"
  agd_b$population <- "AgD"
  
  # IPD data (both treatment arms)
  ipd_a <- data.frame(ipd_samples)
  colnames(ipd_a) <- covariate_names
  ipd_a$treatment <- "A"
  ipd_a$population <- "IPD"
  
  ipd_b <- data.frame(ipd_samples)
  colnames(ipd_b) <- covariate_names
  ipd_b$treatment <- "B"
  ipd_b$population <- "IPD"
  
  # Combine all data
  combined_data <- rbind(agd_a, agd_b, ipd_a, ipd_b)
  
  cat("Successfully generated correlated covariates using NORTA algorithm\n\n")
  
  return(list(
    combined_data = combined_data,
    agd_data = rbind(agd_a, agd_b),
    ipd_data = rbind(ipd_a, ipd_b),
    covariate_names = covariate_names,
    agd_correlations = agd_correlations,
    ipd_correlations = ipd_correlations
  ))
}

#' Generate Outcomes Using Logistic Regression
#' 
#' @param covariate_data Data frame with covariates and treatment indicators
#' @param intercept Model intercept
#' @param covariate_effects Vector of covariate effects
#' @param treatment_effect Effect of treatment B
#' @param interaction_effects Vector of treatment*covariate interaction effects (optional)
#' @return Data frame with outcomes added
#' @export
generate_norta_outcomes <- function(covariate_data,
                                   intercept,
                                   covariate_effects,
                                   treatment_effect,
                                   interaction_effects = NULL) {
  
  covariate_names <- names(covariate_effects)
  
  # Validate inputs
  if (!all(covariate_names %in% colnames(covariate_data))) {
    stop("All covariate names must be present in the data")
  }
  
  if (!is.null(interaction_effects) && length(interaction_effects) != length(covariate_effects)) {
    stop("Interaction effects vector must have same length as covariate effects")
  }
  
  # Create covariate matrix
  X <- as.matrix(covariate_data[, covariate_names, drop = FALSE])
  
  # Calculate linear predictor
  linear_pred <- intercept + X %*% covariate_effects
  
  # Add treatment effect for treatment B
  treatment_b_indicator <- as.numeric(covariate_data$treatment == "B")
  linear_pred <- linear_pred + treatment_effect * treatment_b_indicator
  
  # Add interaction effects if specified
  if (!is.null(interaction_effects)) {
    interaction_terms <- (X * treatment_b_indicator) %*% interaction_effects
    linear_pred <- linear_pred + interaction_terms
  }
  
  # Generate binary outcomes
  success_probabilities <- 1 / (1 + exp(-linear_pred))
  outcomes <- rbinom(nrow(covariate_data), 1, success_probabilities)
  
  # Add outcomes to data
  result_data <- covariate_data
  result_data$outcome_probability <- success_probabilities
  result_data$outcome <- outcomes
  
  cat("Generated outcomes using logistic regression\n")
  cat("Overall event rate:", round(100 * mean(outcomes), 1), "%\n")
  cat("Event rate by treatment:\n")
  cat("  Treatment A:", round(100 * mean(outcomes[covariate_data$treatment == "A"]), 1), "%\n")
  cat("  Treatment B:", round(100 * mean(outcomes[covariate_data$treatment == "B"]), 1), "%\n\n")
  
  return(result_data)
}

################################################################################
##################### NORTA Unanchored STC Analysis ############################
################################################################################

#' Perform NORTA Unanchored STC Analysis
#' 
#' Main function to perform unanchored STC analysis using NORTA-generated data
#' with bootstrap confidence intervals
#' 
#' @param ipd_data IPD data for treatment B
#' @param agd_summary AgD summary data for treatment A
#' @param covariates Vector of covariate names to include in analysis
#' @param n_bootstrap Number of bootstrap iterations
#' @param n_simulation Number of patients to simulate for each bootstrap
#' @return List containing STC analysis results
#' @export
norta_unanchored_stc_analysis <- function(ipd_data,
                                         agd_summary,
                                         covariates,
                                         n_bootstrap = 1000,
                                         n_simulation = 10000) {
  
  cat("===========================================\n")
  cat("    NORTA Unanchored STC Analysis        \n")
  cat("===========================================\n\n")
  
  # Validate inputs
  if (!all(covariates %in% colnames(ipd_data))) {
    stop("All specified covariates must be present in IPD data")
  }
  
  if (!"outcome" %in% colnames(ipd_data)) {
    stop("IPD data must contain 'outcome' column")
  }
  
  # Extract AgD characteristics
  agd_n <- agd_summary$n_patients
  agd_events <- agd_summary$n_events
  agd_covariate_props <- agd_summary[covariates]
  
  cat("Study characteristics:\n")
  cat("IPD sample size:", nrow(ipd_data), "\n")
  cat("AgD sample size:", agd_n, "\n")
  cat("AgD event rate:", round(100 * agd_events / agd_n, 1), "%\n")
  cat("Covariates:", paste(covariates, collapse = ", "), "\n")
  cat("Bootstrap iterations:", n_bootstrap, "\n\n")
  
  # Bootstrap analysis
  cat("Performing bootstrap analysis...\n")
  bootstrap_results <- sapply(1:n_bootstrap, function(i) {
    
    if (i %% 100 == 0) cat("Bootstrap iteration:", i, "\n")
    
    # Bootstrap sample from IPD
    boot_indices <- sample(nrow(ipd_data), nrow(ipd_data), replace = TRUE)
    boot_ipd <- ipd_data[boot_indices, ]
    
    # Estimate correlation structure from bootstrap IPD sample
    ipd_covariate_matrix <- as.matrix(boot_ipd[, covariates, drop = FALSE])
    ipd_correlation <- cor(ipd_covariate_matrix)
    
    # Handle case where correlation is scalar (2 covariates)
    if (length(covariates) == 2) {
      ipd_correlation_scalar <- ipd_correlation[1, 2]
    } else {
      ipd_correlation_scalar <- ipd_correlation
    }
    
    # Generate simulated AgD population with IPD correlation structure
    # but AgD marginal probabilities
    agd_copula <- normalCopula(
      if (length(covariates) == 2) ipd_correlation_scalar else P2p(ipd_correlation),
      dim = length(covariates),
      dispstr = "un"
    )
    
    # Create margin specifications
    agd_margins <- rep("binom", length(covariates))
    agd_margin_params <- lapply(agd_covariate_props, function(p) list(size = 1, prob = p))
    
    agd_mvd <- mvdc(copula = agd_copula,
                    margins = agd_margins,
                    paramMargins = agd_margin_params)
    
    # Generate simulated AgD covariates
    simulated_agd_covariates <- rMvdc(n_simulation, agd_mvd)
    simulated_agd_df <- data.frame(simulated_agd_covariates)
    colnames(simulated_agd_df) <- covariates
    
    # Fit outcome model to bootstrap IPD sample
    formula_str <- paste("outcome ~", paste(covariates, collapse = " + "))
    outcome_model <- glm(as.formula(formula_str), 
                        data = boot_ipd, 
                        family = binomial())
    
    # Predict outcomes for simulated AgD population
    agd_predictions <- predict(outcome_model, 
                              newdata = simulated_agd_df, 
                              type = "response")
    
    # Calculate average predicted probability for AgD population
    # treating with treatment B
    predicted_prob_b <- mean(agd_predictions)
    
    # Convert to log-odds scale
    log_odds_b <- log(predicted_prob_b / (1 - predicted_prob_b))
    
    return(log_odds_b)
  })
  
  # Calculate AgD treatment A log-odds
  agd_prob_a <- agd_events / agd_n
  log_odds_a <- log(agd_prob_a / (1 - agd_prob_a))
  var_log_odds_a <- agd_n / (agd_events * (agd_n - agd_events))
  
  # Calculate treatment effect (log odds ratio)
  mean_log_odds_b <- mean(bootstrap_results)
  log_or <- mean_log_odds_b - log_odds_a
  
  # Calculate standard error
  bootstrap_variance <- var(bootstrap_results)
  total_variance <- bootstrap_variance + var_log_odds_a
  se_log_or <- sqrt(total_variance)
  
  # Calculate confidence intervals
  ci_lower <- log_or - 1.96 * se_log_or
  ci_upper <- log_or + 1.96 * se_log_or
  
  # Convert to odds ratio scale
  odds_ratio <- exp(log_or)
  or_ci_lower <- exp(ci_lower)
  or_ci_upper <- exp(ci_upper)
  
  # Calculate p-value
  z_statistic <- log_or / se_log_or
  p_value <- 2 * (1 - pnorm(abs(z_statistic)))
  
  cat("\n=== ANALYSIS RESULTS ===\n")
  cat("Log Odds Ratio:", round(log_or, 4), "\n")
  cat("Standard Error:", round(se_log_or, 4), "\n")
  cat("Odds Ratio:", round(odds_ratio, 3), "\n")
  cat("95% CI: (", round(or_ci_lower, 3), ", ", round(or_ci_upper, 3), ")\n")
  cat("Z-statistic:", round(z_statistic, 3), "\n")
  cat("P-value:", round(p_value, 4), "\n\n")
  
  return(list(
    study_info = list(
      ipd_n = nrow(ipd_data),
      agd_n = agd_n,
      agd_events = agd_events,
      covariates = covariates,
      n_bootstrap = n_bootstrap,
      n_simulation = n_simulation
    ),
    treatment_effects = list(
      log_odds_ratio = log_or,
      log_or_se = se_log_or,
      odds_ratio = odds_ratio,
      or_ci_lower = or_ci_lower,
      or_ci_upper = or_ci_upper,
      z_statistic = z_statistic,
      p_value = p_value
    ),
    bootstrap_results = bootstrap_results,
    intermediate_results = list(
      agd_prob_a = agd_prob_a,
      log_odds_a = log_odds_a,
      mean_log_odds_b = mean_log_odds_b,
      bootstrap_variance = bootstrap_variance,
      agd_variance = var_log_odds_a
    )
  ))
}

################################################################################
##################### Validation and Diagnostics #############################
################################################################################

#' Calculate True Marginal Treatment Effect for Validation
#' 
#' @param covariate_data Complete simulated dataset
#' @param true_parameters True model parameters used for simulation
#' @return True marginal treatment effect
#' @export
calculate_true_marginal_effect <- function(covariate_data, true_parameters) {
  
  # Extract AgD population data
  agd_data <- covariate_data %>% filter(population == "AgD")
  
  # Fit true model to obtain marginal effect
  true_model <- glm(outcome ~ treatment, data = agd_data, family = binomial())
  true_marginal_effect <- coef(true_model)["treatmentB"]
  
  cat("True marginal treatment effect (log OR):", round(true_marginal_effect, 4), "\n")
  cat("True marginal odds ratio:", round(exp(true_marginal_effect), 3), "\n\n")
  
  return(true_marginal_effect)
}

#' Validate NORTA Correlation Structure
#' 
#' @param generated_data Data generated using NORTA
#' @param target_correlations Target correlation structure
#' @param population Population to validate ("AgD" or "IPD")
#' @return Validation results
#' @export
validate_norta_correlations <- function(generated_data, target_correlations, population = "AgD") {
  
  # Extract population data
  pop_data <- generated_data %>% 
    filter(population == !!population) %>%
    select(starts_with("X"))
  
  # Calculate achieved correlations
  achieved_correlations <- cor(pop_data)
  
  cat("NORTA Correlation Validation for", population, "population:\n")
  cat("Target correlations:\n")
  print(round(target_correlations, 3))
  cat("\nAchieved correlations:\n")
  print(round(achieved_correlations, 3))
  
  # Calculate error metrics
  correlation_error <- achieved_correlations - target_correlations
  max_error <- max(abs(correlation_error[upper.tri(correlation_error)]))
  mean_abs_error <- mean(abs(correlation_error[upper.tri(correlation_error)]))
  
  cat("\nCorrelation Error Metrics:\n")
  cat("Maximum absolute error:", round(max_error, 4), "\n")
  cat("Mean absolute error:", round(mean_abs_error, 4), "\n\n")
  
  return(list(
    target_correlations = target_correlations,
    achieved_correlations = achieved_correlations,
    correlation_error = correlation_error,
    max_error = max_error,
    mean_abs_error = mean_abs_error
  ))
}

################################################################################
##################### Comprehensive Analysis Function #########################
################################################################################

#' Run Complete NORTA STC Analysis
#' 
#' High-level function that runs a complete NORTA-based unanchored STC analysis
#' including data generation, validation, and reporting
#' 
#' @param n_patients Sample size for each arm
#' @param scenario_params List containing scenario parameters
#' @param n_bootstrap Number of bootstrap iterations
#' @return Complete analysis results
#' @export
run_complete_norta_analysis <- function(n_patients = 200,
                                       scenario_params = NULL,
                                       n_bootstrap = 1000) {
  
  cat("################################################################################\n")
  cat("##################### COMPLETE NORTA UNANCHORED STC ANALYSIS ##################\n")
  cat("################################################################################\n\n")
  
  # Use default scenario if none provided
  if (is.null(scenario_params)) {
    scenario_params <- list(
      # Covariate probabilities
      agd_probabilities = c(0.1, 0.1),    # X1, X2 probabilities in AgD
      ipd_probabilities = c(0.2, 0.2),    # X1, X2 probabilities in IPD
      
      # Correlation structures
      agd_correlation = 0.2,               # Correlation in AgD population
      ipd_correlation = 0.2,               # Correlation in IPD population
      
      # Model parameters
      intercept = -0.25,
      covariate_effects = c(X1 = 0.09, X2 = 0.15),
      treatment_effect = -0.45,
      
      # Study info
      covariates = c("X1", "X2")
    )
  }
  
  cat("Analysis Parameters:\n")
  cat("Sample size per arm:", n_patients, "\n")
  cat("AgD covariate probabilities:", paste(scenario_params$agd_probabilities, collapse = ", "), "\n")
  cat("IPD covariate probabilities:", paste(scenario_params$ipd_probabilities, collapse = ", "), "\n")
  cat("Treatment effect (log OR):", scenario_params$treatment_effect, "\n")
  cat("Bootstrap iterations:", n_bootstrap, "\n\n")
  
  # Step 1: Generate correlated covariates using NORTA
  cat("Step 1: Generating correlated covariates using NORTA...\n")
  norta_data <- generate_norta_covariates(
    n_patients = n_patients,
    covariate_type = "binary",
    agd_correlations = scenario_params$agd_correlation,
    ipd_correlations = scenario_params$ipd_correlation,
    agd_probabilities = scenario_params$agd_probabilities,
    ipd_probabilities = scenario_params$ipd_probabilities
  )
  
  # Step 2: Generate outcomes
  cat("Step 2: Generating outcomes using logistic regression...\n")
  complete_data <- generate_norta_outcomes(
    covariate_data = norta_data$combined_data,
    intercept = scenario_params$intercept,
    covariate_effects = scenario_params$covariate_effects,
    treatment_effect = scenario_params$treatment_effect
  )
  
  # Step 3: Prepare data for STC analysis
  cat("Step 3: Preparing data for STC analysis...\n")
  
  # Extract IPD data (treatment B from IPD population)
  ipd_data <- complete_data %>%
    filter(population == "IPD", treatment == "B") %>%
    select(all_of(scenario_params$covariates), outcome)
  
  # Extract AgD summary data (treatment A from AgD population)
  agd_data_a <- complete_data %>%
    filter(population == "AgD", treatment == "A")
  
  agd_summary <- list(
    n_patients = nrow(agd_data_a),
    n_events = sum(agd_data_a$outcome),
    X1 = mean(agd_data_a$X1),
    X2 = mean(agd_data_a$X2)
  )
  
  # Step 4: Run NORTA unanchored STC analysis
  cat("Step 4: Running NORTA unanchored STC analysis...\n")
  stc_results <- norta_unanchored_stc_analysis(
    ipd_data = ipd_data,
    agd_summary = agd_summary,
    covariates = scenario_params$covariates,
    n_bootstrap = n_bootstrap
  )
  
  # Step 5: Validation
  cat("Step 5: Validation and diagnostics...\n")
  
  # Calculate true effect for validation
  true_effect <- calculate_true_marginal_effect(complete_data, scenario_params)
  
  # Validate correlation structures
  agd_correlation_validation <- validate_norta_correlations(
    complete_data, 
    matrix(c(1, scenario_params$agd_correlation, scenario_params$agd_correlation, 1), nrow = 2),
    "AgD"
  )
  
  ipd_correlation_validation <- validate_norta_correlations(
    complete_data,
    matrix(c(1, scenario_params$ipd_correlation, scenario_params$ipd_correlation, 1), nrow = 2),
    "IPD"
  )
  
  # Calculate bias
  estimated_log_or <- stc_results$treatment_effects$log_odds_ratio
  bias <- estimated_log_or - true_effect
  relative_bias <- (bias / true_effect) * 100
  
  cat("=== VALIDATION RESULTS ===\n")
  cat("True treatment effect (log OR):", round(true_effect, 4), "\n")
  cat("Estimated treatment effect:", round(estimated_log_or, 4), "\n")
  cat("Bias:", round(bias, 4), "\n")
  cat("Relative bias:", round(relative_bias, 1), "%\n\n")
  
  # Compile comprehensive results
  results <- list(
    scenario_params = scenario_params,
    generated_data = complete_data,
    norta_data = norta_data,
    stc_results = stc_results,
    validation = list(
      true_effect = true_effect,
      bias = bias,
      relative_bias = relative_bias,
      agd_correlation_validation = agd_correlation_validation,
      ipd_correlation_validation = ipd_correlation_validation
    ),
    analysis_info = list(
      n_patients = n_patients,
      n_bootstrap = n_bootstrap,
      analysis_date = Sys.time()
    )
  )
  
  cat("=== ANALYSIS COMPLETE ===\n")
  cat("NORTA unanchored STC analysis completed successfully\n\n")
  
  return(results)
} 