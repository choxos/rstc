################################################################################
##################### TMLE with Super Learner for STC Analysis #################
################################################################################
#
# Implementation of Targeted Maximum Likelihood Estimation (TMLE) with
# Super Learner for Simulated Treatment Comparison providing doubly robust
# estimation with optimal bias-variance tradeoffs.
#
# TMLE provides:
# - Doubly robust estimation (consistent if either outcome or PS model correct)
# - Targeted bias reduction through clever covariate construction
# - Super Learner ensemble for optimal prediction performance
# - Valid confidence intervals with influence curve-based inference
# - Asymptotic efficiency under correct specification
#
# Based on:
# - van der Laan & Rose (2011): "Targeted Learning: Causal Inference for Observational and Experimental Data"
# - Schuler & Rose (2017): "Targeted Maximum Likelihood Estimation for Causal Inference in Observational Studies"
# - Polley et al. (2011): "Super Learning" 
#
# Author: Advanced STC Methods Package - ML Extension
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("tmle")) install.packages("tmle")
if (!require("SuperLearner")) install.packages("SuperLearner")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("randomForest")) install.packages("randomForest")
if (!require("xgboost")) install.packages("xgboost")
if (!require("glmnet")) install.packages("glmnet")

library(tmle)
library(SuperLearner)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(xgboost)
library(glmnet)

# Source utility functions
source("../utils/ml_stc_utils.R")

################################################################################
##################### Core TMLE Functions ####################################
################################################################################

#' TMLE with Super Learner STC Analysis
#' 
#' Performs Targeted Maximum Likelihood Estimation with Super Learner ensemble
#' for simulated treatment comparison with doubly robust guarantees.
#' 
#' @param data Data frame containing all variables
#' @param outcome_col Name of outcome column
#' @param treatment_col Name of treatment column
#' @param covariate_cols Vector of covariate column names
#' @param Q_SL_library Super Learner library for outcome modeling
#' @param g_SL_library Super Learner library for propensity score modeling
#' @param cv_folds Number of cross-validation folds for Super Learner
#' @param family Outcome family ("gaussian" or "binomial")
#' @param target_population Optional target population data
#' @param delta_range Range for fluctuation parameter (for numerical stability)
#' @param seed Random seed for reproducibility
#' @return TMLE results
#' @export
tmle_stc_analysis <- function(data,
                             outcome_col,
                             treatment_col,
                             covariate_cols,
                             Q_SL_library = c("SL.glm", "SL.randomForest", "SL.xgboost", "SL.glmnet"),
                             g_SL_library = c("SL.glm", "SL.randomForest", "SL.xgboost", "SL.glmnet"),
                             cv_folds = 10,
                             family = "gaussian",
                             target_population = NULL,
                             delta_range = c(-0.5, 0.5),
                             seed = 123) {
  
  cat("################################################################################\n")
  cat("##################### TMLE WITH SUPER LEARNER STC ANALYSIS ###################\n")
  cat("################################################################################\n\n")
  
  set.seed(seed)
  
  # Prepare data
  cat("Step 1: Data preparation...\n")
  data_prep <- prepare_ml_stc_data(data, outcome_col, treatment_col, covariate_cols)
  
  Y <- data_prep$Y
  A <- data_prep$W  # TMLE uses A for treatment
  W <- data_prep$X  # TMLE uses W for covariates
  
  # Auto-detect family if not specified
  if (family == "auto") {
    family <- if (data_prep$outcome_type == "binary") "binomial" else "gaussian"
  }
  
  cat("Outcome family:", family, "\n")
  cat("Q library:", paste(Q_SL_library, collapse = ", "), "\n")
  cat("g library:", paste(g_SL_library, collapse = ", "), "\n")
  cat("CV folds:", cv_folds, "\n")
  cat("Sample size:", length(Y), "\n\n")
  
  # Step 2: Fit TMLE
  cat("Step 2: Fitting TMLE with Super Learner...\n")
  
  # Convert data for tmle package
  W_df <- as.data.frame(W)
  colnames(W_df) <- data_prep$covariate_names
  
  tryCatch({
    tmle_fit <- tmle(
      Y = Y,
      A = A,
      W = W_df,
      Q.SL.library = Q_SL_library,
      g.SL.library = g_SL_library,
      family = family,
      V = cv_folds,
      Delta = delta_range,
      verbose = TRUE
    )
  }, error = function(e) {
    cat("Error in TMLE fitting:", e$message, "\n")
    cat("Trying with reduced Super Learner libraries...\n")
    
    # Fallback with simpler libraries
    tmle_fit <<- tmle(
      Y = Y,
      A = A,
      W = W_df,
      Q.SL.library = c("SL.glm", "SL.mean"),
      g.SL.library = c("SL.glm", "SL.mean"),
      family = family,
      V = cv_folds,
      verbose = TRUE
    )
  })
  
  cat("TMLE fitting completed!\n\n")
  
  # Step 3: Extract results
  cat("Step 3: Extracting TMLE results...\n")
  
  # Treatment effect estimates
  ate_estimate <- tmle_fit$estimates$ATE$psi
  ate_se <- sqrt(tmle_fit$estimates$ATE$var.psi)
  ate_ci <- c(tmle_fit$estimates$ATE$CI[1], tmle_fit$estimates$ATE$CI[2])
  ate_pvalue <- tmle_fit$estimates$ATE$pvalue
  
  # Individual predictions
  Q1W <- tmle_fit$Qstar[, 2]  # E[Y|A=1,W] (targeted estimates)
  Q0W <- tmle_fit$Qstar[, 1]  # E[Y|A=0,W] (targeted estimates)
  
  # Individual treatment effects
  tau_hat <- Q1W - Q0W
  
  # Propensity scores
  g1W <- tmle_fit$g$g1W  # P(A=1|W)
  
  cat("ATE Estimate:", round(ate_estimate, 4), "\n")
  cat("Standard Error:", round(ate_se, 4), "\n")
  cat("95% CI: (", round(ate_ci[1], 4), ", ", round(ate_ci[2], 4), ")\n")
  cat("P-value:", round(ate_pvalue, 4), "\n\n")
  
  # Step 4: Super Learner performance analysis
  cat("Step 4: Analyzing Super Learner performance...\n")
  
  sl_analysis <- analyze_superlearner_performance(tmle_fit)
  
  cat("Q (Outcome) Super Learner:\n")
  print(sl_analysis$Q_weights)
  cat("\ng (Propensity) Super Learner:\n")
  print(sl_analysis$g_weights)
  cat("\n")
  
  # Step 5: Additional treatment effects
  cat("Step 5: Computing additional treatment effects...\n")
  
  treated_idx <- A == 1
  control_idx <- A == 0
  
  # ATT and ATC
  att <- mean(tau_hat[treated_idx])
  atc <- mean(tau_hat[control_idx])
  
  # Effect heterogeneity
  het_var <- var(tau_hat)
  het_sd <- sd(tau_hat)
  
  cat("ATT (effect on treated):", round(att, 4), "\n")
  cat("ATC (effect on control):", round(atc, 4), "\n")
  cat("Effect heterogeneity (SD):", round(het_sd, 4), "\n\n")
  
  # Step 6: Model diagnostics
  cat("Step 6: Performing model diagnostics...\n")
  
  diagnostics <- perform_tmle_diagnostics(
    Y, A, W_df, tmle_fit, Q1W, Q0W, g1W, family
  )
  
  # Step 7: Influence curve analysis
  cat("Step 7: Influence curve analysis...\n")
  
  influence_analysis <- analyze_influence_curves(tmle_fit, Y, A, Q1W, Q0W, g1W)
  
  # Step 8: Target population prediction (if provided)
  target_results <- NULL
  if (!is.null(target_population)) {
    cat("Step 8: Target population analysis...\n")
    
    target_results <- predict_tmle_target_population(
      target_population, covariate_cols, tmle_fit, 
      Q_SL_library, g_SL_library, family, cv_folds
    )
    
    cat("Target population ATE:", round(target_results$target_ate, 4), "\n")
    cat("Target population SE:", round(target_results$target_se, 4), "\n\n")
  }
  
  # Compile comprehensive results
  results <- list(
    # Core TMLE results
    tmle_fit = tmle_fit,
    
    # Treatment effects
    ate = list(
      estimate = ate_estimate,
      se = ate_se,
      ci_lower = ate_ci[1],
      ci_upper = ate_ci[2],
      p_value = ate_pvalue,
      att = att,
      atc = atc
    ),
    
    # Individual predictions
    predictions = list(
      Q1W = Q1W,
      Q0W = Q0W,
      tau_hat = tau_hat,
      g1W = g1W
    ),
    
    # Heterogeneity
    heterogeneity = list(
      variance = het_var,
      standard_deviation = het_sd
    ),
    
    # Super Learner analysis
    superlearner = sl_analysis,
    
    # Diagnostics
    diagnostics = diagnostics,
    
    # Influence curves
    influence_analysis = influence_analysis,
    
    # Target population
    target_population = target_results,
    
    # Methodology info
    methodology = list(
      method = "TMLE with Super Learner",
      Q_SL_library = Q_SL_library,
      g_SL_library = g_SL_library,
      family = family,
      cv_folds = cv_folds
    ),
    
    # Data info
    data_info = data_prep,
    
    # Parameters
    parameters = list(
      delta_range = delta_range,
      seed = seed
    )
  )
  
  class(results) <- "tmle_stc"
  
  cat("=== TMLE STC ANALYSIS COMPLETE ===\n")
  cat("Method: TMLE with Super Learner\n")
  cat("ATE estimate:", round(ate_estimate, 4), "\n")
  cat("95% CI: (", round(ate_ci[1], 4), ", ", round(ate_ci[2], 4), ")\n")
  cat("Statistical significance:", if(ate_pvalue < 0.05) "Yes" else "No", "(p =", round(ate_pvalue, 4), ")\n")
  cat("Doubly robust: Yes\n\n")
  
  return(results)
}

################################################################################
##################### Helper Functions ##########################################
################################################################################

#' Analyze Super Learner Performance
#' 
#' @param tmle_fit Fitted TMLE object
#' @return Super Learner analysis
analyze_superlearner_performance <- function(tmle_fit) {
  
  # Extract Super Learner weights for Q (outcome model)
  Q_sl <- tmle_fit$Q$Q$SuperLearner
  Q_weights <- data.frame(
    Algorithm = names(Q_sl$coef),
    Weight = as.numeric(Q_sl$coef),
    CV_Risk = Q_sl$cvRisk[, 1]
  ) %>%
    arrange(desc(Weight))
  
  # Extract Super Learner weights for g (propensity model)
  g_sl <- tmle_fit$g$g.SL
  g_weights <- data.frame(
    Algorithm = names(g_sl$coef),
    Weight = as.numeric(g_sl$coef),
    CV_Risk = g_sl$cvRisk[, 1]
  ) %>%
    arrange(desc(Weight))
  
  return(list(
    Q_weights = Q_weights,
    g_weights = g_weights,
    Q_superlearner = Q_sl,
    g_superlearner = g_sl
  ))
}

#' Perform TMLE Diagnostics
#' 
#' @param Y Outcomes
#' @param A Treatments
#' @param W Covariates
#' @param tmle_fit TMLE fit object
#' @param Q1W Targeted outcome predictions A=1
#' @param Q0W Targeted outcome predictions A=0
#' @param g1W Propensity scores
#' @param family Outcome family
#' @return Diagnostic results
perform_tmle_diagnostics <- function(Y, A, W, tmle_fit, Q1W, Q0W, g1W, family) {
  
  # Propensity score diagnostics
  ps_summary <- list(
    mean = mean(g1W),
    sd = sd(g1W),
    range = range(g1W),
    treated_mean = mean(g1W[A == 1]),
    control_mean = mean(g1W[A == 0])
  )
  
  # Overlap assessment
  overlap_stats <- assess_overlap(g1W, A)
  
  # Prediction quality
  pred_quality <- assess_prediction_quality(Y, A, Q1W, Q0W, g1W, family)
  
  # Residual analysis
  residuals_treated <- Y[A == 1] - Q1W[A == 1]
  residuals_control <- Y[A == 0] - Q0W[A == 0]
  
  residual_stats <- list(
    treated_rmse = sqrt(mean(residuals_treated^2)),
    control_rmse = sqrt(mean(residuals_control^2)),
    treated_bias = mean(residuals_treated),
    control_bias = mean(residuals_control)
  )
  
  cat("Diagnostic Results:\n")
  cat("Propensity Score Summary:\n")
  cat("  Overall mean:", round(ps_summary$mean, 3), "\n")
  cat("  Range:", round(ps_summary$range[1], 3), "to", round(ps_summary$range[2], 3), "\n")
  cat("  Treated mean:", round(ps_summary$treated_mean, 3), "\n")
  cat("  Control mean:", round(ps_summary$control_mean, 3), "\n")
  
  cat("Overlap Assessment:\n")
  cat("  Effective sample size:", round(overlap_stats$effective_n, 1), "\n")
  cat("  Extreme weights (>10):", overlap_stats$extreme_weights, "\n")
  
  cat("Prediction Quality:\n")
  cat("  Q model performance:", round(pred_quality$q_performance, 4), "\n")
  cat("  g model AUC:", round(pred_quality$g_auc, 3), "\n\n")
  
  return(list(
    propensity_score = ps_summary,
    overlap = overlap_stats,
    prediction_quality = pred_quality,
    residuals = residual_stats
  ))
}

#' Assess Overlap between Treatment Groups
#' 
#' @param g1W Propensity scores
#' @param A Treatment assignments
#' @return Overlap statistics
assess_overlap <- function(g1W, A) {
  
  # Effective sample size based on propensity scores
  weights_treated <- 1 / g1W[A == 1]
  weights_control <- 1 / (1 - g1W[A == 0])
  
  eff_n_treated <- sum(A == 1)^2 / sum(weights_treated^2)
  eff_n_control <- sum(A == 0)^2 / sum(weights_control^2)
  
  effective_n <- eff_n_treated + eff_n_control
  
  # Count extreme weights
  extreme_treated <- sum(weights_treated > 10)
  extreme_control <- sum(weights_control > 10)
  extreme_weights <- extreme_treated + extreme_control
  
  # Overlap region
  overlap_range <- c(max(min(g1W[A == 1]), min(g1W[A == 0])),
                    min(max(g1W[A == 1]), max(g1W[A == 0])))
  
  return(list(
    effective_n = effective_n,
    extreme_weights = extreme_weights,
    overlap_range = overlap_range
  ))
}

#' Assess Prediction Quality
#' 
#' @param Y Outcomes
#' @param A Treatments
#' @param Q1W Outcome predictions A=1
#' @param Q0W Outcome predictions A=0
#' @param g1W Propensity scores
#' @param family Outcome family
#' @return Prediction quality metrics
assess_prediction_quality <- function(Y, A, Q1W, Q0W, g1W, family) {
  
  # Q model performance
  if (family == "binomial") {
    # Brier score for binary outcomes
    pred_Y <- A * Q1W + (1 - A) * Q0W
    q_performance <- mean((Y - pred_Y)^2)  # Brier score
  } else {
    # MSE for continuous outcomes
    pred_Y <- A * Q1W + (1 - A) * Q0W
    q_performance <- mean((Y - pred_Y)^2)  # MSE
  }
  
  # g model performance (AUC)
  g_auc <- tryCatch({
    if (requireNamespace("pROC", quietly = TRUE)) {
      pROC::auc(pROC::roc(A, g1W, quiet = TRUE))
    } else {
      NA
    }
  }, error = function(e) NA)
  
  return(list(
    q_performance = q_performance,
    g_auc = as.numeric(g_auc)
  ))
}

#' Analyze Influence Curves
#' 
#' @param tmle_fit TMLE fit object
#' @param Y Outcomes
#' @param A Treatments
#' @param Q1W Outcome predictions A=1
#' @param Q0W Outcome predictions A=0
#' @param g1W Propensity scores
#' @return Influence curve analysis
analyze_influence_curves <- function(tmle_fit, Y, A, Q1W, Q0W, g1W) {
  
  # Extract influence curve for ATE
  IC <- tmle_fit$estimates$ATE$IC
  
  # Influence curve statistics
  ic_stats <- list(
    mean = mean(IC),
    sd = sd(IC),
    range = range(IC),
    extreme_values = sum(abs(IC) > 3 * sd(IC))
  )
  
  # Components of influence curve
  # IC = (A/g1W - (1-A)/(1-g1W)) * (Y - Q_AW) + (Q1W - Q0W) - psi
  
  clever_covariate <- A / g1W - (1 - A) / (1 - g1W)
  residual_component <- clever_covariate * (Y - (A * Q1W + (1 - A) * Q0W))
  bias_component <- Q1W - Q0W - tmle_fit$estimates$ATE$psi
  
  components <- list(
    clever_covariate = clever_covariate,
    residual_component = residual_component,
    bias_component = bias_component
  )
  
  cat("Influence Curve Analysis:\n")
  cat("  Mean IC:", round(ic_stats$mean, 6), "(should be ~0)\n")
  cat("  SD IC:", round(ic_stats$sd, 4), "\n")
  cat("  Extreme values:", ic_stats$extreme_values, "\n")
  
  return(list(
    influence_curve = IC,
    statistics = ic_stats,
    components = components
  ))
}

#' Predict Target Population Effects with TMLE
#' 
#' @param target_population Target population data
#' @param covariate_cols Covariate columns
#' @param tmle_fit Fitted TMLE object
#' @param Q_SL_library Q Super Learner library
#' @param g_SL_library g Super Learner library
#' @param family Outcome family
#' @param cv_folds CV folds
#' @return Target population results
predict_tmle_target_population <- function(target_population, covariate_cols, tmle_fit,
                                          Q_SL_library, g_SL_library, family, cv_folds) {
  
  target_W <- as.data.frame(target_population[, covariate_cols, drop = FALSE])
  
  # Use the fitted Super Learner models to predict for target population
  
  # Q predictions
  Q_sl_fit <- tmle_fit$Q$Q$SuperLearner
  target_Q1 <- predict(Q_sl_fit, newdata = cbind(A = 1, target_W))$pred
  target_Q0 <- predict(Q_sl_fit, newdata = cbind(A = 0, target_W))$pred
  
  # Treatment effects
  target_tau <- target_Q1 - target_Q0
  target_ate <- mean(target_tau)
  target_se <- sd(target_tau) / sqrt(length(target_tau))
  
  return(list(
    target_effects = target_tau,
    target_Q1 = target_Q1,
    target_Q0 = target_Q0,
    target_ate = target_ate,
    target_se = target_se,
    target_ci = target_ate + c(-1.96, 1.96) * target_se
  ))
}

################################################################################
##################### Visualization Functions ###################################
################################################################################

#' Plot TMLE Results
#' 
#' @param tmle_results Results from tmle_stc_analysis
#' @return List of ggplot objects
#' @export
plot.tmle_stc <- function(tmle_results) {
  
  plots <- list()
  
  # 1. Treatment effect distribution
  tau_hat <- tmle_results$predictions$tau_hat
  
  p1 <- ggplot(data.frame(tau = tau_hat), aes(x = tau)) +
    geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7, color = "white") +
    geom_vline(xintercept = tmle_results$ate$estimate, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = tmle_results$ate$ci_lower, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    geom_vline(xintercept = tmle_results$ate$ci_upper, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    labs(title = "TMLE: Individual Treatment Effect Distribution",
         subtitle = paste("ATE =", round(tmle_results$ate$estimate, 3)),
         x = "Treatment Effect",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$distribution <- p1
  
  # 2. Super Learner weights
  Q_weights <- tmle_results$superlearner$Q_weights
  
  p2 <- ggplot(Q_weights, aes(x = reorder(Algorithm, Weight), y = Weight)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(title = "TMLE: Super Learner Weights for Q (Outcome Model)",
         x = "Algorithm",
         y = "Weight") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$q_weights <- p2
  
  # 3. Propensity score distribution
  g1W <- tmle_results$predictions$g1W
  A <- tmle_results$data_info$W
  
  ps_data <- data.frame(
    propensity = g1W,
    treatment = factor(A, labels = c("Control", "Treated"))
  )
  
  p3 <- ggplot(ps_data, aes(x = propensity, fill = treatment)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("Control" = "lightblue", "Treated" = "lightcoral")) +
    labs(title = "TMLE: Propensity Score Distribution",
         x = "Propensity Score",
         y = "Frequency",
         fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$propensity <- p3
  
  # 4. Influence curve
  if (!is.null(tmle_results$influence_analysis$influence_curve)) {
    IC <- tmle_results$influence_analysis$influence_curve
    
    p4 <- ggplot(data.frame(IC = IC, index = 1:length(IC)), aes(x = index, y = IC)) +
      geom_point(alpha = 0.6, color = "purple") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_hline(yintercept = c(-3, 3) * sd(IC), linetype = "dotted", color = "red") +
      labs(title = "TMLE: Influence Curve Values",
           x = "Observation Index",
           y = "Influence Curve Value") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    plots$influence_curve <- p4
  }
  
  return(plots)
}

cat("TMLE with Super Learner STC functions loaded successfully.\n")
cat("Main function: tmle_stc_analysis()\n")
cat("Plotting: plot.tmle_stc()\n")
cat("Provides doubly robust estimation with Super Learner ensemble.\n\n") 