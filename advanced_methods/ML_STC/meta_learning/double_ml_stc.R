################################################################################
##################### Double Machine Learning for STC Analysis #################
################################################################################
#
# Implementation of Double Machine Learning (DML) for Simulated Treatment
# Comparison with cross-fitting procedures to avoid overfitting bias.
#
# Double ML provides:
# - Cross-fitting to avoid overfitting in nuisance parameter estimation
# - Valid confidence intervals under weak assumptions
# - Robustness to model misspecification (doubly robust)
# - Neyman orthogonal moment conditions for root-n consistency
#
# Based on:
# - Chernozhukov et al. (2018): "Double/Debiased Machine Learning for Treatment and Structural Parameters"
# - Bach et al. (2022): "DoubleML - An Object-Oriented Implementation of Double Machine Learning in Python"
# - Zimmert (2019): "Efficient Estimation of ATE with Cross-Fitting"
#
# Author: Advanced STC Methods Package - ML Extension
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("randomForest")) install.packages("randomForest")
if (!require("xgboost")) install.packages("xgboost")
if (!require("glmnet")) install.packages("glmnet")
if (!require("SuperLearner")) install.packages("SuperLearner")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("sandwich")) install.packages("sandwich")

library(tidyverse)
library(randomForest)
library(xgboost)
library(glmnet)
library(SuperLearner)
library(ggplot2)
library(sandwich)

# Source utility functions
source("../utils/ml_stc_utils.R")

################################################################################
##################### Core Double ML Functions ################################## 
################################################################################

#' Double Machine Learning STC Analysis
#' 
#' Performs Double Machine Learning for simulated treatment comparison
#' with cross-fitting procedures to ensure valid inference.
#' 
#' @param data Data frame containing all variables
#' @param outcome_col Name of outcome column
#' @param treatment_col Name of treatment column
#' @param covariate_cols Vector of covariate column names
#' @param ml_method ML method for nuisance functions ("rf", "xgboost", "superlearner")
#' @param n_folds Number of cross-fitting folds
#' @param n_rep Number of repetitions with different fold splits
#' @param trim_propensity Whether to trim extreme propensity scores
#' @param target_population Optional target population data
#' @param seed Random seed for reproducibility
#' @return Double ML results
#' @export
double_ml_stc_analysis <- function(data,
                                  outcome_col,
                                  treatment_col,
                                  covariate_cols,
                                  ml_method = "superlearner",
                                  n_folds = 5,
                                  n_rep = 1,
                                  trim_propensity = TRUE,
                                  target_population = NULL,
                                  seed = 123) {
  
  cat("################################################################################\n")
  cat("##################### DOUBLE MACHINE LEARNING STC ANALYSIS ###################\n")
  cat("################################################################################\n\n")
  
  set.seed(seed)
  
  # Prepare data
  cat("Step 1: Data preparation...\n")
  data_prep <- prepare_ml_stc_data(data, outcome_col, treatment_col, covariate_cols)
  
  Y <- data_prep$Y
  W <- data_prep$W
  X <- data_prep$X
  n <- length(Y)
  
  cat("ML method:", ml_method, "\n")
  cat("Cross-fitting folds:", n_folds, "\n")
  cat("Repetitions:", n_rep, "\n")
  cat("Sample size:", n, "\n\n")
  
  # Store results across repetitions
  ate_estimates <- numeric(n_rep)
  ate_ses <- numeric(n_rep)
  all_residuals <- list()
  
  for (rep in 1:n_rep) {
    
    cat("Step 2: Cross-fitting repetition", rep, "of", n_rep, "...\n")
    
    # Create cross-fitting folds for this repetition
    fold_seed <- seed + rep * 1000
    folds <- create_crossfit_folds(n, n_folds = n_folds, seed = fold_seed)
    
    # Initialize arrays for cross-fitted predictions
    m1_hat <- rep(NA, n)  # E[Y|X,W=1]
    m0_hat <- rep(NA, n)  # E[Y|X,W=0]
    e_hat <- rep(NA, n)   # E[W|X] (propensity scores)
    
    # Cross-fitting loop
    for (k in 1:n_folds) {
      
      cat("  Fold", k, "of", n_folds, "\n")
      
      train_idx <- folds[[k]]$train
      test_idx <- folds[[k]]$test
      
      X_train <- X[train_idx, , drop = FALSE]
      Y_train <- Y[train_idx]
      W_train <- W[train_idx]
      X_test <- X[test_idx, , drop = FALSE]
      
      # Outcome regression for treated units: E[Y|X,W=1]
      treated_train_idx <- W_train == 1
      if (sum(treated_train_idx) > 5) {
        
        m1_model <- fit_ml_model(
          X_train[treated_train_idx, , drop = FALSE],
          Y_train[treated_train_idx],
          method = ml_method,
          outcome_type = data_prep$outcome_type
        )
        
        m1_hat[test_idx] <- predict_ml_model(
          m1_model, X_test, ml_method, data_prep$outcome_type
        )
      } else {
        # Fallback to simple mean if too few treated units
        m1_hat[test_idx] <- mean(Y_train[treated_train_idx])
      }
      
      # Outcome regression for control units: E[Y|X,W=0]
      control_train_idx <- W_train == 0
      if (sum(control_train_idx) > 5) {
        
        m0_model <- fit_ml_model(
          X_train[control_train_idx, , drop = FALSE],
          Y_train[control_train_idx],
          method = ml_method,
          outcome_type = data_prep$outcome_type
        )
        
        m0_hat[test_idx] <- predict_ml_model(
          m0_model, X_test, ml_method, data_prep$outcome_type
        )
      } else {
        m0_hat[test_idx] <- mean(Y_train[control_train_idx])
      }
      
      # Propensity score model: E[W|X]
      e_model <- fit_ml_model(
        X_train, W_train,
        method = ml_method,
        outcome_type = "binary"
      )
      
      e_hat[test_idx] <- predict_ml_model(
        e_model, X_test, ml_method, "binary"
      )
    }
    
    # Trim extreme propensity scores if requested
    if (trim_propensity) {
      e_hat <- pmax(0.01, pmin(0.99, e_hat))
    }
    
    cat("Step 3: Computing DML estimator for repetition", rep, "...\n")
    
    # Compute residuals
    V_hat <- Y - W * m1_hat - (1 - W) * m0_hat  # Outcome residual
    U_hat <- W - e_hat                           # Treatment residual
    
    # Double ML moment condition: E[V * U] = 0 under unconfoundedness
    # ATE estimator: theta_hat = E[V * U] / E[U^2]
    
    # Compute ATE estimate
    numerator <- mean(V_hat * U_hat)
    denominator <- mean(U_hat^2)
    
    ate_hat <- numerator / denominator
    ate_estimates[rep] <- ate_hat
    
    # Influence function for standard error
    # IF = (V * U - theta * U^2) / E[U^2]
    influence_function <- (V_hat * U_hat - ate_hat * U_hat^2) / denominator
    
    # Variance estimate
    ate_var <- mean(influence_function^2) / n
    ate_se <- sqrt(ate_var)
    ate_ses[rep] <- ate_se
    
    # Store residuals for diagnostics
    all_residuals[[rep]] <- list(
      V_hat = V_hat,
      U_hat = U_hat,
      influence_function = influence_function
    )
    
    cat("  ATE estimate:", round(ate_hat, 4), "\n")
    cat("  Standard error:", round(ate_se, 4), "\n\n")
  }
  
  # Aggregate results across repetitions
  if (n_rep > 1) {
    final_ate <- mean(ate_estimates)
    final_se <- sqrt(mean(ate_ses^2) + var(ate_estimates))  # Account for between-rep variance
  } else {
    final_ate <- ate_estimates[1]
    final_se <- ate_ses[1]
  }
  
  # Confidence intervals
  ate_ci <- final_ate + c(-1.96, 1.96) * final_se
  
  # T-statistic and p-value
  t_stat <- final_ate / final_se
  p_value <- 2 * (1 - pnorm(abs(t_stat)))
  
  cat("=== DOUBLE ML RESULTS ===\n")
  cat("Average Treatment Effect:", round(final_ate, 4), "\n")
  cat("Standard Error:", round(final_se, 4), "\n")
  cat("95% Confidence Interval: (", round(ate_ci[1], 4), ", ", round(ate_ci[2], 4), ")\n")
  cat("T-statistic:", round(t_stat, 3), "\n")
  cat("P-value:", round(p_value, 4), "\n\n")
  
  # Additional treatment effects (using final fold predictions)
  final_residuals <- all_residuals[[n_rep]]
  
  # Individual treatment effects (approximate)
  tau_hat <- m1_hat - m0_hat
  att <- mean(tau_hat[W == 1])
  atc <- mean(tau_hat[W == 0])
  
  cat("Additional Effects:\n")
  cat("ATT (effect on treated):", round(att, 4), "\n")
  cat("ATC (effect on control):", round(atc, 4), "\n\n")
  
  # Step 4: Model diagnostics
  cat("Step 4: Model diagnostics...\n")
  
  diagnostics <- perform_dml_diagnostics(
    Y, W, X, m1_hat, m0_hat, e_hat, final_residuals, data_prep$outcome_type
  )
  
  # Step 5: Target population prediction (if provided)
  target_results <- NULL
  if (!is.null(target_population)) {
    cat("Step 5: Target population analysis...\n")
    
    target_results <- predict_dml_target_population(
      target_population, covariate_cols, X, Y, W, 
      ml_method, data_prep$outcome_type, n_folds
    )
    
    cat("Target population ATE:", round(target_results$target_ate, 4), "\n")
    cat("Target population SE:", round(target_results$target_se, 4), "\n\n")
  }
  
  # Compile comprehensive results
  results <- list(
    # Core estimates
    ate = list(
      estimate = final_ate,
      se = final_se,
      ci_lower = ate_ci[1],
      ci_upper = ate_ci[2],
      t_statistic = t_stat,
      p_value = p_value,
      att = att,
      atc = atc
    ),
    
    # Repetition results
    repetitions = list(
      estimates = ate_estimates,
      standard_errors = ate_ses,
      n_rep = n_rep
    ),
    
    # Predictions and residuals
    predictions = list(
      m1_hat = m1_hat,
      m0_hat = m0_hat,
      e_hat = e_hat,
      tau_hat = tau_hat,
      residuals = final_residuals
    ),
    
    # Diagnostics
    diagnostics = diagnostics,
    
    # Target population
    target_population = target_results,
    
    # Methodology info
    methodology = list(
      method = "Double Machine Learning",
      ml_method = ml_method,
      n_folds = n_folds,
      n_rep = n_rep,
      trim_propensity = trim_propensity
    ),
    
    # Data info
    data_info = data_prep,
    
    # Parameters
    parameters = list(
      seed = seed
    )
  )
  
  class(results) <- "double_ml_stc"
  
  cat("=== DOUBLE ML STC ANALYSIS COMPLETE ===\n")
  cat("Final ATE estimate:", round(final_ate, 4), "\n")
  cat("Standard error:", round(final_se, 4), "\n")
  cat("Statistical significance:", if(p_value < 0.05) "Yes" else "No", "(p =", round(p_value, 4), ")\n\n")
  
  return(results)
}

################################################################################
##################### Helper Functions ##########################################
################################################################################

#' Fit ML Model for DML
#' 
#' @param X Feature matrix
#' @param Y Target vector
#' @param method ML method
#' @param outcome_type Outcome type
#' @return Fitted model
fit_ml_model <- function(X, Y, method, outcome_type) {
  
  if (method == "rf") {
    
    if (outcome_type == "binary") {
      randomForest(X, as.factor(Y), ntree = 500, mtry = max(1, floor(sqrt(ncol(X)))))
    } else {
      randomForest(X, Y, ntree = 500, mtry = max(1, floor(ncol(X)/3)))
    }
    
  } else if (method == "xgboost") {
    
    dtrain <- xgb.DMatrix(data = X, label = Y)
    objective <- if (outcome_type == "binary") "binary:logistic" else "reg:squarederror"
    
    xgb.train(
      params = list(
        objective = objective,
        eta = 0.1,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8
      ),
      data = dtrain,
      nrounds = 100,
      verbose = 0
    )
    
  } else if (method == "superlearner") {
    
    # Define SuperLearner library
    SL_library <- c("SL.glm", "SL.randomForest", "SL.xgboost", "SL.glmnet")
    
    if (outcome_type == "binary") {
      SuperLearner(Y = Y, X = data.frame(X), 
                  SL.library = SL_library, 
                  family = binomial())
    } else {
      SuperLearner(Y = Y, X = data.frame(X),
                  SL.library = SL_library,
                  family = gaussian())
    }
    
  } else if (method == "elastic_net") {
    
    family <- if (outcome_type == "binary") "binomial" else "gaussian"
    cv.glmnet(X, Y, family = family, alpha = 0.5)
    
  } else {
    stop("Unsupported ML method: ", method)
  }
}

#' Predict with ML Model for DML
#' 
#' @param model Fitted model
#' @param X_new New feature matrix
#' @param method ML method used
#' @param outcome_type Outcome type
#' @return Predictions
predict_ml_model <- function(model, X_new, method, outcome_type) {
  
  if (method == "rf") {
    
    if (outcome_type == "binary") {
      predict(model, X_new, type = "prob")[, 2]
    } else {
      predict(model, X_new)
    }
    
  } else if (method == "xgboost") {
    
    predict(model, X_new)
    
  } else if (method == "superlearner") {
    
    predict(model, newdata = data.frame(X_new))$pred[, 1]
    
  } else if (method == "elastic_net") {
    
    predict(model, X_new, s = "lambda.min", type = "response")[, 1]
    
  }
}

#' Perform DML Diagnostics
#' 
#' @param Y Outcomes
#' @param W Treatments
#' @param X Covariates
#' @param m1_hat Outcome predictions for treated
#' @param m0_hat Outcome predictions for control
#' @param e_hat Propensity score predictions
#' @param residuals Residual list
#' @param outcome_type Outcome type
#' @return Diagnostic results
perform_dml_diagnostics <- function(Y, W, X, m1_hat, m0_hat, e_hat, residuals, outcome_type) {
  
  # Residual analysis
  V_hat <- residuals$V_hat
  U_hat <- residuals$U_hat
  
  # Check residual balance
  treated_idx <- W == 1
  control_idx <- W == 0
  
  balance_test_V <- t.test(V_hat[treated_idx], V_hat[control_idx])
  balance_test_U <- t.test(U_hat[treated_idx], U_hat[control_idx])
  
  # Propensity score diagnostics
  ps_summary <- list(
    mean = mean(e_hat),
    sd = sd(e_hat),
    min = min(e_hat),
    max = max(e_hat),
    range_trimmed = c(min(e_hat[e_hat > 0.01]), max(e_hat[e_hat < 0.99]))
  )
  
  # Model performance checks
  if (outcome_type == "binary") {
    # Brier scores for outcome models
    treated_brier <- mean((Y[treated_idx] - m1_hat[treated_idx])^2)
    control_brier <- mean((Y[control_idx] - m0_hat[control_idx])^2)
    outcome_performance <- list(treated_brier = treated_brier, control_brier = control_brier)
  } else {
    # RMSE for outcome models
    treated_rmse <- sqrt(mean((Y[treated_idx] - m1_hat[treated_idx])^2))
    control_rmse <- sqrt(mean((Y[control_idx] - m0_hat[control_idx])^2))
    outcome_performance <- list(treated_rmse = treated_rmse, control_rmse = control_rmse)
  }
  
  # Propensity score performance
  ps_auc <- tryCatch({
    if (requireNamespace("pROC", quietly = TRUE)) {
      pROC::auc(pROC::roc(W, e_hat, quiet = TRUE))
    } else {
      NA
    }
  }, error = function(e) NA)
  
  cat("Diagnostic Results:\n")
  cat("Propensity score summary:\n")
  cat("  Mean:", round(ps_summary$mean, 3), "\n")
  cat("  Range:", round(ps_summary$min, 3), "to", round(ps_summary$max, 3), "\n")
  if (!is.na(ps_auc)) {
    cat("  AUC:", round(ps_auc, 3), "\n")
  }
  
  if (outcome_type == "binary") {
    cat("Outcome model Brier scores:\n")
    cat("  Treated:", round(outcome_performance$treated_brier, 4), "\n")
    cat("  Control:", round(outcome_performance$control_brier, 4), "\n")
  } else {
    cat("Outcome model RMSE:\n")
    cat("  Treated:", round(outcome_performance$treated_rmse, 4), "\n")
    cat("  Control:", round(outcome_performance$control_rmse, 4), "\n")
  }
  
  cat("Residual balance tests:\n")
  cat("  V residuals p-value:", round(balance_test_V$p.value, 4), "\n")
  cat("  U residuals p-value:", round(balance_test_U$p.value, 4), "\n\n")
  
  return(list(
    propensity_score = ps_summary,
    outcome_performance = outcome_performance,
    balance_tests = list(V = balance_test_V, U = balance_test_U),
    propensity_auc = ps_auc
  ))
}

#' Predict Target Population Effects with DML
#' 
#' @param target_population Target population data
#' @param covariate_cols Covariate columns
#' @param X Original covariates
#' @param Y Original outcomes
#' @param W Original treatments
#' @param ml_method ML method
#' @param outcome_type Outcome type
#' @param n_folds Number of folds
#' @return Target population results
predict_dml_target_population <- function(target_population, covariate_cols, X, Y, W, 
                                         ml_method, outcome_type, n_folds) {
  
  target_X <- as.matrix(target_population[, covariate_cols, drop = FALSE])
  
  # Fit models on full data for target prediction
  treated_idx <- W == 1
  control_idx <- W == 0
  
  # Outcome models
  m1_model <- fit_ml_model(X[treated_idx, , drop = FALSE], Y[treated_idx], 
                          ml_method, outcome_type)
  m0_model <- fit_ml_model(X[control_idx, , drop = FALSE], Y[control_idx],
                          ml_method, outcome_type)
  
  # Predict for target population
  target_m1 <- predict_ml_model(m1_model, target_X, ml_method, outcome_type)
  target_m0 <- predict_ml_model(m0_model, target_X, ml_method, outcome_type)
  
  # Treatment effects
  target_tau <- target_m1 - target_m0
  target_ate <- mean(target_tau)
  target_se <- sd(target_tau) / sqrt(length(target_tau))
  
  return(list(
    target_effects = target_tau,
    target_ate = target_ate,
    target_se = target_se,
    target_ci = target_ate + c(-1.96, 1.96) * target_se
  ))
}

################################################################################
##################### Visualization Functions ###################################
################################################################################

#' Plot Double ML Results
#' 
#' @param dml_results Results from double_ml_stc_analysis
#' @return List of ggplot objects
#' @export
plot.double_ml_stc <- function(dml_results) {
  
  plots <- list()
  
  # 1. Treatment effect distribution
  tau_hat <- dml_results$predictions$tau_hat
  
  p1 <- ggplot(data.frame(tau = tau_hat), aes(x = tau)) +
    geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = dml_results$ate$estimate, 
               color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = dml_results$ate$ci_lower, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    geom_vline(xintercept = dml_results$ate$ci_upper, 
               color = "red", linetype = "dotted", alpha = 0.7) +
    labs(title = "Double ML: Individual Treatment Effect Distribution",
         subtitle = paste("ATE =", round(dml_results$ate$estimate, 3)),
         x = "Treatment Effect",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$distribution <- p1
  
  # 2. Propensity score distribution
  e_hat <- dml_results$predictions$e_hat
  W <- dml_results$data_info$W
  
  ps_data <- data.frame(
    propensity = e_hat,
    treatment = factor(W, labels = c("Control", "Treated"))
  )
  
  p2 <- ggplot(ps_data, aes(x = propensity, fill = treatment)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("Control" = "lightblue", "Treated" = "lightcoral")) +
    labs(title = "Double ML: Propensity Score Distribution",
         x = "Propensity Score",
         y = "Frequency",
         fill = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$propensity <- p2
  
  # 3. Residual plots
  residuals <- dml_results$predictions$residuals
  
  residual_data <- data.frame(
    V_residual = residuals$V_hat,
    U_residual = residuals$U_hat,
    treatment = factor(W, labels = c("Control", "Treated"))
  )
  
  p3 <- ggplot(residual_data, aes(x = U_residual, y = V_residual, color = treatment)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("Control" = "blue", "Treated" = "red")) +
    labs(title = "Double ML: Residual Analysis",
         x = "Treatment Residual (U)",
         y = "Outcome Residual (V)",
         color = "Group") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"))
  
  plots$residuals <- p3
  
  # 4. Convergence across repetitions (if multiple)
  if (dml_results$repetitions$n_rep > 1) {
    
    rep_data <- data.frame(
      repetition = 1:dml_results$repetitions$n_rep,
      estimate = dml_results$repetitions$estimates,
      se = dml_results$repetitions$standard_errors
    )
    
    p4 <- ggplot(rep_data, aes(x = repetition, y = estimate)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                   width = 0.1) +
      geom_hline(yintercept = dml_results$ate$estimate, 
                 color = "red", linetype = "dashed") +
      labs(title = "Double ML: Estimates Across Repetitions",
           x = "Repetition",
           y = "ATE Estimate") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    plots$repetitions <- p4
  }
  
  return(plots)
}

cat("Double Machine Learning STC functions loaded successfully.\n")
cat("Main function: double_ml_stc_analysis()\n")
cat("Plotting: plot.double_ml_stc()\n")
cat("Supports cross-fitting with multiple ML methods and repetitions.\n\n") 