################################################################################
##################### ML-Based STC Utility Functions ##########################
################################################################################
#
# Comprehensive utility functions supporting machine learning-based
# simulated treatment comparison methods including:
# - Data preparation and validation
# - Cross-fitting procedures for causal inference
# - Performance evaluation metrics
# - Common interfaces for different ML methods
#
# Author: Advanced STC Methods Package - ML Extension
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries for ML-based STC
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("xgboost")) install.packages("xgboost")
if (!require("glmnet")) install.packages("glmnet")
if (!require("SuperLearner")) install.packages("SuperLearner")

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(glmnet)
library(SuperLearner)

################################################################################
##################### Data Preparation Functions ###############################
################################################################################

#' Prepare Data for ML-Based STC Analysis
#' 
#' @param data Data frame containing IPD and/or aggregate data
#' @param outcome_col Name of outcome column
#' @param treatment_col Name of treatment column  
#' @param covariate_cols Vector of covariate column names
#' @param data_type Type of data ("ipd", "mixed", "simulated")
#' @return List containing prepared data components
#' @export
prepare_ml_stc_data <- function(data, 
                               outcome_col, 
                               treatment_col, 
                               covariate_cols,
                               data_type = "ipd") {
  
  cat("=== PREPARING DATA FOR ML-BASED STC ===\n")
  cat("Data type:", data_type, "\n")
  cat("Sample size:", nrow(data), "\n")
  cat("Outcome column:", outcome_col, "\n")
  cat("Treatment column:", treatment_col, "\n")
  cat("Covariates:", paste(covariate_cols, collapse = ", "), "\n\n")
  
  # Validate inputs
  required_cols <- c(outcome_col, treatment_col, covariate_cols)
  missing_cols <- required_cols[!required_cols %in% colnames(data)]
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Extract components
  Y <- data[[outcome_col]]
  W <- data[[treatment_col]]  # Treatment indicator
  X <- as.matrix(data[, covariate_cols, drop = FALSE])
  
  # Convert treatment to binary if needed
  if (is.factor(W) || is.character(W)) {
    unique_treatments <- unique(W)
    if (length(unique_treatments) != 2) {
      stop("Treatment must be binary for current ML methods")
    }
    W <- as.numeric(W == unique_treatments[2])  # Second level is treated
    treatment_levels <- unique_treatments
  } else {
    treatment_levels <- unique(W)
  }
  
  # Check for missing values
  missing_Y <- sum(is.na(Y))
  missing_W <- sum(is.na(W))
  missing_X <- sum(is.na(X))
  
  if (missing_Y > 0 || missing_W > 0 || missing_X > 0) {
    cat("Warning: Missing values detected\n")
    cat("Missing outcomes:", missing_Y, "\n")
    cat("Missing treatments:", missing_W, "\n") 
    cat("Missing covariates:", missing_X, "\n")
    
    # Complete case analysis
    complete_cases <- complete.cases(Y, W, X)
    Y <- Y[complete_cases]
    W <- W[complete_cases]
    X <- X[complete_cases, , drop = FALSE]
    
    cat("Complete cases:", sum(complete_cases), "\n")
  }
  
  # Summary statistics
  n_total <- length(Y)
  n_treated <- sum(W)
  n_control <- n_total - n_treated
  
  cat("Final sample composition:\n")
  cat("Total:", n_total, "\n")
  cat("Treated:", n_treated, "(", round(100 * n_treated / n_total, 1), "%)\n")
  cat("Control:", n_control, "(", round(100 * n_control / n_total, 1), "%)\n")
  
  if (is.numeric(Y)) {
    cat("Outcome summary (treated):", 
        round(mean(Y[W == 1]), 3), "±", round(sd(Y[W == 1]), 3), "\n")
    cat("Outcome summary (control):", 
        round(mean(Y[W == 0]), 3), "±", round(sd(Y[W == 0]), 3), "\n")
  } else {
    cat("Outcome rates (treated):", round(100 * mean(Y[W == 1]), 1), "%\n")
    cat("Outcome rates (control):", round(100 * mean(Y[W == 0]), 1), "%\n")
  }
  
  cat("\n")
  
  return(list(
    Y = Y,
    W = W, 
    X = X,
    n_total = n_total,
    n_treated = n_treated,
    n_control = n_control,
    covariate_names = covariate_cols,
    treatment_levels = treatment_levels,
    outcome_type = if(all(Y %in% c(0, 1))) "binary" else "continuous"
  ))
}

################################################################################
##################### Cross-Fitting Procedures #################################
################################################################################

#' Create Cross-Fitting Folds for Causal Inference
#' 
#' @param n Sample size
#' @param n_folds Number of folds for cross-fitting
#' @param seed Random seed for reproducibility
#' @return List of fold indices
#' @export
create_crossfit_folds <- function(n, n_folds = 5, seed = 123) {
  
  set.seed(seed)
  fold_ids <- sample(rep(1:n_folds, length.out = n))
  
  folds <- list()
  for (k in 1:n_folds) {
    folds[[k]] <- list(
      train = which(fold_ids != k),
      test = which(fold_ids == k)
    )
  }
  
  cat("Created", n_folds, "cross-fitting folds\n")
  cat("Average fold size:", round(n / n_folds), "\n")
  
  return(folds)
}

#' Perform Cross-Fitted Prediction
#' 
#' @param X Covariate matrix
#' @param Y Outcome vector
#' @param W Treatment vector (optional)
#' @param learner ML learner function
#' @param folds Cross-fitting folds
#' @param ... Additional arguments to learner
#' @return Vector of cross-fitted predictions
#' @export
crossfit_predict <- function(X, Y, W = NULL, learner, folds, ...) {
  
  n <- nrow(X)
  predictions <- rep(NA, n)
  
  for (k in 1:length(folds)) {
    
    # Training data
    train_idx <- folds[[k]]$train
    test_idx <- folds[[k]]$test
    
    X_train <- X[train_idx, , drop = FALSE]
    Y_train <- Y[train_idx]
    X_test <- X[test_idx, , drop = FALSE]
    
    if (!is.null(W)) {
      W_train <- W[train_idx]
      # Fit model with treatment
      model <- learner(X_train, Y_train, W_train, ...)
      pred <- predict(model, X_test, ...)
    } else {
      # Fit model without treatment
      model <- learner(X_train, Y_train, ...)
      pred <- predict(model, X_test, ...)
    }
    
    predictions[test_idx] <- pred
  }
  
  return(predictions)
}

################################################################################
##################### Propensity Score Estimation ##############################
################################################################################

#' Estimate Propensity Scores Using ML Methods
#' 
#' @param X Covariate matrix
#' @param W Treatment vector
#' @param method ML method ("logistic", "rf", "xgboost", "superlearner")
#' @param folds Cross-fitting folds (optional)
#' @return List containing propensity scores and model
#' @export
estimate_propensity_scores <- function(X, W, method = "logistic", folds = NULL) {
  
  cat("Estimating propensity scores using:", method, "\n")
  
  if (is.null(folds)) {
    # No cross-fitting
    if (method == "logistic") {
      model <- glm(W ~ ., data = data.frame(W = W, X), family = binomial())
      ps <- predict(model, type = "response")
    } else if (method == "rf") {
      model <- randomForest(W ~ ., data = data.frame(W = as.factor(W), X))
      ps <- predict(model, type = "prob")[, 2]
    } else if (method == "xgboost") {
      dtrain <- xgb.DMatrix(data = X, label = W)
      model <- xgb.train(
        params = list(objective = "binary:logistic", eval_metric = "logloss"),
        data = dtrain,
        nrounds = 100,
        verbose = 0
      )
      ps <- predict(model, X)
    } else if (method == "superlearner") {
      model <- SuperLearner(Y = W, X = data.frame(X), 
                           SL.library = c("SL.glm", "SL.randomForest", "SL.xgboost"),
                           family = binomial())
      ps <- model$SL.predict[, 1]
    }
  } else {
    # Cross-fitting
    ps <- crossfit_predict(X, W, learner = function(X, Y, ...) {
      if (method == "logistic") {
        glm(Y ~ ., data = data.frame(Y = Y, X), family = binomial())
      } else if (method == "rf") {
        randomForest(Y ~ ., data = data.frame(Y = as.factor(Y), X))
      } else if (method == "xgboost") {
        dtrain <- xgb.DMatrix(data = X, label = Y)
        xgb.train(
          params = list(objective = "binary:logistic", eval_metric = "logloss"),
          data = dtrain,
          nrounds = 100,
          verbose = 0
        )
      }
    }, folds = folds)
    model <- NULL  # Not saved in cross-fitting
  }
  
  # Bound propensity scores to avoid extreme values
  ps <- pmax(0.01, pmin(0.99, ps))
  
  cat("Propensity score summary:\n")
  cat("Mean:", round(mean(ps), 3), "\n")
  cat("Range:", round(range(ps), 3), "\n")
  cat("Treated group mean PS:", round(mean(ps[W == 1]), 3), "\n")
  cat("Control group mean PS:", round(mean(ps[W == 0]), 3), "\n\n")
  
  return(list(
    propensity_scores = ps,
    model = model,
    method = method
  ))
}

################################################################################
##################### Outcome Modeling ##########################################
################################################################################

#' Fit Outcome Models Using ML Methods
#' 
#' @param X Covariate matrix
#' @param Y Outcome vector
#' @param W Treatment vector
#' @param method ML method ("linear", "rf", "xgboost", "superlearner")
#' @param separate_models Whether to fit separate models for treated/control
#' @param folds Cross-fitting folds (optional)
#' @return List containing outcome models and predictions
#' @export
fit_outcome_models <- function(X, Y, W, method = "linear", 
                              separate_models = TRUE, folds = NULL) {
  
  cat("Fitting outcome models using:", method, "\n")
  cat("Separate models for treated/control:", separate_models, "\n")
  
  if (separate_models) {
    # Fit separate models for treated and control groups
    treated_idx <- W == 1
    control_idx <- W == 0
    
    if (is.null(folds)) {
      # No cross-fitting
      if (method == "linear") {
        model_treated <- lm(Y ~ ., data = data.frame(Y = Y[treated_idx], X[treated_idx, , drop = FALSE]))
        model_control <- lm(Y ~ ., data = data.frame(Y = Y[control_idx], X[control_idx, , drop = FALSE]))
      } else if (method == "rf") {
        model_treated <- randomForest(Y ~ ., data = data.frame(Y = Y[treated_idx], X[treated_idx, , drop = FALSE]))
        model_control <- randomForest(Y ~ ., data = data.frame(Y = Y[control_idx], X[control_idx, , drop = FALSE]))
      } else if (method == "xgboost") {
        dtrain_treated <- xgb.DMatrix(data = X[treated_idx, , drop = FALSE], label = Y[treated_idx])
        dtrain_control <- xgb.DMatrix(data = X[control_idx, , drop = FALSE], label = Y[control_idx])
        
        objective <- if (all(Y %in% c(0, 1))) "binary:logistic" else "reg:squarederror"
        
        model_treated <- xgb.train(
          params = list(objective = objective),
          data = dtrain_treated,
          nrounds = 100,
          verbose = 0
        )
        model_control <- xgb.train(
          params = list(objective = objective),
          data = dtrain_control,
          nrounds = 100,
          verbose = 0
        )
      }
      
      # Predict both potential outcomes for all individuals
      mu1 <- predict(model_treated, data.frame(X))
      mu0 <- predict(model_control, data.frame(X))
      
    } else {
      # Cross-fitting
      mu1 <- rep(NA, nrow(X))
      mu0 <- rep(NA, nrow(X))
      
      for (k in 1:length(folds)) {
        train_idx <- folds[[k]]$train
        test_idx <- folds[[k]]$test
        
        # Subset to treated/control within this fold
        train_treated <- intersect(train_idx, which(W == 1))
        train_control <- intersect(train_idx, which(W == 0))
        
        if (length(train_treated) > 0 && length(train_control) > 0) {
          X_train_treated <- X[train_treated, , drop = FALSE]
          Y_train_treated <- Y[train_treated]
          X_train_control <- X[train_control, , drop = FALSE]
          Y_train_control <- Y[train_control]
          X_test <- X[test_idx, , drop = FALSE]
          
          if (method == "linear") {
            model_treated <- lm(Y_train_treated ~ ., data = data.frame(Y_train_treated, X_train_treated))
            model_control <- lm(Y_train_control ~ ., data = data.frame(Y_train_control, X_train_control))
            
            mu1[test_idx] <- predict(model_treated, data.frame(X_test))
            mu0[test_idx] <- predict(model_control, data.frame(X_test))
          } else if (method == "rf") {
            model_treated <- randomForest(Y_train_treated ~ ., data = data.frame(Y_train_treated, X_train_treated))
            model_control <- randomForest(Y_train_control ~ ., data = data.frame(Y_train_control, X_train_control))
            
            mu1[test_idx] <- predict(model_treated, data.frame(X_test))
            mu0[test_idx] <- predict(model_control, data.frame(X_test))
          }
        }
      }
      
      model_treated <- NULL
      model_control <- NULL
    }
    
  } else {
    # Single model with treatment as covariate
    X_with_treatment <- cbind(X, W = W)
    
    if (is.null(folds)) {
      if (method == "linear") {
        model <- lm(Y ~ ., data = data.frame(Y = Y, X_with_treatment))
      } else if (method == "rf") {
        model <- randomForest(Y ~ ., data = data.frame(Y = Y, X_with_treatment))
      } else if (method == "xgboost") {
        dtrain <- xgb.DMatrix(data = X_with_treatment, label = Y)
        objective <- if (all(Y %in% c(0, 1))) "binary:logistic" else "reg:squarederror"
        model <- xgb.train(
          params = list(objective = objective),
          data = dtrain,
          nrounds = 100,
          verbose = 0
        )
      }
      
      # Predict potential outcomes
      X_treated <- cbind(X, W = 1)
      X_control <- cbind(X, W = 0)
      
      mu1 <- predict(model, data.frame(X_treated))
      mu0 <- predict(model, data.frame(X_control))
      
    } else {
      # Cross-fitting for single model
      mu1 <- rep(NA, nrow(X))
      mu0 <- rep(NA, nrow(X))
      
      for (k in 1:length(folds)) {
        train_idx <- folds[[k]]$train
        test_idx <- folds[[k]]$test
        
        X_train <- cbind(X[train_idx, , drop = FALSE], W = W[train_idx])
        Y_train <- Y[train_idx]
        X_test <- X[test_idx, , drop = FALSE]
        
        if (method == "linear") {
          model <- lm(Y_train ~ ., data = data.frame(Y_train, X_train))
          
          mu1[test_idx] <- predict(model, data.frame(cbind(X_test, W = 1)))
          mu0[test_idx] <- predict(model, data.frame(cbind(X_test, W = 0)))
        } else if (method == "rf") {
          model <- randomForest(Y_train ~ ., data = data.frame(Y_train, X_train))
          
          mu1[test_idx] <- predict(model, data.frame(cbind(X_test, W = 1)))
          mu0[test_idx] <- predict(model, data.frame(cbind(X_test, W = 0)))
        }
      }
      
      model <- NULL
      model_treated <- NULL
      model_control <- NULL
    }
  }
  
  cat("Outcome modeling complete\n")
  cat("Mean predicted Y(1):", round(mean(mu1, na.rm = TRUE), 3), "\n")
  cat("Mean predicted Y(0):", round(mean(mu0, na.rm = TRUE), 3), "\n")
  cat("Estimated ATE:", round(mean(mu1 - mu0, na.rm = TRUE), 3), "\n\n")
  
  return(list(
    mu1 = mu1,  # Predicted outcomes under treatment
    mu0 = mu0,  # Predicted outcomes under control
    model_treated = model_treated,
    model_control = model_control,
    model_combined = if (!separate_models) model else NULL,
    method = method,
    separate_models = separate_models
  ))
}

################################################################################
##################### Evaluation Metrics ####################################
################################################################################

#' Calculate Treatment Effect Metrics
#' 
#' @param mu1 Predicted outcomes under treatment
#' @param mu0 Predicted outcomes under control
#' @param Y Observed outcomes
#' @param W Treatment assignment
#' @param ps Propensity scores (optional)
#' @return List of treatment effect estimates and metrics
#' @export
calculate_treatment_effects <- function(mu1, mu0, Y, W, ps = NULL) {
  
  # Average Treatment Effect (ATE)
  ate <- mean(mu1 - mu0, na.rm = TRUE)
  
  # Average Treatment Effect on Treated (ATT)
  att <- mean((mu1 - mu0)[W == 1], na.rm = TRUE)
  
  # Average Treatment Effect on Control (ATC)
  atc <- mean((mu1 - mu0)[W == 0], na.rm = TRUE)
  
  # Naive difference in means
  naive_effect <- mean(Y[W == 1]) - mean(Y[W == 0])
  
  # Calculate confidence intervals using influence functions
  # This is a simplified version - more sophisticated methods available
  tau_hat <- mu1 - mu0
  ate_se <- sd(tau_hat, na.rm = TRUE) / sqrt(sum(!is.na(tau_hat)))
  ate_ci <- ate + c(-1.96, 1.96) * ate_se
  
  # If propensity scores available, calculate IPW estimate
  if (!is.null(ps)) {
    ipw_weights <- W / ps + (1 - W) / (1 - ps)
    ipw_effect <- sum(ipw_weights * W * Y) / sum(ipw_weights * W) - 
                  sum(ipw_weights * (1 - W) * Y) / sum(ipw_weights * (1 - W))
  } else {
    ipw_effect <- NA
  }
  
  cat("=== TREATMENT EFFECT ESTIMATES ===\n")
  cat("Average Treatment Effect (ATE):", round(ate, 4), "\n")
  cat("95% CI for ATE: (", round(ate_ci[1], 4), ", ", round(ate_ci[2], 4), ")\n")
  cat("ATT (effect on treated):", round(att, 4), "\n")
  cat("ATC (effect on control):", round(atc, 4), "\n")
  cat("Naive difference in means:", round(naive_effect, 4), "\n")
  if (!is.na(ipw_effect)) {
    cat("IPW estimate:", round(ipw_effect, 4), "\n")
  }
  cat("\n")
  
  return(list(
    ate = ate,
    att = att,
    atc = atc,
    ate_se = ate_se,
    ate_ci = ate_ci,
    naive_effect = naive_effect,
    ipw_effect = ipw_effect,
    individual_effects = tau_hat
  ))
}

#' Evaluate Model Performance
#' 
#' @param predictions Predicted values
#' @param actual Actual values
#' @param type Type of outcome ("binary" or "continuous")
#' @return List of performance metrics
#' @export
evaluate_model_performance <- function(predictions, actual, type = "continuous") {
  
  # Remove missing values
  complete_cases <- !is.na(predictions) & !is.na(actual)
  pred <- predictions[complete_cases]
  obs <- actual[complete_cases]
  
  if (type == "continuous") {
    # Continuous outcome metrics
    mse <- mean((pred - obs)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(pred - obs))
    r_squared <- cor(pred, obs)^2
    
    metrics <- list(
      mse = mse,
      rmse = rmse,
      mae = mae,
      r_squared = r_squared,
      correlation = cor(pred, obs)
    )
    
  } else {
    # Binary outcome metrics  
    obs_binary <- as.numeric(obs)
    pred_binary <- as.numeric(pred > 0.5)
    
    accuracy <- mean(pred_binary == obs_binary)
    
    # AUC calculation
    if (all(pred >= 0) && all(pred <= 1)) {
      auc <- tryCatch({
        pROC::auc(pROC::roc(obs_binary, pred, quiet = TRUE))
      }, error = function(e) NA)
    } else {
      auc <- NA
    }
    
    # Brier score
    brier_score <- mean((pred - obs_binary)^2)
    
    metrics <- list(
      accuracy = accuracy,
      auc = as.numeric(auc),
      brier_score = brier_score,
      log_loss = -mean(obs_binary * log(pmax(pred, 1e-15)) + 
                      (1 - obs_binary) * log(pmax(1 - pred, 1e-15)))
    )
  }
  
  return(metrics)
}

################################################################################
##################### Visualization Functions ################################## 
################################################################################

#' Plot Treatment Effect Heterogeneity
#' 
#' @param individual_effects Vector of individual treatment effects
#' @param X Covariate matrix
#' @param covariate_names Names of covariates
#' @return ggplot object
#' @export
plot_treatment_heterogeneity <- function(individual_effects, X, covariate_names) {
  
  if (!require("ggplot2")) {
    stop("ggplot2 package required for plotting")
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    tau = individual_effects,
    X
  )
  colnames(plot_data) <- c("tau", covariate_names)
  
  # Create histogram of treatment effects
  p1 <- ggplot(plot_data, aes(x = tau)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = mean(individual_effects, na.rm = TRUE), 
               color = "red", linetype = "dashed", size = 1) +
    labs(title = "Distribution of Individual Treatment Effects",
         x = "Treatment Effect",
         y = "Frequency") +
    theme_minimal()
  
  # If we have continuous covariates, show scatter plots
  continuous_vars <- sapply(plot_data[covariate_names], is.numeric)
  
  if (any(continuous_vars)) {
    first_continuous <- names(continuous_vars)[continuous_vars][1]
    
    p2 <- ggplot(plot_data, aes_string(x = first_continuous, y = "tau")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "loess", color = "red") +
      labs(title = paste("Treatment Effects vs", first_continuous),
           x = first_continuous,
           y = "Treatment Effect") +
      theme_minimal()
    
    return(list(distribution = p1, scatter = p2))
  } else {
    return(list(distribution = p1))
  }
}

################################################################################
##################### Summary and Export Functions ##############################
################################################################################

#' Generate ML-STC Analysis Summary
#' 
#' @param results List containing analysis results
#' @param method_name Name of the ML method used
#' @return Formatted summary
#' @export
generate_ml_stc_summary <- function(results, method_name) {
  
  cat("################################################################################\n")
  cat("##################### ML-STC ANALYSIS SUMMARY ################################\n")
  cat("################################################################################\n\n")
  
  cat("Method:", method_name, "\n")
  cat("Analysis Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  if ("data_info" %in% names(results)) {
    cat("DATA SUMMARY:\n")
    cat("Sample size:", results$data_info$n_total, "\n")
    cat("Treated:", results$data_info$n_treated, "\n")
    cat("Control:", results$data_info$n_control, "\n")
    cat("Outcome type:", results$data_info$outcome_type, "\n\n")
  }
  
  if ("treatment_effects" %in% names(results)) {
    cat("TREATMENT EFFECT ESTIMATES:\n")
    cat("ATE:", round(results$treatment_effects$ate, 4), "\n")
    cat("95% CI: (", round(results$treatment_effects$ate_ci[1], 4), 
        ", ", round(results$treatment_effects$ate_ci[2], 4), ")\n")
    cat("ATT:", round(results$treatment_effects$att, 4), "\n")
    cat("ATC:", round(results$treatment_effects$atc, 4), "\n\n")
  }
  
  if ("model_performance" %in% names(results)) {
    cat("MODEL PERFORMANCE:\n")
    perf <- results$model_performance
    for (metric in names(perf)) {
      if (!is.na(perf[[metric]])) {
        cat(metric, ":", round(perf[[metric]], 4), "\n")
      }
    }
    cat("\n")
  }
  
  cat("Analysis completed successfully.\n")
  cat("Use plot() methods for visualization.\n\n")
  
  invisible(results)
}

cat("ML-STC utility functions loaded successfully.\n")
cat("Available functions:\n")
cat("- prepare_ml_stc_data(): Data preparation\n")
cat("- create_crossfit_folds(): Cross-fitting setup\n") 
cat("- estimate_propensity_scores(): PS estimation\n")
cat("- fit_outcome_models(): Outcome modeling\n")
cat("- calculate_treatment_effects(): Effect estimation\n")
cat("- evaluate_model_performance(): Model evaluation\n")
cat("- plot_treatment_heterogeneity(): Visualization\n")
cat("- generate_ml_stc_summary(): Results summary\n\n") 