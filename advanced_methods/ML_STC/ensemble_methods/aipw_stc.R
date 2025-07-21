################################################################################
##################### AIPW with Machine Learning for STC Analysis ##############
################################################################################
#
# This module implements Augmented Inverse Propensity Weighting (AIPW) with
# machine learning methods for robust causal inference in STC analysis.
#
# AIPW is doubly robust: provides consistent estimates if either the propensity
# score model OR the outcome model is correctly specified.
#
# Author: Advanced STC Methods Package
# Date: 2024
#
# Dependencies: randomForest, glmnet, xgboost, SuperLearner, tmle

# Load required libraries
required_packages <- c("randomForest", "glmnet", "xgboost", "nnet")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(sprintf("Package '%s' not available. Some ML methods will not work.", pkg))
  }
}

# Try to load SuperLearner if available
if (!requireNamespace("SuperLearner", quietly = TRUE)) {
  warning("SuperLearner package not available. Advanced ensemble methods will be limited.")
}

#' AIPW with Machine Learning for STC Analysis
#' 
#' Implements Augmented Inverse Propensity Weighting using machine learning
#' methods for both propensity score and outcome modeling
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional target population data for prediction
#' @param ps_method Method for propensity score estimation ('logit', 'rf', 'xgboost', 'superlearner')
#' @param outcome_method Method for outcome modeling ('lm', 'rf', 'xgboost', 'superlearner')
#' @param cross_fit Logical, whether to use cross-fitting
#' @param n_folds Number of folds for cross-fitting
#' @param trim_bounds Vector of length 2 with lower and upper bounds for propensity score trimming
#' @param superlearner_lib Library of algorithms for SuperLearner (if applicable)
#' @param seed Random seed for reproducibility
#' @return List containing AIPW estimates, diagnostics, and model information
aipw_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                             target_population = NULL, ps_method = "rf",
                             outcome_method = "rf", cross_fit = TRUE, n_folds = 5,
                             trim_bounds = c(0.05, 0.95), 
                             superlearner_lib = c("SL.glm", "SL.randomForest", "SL.xgboost"),
                             seed = 123) {
  
  set.seed(seed)
  
  # Prepare data
  cat("Preparing data for AIPW analysis...\n")
  X <- data[, covariate_cols, drop = FALSE]
  y <- data[[outcome_col]]
  t <- as.numeric(data[[treatment_col]])
  n <- nrow(data)
  
  # Validate treatment variable
  if (!all(t %in% c(0, 1))) {
    stop("Treatment variable must be binary (0/1)")
  }
  
  # Initialize results containers
  if (cross_fit) {
    cat(sprintf("Using %d-fold cross-fitting for robust estimation...\n", n_folds))
    
    # Create folds
    fold_ids <- sample(rep(1:n_folds, length.out = n))
    aipw_estimates <- numeric(n)
    ps_estimates <- numeric(n)
    mu0_estimates <- numeric(n)
    mu1_estimates <- numeric(n)
    
    for (fold in 1:n_folds) {
      cat(sprintf("Processing fold %d/%d...\n", fold, n_folds))
      
      # Split data
      train_idx <- which(fold_ids != fold)
      test_idx <- which(fold_ids == fold)
      
      X_train <- X[train_idx, , drop = FALSE]
      y_train <- y[train_idx]
      t_train <- t[train_idx]
      
      X_test <- X[test_idx, , drop = FALSE]
      y_test <- y[test_idx]
      t_test <- t[test_idx]
      
      # Estimate propensity scores
      ps_model <- fit_propensity_model(X_train, t_train, method = ps_method, 
                                      superlearner_lib = superlearner_lib)
      ps_test <- predict_propensity_scores(ps_model, X_test, method = ps_method)
      ps_estimates[test_idx] <- ps_test
      
      # Estimate outcome models
      outcome_models <- fit_outcome_models(X_train, y_train, t_train, 
                                         method = outcome_method,
                                         superlearner_lib = superlearner_lib)
      
      mu0_test <- predict_outcomes(outcome_models$mu0_model, X_test, 
                                 method = outcome_method, treatment = 0)
      mu1_test <- predict_outcomes(outcome_models$mu1_model, X_test, 
                                 method = outcome_method, treatment = 1)
      
      mu0_estimates[test_idx] <- mu0_test
      mu1_estimates[test_idx] <- mu1_test
      
      # Calculate AIPW estimates for this fold
      ps_trimmed <- pmax(pmin(ps_test, trim_bounds[2]), trim_bounds[1])
      
      aipw_fold <- mu1_test - mu0_test + 
                   t_test * (y_test - mu1_test) / ps_trimmed - 
                   (1 - t_test) * (y_test - mu0_test) / (1 - ps_trimmed)
      
      aipw_estimates[test_idx] <- aipw_fold
    }
    
  } else {
    cat("Using full sample estimation...\n")
    
    # Estimate propensity scores
    ps_model <- fit_propensity_model(X, t, method = ps_method,
                                    superlearner_lib = superlearner_lib)
    ps_estimates <- predict_propensity_scores(ps_model, X, method = ps_method)
    
    # Estimate outcome models
    outcome_models <- fit_outcome_models(X, y, t, method = outcome_method,
                                       superlearner_lib = superlearner_lib)
    
    mu0_estimates <- predict_outcomes(outcome_models$mu0_model, X, 
                                    method = outcome_method, treatment = 0)
    mu1_estimates <- predict_outcomes(outcome_models$mu1_model, X, 
                                    method = outcome_method, treatment = 1)
    
    # Calculate AIPW estimates
    ps_trimmed <- pmax(pmin(ps_estimates, trim_bounds[2]), trim_bounds[1])
    
    aipw_estimates <- mu1_estimates - mu0_estimates + 
                      t * (y - mu1_estimates) / ps_trimmed - 
                      (1 - t) * (y - mu0_estimates) / (1 - ps_trimmed)
  }
  
  # Calculate ATE
  ate_estimate <- mean(aipw_estimates)
  
  # Calculate influence function for standard errors
  influence_function <- aipw_estimates - ate_estimate
  ate_se <- sqrt(var(influence_function) / n)
  
  # Confidence intervals
  ci_lower <- ate_estimate - 1.96 * ate_se
  ci_upper <- ate_estimate + 1.96 * ate_se
  
  # Predictions for target population if provided
  target_predictions <- NULL
  if (!is.null(target_population)) {
    cat("Making predictions for target population...\n")
    
    X_target <- target_population[, covariate_cols, drop = FALSE]
    
    # Refit models on full data if cross-fitting was used
    if (cross_fit) {
      ps_model <- fit_propensity_model(X, t, method = ps_method,
                                      superlearner_lib = superlearner_lib)
      outcome_models <- fit_outcome_models(X, y, t, method = outcome_method,
                                         superlearner_lib = superlearner_lib)
    }
    
    # Predict for target population
    ps_target <- predict_propensity_scores(ps_model, X_target, method = ps_method)
    mu0_target <- predict_outcomes(outcome_models$mu0_model, X_target, 
                                 method = outcome_method, treatment = 0)
    mu1_target <- predict_outcomes(outcome_models$mu1_model, X_target, 
                                 method = outcome_method, treatment = 1)
    
    tau_target <- mu1_target - mu0_target
    ate_target <- mean(tau_target)
    
    target_predictions <- list(
      propensity_scores = ps_target,
      mu0_pred = mu0_target,
      mu1_pred = mu1_target,
      individual_effects = tau_target,
      ate_estimate = ate_target
    )
  }
  
  # Model diagnostics
  diagnostics <- evaluate_aipw_diagnostics(ps_estimates, mu0_estimates, mu1_estimates,
                                          y, t, trim_bounds)
  
  cat("AIPW analysis completed.\n")
  cat(sprintf("Estimated ATE: %.4f (SE: %.4f, 95%% CI: [%.4f, %.4f])\n", 
              ate_estimate, ate_se, ci_lower, ci_upper))
  cat(sprintf("Propensity score overlap: %.3f\n", diagnostics$ps_overlap))
  
  return(list(
    method = "AIPW",
    ate_estimate = ate_estimate,
    ate_se = ate_se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    individual_effects = aipw_estimates,
    propensity_scores = ps_estimates,
    mu0_pred = mu0_estimates,
    mu1_pred = mu1_estimates,
    target_predictions = target_predictions,
    diagnostics = diagnostics,
    model_params = list(
      ps_method = ps_method,
      outcome_method = outcome_method,
      cross_fit = cross_fit,
      n_folds = n_folds,
      trim_bounds = trim_bounds
    )
  ))
}

#' Fit Propensity Score Model
#' 
#' Fits propensity score model using specified method
#' 
#' @param X Covariate matrix
#' @param t Treatment vector
#' @param method Method for propensity score estimation
#' @param superlearner_lib Library for SuperLearner
#' @return Fitted propensity score model
fit_propensity_model <- function(X, t, method = "rf", superlearner_lib = NULL) {
  
  if (method == "logit") {
    # Logistic regression
    ps_data <- data.frame(t = t, X)
    model <- glm(t ~ ., data = ps_data, family = binomial())
    
  } else if (method == "rf" && requireNamespace("randomForest", quietly = TRUE)) {
    # Random Forest
    model <- randomForest::randomForest(x = X, y = as.factor(t), ntree = 500)
    
  } else if (method == "xgboost" && requireNamespace("xgboost", quietly = TRUE)) {
    # XGBoost
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = t)
    params <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 6,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 100, 
                               verbose = 0)
    
  } else if (method == "superlearner" && requireNamespace("SuperLearner", quietly = TRUE)) {
    # SuperLearner
    if (is.null(superlearner_lib)) {
      superlearner_lib <- c("SL.glm", "SL.randomForest")
    }
    model <- SuperLearner::SuperLearner(Y = t, X = X, family = binomial(),
                                       SL.library = superlearner_lib)
    
  } else {
    # Fallback to logistic regression
    warning("Requested method not available, using logistic regression")
    ps_data <- data.frame(t = t, X)
    model <- glm(t ~ ., data = ps_data, family = binomial())
    method <- "logit"
  }
  
  return(list(model = model, method = method))
}

#' Predict Propensity Scores
#' 
#' Predicts propensity scores using fitted model
#' 
#' @param ps_model Fitted propensity score model
#' @param X Covariate matrix for prediction
#' @param method Method used for fitting
#' @return Vector of predicted propensity scores
predict_propensity_scores <- function(ps_model, X, method = "rf") {
  
  model <- ps_model$model
  method <- ps_model$method
  
  if (method == "logit") {
    pred_data <- data.frame(X)
    names(pred_data) <- names(model$coefficients)[-1]  # Remove intercept
    ps <- predict(model, newdata = pred_data, type = "response")
    
  } else if (method == "rf") {
    ps <- predict(model, newdata = X, type = "prob")[, "1"]
    
  } else if (method == "xgboost") {
    dtest <- xgboost::xgb.DMatrix(data = as.matrix(X))
    ps <- predict(model, newdata = dtest)
    
  } else if (method == "superlearner") {
    ps <- predict(model, newdata = X)$pred
    
  } else {
    stop("Unknown method for propensity score prediction")
  }
  
  return(as.numeric(ps))
}

#' Fit Outcome Models
#' 
#' Fits separate outcome models for each treatment group
#' 
#' @param X Covariate matrix
#' @param y Outcome vector
#' @param t Treatment vector
#' @param method Method for outcome modeling
#' @param superlearner_lib Library for SuperLearner
#' @return List with fitted outcome models for treatment and control
fit_outcome_models <- function(X, y, t, method = "rf", superlearner_lib = NULL) {
  
  # Split data by treatment
  X0 <- X[t == 0, , drop = FALSE]
  y0 <- y[t == 0]
  X1 <- X[t == 1, , drop = FALSE]
  y1 <- y[t == 1]
  
  # Fit models
  mu0_model <- fit_single_outcome_model(X0, y0, method, superlearner_lib)
  mu1_model <- fit_single_outcome_model(X1, y1, method, superlearner_lib)
  
  return(list(mu0_model = mu0_model, mu1_model = mu1_model))
}

#' Fit Single Outcome Model
#' 
#' Fits a single outcome model using specified method
#' 
#' @param X Covariate matrix
#' @param y Outcome vector
#' @param method Method for outcome modeling
#' @param superlearner_lib Library for SuperLearner
#' @return Fitted outcome model
fit_single_outcome_model <- function(X, y, method = "rf", superlearner_lib = NULL) {
  
  if (method == "lm") {
    # Linear regression
    outcome_data <- data.frame(y = y, X)
    model <- lm(y ~ ., data = outcome_data)
    
  } else if (method == "rf" && requireNamespace("randomForest", quietly = TRUE)) {
    # Random Forest
    model <- randomForest::randomForest(x = X, y = y, ntree = 500)
    
  } else if (method == "xgboost" && requireNamespace("xgboost", quietly = TRUE)) {
    # XGBoost
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(X), label = y)
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = 6,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
    model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 100, 
                               verbose = 0)
    
  } else if (method == "superlearner" && requireNamespace("SuperLearner", quietly = TRUE)) {
    # SuperLearner
    if (is.null(superlearner_lib)) {
      superlearner_lib <- c("SL.glm", "SL.randomForest")
    }
    model <- SuperLearner::SuperLearner(Y = y, X = X, family = gaussian(),
                                       SL.library = superlearner_lib)
    
  } else {
    # Fallback to linear regression
    warning("Requested method not available, using linear regression")
    outcome_data <- data.frame(y = y, X)
    model <- lm(y ~ ., data = outcome_data)
    method <- "lm"
  }
  
  return(list(model = model, method = method))
}

#' Predict Outcomes
#' 
#' Predicts outcomes using fitted model
#' 
#' @param outcome_model Fitted outcome model
#' @param X Covariate matrix for prediction
#' @param method Method used for fitting
#' @param treatment Treatment level (for naming consistency)
#' @return Vector of predicted outcomes
predict_outcomes <- function(outcome_model, X, method = "rf", treatment = NULL) {
  
  model <- outcome_model$model
  method <- outcome_model$method
  
  if (method == "lm") {
    pred_data <- data.frame(X)
    pred <- predict(model, newdata = pred_data)
    
  } else if (method == "rf") {
    pred <- predict(model, newdata = X)
    
  } else if (method == "xgboost") {
    dtest <- xgboost::xgb.DMatrix(data = as.matrix(X))
    pred <- predict(model, newdata = dtest)
    
  } else if (method == "superlearner") {
    pred <- predict(model, newdata = X)$pred
    
  } else {
    stop("Unknown method for outcome prediction")
  }
  
  return(as.numeric(pred))
}

#' Evaluate AIPW Diagnostics
#' 
#' Evaluates diagnostic measures for AIPW analysis
#' 
#' @param ps Propensity scores
#' @param mu0 Predicted outcomes under control
#' @param mu1 Predicted outcomes under treatment
#' @param y Observed outcomes
#' @param t Treatment assignments
#' @param trim_bounds Trimming bounds for propensity scores
#' @return List with diagnostic measures
evaluate_aipw_diagnostics <- function(ps, mu0, mu1, y, t, trim_bounds) {
  
  # Propensity score diagnostics
  ps_overlap <- mean(ps > trim_bounds[1] & ps < trim_bounds[2])
  ps_extreme <- mean(ps <= trim_bounds[1] | ps >= trim_bounds[2])
  
  # Outcome model diagnostics
  mu_factual <- ifelse(t == 1, mu1, mu0)
  outcome_rmse <- sqrt(mean((y - mu_factual)^2))
  outcome_bias <- mean(y - mu_factual)
  
  # Balance diagnostics (simplified)
  ps_treated_mean <- mean(ps[t == 1])
  ps_control_mean <- mean(ps[t == 0])
  ps_balance <- abs(ps_treated_mean - ps_control_mean)
  
  # Double robustness check
  ps_model_quality <- 1 - ps_extreme  # Higher is better
  outcome_model_quality <- 1 / (1 + outcome_rmse)  # Higher is better
  double_robust_score <- max(ps_model_quality, outcome_model_quality)
  
  return(list(
    ps_overlap = ps_overlap,
    ps_extreme = ps_extreme,
    ps_balance = ps_balance,
    outcome_rmse = outcome_rmse,
    outcome_bias = outcome_bias,
    double_robust_score = double_robust_score,
    ps_summary = summary(ps),
    treatment_prevalence = mean(t)
  ))
}

#' AIPW Sensitivity Analysis
#' 
#' Performs sensitivity analysis for AIPW estimates
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable
#' @param covariate_cols Vector of covariate column names
#' @param methods_grid Grid of method combinations to test
#' @param trim_bounds_grid Grid of trimming bounds to test
#' @return List with sensitivity analysis results
aipw_sensitivity_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                                     methods_grid = NULL, trim_bounds_grid = NULL) {
  
  # Default grids
  if (is.null(methods_grid)) {
    methods_grid <- expand.grid(
      ps_method = c("logit", "rf"),
      outcome_method = c("lm", "rf"),
      stringsAsFactors = FALSE
    )
  }
  
  if (is.null(trim_bounds_grid)) {
    trim_bounds_grid <- list(
      c(0.01, 0.99),
      c(0.05, 0.95),
      c(0.1, 0.9)
    )
  }
  
  results <- list()
  
  cat("Running AIPW sensitivity analysis...\n")
  
  for (i in 1:nrow(methods_grid)) {
    for (j in 1:length(trim_bounds_grid)) {
      
      ps_method <- methods_grid$ps_method[i]
      outcome_method <- methods_grid$outcome_method[i]
      trim_bounds <- trim_bounds_grid[[j]]
      
      cat(sprintf("Testing: PS=%s, Outcome=%s, Trim=[%.2f,%.2f]\n",
                  ps_method, outcome_method, trim_bounds[1], trim_bounds[2]))
      
      tryCatch({
        result <- aipw_stc_analysis(
          data = data,
          outcome_col = outcome_col,
          treatment_col = treatment_col,
          covariate_cols = covariate_cols,
          ps_method = ps_method,
          outcome_method = outcome_method,
          trim_bounds = trim_bounds,
          cross_fit = FALSE  # Faster for sensitivity analysis
        )
        
        results[[length(results) + 1]] <- list(
          ps_method = ps_method,
          outcome_method = outcome_method,
          trim_bounds = trim_bounds,
          ate_estimate = result$ate_estimate,
          ate_se = result$ate_se,
          ps_overlap = result$diagnostics$ps_overlap,
          outcome_rmse = result$diagnostics$outcome_rmse
        )
        
      }, error = function(e) {
        warning(sprintf("Failed for PS=%s, Outcome=%s: %s", 
                       ps_method, outcome_method, e$message))
      })
    }
  }
  
  # Convert to data frame
  results_df <- do.call(rbind, lapply(results, data.frame))
  
  return(list(
    results = results_df,
    summary = list(
      ate_range = range(results_df$ate_estimate),
      ate_sd = sd(results_df$ate_estimate),
      mean_ate = mean(results_df$ate_estimate)
    )
  ))
}

# Message when loading
cat("AIPW with Machine Learning for STC Analysis loaded successfully.\n")
cat("Available functions:\n")
cat("- aipw_stc_analysis(): Main AIPW analysis\n")
cat("- aipw_sensitivity_analysis(): Sensitivity analysis\n")
cat("- fit_propensity_model(): Propensity score modeling\n")
cat("- fit_outcome_models(): Outcome modeling\n")
cat("- evaluate_aipw_diagnostics(): Model diagnostics\n\n") 