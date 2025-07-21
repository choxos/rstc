################################################################################
##################### Multivariate Normal Simulation STC Package ##############
################################################################################
#
# Implementation of Non-Linear Simulated Treatment Comparison using 
# Multivariate Normal Simulation approach described in Ishak et al. (2015)
#
# Reference: "Simulated Treatment Comparison of Time-To-Event (And Other Non-Linear) 
# Outcomes" - Value in Health Journal
# DOI: https://www.valueinhealthjournal.com/article/S1098-3015(15)04799-3/fulltext
#
# Key Innovation:
# Traditional STC methods use population means in regression equations, but for
# non-linear outcomes this introduces bias because f(E[X]) ≠ E[f(X)].
#
# Solution:
# 1. Sample predictor values from multivariate normal distribution 
# 2. Use means from target population and covariance from index trial
# 3. Generate individual predictions and average them
# 4. Provides unbiased estimates for non-linear outcomes
#
# Author: Advanced STC Methods Package  
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("MASS")) install.packages("MASS")
if (!require("mvtnorm")) install.packages("mvtnorm")
if (!require("survival")) install.packages("survival")
if (!require("flexsurv")) install.packages("flexsurv")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

library(MASS)
library(mvtnorm)
library(survival)
library(flexsurv)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

################################################################################
##################### Core Theoretical Framework ###############################
################################################################################

#' Demonstrate Non-Linear Bias Problem
#' 
#' Illustrates why using mean covariate values in non-linear functions 
#' leads to biased estimates
#' 
#' @param n_samples Number of samples to simulate for demonstration
#' @param mean_x Mean of the covariate distribution
#' @param sd_x Standard deviation of the covariate
#' @param beta Coefficient for the covariate in the model
#' @param model_type Type of non-linear model ("exponential", "weibull", "poisson", "logistic")
#' @return List containing bias demonstration results
#' @export
demonstrate_nonlinear_bias <- function(n_samples = 10000,
                                     mean_x = 2.0,
                                     sd_x = 0.5,
                                     beta = 0.3,
                                     model_type = "exponential") {
  
  # Generate covariate values from normal distribution
  x_values <- rnorm(n_samples, mean = mean_x, sd = sd_x)
  
  # Calculate predictions based on model type
  if (model_type == "exponential") {
    # For exponential survival model: hazard = exp(beta * x)
    individual_predictions <- exp(beta * x_values)
    mean_at_mean <- exp(beta * mean_x)
    outcome_label <- "Hazard Rate"
    
  } else if (model_type == "weibull") {
    # For Weibull AFT model: log(T) = -beta * x + error
    individual_predictions <- exp(-beta * x_values)
    mean_at_mean <- exp(-beta * mean_x)
    outcome_label <- "Survival Time"
    
  } else if (model_type == "poisson") {
    # For Poisson regression: rate = exp(beta * x)
    individual_predictions <- exp(beta * x_values)
    mean_at_mean <- exp(beta * mean_x)
    outcome_label <- "Event Rate"
    
  } else if (model_type == "logistic") {
    # For logistic regression: prob = 1/(1 + exp(-beta * x))
    individual_predictions <- 1 / (1 + exp(-beta * x_values))
    mean_at_mean <- 1 / (1 + exp(-beta * mean_x))
    outcome_label <- "Event Probability"
    
  } else {
    stop("Unsupported model type. Use 'exponential', 'weibull', 'poisson', or 'logistic'")
  }
  
  # Calculate the true mean (mean of individual predictions)
  true_mean <- mean(individual_predictions)
  
  # Calculate bias
  bias <- mean_at_mean - true_mean
  relative_bias <- (bias / true_mean) * 100
  
  cat("=== NON-LINEAR BIAS DEMONSTRATION ===\n")
  cat("Model Type:", model_type, "\n")
  cat("Sample Size:", n_samples, "\n")
  cat("Covariate Mean:", mean_x, ", SD:", sd_x, "\n")
  cat("Model Coefficient (beta):", beta, "\n\n")
  
  cat("RESULTS:\n")
  cat("True Mean E[f(X)]:", round(true_mean, 4), "\n")
  cat("Prediction at Mean f(E[X]):", round(mean_at_mean, 4), "\n")
  cat("Absolute Bias:", round(bias, 4), "\n")
  cat("Relative Bias:", round(relative_bias, 2), "%\n\n")
  
  if (abs(relative_bias) > 5) {
    cat("⚠️  SUBSTANTIAL BIAS DETECTED!\n")
    cat("Non-linear STC correction is strongly recommended.\n\n")
  } else if (abs(relative_bias) > 1) {
    cat("⚠️  MODERATE BIAS DETECTED\n")
    cat("Consider non-linear STC for improved accuracy.\n\n")
  } else {
    cat("✅ MINIMAL BIAS DETECTED\n")
    cat("Traditional method may be acceptable.\n\n")
  }
  
  return(list(
    x_values = x_values,
    individual_predictions = individual_predictions,
    true_mean = true_mean,
    mean_at_mean = mean_at_mean,
    bias = bias,
    relative_bias = relative_bias,
    model_type = model_type,
    outcome_label = outcome_label
  ))
}

################################################################################
##################### Covariance Matrix Estimation #############################
################################################################################

#' Estimate Covariance Matrix from Index Trial Data
#' 
#' @param data Data frame containing covariate data from index trial
#' @param covariates Vector of covariate names
#' @param center_continuous Whether to center continuous variables
#' @param centering_values Named vector of centering values (if not provided, uses means)
#' @return List containing covariance matrix and related information
#' @export
estimate_covariance_matrix <- function(data, 
                                     covariates, 
                                     center_continuous = TRUE,
                                     centering_values = NULL) {
  
  # Extract covariate matrix
  X <- data[, covariates, drop = FALSE]
  
  # Remove rows with missing values
  X_complete <- X[complete.cases(X), ]
  
  if (nrow(X_complete) == 0) {
    stop("No complete cases found in covariate data")
  }
  
  # Center continuous variables if requested
  if (center_continuous) {
    if (is.null(centering_values)) {
      centering_values <- colMeans(X_complete)
    }
    
    for (var in names(centering_values)) {
      if (var %in% covariates) {
        X_complete[[var]] <- X_complete[[var]] - centering_values[[var]]
      }
    }
  }
  
  # Calculate covariance matrix
  cov_matrix <- cov(X_complete)
  
  # Calculate correlation matrix for reference
  cor_matrix <- cor(X_complete)
  
  cat("=== COVARIANCE MATRIX ESTIMATION ===\n")
  cat("Original sample size:", nrow(X), "\n")
  cat("Complete cases:", nrow(X_complete), "\n")
  cat("Number of covariates:", length(covariates), "\n")
  
  if (center_continuous) {
    cat("Centering values used:\n")
    print(round(centering_values, 3))
  }
  
  cat("\nCovariance Matrix:\n")
  print(round(cov_matrix, 4))
  
  cat("\nCorrelation Matrix:\n")
  print(round(cor_matrix, 3))
  cat("\n")
  
  return(list(
    covariance_matrix = cov_matrix,
    correlation_matrix = cor_matrix,
    complete_data = X_complete,
    n_complete = nrow(X_complete),
    centering_values = centering_values,
    covariate_names = covariates
  ))
}

################################################################################
##################### Patient Simulation Functions #############################
################################################################################

#' Generate Simulated Patient Profiles
#' 
#' @param n_patients Number of patients to simulate
#' @param target_means Vector of means for target population
#' @param covariance_matrix Covariance matrix from index trial
#' @param covariate_names Names of covariates
#' @param ensure_positive_definite Whether to ensure covariance matrix is positive definite
#' @return Data frame of simulated patient profiles
#' @export
generate_simulated_patients <- function(n_patients,
                                      target_means,
                                      covariance_matrix,
                                      covariate_names,
                                      ensure_positive_definite = TRUE) {
  
  # Validate inputs
  if (length(target_means) != length(covariate_names)) {
    stop("Length of target_means must equal length of covariate_names")
  }
  
  if (nrow(covariance_matrix) != length(covariate_names)) {
    stop("Covariance matrix dimensions must match number of covariates")
  }
  
  # Ensure covariance matrix is positive definite if requested
  if (ensure_positive_definite) {
    eigenvalues <- eigen(covariance_matrix)$values
    min_eigenvalue <- min(eigenvalues)
    
    if (min_eigenvalue <= 0) {
      cat("Warning: Covariance matrix is not positive definite (min eigenvalue:", 
          round(min_eigenvalue, 6), ")\n")
      cat("Adding small regularization term...\n")
      
      # Add small regularization to diagonal
      regularization <- abs(min_eigenvalue) + 1e-6
      covariance_matrix <- covariance_matrix + diag(regularization, nrow(covariance_matrix))
    }
  }
  
  # Generate multivariate normal samples
  tryCatch({
    simulated_profiles <- rmvnorm(
      n = n_patients,
      mean = target_means,
      sigma = covariance_matrix
    )
  }, error = function(e) {
    cat("Error in multivariate normal generation:\n")
    cat(e$message, "\n")
    stop("Failed to generate simulated patients. Check covariance matrix.")
  })
  
  # Convert to data frame
  simulated_df <- as.data.frame(simulated_profiles)
  colnames(simulated_df) <- covariate_names
  
  cat("=== PATIENT SIMULATION RESULTS ===\n")
  cat("Number of patients generated:", n_patients, "\n")
  cat("Covariates:", paste(covariate_names, collapse = ", "), "\n")
  cat("\nTarget vs Achieved Means:\n")
  
  achieved_means <- colMeans(simulated_df)
  comparison_table <- data.frame(
    Covariate = covariate_names,
    Target = round(target_means, 4),
    Achieved = round(achieved_means, 4),
    Difference = round(achieved_means - target_means, 4)
  )
  print(comparison_table)
  
  # Calculate quality metrics
  mean_abs_error <- mean(abs(achieved_means - target_means))
  max_abs_error <- max(abs(achieved_means - target_means))
  
  cat("\nSimulation Quality:\n")
  cat("Mean absolute error:", round(mean_abs_error, 4), "\n")
  cat("Maximum absolute error:", round(max_abs_error, 4), "\n")
  
  if (mean_abs_error < 0.01) {
    cat("✅ Excellent simulation quality\n")
  } else if (mean_abs_error < 0.05) {
    cat("✅ Good simulation quality\n")
  } else {
    cat("⚠️  Consider increasing sample size for better precision\n")
  }
  cat("\n")
  
  return(simulated_df)
}

################################################################################
##################### Non-Linear STC Application ###############################
################################################################################

#' Apply Multivariate Normal Simulation STC Method
#' 
#' @param index_model Fitted regression model from index trial
#' @param target_means Vector of means for target population
#' @param index_covariance_matrix Covariance matrix from index trial
#' @param covariate_names Names of covariates in the model
#' @param n_simulations Number of patient profiles to simulate
#' @param outcome_type Type of outcome ("survival", "poisson", "binary", "linear")
#' @param prediction_type Type of prediction to extract
#' @return List containing comprehensive STC results
#' @export
apply_mvn_simulation_stc <- function(index_model,
                                   target_means,
                                   index_covariance_matrix,
                                   covariate_names,
                                   n_simulations = 10000,
                                   outcome_type = "survival",
                                   prediction_type = "response") {
  
  cat("=== APPLYING MULTIVARIATE NORMAL SIMULATION STC ===\n")
  cat("Outcome type:", outcome_type, "\n")
  cat("Prediction type:", prediction_type, "\n")
  cat("Number of simulations:", n_simulations, "\n\n")
  
  # Generate simulated patient profiles
  simulated_patients <- generate_simulated_patients(
    n_patients = n_simulations,
    target_means = target_means,
    covariance_matrix = index_covariance_matrix,
    covariate_names = covariate_names
  )
  
  # Generate predictions for each simulated patient
  cat("Generating individual predictions...\n")
  
  if (outcome_type == "survival") {
    
    if (inherits(index_model, "flexsurvreg")) {
      # For flexsurv models
      predictions <- predict(index_model, 
                           newdata = simulated_patients,
                           type = prediction_type)
      
      # Handle different flexsurv output formats
      if (is.data.frame(predictions)) {
        predictions <- predictions[[1]]  # Usually first column contains predictions
      }
      
    } else if (inherits(index_model, "coxph")) {
      # For Cox models - predict relative risk or linear predictor
      valid_types <- c("risk", "lp", "expected")
      if (!prediction_type %in% valid_types) {
        warning("Invalid prediction type for Cox model. Using 'risk'")
        prediction_type <- "risk"
      }
      predictions <- predict(index_model, 
                           newdata = simulated_patients,
                           type = prediction_type)
      
    } else if (inherits(index_model, c("survreg", "survreg.penal"))) {
      # For parametric survival models (survreg)
      predictions <- predict(index_model, 
                           newdata = simulated_patients,
                           type = prediction_type)
    } else {
      stop("Unsupported survival model type: ", class(index_model))
    }
    
  } else if (outcome_type == "poisson") {
    
    # For Poisson/GLM models
    predictions <- predict(index_model, 
                         newdata = simulated_patients,
                         type = prediction_type)
    
  } else if (outcome_type == "binary") {
    
    # For logistic regression
    predictions <- predict(index_model, 
                         newdata = simulated_patients,
                         type = prediction_type)
    
  } else if (outcome_type == "linear") {
    
    # For linear regression
    predictions <- predict(index_model, 
                         newdata = simulated_patients,
                         type = prediction_type)
    
  } else {
    stop("Unsupported outcome type: ", outcome_type)
  }
  
  # Calculate traditional (biased) estimate using means
  traditional_newdata <- data.frame(matrix(target_means, nrow = 1))
  colnames(traditional_newdata) <- covariate_names
  
  cat("Calculating traditional estimate at population means...\n")
  
  if (outcome_type == "survival") {
    if (inherits(index_model, "flexsurvreg")) {
      traditional_pred <- predict(index_model, 
                                 newdata = traditional_newdata,
                                 type = prediction_type)
      if (is.data.frame(traditional_pred)) {
        traditional_estimate <- traditional_pred[[1]][1]
      } else {
        traditional_estimate <- traditional_pred[1]
      }
    } else {
      traditional_estimate <- predict(index_model, 
                                    newdata = traditional_newdata,
                                    type = prediction_type)[1]
    }
  } else {
    traditional_estimate <- predict(index_model, 
                                  newdata = traditional_newdata,
                                  type = prediction_type)[1]
  }
  
  # Calculate corrected estimate (mean of individual predictions)
  corrected_estimate <- mean(predictions, na.rm = TRUE)
  
  # Calculate confidence intervals using simulated data
  ci_lower <- quantile(predictions, 0.025, na.rm = TRUE)
  ci_upper <- quantile(predictions, 0.975, na.rm = TRUE)
  
  # Calculate prediction interval (wider than confidence interval)
  pi_lower <- quantile(predictions, 0.05, na.rm = TRUE)
  pi_upper <- quantile(predictions, 0.95, na.rm = TRUE)
  
  # Calculate bias metrics
  bias <- traditional_estimate - corrected_estimate
  relative_bias <- (bias / corrected_estimate) * 100
  
  # Calculate variability metrics
  pred_sd <- sd(predictions, na.rm = TRUE)
  pred_cv <- pred_sd / corrected_estimate * 100
  
  cat("=== SIMULATION RESULTS ===\n")
  cat("Traditional STC (at means):", round(traditional_estimate, 4), "\n")
  cat("MVN Simulation STC (corrected):", round(corrected_estimate, 4), "\n")
  cat("Standard deviation:", round(pred_sd, 4), "\n")
  cat("Coefficient of variation:", round(pred_cv, 1), "%\n")
  cat("95% Confidence interval: (", round(ci_lower, 4), ", ", round(ci_upper, 4), ")\n")
  cat("90% Prediction interval: (", round(pi_lower, 4), ", ", round(pi_upper, 4), ")\n")
  cat("Absolute bias:", round(bias, 4), "\n")
  cat("Relative bias:", round(relative_bias, 2), "%\n\n")
  
  # Bias assessment
  if (abs(relative_bias) > 10) {
    cat("⚠️  SUBSTANTIAL BIAS DETECTED!\n")
    cat("The traditional STC method shows", round(abs(relative_bias), 1), "% bias.\n")
    cat("MVN simulation STC correction is strongly recommended.\n\n")
  } else if (abs(relative_bias) > 2) {
    cat("⚠️  MODERATE BIAS DETECTED\n")
    cat("The traditional STC method shows", round(abs(relative_bias), 1), "% bias.\n")
    cat("Consider MVN simulation STC for improved accuracy.\n\n")
  } else {
    cat("✅ MINIMAL BIAS DETECTED\n")
    cat("The traditional STC method shows", round(abs(relative_bias), 1), "% bias.\n")
    cat("Traditional method may be acceptable, but MVN simulation provides additional precision.\n\n")
  }
  
  return(list(
    estimates = list(
      traditional_estimate = traditional_estimate,
      corrected_estimate = corrected_estimate,
      standard_deviation = pred_sd,
      coefficient_of_variation = pred_cv
    ),
    confidence_intervals = list(
      ci_95_lower = ci_lower,
      ci_95_upper = ci_upper,
      pi_90_lower = pi_lower,
      pi_90_upper = pi_upper
    ),
    bias_metrics = list(
      bias = bias,
      relative_bias = relative_bias
    ),
    simulation_data = list(
      individual_predictions = predictions,
      simulated_patients = simulated_patients,
      target_means = target_means,
      n_simulations = n_simulations
    ),
    model_info = list(
      outcome_type = outcome_type,
      prediction_type = prediction_type,
      model_class = class(index_model)
    )
  ))
}

################################################################################
##################### Clinical Example: Atrial Fibrillation ####################
################################################################################

#' Generate Atrial Fibrillation Bleeding Risk Example
#' 
#' Recreates the clinical example from Ishak et al. for major bleeding
#' risk in atrial fibrillation patients
#' 
#' @param n_patients Number of patients in index trial
#' @param include_interactions Whether to include interaction terms
#' @return List containing example data and fitted model
#' @export
generate_atrial_fib_example <- function(n_patients = 1000, 
                                       include_interactions = FALSE) {
  
  cat("=== GENERATING ATRIAL FIBRILLATION EXAMPLE ===\n")
  cat("Clinical scenario: Major bleeding risk prediction\n")
  cat("Index trial sample size:", n_patients, "\n")
  cat("Include interactions:", include_interactions, "\n\n")
  
  # Set seed for reproducibility
  set.seed(42)
  
  # Generate patient characteristics for index trial (Drug A)
  index_data <- data.frame(
    # Demographics
    age = rnorm(n_patients, mean = 71, sd = 9),
    female = rbinom(n_patients, 1, 0.38),  # 38% female
    
    # Clinical history
    stroke_history = rbinom(n_patients, 1, 0.19),  # 19% stroke/TIA history
    hypertension = rbinom(n_patients, 1, 0.87),    # 87% hypertension
    diabetes = rbinom(n_patients, 1, 0.23),        # 23% diabetes
    renal_dysfunction = rbinom(n_patients, 1, 0.15), # 15% renal dysfunction
    
    # Geographic region
    region_europe = rbinom(n_patients, 1, 0.45),   # 45% European
    
    # Previous treatments
    prior_warfarin = rbinom(n_patients, 1, 0.57),  # 57% prior warfarin
    prior_aspirin = rbinom(n_patients, 1, 0.35)    # 35% prior aspirin
  )
  
  # Ensure age is within reasonable bounds
  index_data$age <- pmax(50, pmin(index_data$age, 95))
  
  # Generate follow-up time (person-years)
  index_data$follow_up_years <- runif(n_patients, min = 0.5, max = 3.0)
  
  # Generate major bleeding events using Poisson regression
  # Model: log(rate) = β₀ + β₁×(age-71) + β₂×female + ... + interactions
  
  linear_predictor <- -4.5 +                                    # Intercept
    0.03 * (index_data$age - 71) +                             # Age effect (centered)
    0.4 * index_data$female +                                  # Female higher risk
    0.3 * index_data$stroke_history +                          # Stroke history
    0.2 * index_data$hypertension +                            # Hypertension
    0.25 * index_data$diabetes +                               # Diabetes
    0.6 * index_data$renal_dysfunction +                       # Renal dysfunction
    -0.1 * index_data$region_europe +                          # Regional difference
    0.15 * index_data$prior_warfarin +                         # Prior warfarin
    0.1 * index_data$prior_aspirin                             # Prior aspirin
  
  # Add interaction terms if requested
  if (include_interactions) {
    linear_predictor <- linear_predictor +
      0.02 * index_data$female * (index_data$age - 71) +       # Age-sex interaction
      0.3 * index_data$diabetes * index_data$renal_dysfunction  # Diabetes-renal interaction
  }
  
  # Generate number of bleeding events
  expected_rate <- exp(linear_predictor) * index_data$follow_up_years
  index_data$bleeding_events <- rpois(n_patients, lambda = expected_rate)
  
  # Fit Poisson regression model to the index trial data
  if (include_interactions) {
    model_formula <- bleeding_events ~ 
      I(age - 71) + female + stroke_history + hypertension + 
      diabetes + renal_dysfunction + region_europe + 
      prior_warfarin + prior_aspirin + 
      female:I(age - 71) + diabetes:renal_dysfunction +
      offset(log(follow_up_years))
  } else {
    model_formula <- bleeding_events ~ 
      I(age - 71) + female + stroke_history + hypertension + 
      diabetes + renal_dysfunction + region_europe + 
      prior_warfarin + prior_aspirin + offset(log(follow_up_years))
  }
  
  poisson_model <- glm(model_formula, data = index_data, family = poisson())
  
  # Define target population characteristics (Drug B population)
  target_characteristics <- list(
    age = 73.2,                    # Slightly older population
    female = 0.42,                 # 42% female (slightly more)
    stroke_history = 0.22,         # 22% stroke history
    hypertension = 0.89,           # 89% hypertension
    diabetes = 0.27,               # 27% diabetes
    renal_dysfunction = 0.18,      # 18% renal dysfunction
    region_europe = 0.40,          # 40% European (less than Drug A)
    prior_warfarin = 0.62,         # 62% prior warfarin
    prior_aspirin = 0.30           # 30% prior aspirin
  )
  
  # Prepare target means (centered age)
  target_means <- c(
    target_characteristics$age - 71,  # Age centered at 71
    target_characteristics$female,
    target_characteristics$stroke_history,
    target_characteristics$hypertension,
    target_characteristics$diabetes,
    target_characteristics$renal_dysfunction,
    target_characteristics$region_europe,
    target_characteristics$prior_warfarin,
    target_characteristics$prior_aspirin
  )
  
  covariate_names <- c("I(age - 71)", "female", "stroke_history", "hypertension",
                      "diabetes", "renal_dysfunction", "region_europe",
                      "prior_warfarin", "prior_aspirin")
  names(target_means) <- covariate_names
  
  # Calculate observed bleeding rate
  observed_rate <- 1000 * sum(index_data$bleeding_events) / sum(index_data$follow_up_years)
  
  cat("Model fitted successfully\n")
  cat("Observed bleeding rate:", round(observed_rate, 1), "events per 1000 person-years\n")
  cat("Model AIC:", round(AIC(poisson_model), 1), "\n\n")
  
  return(list(
    index_data = index_data,
    index_model = poisson_model,
    target_means = target_means,
    target_characteristics = target_characteristics,
    covariate_names = covariate_names,
    observed_rate = observed_rate,
    model_formula = model_formula
  ))
}

################################################################################
##################### Survival Analysis Example ################################
################################################################################

#' Generate Survival Analysis Example for MVN Simulation STC
#' 
#' @param n_patients Number of patients in index trial
#' @param distribution Survival distribution to use ("weibull", "gamma", "lnorm")
#' @return List containing survival analysis example
#' @export
generate_survival_stc_example <- function(n_patients = 500, 
                                         distribution = "weibull") {
  
  cat("=== GENERATING SURVIVAL STC EXAMPLE ===\n")
  cat("Outcome: Overall survival with time-to-event\n")
  cat("Distribution:", distribution, "\n")
  cat("Sample size:", n_patients, "\n\n")
  
  set.seed(123)
  
  # Generate patient characteristics
  survival_data <- data.frame(
    age = rnorm(n_patients, mean = 65, sd = 10),
    performance_status = rbinom(n_patients, 1, 0.7),  # 70% good PS
    stage_advanced = rbinom(n_patients, 1, 0.6),      # 60% advanced stage
    biomarker_high = rbinom(n_patients, 1, 0.4),      # 40% high biomarker
    prior_therapy = rbinom(n_patients, 1, 0.3)        # 30% prior therapy
  )
  
  # Ensure reasonable age bounds
  survival_data$age <- pmax(18, pmin(survival_data$age, 90))
  
  # Generate survival times using specified distribution
  # AFT model: log(T) = β₀ + β₁×(age-65) + β₂×PS + β₃×stage + β₄×biomarker + β₅×prior + ε
  
  linear_predictor <- 4.5 +                                      # Intercept
    -0.02 * (survival_data$age - 65) +                          # Age effect (centered)
    0.4 * survival_data$performance_status +                    # Good PS advantage
    -0.5 * survival_data$stage_advanced +                       # Advanced stage disadvantage
    -0.3 * survival_data$biomarker_high +                       # High biomarker disadvantage
    0.2 * survival_data$prior_therapy                           # Prior therapy advantage
  
  if (distribution == "weibull") {
    # Weibull AFT parameterization
    shape_param <- 1.2
    survival_data$true_time <- rweibull(n_patients,
                                       shape = shape_param,
                                       scale = exp(linear_predictor))
    
    # Fit flexsurv model
    survival_data$time_temp <- survival_data$true_time
    survival_data$status_temp <- rep(1, n_patients)  # No censoring for model fitting
    
    surv_model <- flexsurvreg(Surv(time_temp, status_temp) ~
                             I(age - 65) + performance_status +
                             stage_advanced + biomarker_high + prior_therapy,
                             data = survival_data,
                             dist = "weibull")
    
  } else if (distribution == "gamma") {
    # Gamma AFT parameterization
    shape_param <- 2.0
    survival_data$true_time <- rgamma(n_patients,
                                     shape = shape_param,
                                     scale = exp(linear_predictor) / shape_param)
    
    survival_data$time_temp <- survival_data$true_time
    survival_data$status_temp <- rep(1, n_patients)
    
    surv_model <- flexsurvreg(Surv(time_temp, status_temp) ~
                             I(age - 65) + performance_status +
                             stage_advanced + biomarker_high + prior_therapy,
                             data = survival_data,
                             dist = "gamma")
    
  } else if (distribution == "lnorm") {
    # Log-normal AFT parameterization
    sigma_param <- 0.5
    survival_data$true_time <- rlnorm(n_patients,
                                     meanlog = linear_predictor,
                                     sdlog = sigma_param)
    
    survival_data$time_temp <- survival_data$true_time
    survival_data$status_temp <- rep(1, n_patients)
    
    surv_model <- flexsurvreg(Surv(time_temp, status_temp) ~
                             I(age - 65) + performance_status +
                             stage_advanced + biomarker_high + prior_therapy,
                             data = survival_data,
                             dist = "lnorm")
  } else {
    stop("Unsupported distribution. Use 'weibull', 'gamma', or 'lnorm'")
  }
  
  # Add realistic censoring for final dataset
  censor_time <- runif(n_patients, min = 1, max = 5)
  survival_data$time <- pmin(survival_data$true_time, censor_time)
  survival_data$status <- as.numeric(survival_data$true_time <= censor_time)
  
  # Define target population (different characteristics)
  target_means <- c(
    2,      # Age: 67 years (centered: 67-65=2)
    0.8,    # Performance status: 80% good (vs 70% in index)
    0.7,    # Stage: 70% advanced (vs 60% in index)
    0.5,    # Biomarker: 50% high (vs 40% in index)
    0.4     # Prior therapy: 40% (vs 30% in index)
  )
  
  covariate_names <- c("I(age - 65)", "performance_status", "stage_advanced",
                      "biomarker_high", "prior_therapy")
  names(target_means) <- covariate_names
  
  cat("Model fitted successfully\n")
  cat("Event rate:", round(100 * mean(survival_data$status), 1), "%\n")
  cat("Median survival:", round(median(survival_data$time), 2), "years\n")
  cat("Model AIC:", round(AIC(surv_model), 1), "\n\n")
  
  return(list(
    survival_data = survival_data,
    survival_model = surv_model,
    target_means = target_means,
    covariate_names = covariate_names,
    distribution = distribution
  ))
}

################################################################################
##################### Comprehensive Analysis Function #########################
################################################################################

#' Run Complete Multivariate Normal Simulation STC Analysis
#' 
#' @param analysis_type Type of analysis ("atrial_fib", "survival", "custom")
#' @param custom_data Custom data for analysis (if analysis_type = "custom")
#' @param n_simulations Number of patient simulations
#' @param survival_distribution Distribution for survival analysis
#' @return Comprehensive analysis results
#' @export
run_complete_mvn_stc_analysis <- function(analysis_type = "atrial_fib",
                                         custom_data = NULL,
                                         n_simulations = 10000,
                                         survival_distribution = "weibull") {
  
  cat("################################################################################\n")
  cat("##################### COMPREHENSIVE MVN SIMULATION STC ANALYSIS ##############\n")
  cat("################################################################################\n\n")
  
  # Step 1: Generate or use example data
  if (analysis_type == "atrial_fib") {
    
    cat("Step 1: Generating atrial fibrillation bleeding risk example...\n")
    example_data <- generate_atrial_fib_example(n_patients = 1000)
    outcome_type <- "poisson"
    prediction_type <- "response"
    
  } else if (analysis_type == "survival") {
    
    cat("Step 1: Generating survival analysis example...\n")
    example_data <- generate_survival_stc_example(n_patients = 500, 
                                                 distribution = survival_distribution)
    outcome_type <- "survival"
    prediction_type <- "response"
    
  } else if (analysis_type == "custom") {
    
    if (is.null(custom_data)) {
      stop("custom_data must be provided when analysis_type = 'custom'")
    }
    example_data <- custom_data
    outcome_type <- custom_data$outcome_type
    prediction_type <- custom_data$prediction_type
    
  } else {
    stop("analysis_type must be 'atrial_fib', 'survival', or 'custom'")
  }
  
  # Step 2: Estimate covariance matrix from index trial
  cat("Step 2: Estimating covariance matrix from index trial...\n")
  
  if (analysis_type == "atrial_fib") {
    cov_data <- example_data$index_data[, c("age", "female", "stroke_history",
                                           "hypertension", "diabetes", "renal_dysfunction",
                                           "region_europe", "prior_warfarin", "prior_aspirin")]
    # Center age
    cov_data$age <- cov_data$age - 71
    cov_covariate_names <- c("age", "female", "stroke_history", "hypertension",
                            "diabetes", "renal_dysfunction", "region_europe",
                            "prior_warfarin", "prior_aspirin")
  } else {
    cov_data <- example_data$survival_data[, c("age", "performance_status", "stage_advanced",
                                              "biomarker_high", "prior_therapy")]
    # Center age
    cov_data$age <- cov_data$age - 65
    cov_covariate_names <- c("age", "performance_status", "stage_advanced",
                            "biomarker_high", "prior_therapy")
  }
  
  cov_results <- estimate_covariance_matrix(
    data = cov_data,
    covariates = cov_covariate_names,
    center_continuous = FALSE  # Already centered age above
  )
  
  # Step 3: Demonstrate bias problem
  cat("Step 3: Demonstrating non-linear bias problem...\n")
  bias_demo <- demonstrate_nonlinear_bias(
    n_samples = 10000,
    mean_x = 2,
    sd_x = 0.5,
    beta = 0.3,
    model_type = if (analysis_type == "survival") "weibull" else "poisson"
  )
  
  # Step 4: Apply MVN simulation STC method
  cat("Step 4: Applying multivariate normal simulation STC...\n")
  stc_results <- apply_mvn_simulation_stc(
    index_model = if (analysis_type == "atrial_fib") example_data$index_model else example_data$survival_model,
    target_means = example_data$target_means,
    index_covariance_matrix = cov_results$covariance_matrix,
    covariate_names = example_data$covariate_names,
    n_simulations = n_simulations,
    outcome_type = outcome_type,
    prediction_type = prediction_type
  )
  
  # Step 5: Compile comprehensive results
  results <- list(
    analysis_type = analysis_type,
    example_data = example_data,
    covariance_results = cov_results,
    bias_demonstration = bias_demo,
    stc_results = stc_results,
    analysis_info = list(
      n_simulations = n_simulations,
      analysis_date = Sys.time(),
      outcome_type = outcome_type,
      prediction_type = prediction_type
    )
  )
  
  cat("=== ANALYSIS COMPLETE ===\n")
  cat("Analysis type:", analysis_type, "\n")
  cat("Number of simulations:", n_simulations, "\n")
  cat("Bias correction achieved:", round(abs(stc_results$bias_metrics$relative_bias), 2), "%\n\n")
  
  return(results)
} 