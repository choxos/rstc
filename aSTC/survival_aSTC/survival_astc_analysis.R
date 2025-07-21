################################################################################
##################### Survival Anchored STC Analysis Package ##################
################################################################################
# 
# This package provides comprehensive survival anchored STC analysis following
# NICE DSU TSD 18 guidance with integrated pseudo-IPD reconstruction and
# quantitative bias analysis capabilities using the established methodology:
#
# 1. Distribution selection using flexsurv and AIC on anchor arm data
# 2. Pseudo-IPD reconstruction for new comparator only using CurveTracingMod and GuyotAlgorithm
# 3. Data preparation and covariate centering to target population
# 4. Parametric survival regression with flexsurv across multiple covariate models
# 5. Results reporting based on distribution type (PH vs AFT models)
# 6. Professional HTML reporting with TEM-style theming
#
# Main Functions:
# - survival_astc_analysis(): Main anchored STC analysis function
# - determine_best_distribution_astc(): Distribution selection using anchor arm IPD data
# - reconstruct_pseudo_ipd_for_astc(): Pseudo-IPD reconstruction for new comparator only
# - prepare_survival_astc_data(): Data preparation and covariate centering
# - run_survival_astc_models(): Model fitting with multiple covariate sets
# - calculate_survival_astc_results(): Results calculation based on distribution type
#
# Specialized aSTC Functions:
# - check_median_reached(): Determine if median survival is reached for AFT models
# - extract_hazard_ratios_astc(): Extract HR results for PH models
# - calculate_median_differences_astc(): Calculate median survival differences for AFT models
# - calculate_milestone_analysis_astc(): Milestone analysis when median not reached
# - generate_astc_summary(): Generate comprehensive results summary
#
# Validation and Utility Functions:
# - validate_astc_inputs(): Input validation for anchored STC
# - format_astc_results(): Format results for reporting
# - create_astc_diagnostic_plots(): Generate diagnostic and validation plots
#
# HTML Reporting Functions (imported from survival_astc_html_reporting.R):
# - generate_survival_astc_html_report(): Main HTML report generation
# - create_overview_tab_astc(): Study overview and methodology section
# - create_distribution_tab_astc(): Distribution selection results
# - create_pseudo_ipd_tab_astc(): Pseudo-IPD reconstruction and validation
# - create_results_tab_astc(): Survival analysis results and comparisons
#
# Author: Survival Anchored STC Analysis Package
# Version: 1.0
# Last Updated: 2024
# Based on: NICE DSU TSD 18 and established survival uSTC methodology
################################################################################

# Required libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("survival")) install.packages("survival")
if (!require("flexsurv")) install.packages("flexsurv")
if (!require("broom")) install.packages("broom")
if (!require("MASS")) install.packages("MASS")
if (!require("zoo")) install.packages("zoo")
if (!require("dplyr")) install.packages("dplyr")

library(tidyverse)
library(survival)
library(flexsurv)
library(broom)
library(MASS)
library(zoo)
library(dplyr)

# Source required functions
# Get current directory in a way that works both in RStudio and command line
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # Running in RStudio
  current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Running from command line or other environment
  current_dir <- getwd()
}

# Source required HTML reporting and pseudo-IPD functions
source("pseudo_ipd_reconstruction_astc.R")
source("survival_astc_html_reporting.R")

# Define utility operators
`%||%` <- function(x, y) if (is.null(x)) y else x

################################################################################
##################### Utility Functions for aSTC Analysis ####################
################################################################################

#' Format p-values for display with proper rounding
format_p_value <- function(p_value) {
  if (is.null(p_value) || length(p_value) == 0) {
    return("N/A")
  }
  
  # Vectorized approach - handle each element
  result <- sapply(p_value, function(x) {
    if (is.na(x) || !is.numeric(x)) {
      return("N/A")
    }
    
    if (x < 0.001) {
      return("<0.001")
    } else {
      return(sprintf("%.3f", round(x, 3)))
    }
  }, USE.NAMES = FALSE)
  
  return(result)
}

#' Format numeric values with proper rounding (default 2 digits)
format_numeric <- function(value, digits = 2) {
  if (is.null(value) || length(value) == 0) {
    return("N/A")
  }
  
  # Vectorized approach - handle each element
  result <- sapply(value, function(x) {
    if (is.na(x) || !is.numeric(x)) {
      return("N/A")
    }
    return(sprintf(paste0("%.", digits, "f"), round(x, digits)))
  }, USE.NAMES = FALSE)
  
  return(result)
}

################################################################################
##################### Main Survival aSTC Analysis Function ###################
################################################################################

#' Comprehensive Survival Anchored STC Analysis
#'
#' Performs anchored indirect comparison for survival outcomes using IPD from 
#' one trial and aggregate data from another, connected via shared comparator.
#'
#' @param ipd_data Data frame with IPD survival data (time, status, treatment, covariates)
#' @param km_data_comparator Data frame with KM curve for new comparator (time, survival)
#' @param nrisk_data_comparator Data frame with number at risk for new comparator (time, nrisk)
#' @param target_population_means List of covariate means for target population
#' @param time_col Name of time column in IPD data
#' @param status_col Name of status column in IPD data  
#' @param treatment_col Name of treatment column in IPD data
#' @param anchor_arm Name of shared anchor treatment
#' @param ipd_comparator_arm Name of comparator in IPD trial
#' @param new_comparator_arm Name of new comparator from external trial
#' @param covariate_cols Vector of covariate column names
#' @param models Named list of model formulas
#' @param distributions Vector of distributions to consider
#' @param study_name Study identifier
#' @param outcome_name Outcome description
#' @return Comprehensive aSTC analysis results
#' @export
survival_astc_analysis <- function(
  ipd_data,
  km_data_comparator,
  nrisk_data_comparator,
  target_population_means,
  time_col = "time",
  status_col = "status", 
  treatment_col = "treatment",
  anchor_arm = "A",
  ipd_comparator_arm = "B", 
  new_comparator_arm = "C",
  covariate_cols = NULL,
  models = list(
    "Naive" = ~ treatment,
    "Adjusted" = ~ treatment + age_centered
  ),
  distributions = c("weibull", "gamma", "lnorm", "gompertz", "exp", "llogis"),
  study_name = "Survival aSTC Analysis",
  outcome_name = "Overall Survival"
) {
  
  cat("==============================================\n")
  cat("SURVIVAL ANCHORED STC ANALYSIS\n") 
  cat("==============================================\n\n")
  
  # Validate inputs
  validate_astc_inputs(ipd_data, km_data_comparator, nrisk_data_comparator, 
                      target_population_means, time_col, status_col, treatment_col)
  
  # Initialize results structure
  results <- list(
    study_info = list(
      study_name = study_name,
      outcome_name = outcome_name,
      anchor_arm = anchor_arm,
      ipd_comparator_arm = ipd_comparator_arm,
      new_comparator_arm = new_comparator_arm,
      analysis_type = "anchored"
    ),
    data_info = list(
      ipd_sample_size = nrow(ipd_data),
      ipd_treatments = unique(ipd_data[[treatment_col]]),
      target_population = target_population_means,
      covariate_cols = covariate_cols
    )
  )
  
  # Step 1: Distribution Selection using anchor arm
  cat("Step 1: Selecting optimal distribution using anchor arm data...\n")
  anchor_data <- ipd_data[ipd_data[[treatment_col]] == anchor_arm, ]
  
  distribution_results <- determine_best_distribution_astc(
    data = anchor_data,
    time_col = time_col,
    status_col = status_col,
    distributions = distributions
  )
  
  best_distribution <- distribution_results$best_distribution
  results$distribution_selection <- distribution_results
  
  cat("Selected distribution:", best_distribution, "\n\n")
  
  # Step 2: Pseudo-IPD reconstruction for new comparator
  cat("Step 2: Reconstructing pseudo-IPD for new comparator", new_comparator_arm, "...\n")
  
  pseudo_ipd_results <- reconstruct_pseudo_ipd_for_astc(
    km_data = km_data_comparator,
    nrisk_data = nrisk_data_comparator,
    treatment_name = new_comparator_arm,
    covariate_cols = covariate_cols,
    target_population_means = target_population_means
  )
  
  results$pseudo_ipd_reconstruction <- pseudo_ipd_results
  
  cat("Pseudo-IPD reconstruction completed:", nrow(pseudo_ipd_results$pseudo_ipd), "observations\n\n")
  
  # Step 3: Data preparation and centering
  cat("Step 3: Preparing and centering survival data...\n")
  
  prepared_data <- prepare_survival_astc_data(
    ipd_data = ipd_data,
    pseudo_ipd = pseudo_ipd_results$pseudo_ipd,
    target_population_means = target_population_means,
    time_col = time_col,
    status_col = status_col,
    treatment_col = treatment_col,
    covariate_cols = covariate_cols
  )
  
  results$prepared_data <- prepared_data
  
  cat("Data preparation completed:", nrow(prepared_data$merged_data), "total observations\n\n")
  
  # Step 4: Survival aSTC model fitting
  cat("Step 4: Fitting survival aSTC models...\n")
  
  model_results <- run_survival_astc_models(
    data = prepared_data$merged_data,
    distribution = best_distribution,
    models = models,
    time_col = time_col,
    status_col = status_col
  )
  
  results$model_results <- model_results
  
  cat("Model fitting completed:", length(models), "models fitted\n\n")
  
  # Step 5: Calculate survival results
  cat("Step 5: Calculating survival comparison results...\n")
  
  comparison_results <- calculate_survival_astc_results(
    model_results = model_results,
    distribution = best_distribution,
    anchor_arm = anchor_arm,
    ipd_comparator_arm = ipd_comparator_arm,
    new_comparator_arm = new_comparator_arm,
    pseudo_ipd_data = pseudo_ipd_results$pseudo_ipd,
    km_data_comparator = km_data_comparator
  )
  
  results$comparison_results <- comparison_results
  
  cat("Survival comparison analysis completed\n\n")
  
  # Step 6: Generate summary
  results$summary <- generate_astc_summary(results)
  
  cat("==============================================\n")
  cat("SURVIVAL ANCHORED STC ANALYSIS COMPLETED\n")
  cat("==============================================\n\n")
  
  # Set class for S3 methods
  class(results) <- "survival_astc_analysis"
  
  return(results)
}

################################################################################
##################### Distribution Selection for aSTC #########################
################################################################################

#' Determine Best Distribution for Survival aSTC
#'
#' Selects optimal parametric distribution using anchor arm data from IPD trial.
#' This ensures consistency since anchor arm appears in both trials.
#'
#' @param data Survival data (typically anchor arm from IPD trial)
#' @param time_col Name of time column
#' @param status_col Name of status column  
#' @param distributions Vector of distributions to evaluate
#' @return List with best distribution and AIC comparison
determine_best_distribution_astc <- function(
  data, 
  time_col = "time",
  status_col = "status",
  distributions = c("weibull", "gamma", "lnorm", "gompertz", "exp", "llogis")
) {
  
  cat("Evaluating", length(distributions), "distributions using anchor arm data...\n")
  
  # Create survival object
  surv_obj <- Surv(data[[time_col]], data[[status_col]])
  
  # Fit each distribution and calculate AIC
  distribution_fits <- list()
  aic_values <- numeric(length(distributions))
  names(aic_values) <- distributions
  
  for (i in seq_along(distributions)) {
    dist <- distributions[i]
    
    tryCatch({
      if (dist == "gengamma") {
        fit <- flexsurvreg(surv_obj ~ 1, data = data, dist = "gengamma")
      } else if (dist == "genf") {
        fit <- flexsurvreg(surv_obj ~ 1, data = data, dist = "genf")
      } else {
        fit <- flexsurvreg(surv_obj ~ 1, data = data, dist = dist)
      }
      
      distribution_fits[[dist]] <- fit
      aic_values[dist] <- AIC(fit)
      
      cat("  ", dist, ": AIC =", round(aic_values[dist], 2), "\n")
      
    }, error = function(e) {
      cat("  ", dist, ": Failed to fit\n")
      aic_values[dist] <- NA
    })
  }
  
  # Select best distribution (lowest AIC)
  valid_aic <- aic_values[!is.na(aic_values)]
  best_distribution <- names(valid_aic)[which.min(valid_aic)]
  
  # Create comparison table
  aic_table <- data.frame(
    Distribution = names(aic_values),
    AIC = aic_values,
    Delta_AIC = aic_values - min(valid_aic, na.rm = TRUE),
    Selected = names(aic_values) == best_distribution,
    stringsAsFactors = FALSE
  ) %>%
    arrange(AIC)
  
  return(list(
    best_distribution = best_distribution,
    aic_table = aic_table,
    distribution_fits = distribution_fits,
    best_fit = distribution_fits[[best_distribution]]
  ))
}

################################################################################
##################### Data Preparation and Centering for aSTC #################
################################################################################

#' Prepare Survival Data for aSTC Analysis
#'
#' Prepares and centers survival data for anchored STC analysis.
#' Centers covariates on target population means.
#'
#' @param ipd_data Individual patient data
#' @param pseudo_ipd Reconstructed pseudo-IPD for new comparator
#' @param target_population_means List of target population covariate means
#' @param time_col Time column name
#' @param status_col Status column name
#' @param treatment_col Treatment column name
#' @param covariate_cols Covariate column names
#' @return List with prepared data
prepare_survival_astc_data <- function(
  ipd_data,
  pseudo_ipd,
  target_population_means,
  time_col = "time",
  status_col = "status", 
  treatment_col = "treatment",
  covariate_cols = NULL
) {
  
  # Center covariates in IPD data
  ipd_centered <- ipd_data
  centering_values <- list()
  
  if (!is.null(covariate_cols)) {
    for (cov in covariate_cols) {
      if (cov %in% names(target_population_means)) {
        center_val <- target_population_means[[cov]]
        centering_values[[cov]] <- center_val
        
        # Create centered variable
        centered_col <- paste0(cov, "_centered")
        ipd_centered[[centered_col]] <- ipd_centered[[cov]] - center_val
      }
    }
  }
  
  # Pseudo-IPD should already have centered covariates (created at target means)
  # Ensure column names match
  if (!is.null(covariate_cols)) {
    for (cov in covariate_cols) {
      centered_col <- paste0(cov, "_centered")
      if (!centered_col %in% names(pseudo_ipd) && cov %in% names(pseudo_ipd)) {
        pseudo_ipd[[centered_col]] <- pseudo_ipd[[cov]] - target_population_means[[cov]]
      }
    }
  }
  
  # Merge IPD and pseudo-IPD
  merged_data <- rbind(
    ipd_centered[, c(time_col, status_col, treatment_col, 
                    paste0(covariate_cols, "_centered"))],
    pseudo_ipd[, c(time_col, status_col, treatment_col,
                  paste0(covariate_cols, "_centered"))]
  )
  
  # Ensure treatment is factor with correct reference level
  merged_data[[treatment_col]] <- factor(merged_data[[treatment_col]])
  
  return(list(
    merged_data = merged_data,
    ipd_centered = ipd_centered,
    pseudo_ipd = pseudo_ipd,
    centering_values = centering_values
  ))
}

################################################################################
##################### Model Fitting for aSTC Analysis ########################
################################################################################

#' Run Survival aSTC Models
#'
#' Fits multiple parametric survival models with different covariate adjustments.
#'
#' @param data Merged survival data (IPD + pseudo-IPD)
#' @param distribution Selected parametric distribution
#' @param models Named list of model formulas
#' @param time_col Time column name
#' @param status_col Status column name
#' @return List of fitted models with diagnostics
run_survival_astc_models <- function(
  data,
  distribution,
  models,
  time_col = "time",
  status_col = "status"
) {
  
  # Create survival object
  surv_formula_base <- paste0("Surv(", time_col, ", ", status_col, ")")
  
  model_results <- list()
  
  for (model_name in names(models)) {
    cat("  Fitting model:", model_name, "\n")
    
    # Construct full formula
    formula_rhs <- as.character(models[[model_name]])[2]  # Get RHS of formula
    full_formula <- as.formula(paste(surv_formula_base, "~", formula_rhs))
    
    tryCatch({
      # Fit model using flexsurv
      fit <- flexsurvreg(
        formula = full_formula,
        data = data,
        dist = distribution
      )
      
      # Calculate model diagnostics
      diagnostics <- list(
        AIC = AIC(fit),
        log_likelihood = fit$loglik,
        n_parameters = length(fit$coefficients),
        convergence = fit$optim$convergence
      )
      
      model_results[[model_name]] <- list(
        fit = fit,
        formula = full_formula,
        distribution = distribution,
        diagnostics = diagnostics,
        data_summary = list(
          n_obs = nrow(data),
          n_events = sum(data[[status_col]]),
          treatments = table(data$treatment)
        )
      )
      
      cat("    AIC:", round(diagnostics$AIC, 2), 
          " | LogLik:", round(diagnostics$log_likelihood, 2), "\n")
      
    }, error = function(e) {
      cat("    Failed to fit model:", model_name, "\n")
      cat("    Error:", e$message, "\n")
      model_results[[model_name]] <- list(
        fit = NULL,
        error = e$message
      )
    })
  }
  
  return(model_results)
}

################################################################################
##################### Results Calculation Functions ###########################
################################################################################

#' Calculate Survival aSTC Results
#'
#' Calculates treatment comparison results based on distribution type.
#' For PH models: hazard ratios
#' For AFT models: median/milestone differences
#'
#' @param model_results Fitted model results
#' @param distribution Selected distribution
#' @param anchor_arm Anchor treatment name
#' @param ipd_comparator_arm IPD comparator name
#' @param new_comparator_arm New comparator name  
#' @param pseudo_ipd_data Pseudo-IPD data for validation
#' @return Comprehensive results list
calculate_survival_astc_results <- function(
  model_results,
  distribution,
  anchor_arm,
  ipd_comparator_arm, 
  new_comparator_arm,
  pseudo_ipd_data,
  km_data_comparator
) {
  
  # Determine if distribution follows proportional hazards
  ph_distributions <- c("exp", "gompertz")
  is_ph_model <- distribution %in% ph_distributions
  
  results_list <- list()
  
  for (model_name in names(model_results)) {
    model_result <- model_results[[model_name]]
    
    if (is.null(model_result$fit)) {
      results_list[[model_name]] <- list(error = "Model fitting failed")
      next
    }
    
    fit <- model_result$fit
    
    if (is_ph_model) {
      # Proportional hazards: extract hazard ratios directly
      results_list[[model_name]] <- extract_hazard_ratios_astc(
        fit, anchor_arm, ipd_comparator_arm, new_comparator_arm
      )
    } else {
      # AFT models: determine if median is reached
      median_reached <- check_median_reached(km_data_comparator)
      
      if (median_reached) {
        # Calculate median survival differences
        results_list[[model_name]] <- calculate_median_differences_astc(
          fit, anchor_arm, ipd_comparator_arm, new_comparator_arm
        )
      } else {
        # Perform milestone analysis
        # Get reasonable time points (e.g., 25%, 50%, 75% of follow-up)
        max_time <- max(pseudo_ipd_data$time, na.rm = TRUE)
        time_points <- c(max_time * 0.25, max_time * 0.5)
        
        results_list[[model_name]] <- calculate_milestone_analysis_astc(
          fit, anchor_arm, ipd_comparator_arm, new_comparator_arm, time_points
        )
      }
    }
    
    # Add model information
    results_list[[model_name]]$model_info <- list(
      distribution = distribution,
      is_ph_model = is_ph_model,
      analysis_type = if (is_ph_model) "Hazard Ratios" else 
                     if (median_reached) "Median Differences" else "Milestone Analysis"
    )
  }
  
  return(results_list)
}

################################################################################
##################### Helper Functions for Results Calculation ################
################################################################################

#' Extract Hazard Ratios for aSTC (PH Models)
extract_hazard_ratios_astc <- function(fit, anchor_arm, ipd_comparator_arm, new_comparator_arm) {
  
  # Get coefficients and standard errors
  coef_table <- summary(fit)$coef
  
  # Extract treatment effects
  treatment_rows <- grep("treatment", rownames(coef_table))
  
  results <- list()
  
  for (i in treatment_rows) {
    treatment_name <- gsub("treatment", "", rownames(coef_table)[i])
    
    hr <- exp(coef_table[i, "coef"])
    se <- coef_table[i, "se"]
    
    # Calculate CI for HR
    hr_lower <- exp(coef_table[i, "coef"] - 1.96 * se)
    hr_upper <- exp(coef_table[i, "coef"] + 1.96 * se)
    
    results[[treatment_name]] <- list(
      hr = hr,
      hr_lower = hr_lower,
      hr_upper = hr_upper,
      log_hr = coef_table[i, "coef"],
      log_hr_se = se,
      p_value = coef_table[i, "p"]
    )
  }
  
  # Calculate indirect comparison (B vs C via A)
  if (ipd_comparator_arm %in% names(results) && new_comparator_arm %in% names(results)) {
    # HR for B vs C = HR(B vs A) / HR(C vs A)
    log_hr_indirect <- results[[ipd_comparator_arm]]$log_hr - results[[new_comparator_arm]]$log_hr
    log_hr_se_indirect <- sqrt(results[[ipd_comparator_arm]]$log_hr_se^2 + 
                              results[[new_comparator_arm]]$log_hr_se^2)
    
    hr_indirect <- exp(log_hr_indirect)
    hr_indirect_lower <- exp(log_hr_indirect - 1.96 * log_hr_se_indirect)
    hr_indirect_upper <- exp(log_hr_indirect + 1.96 * log_hr_se_indirect)
    
    results$indirect_comparison <- list(
      comparison = paste(ipd_comparator_arm, "vs", new_comparator_arm),
      hr = hr_indirect,
      hr_lower = hr_indirect_lower,
      hr_upper = hr_indirect_upper,
      log_hr = log_hr_indirect,
      log_hr_se = log_hr_se_indirect
    )
  }
  
  return(results)
}

#' Check if Median Survival is Reached
check_median_reached <- function(km_data) {
  # Check if survival drops below 0.5 in KM curve data
  min_survival <- min(km_data$survival, na.rm = TRUE)
  return(min_survival <= 0.5)
}

#' Calculate Median Survival Differences for aSTC (AFT Models)
calculate_median_differences_astc <- function(fit, anchor_arm, ipd_comparator_arm, new_comparator_arm) {
  
  # Predict median survival for each treatment
  treatments <- c(anchor_arm, ipd_comparator_arm, new_comparator_arm)
  
  median_predictions <- list()
  
  for (treatment in treatments) {
    # Create newdata with treatment and all centered covariates at 0 (target population)
    newdata <- data.frame(treatment = treatment)
    
    # Get covariate names from the model - use the values, not the names
    if (!is.null(fit$covdata$covnames)) {
      cov_names <- as.character(fit$covdata$covnames)  # The actual covariate names
      centered_vars <- cov_names[grepl("_centered$", cov_names)]
      
      # Set all centered covariates to 0 (representing target population means)
      for (centered_var in centered_vars) {
        newdata[[centered_var]] <- 0
      }
    }
    
    pred <- predict(fit, 
                   type = "quantile",
                   p = 0.5,
                   se = TRUE,
                   newdata = newdata)
    
    median_predictions[[treatment]] <- list(
      median = pred$fit,
      se = pred$se.fit,
      lower = pred$fit - 1.96 * pred$se.fit,
      upper = pred$fit + 1.96 * pred$se.fit
    )
  }
  
  # Calculate differences
  results <- list(
    median_predictions = median_predictions
  )
  
  # B vs A difference
  if (ipd_comparator_arm %in% names(median_predictions) && anchor_arm %in% names(median_predictions)) {
    diff_ba <- median_predictions[[ipd_comparator_arm]]$median - median_predictions[[anchor_arm]]$median
    se_diff_ba <- sqrt(median_predictions[[ipd_comparator_arm]]$se^2 + median_predictions[[anchor_arm]]$se^2)
    
    results$b_vs_a <- list(
      difference = diff_ba,
      se = se_diff_ba,
      lower = diff_ba - 1.96 * se_diff_ba,
      upper = diff_ba + 1.96 * se_diff_ba
    )
  }
  
  # C vs A difference  
  if (new_comparator_arm %in% names(median_predictions) && anchor_arm %in% names(median_predictions)) {
    diff_ca <- median_predictions[[new_comparator_arm]]$median - median_predictions[[anchor_arm]]$median
    se_diff_ca <- sqrt(median_predictions[[new_comparator_arm]]$se^2 + median_predictions[[anchor_arm]]$se^2)
    
    results$c_vs_a <- list(
      difference = diff_ca,
      se = se_diff_ca,
      lower = diff_ca - 1.96 * se_diff_ca,
      upper = diff_ca + 1.96 * se_diff_ca
    )
  }
  
  # B vs C (indirect)
  if ("b_vs_a" %in% names(results) && "c_vs_a" %in% names(results)) {
    diff_bc <- results$b_vs_a$difference - results$c_vs_a$difference
    se_diff_bc <- sqrt(results$b_vs_a$se^2 + results$c_vs_a$se^2)
    
    results$b_vs_c_indirect <- list(
      difference = diff_bc,
      se = se_diff_bc,
      lower = diff_bc - 1.96 * se_diff_bc,
      upper = diff_bc + 1.96 * se_diff_bc
    )
  }
  
  return(results)
}

#' Calculate Milestone Analysis for aSTC (AFT Models)
calculate_milestone_analysis_astc <- function(fit, anchor_arm, ipd_comparator_arm, new_comparator_arm, time_points) {
  
  treatments <- c(anchor_arm, ipd_comparator_arm, new_comparator_arm)
  
  milestone_predictions <- list()
  
  for (treatment in treatments) {
    # Create newdata with treatment and all centered covariates at 0 (target population)
    newdata <- data.frame(treatment = treatment)
    
    # Get covariate names from the model - use the values, not the names
    if (!is.null(fit$covdata$covnames)) {
      cov_names <- as.character(fit$covdata$covnames)  # The actual covariate names
      centered_vars <- cov_names[grepl("_centered$", cov_names)]
      
      # Set all centered covariates to 0 (representing target population means)
      for (centered_var in centered_vars) {
        newdata[[centered_var]] <- 0
      }
    }
    
    pred <- predict(fit,
                   type = "survival", 
                   t = time_points,
                   newdata = newdata)
    
    milestone_predictions[[treatment]] <- list(
      time_points = time_points,
      survival_probs = pred,
      treatment = treatment
    )
  }
  
  # Calculate survival differences at each time point
  results <- list(
    milestone_predictions = milestone_predictions,
    time_points = time_points
  )
  
  # Add difference calculations for each time point
  for (i in seq_along(time_points)) {
    time_point <- time_points[i]
    
    if (ipd_comparator_arm %in% names(milestone_predictions) && 
        new_comparator_arm %in% names(milestone_predictions)) {
      
      surv_b <- milestone_predictions[[ipd_comparator_arm]]$survival_probs[i]
      surv_c <- milestone_predictions[[new_comparator_arm]]$survival_probs[i]
      
      diff_bc <- surv_b - surv_c
      
      results[[paste0("time_", time_point, "_months")]] <- list(
        time = time_point,
        survival_b = surv_b,
        survival_c = surv_c,
        difference = diff_bc
      )
    }
  }
  
  return(results)
}

################################################################################
##################### Input Validation Function ##############################
################################################################################

#' Validate aSTC Analysis Inputs
validate_astc_inputs <- function(ipd_data, km_data_comparator, nrisk_data_comparator,
                                target_population_means, time_col, status_col, treatment_col) {
  
  # Check IPD data
  if (!is.data.frame(ipd_data)) stop("ipd_data must be a data frame")
  if (nrow(ipd_data) == 0) stop("ipd_data cannot be empty")
  if (!all(c(time_col, status_col, treatment_col) %in% names(ipd_data))) {
    stop("Required columns missing from ipd_data")
  }
  
  # Check KM data
  if (!is.data.frame(km_data_comparator)) stop("km_data_comparator must be a data frame")
  if (!"survival" %in% names(km_data_comparator)) stop("km_data_comparator must have 'survival' column")
  
  # Check number at risk data
  if (!is.data.frame(nrisk_data_comparator)) stop("nrisk_data_comparator must be a data frame")
  if (!"nrisk" %in% names(nrisk_data_comparator)) stop("nrisk_data_comparator must have 'nrisk' column")
  
  # Check target population means
  if (!is.list(target_population_means)) stop("target_population_means must be a list")
  
  cat("Input validation completed successfully\n")
}

################################################################################
##################### Summary Generation Function #############################
################################################################################

#' Generate aSTC Analysis Summary
generate_astc_summary <- function(results) {
  
  best_dist <- results$distribution_selection$best_distribution
  n_models <- length(results$model_results)
  
  # Find primary model (usually most adjusted successful model)
  primary_model <- NULL
  for (model_name in rev(names(results$model_results))) {
    if (!is.null(results$model_results[[model_name]]$fit)) {
      primary_model <- model_name
      break
    }
  }
  
  summary_text <- paste0(
    "Survival Anchored STC Analysis Summary\n",
    "=====================================\n\n",
    "Study: ", results$study_info$study_name, "\n",
    "Outcome: ", results$study_info$outcome_name, "\n",
    "Comparison: ", results$study_info$new_comparator_arm, " vs ", 
    results$study_info$ipd_comparator_arm, " (via anchor ", results$study_info$anchor_arm, ")\n\n",
    "Selected Distribution: ", best_dist, "\n",
    "Models Fitted: ", n_models, "\n",
    "Primary Model: ", primary_model %||% "None successful", "\n\n"
  )
  
  return(summary_text)
}

################################################################################
##################### S3 Methods ##############################################
################################################################################

#' Print Method for Survival aSTC Analysis
#' @export
print.survival_astc_analysis <- function(x, ...) {
  cat(x$summary)
}

#' Summary Method for Survival aSTC Analysis  
#' @export
summary.survival_astc_analysis <- function(object, ...) {
  cat(object$summary)
  
  # Add distribution selection details
  cat("Distribution Selection Results:\n")
  print(object$distribution_selection$aic_table)
  
  # Add model fitting results
  cat("\nModel Fitting Results:\n")
  for (model_name in names(object$model_results)) {
    result <- object$model_results[[model_name]]
    if (!is.null(result$fit)) {
      cat("  ", model_name, ": AIC =", round(result$diagnostics$AIC, 2), "\n")
    } else {
      cat("  ", model_name, ": Failed\n")
    }
  }
} 