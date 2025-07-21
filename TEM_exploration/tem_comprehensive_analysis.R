# Comprehensive TEM (Treatment Effect Modifier) Analysis Function
# Author: Unanchored STC Analysis Package
# Enhanced with modern HTML reporting and comprehensive outcome analysis

library(survival)
library(ggplot2)
library(dplyr)
library(officer)
library(flextable)
library(htmltools)
library(knitr)
library(rmarkdown)
library(DT)
library(plotly)
library(rlang)
library(scales)

#' Comprehensive TEM Analysis with Enhanced HTML Reporting
#' 
#' This function performs treatment effect modifier analysis for multiple outcomes
#' with modern HTML reporting, tabbed interface, and comprehensive documentation.
#' 
#' @param data A data frame containing the analysis dataset
#' @param outcomes A named list of outcomes where names are outcome labels and values are variable names
#' @param time_vars Named vector of time variables for survival outcomes (if any)
#' @param event_vars Named vector of event variables for survival outcomes (if any)
#' @param covariates Character vector of covariate names to include in analysis
#' @param univariate_alpha Significance level for univariate analysis (default: 0.2)
#' @param multivariate_alpha Significance level for multivariate analysis (default: 0.2)
#' @param selection_method Variable selection method ("backward", "forward", "both", "none")
#' @param report_title Custom title for the HTML report
#' @param output_dir Directory to save the report (will create timestamped subfolder)
#' @param include_plots Whether to include survival curves and other plots
#' @return List containing analysis results and report file path
tem_comprehensive_analysis <- function(data, 
                                     outcomes, 
                                     time_vars = NULL, 
                                     event_vars = NULL,
                                     covariates, 
                                     univariate_alpha = 0.2, 
                                     multivariate_alpha = 0.2,
                                     selection_method = "backward",
                                     report_title = "Comprehensive TEM Analysis",
                                     output_dir = "reports",
                                     include_plots = TRUE) {
  
  # Start timing
  start_time <- Sys.time()
  
  # Create date-only output directory (consolidate daily reports)
  date_stamp <- format(Sys.time(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Keep full timestamp for filenames
  report_dir <- file.path(output_dir, paste0("TEM_report_", date_stamp))
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Validate inputs
  if (missing(data) || missing(outcomes) || missing(covariates)) {
    stop("data, outcomes, and covariates are required parameters")
  }
  
  # Initialize results storage
  comprehensive_results <- list()
  
  # Perform analysis for each outcome
  cat("Starting comprehensive TEM analysis for", length(outcomes), "outcomes...\n")
  
  # Store results for each outcome
  outcome_results <- list()
  univariate_table <- data.frame()
  multivariate_table <- data.frame()
  
  for (i in seq_along(outcomes)) {
    outcome_name <- names(outcomes)[i]
    outcome_var <- outcomes[[i]]
    
    cat("Analyzing outcome:", outcome_name, "...\n")
    
    # Determine outcome type
    outcome_type <- determine_outcome_type(data, outcome_name, outcome_var, time_vars, event_vars)
    
    # Perform analysis
    outcome_analysis <- analyze_single_outcome(
      data = data,
      outcome_name = outcome_name,
      outcome_var = outcome_var,
      outcome_type = outcome_type,
      time_var = if (outcome_type == "survival") unname(time_vars[outcome_name]) else NULL,
      event_var = if (outcome_type == "survival") unname(event_vars[outcome_name]) else NULL,
      covariates = covariates,
      univariate_alpha = univariate_alpha,
      multivariate_alpha = multivariate_alpha,
      selection_method = selection_method,
      include_plots = include_plots
    )
    
    # Store results
    outcome_results[[outcome_name]] <- outcome_analysis
    
    # Add to combined tables
    if (nrow(outcome_analysis$univariate_table) > 0) {
      univariate_table <- rbind(univariate_table, outcome_analysis$univariate_table)
    }
    
    if (nrow(outcome_analysis$multivariate_table) > 0) {
      multivariate_table <- rbind(multivariate_table, outcome_analysis$multivariate_table)
    }
  }
  
  # Compile comprehensive results
  comprehensive_results <- list(
    outcomes = outcome_results,
    univariate_table = univariate_table,
    multivariate_table = multivariate_table,
    parameters = list(
      univariate_alpha = univariate_alpha,
      multivariate_alpha = multivariate_alpha,
      selection_method = selection_method,
      n_outcomes = length(outcomes),
      n_covariates = length(covariates)
    ),
    original_data = data,
    time_vars = time_vars,
    event_vars = event_vars,
    metadata = list(
      analysis_date = Sys.Date(),
      analysis_time = Sys.time(),
      package_version = "1.0.0",
      report_dir = report_dir
    )
  )
  
  # Generate HTML report
  cat("Generating comprehensive HTML report...\n")
  report_file <- generate_tem_html_report(
    results = comprehensive_results,
    report_title = report_title,
    output_dir = report_dir,
    timestamp = timestamp
  )
  
  # Calculate runtime
  end_time <- Sys.time()
  runtime <- end_time - start_time
  
  cat("Analysis completed successfully!\n")
  cat("Runtime:", sprintf("%.2f", as.numeric(runtime)), attr(runtime, "units"), "\n")
  cat("Report saved to:", report_file, "\n")
  
  # Return results
  return(list(
    results = comprehensive_results,
    report_file = report_file,
    runtime = runtime
  ))
}

#' Determine outcome type based on data characteristics
determine_outcome_type <- function(data, outcome_name, outcome_var, time_vars, event_vars) {
  if (outcome_name %in% names(time_vars) && outcome_name %in% names(event_vars)) {
    return("survival")
  } else if (is.factor(data[[outcome_var]]) || length(unique(data[[outcome_var]])) == 2) {
    return("binary")
  } else if (all(data[[outcome_var]] %% 1 == 0, na.rm = TRUE) && min(data[[outcome_var]], na.rm = TRUE) >= 0) {
    return("count")
  } else {
    return("continuous")
  }
}

#' Analyze single outcome with comprehensive reporting
analyze_single_outcome <- function(data, outcome_name, outcome_var, outcome_type, 
                                 time_var, event_var, covariates, 
                                 univariate_alpha, multivariate_alpha, 
                                 selection_method, include_plots) {
  
  # Perform univariate analysis
  univariate_results <- perform_univariate_analysis(
    data, outcome_var, outcome_type, time_var, event_var, covariates, univariate_alpha
  )
  
  # Perform multivariate analysis
  multivariate_results <- perform_multivariate_analysis(
    data, outcome_var, outcome_type, time_var, event_var, 
    univariate_results$significant_vars, multivariate_alpha, selection_method
  )
  
  # Generate plots if requested
  plots <- NULL
  if (include_plots) {
    plots <- generate_outcome_plots(data, outcome_var, outcome_type, time_var, event_var)
  }
  
  # Compile outcome results
  return(list(
    outcome_name = outcome_name,
    outcome_var = outcome_var,
    outcome_type = outcome_type,
    univariate_results = univariate_results,
    multivariate_results = multivariate_results,
    univariate_table = build_univariate_table(univariate_results, outcome_name, data),
    multivariate_table = build_multivariate_table(multivariate_results, outcome_name, multivariate_alpha, data),
    plots = plots,
    summary_stats = calculate_outcome_summary_stats(data, outcome_var, outcome_type)
  ))
}

#' Perform univariate analysis for all covariates
perform_univariate_analysis <- function(data, outcome_var, outcome_type, time_var, event_var, covariates, alpha) {
  
  results <- list()
  significant_vars <- character()
  
  for (covariate in covariates) {
    # Skip if covariate has too many missing values
    if (sum(is.na(data[[covariate]])) > nrow(data) * 0.5) {
      next
    }
    
    # Fit univariate model
    model_result <- fit_univariate_model(data, outcome_var, outcome_type, time_var, event_var, covariate)
    
    if (!is.null(model_result) && !is.null(model_result$p_value)) {
      results[[covariate]] <- model_result
      
      # Check significance
      if (model_result$p_value <= alpha) {
        significant_vars <- c(significant_vars, covariate)
      }
    }
  }
  
  return(list(
    results = results,
    significant_vars = significant_vars,
    alpha_used = alpha
  ))
}

#' Fit univariate model based on outcome type
fit_univariate_model <- function(data, outcome_var, outcome_type, time_var, event_var, covariate) {
  
  tryCatch({
    if (outcome_type == "survival") {
      formula_str <- paste("Surv(", time_var, ",", event_var, ") ~", covariate)
      model <- coxph(as.formula(formula_str), data = data)
      
      coef_summary <- summary(model)$coefficients
      if (nrow(coef_summary) > 0) {
        return(list(
          model = model,
          coefficient = coef_summary[1, "coef"],
          se = coef_summary[1, "se(coef)"],
          hr = exp(coef_summary[1, "coef"]),
          hr_lci = exp(coef_summary[1, "coef"] - 1.96 * coef_summary[1, "se(coef)"]),
          hr_uci = exp(coef_summary[1, "coef"] + 1.96 * coef_summary[1, "se(coef)"]),
          p_value = coef_summary[1, "Pr(>|z|)"],
          n_events = model$nevent,
          n_total = model$n
        ))
      }
      
    } else if (outcome_type == "binary") {
      formula_str <- paste(outcome_var, "~", covariate)
      model <- glm(as.formula(formula_str), data = data, family = binomial())
      
      coef_summary <- summary(model)$coefficients
      if (nrow(coef_summary) >= 2) {
        return(list(
          model = model,
          coefficient = coef_summary[2, "Estimate"],
          se = coef_summary[2, "Std. Error"],
          or = exp(coef_summary[2, "Estimate"]),
          or_lci = exp(coef_summary[2, "Estimate"] - 1.96 * coef_summary[2, "Std. Error"]),
          or_uci = exp(coef_summary[2, "Estimate"] + 1.96 * coef_summary[2, "Std. Error"]),
          p_value = coef_summary[2, "Pr(>|z|)"],
          n_total = nrow(model$model)
        ))
      }
      
    } else if (outcome_type == "count") {
      formula_str <- paste(outcome_var, "~", covariate)
      model <- glm(as.formula(formula_str), data = data, family = poisson())
      
      coef_summary <- summary(model)$coefficients
      if (nrow(coef_summary) >= 2) {
        return(list(
          model = model,
          coefficient = coef_summary[2, "Estimate"],
          se = coef_summary[2, "Std. Error"],
          rr = exp(coef_summary[2, "Estimate"]),
          rr_lci = exp(coef_summary[2, "Estimate"] - 1.96 * coef_summary[2, "Std. Error"]),
          rr_uci = exp(coef_summary[2, "Estimate"] + 1.96 * coef_summary[2, "Std. Error"]),
          p_value = coef_summary[2, "Pr(>|z|)"],
          n_total = nrow(model$model)
        ))
      }
      
    } else { # continuous
      formula_str <- paste(outcome_var, "~", covariate)
      model <- lm(as.formula(formula_str), data = data)
      
      coef_summary <- summary(model)$coefficients
      if (nrow(coef_summary) >= 2) {
        return(list(
          model = model,
          coefficient = coef_summary[2, "Estimate"],
          se = coef_summary[2, "Std. Error"],
          t_value = coef_summary[2, "t value"],
          p_value = coef_summary[2, "Pr(>|t|)"],
          r_squared = summary(model)$r.squared,
          n_total = nrow(model$model)
        ))
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    warning(paste("Error fitting model for", covariate, ":", e$message))
    return(NULL)
  })
}

#' Perform multivariate analysis with variable selection
perform_multivariate_analysis <- function(data, outcome_var, outcome_type, time_var, event_var, 
                                        significant_vars, alpha, selection_method) {
  
  if (length(significant_vars) == 0) {
    return(list(
      final_model = NULL,
      selection_details = NULL,
      has_multivariate = FALSE
    ))
  }
  
  # Build initial formula
  if (outcome_type == "survival") {
    formula_str <- paste("Surv(", time_var, ",", event_var, ") ~", paste(significant_vars, collapse = " + "))
  } else {
    formula_str <- paste(outcome_var, "~", paste(significant_vars, collapse = " + "))
  }
  
  # Fit initial model
  initial_model <- fit_multivariate_model(formula_str, data, outcome_type)
  
  if (is.null(initial_model)) {
    return(list(
      final_model = NULL,
      selection_details = NULL,
      has_multivariate = FALSE
    ))
  }
  
  # Perform variable selection
  selection_results <- perform_variable_selection(initial_model, data, outcome_type, alpha, selection_method)
  
  return(list(
    final_model = selection_results$final_model,
    selection_steps = selection_results$selection_steps,
    selection_details = selection_results,
    has_multivariate = TRUE,
    initial_variables = significant_vars
  ))
}

#' Fit multivariate model based on outcome type
fit_multivariate_model <- function(formula_str, data, outcome_type) {
  
  tryCatch({
    if (outcome_type == "survival") {
      return(coxph(as.formula(formula_str), data = data))
    } else if (outcome_type == "binary") {
      return(glm(as.formula(formula_str), data = data, family = binomial()))
    } else if (outcome_type == "count") {
      return(glm(as.formula(formula_str), data = data, family = poisson()))
    } else {
      return(lm(as.formula(formula_str), data = data))
    }
  }, error = function(e) {
    warning(paste("Error fitting multivariate model:", e$message))
    return(NULL)
  })
}

#' Perform variable selection
perform_variable_selection <- function(full_model, data, outcome_type, alpha, selection_method) {
  
  selection_steps <- list()
  step_counter <- 1
  current_model <- full_model
  
  # Initial model state
  selection_steps[[step_counter]] <- list(
    step = "Initial Full Model",
    action = "Starting with all univariate-significant variables.",
    variable = NA,
    p_value = NA,
    aic = AIC(current_model),
    formula = formula(current_model),
    regression_table = extract_full_regression_table(current_model, outcome_type, data)
  )
  
  # Perform backward selection
  if (selection_method == "backward" || selection_method == "both") {
    while (TRUE) {
      step_counter <- step_counter + 1
      
      # Determine test type
      test_type <- if (outcome_type %in% c("binary", "count", "survival")) "Chisq" else "F"
      
      # Get drop-one statistics
      drop1_results <- drop1(current_model, test = test_type)
      p_values <- drop1_results$`Pr(>Chi)`
      if (is.null(p_values)) p_values <- drop1_results$`Pr(>F)`
      
      # Find variable to drop (highest p-value > alpha)
      variable_to_drop <- NA
      max_p_value <- -1
      
      # Check if any variables have p-values > alpha
      if (any(!is.na(p_values) & p_values > alpha)) {
        # Find the variable with the highest p-value that exceeds alpha
        eligible_indices <- which(!is.na(p_values) & p_values > alpha)
        if (length(eligible_indices) > 0) {
          max_p_index <- eligible_indices[which.max(p_values[eligible_indices])]
          variable_to_drop <- rownames(drop1_results)[max_p_index]
          max_p_value <- p_values[max_p_index]
        }
      }
      
      # Break if no variable to drop
      if (is.na(variable_to_drop)) {
        selection_steps[[step_counter]] <- list(
          step = "Final Model",
          action = paste("All variables entered the final model without requiring backward elimination (all p-values ≤ α =", alpha, ")."),
          variable = NA,
          p_value = NA,
          aic = AIC(current_model),
          formula = formula(current_model),
          regression_table = extract_full_regression_table(current_model, outcome_type, data)
        )
        break
      }
      
      # Update model by dropping variable
      new_formula <- update_formula_drop_variable(formula(current_model), variable_to_drop)
      updated_model <- fit_multivariate_model(deparse(new_formula), data, outcome_type)
      
      # Record step
      selection_steps[[step_counter]] <- list(
        step = paste("Removed", variable_to_drop),
        action = paste("Removed", variable_to_drop, "(p-value =", round(max_p_value, 4), ")"),
        variable = variable_to_drop,
        p_value = max_p_value,
        aic = AIC(updated_model),
        formula = new_formula,
        regression_table = extract_full_regression_table(updated_model, outcome_type, data)
      )
      
      current_model <- updated_model
    }
  }
  
  return(list(
    final_model = current_model,
    selection_steps = selection_steps,
    method_used = selection_method,
    final_variables = names(coef(current_model))[-1] # Exclude intercept
  ))
}

#' Extract comprehensive regression table
extract_full_regression_table <- function(model, outcome_type, data = NULL) {
  
  tryCatch({
    if (outcome_type == "survival") {
      coef_summary <- summary(model)$coefficients
      reg_table <- data.frame(
        Variable = rownames(coef_summary),
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary), function(x) determine_variable_type(data, x)) else "Unknown",
        HR = round(exp(coef_summary[, "coef"]), 3),
        HR_LCI = round(exp(coef_summary[, "coef"] - 1.96 * coef_summary[, "se(coef)"]), 3),
        HR_UCI = round(exp(coef_summary[, "coef"] + 1.96 * coef_summary[, "se(coef)"]), 3),
        P_value = round(coef_summary[, "Pr(>|z|)"], 6),
        stringsAsFactors = FALSE
      )
      reg_table$HR_CI <- paste0(reg_table$HR, " (", reg_table$HR_LCI, "-", reg_table$HR_UCI, ")")
      
    } else if (outcome_type == "binary") {
      coef_summary <- summary(model)$coefficients
      reg_table <- data.frame(
        Variable = rownames(coef_summary),
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary), function(x) determine_variable_type(data, x)) else "Unknown",
        OR = round(exp(coef_summary[, "Estimate"]), 3),
        OR_LCI = round(exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"]), 3),
        OR_UCI = round(exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"]), 3),
        P_value = round(coef_summary[, "Pr(>|z|)"], 6),
        stringsAsFactors = FALSE
      )
      reg_table$OR_CI <- paste0(reg_table$OR, " (", reg_table$OR_LCI, "-", reg_table$OR_UCI, ")")
      
    } else if (outcome_type == "count") {
      coef_summary <- summary(model)$coefficients
      reg_table <- data.frame(
        Variable = rownames(coef_summary),
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary), function(x) determine_variable_type(data, x)) else "Unknown",
        RR = round(exp(coef_summary[, "Estimate"]), 3),
        RR_LCI = round(exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"]), 3),
        RR_UCI = round(exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"]), 3),
        P_value = round(coef_summary[, "Pr(>|z|)"], 6),
        stringsAsFactors = FALSE
      )
      reg_table$RR_CI <- paste0(reg_table$RR, " (", reg_table$RR_LCI, "-", reg_table$RR_UCI, ")")
      
    } else { # continuous
      coef_summary <- summary(model)$coefficients
      reg_table <- data.frame(
        Variable = rownames(coef_summary),
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary), function(x) determine_variable_type(data, x)) else "Unknown",
        Coefficient = round(coef_summary[, "Estimate"], 3),
        Coeff_LCI = round(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"], 3),
        Coeff_UCI = round(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"], 3),
        P_value = round(coef_summary[, "Pr(>|t|)"], 6),
        stringsAsFactors = FALSE
      )
      reg_table$Coeff_CI <- paste0(reg_table$Coefficient, " (", reg_table$Coeff_LCI, "-", reg_table$Coeff_UCI, ")")
    }
    
    reg_table$Significant <- ifelse(reg_table$P_value < 0.05, "✓", "✗")
    return(reg_table)
    
  }, error = function(e) {
    return(data.frame(Error = paste("Could not extract regression table:", e$message)))
  })
}

#' Build univariate summary table
build_univariate_table <- function(univariate_results, outcome_name, data = NULL) {
  
  table_data <- data.frame()
  
  for (var_name in names(univariate_results$results)) {
    result <- univariate_results$results[[var_name]]
    
    if (!is.null(result)) {
      # Determine effect measure based on available data
      if (!is.null(result$hr)) {
        effect_estimate <- sprintf("%.3f (%.3f-%.3f)", result$hr, result$hr_lci, result$hr_uci)
        effect_type <- "HR"
      } else if (!is.null(result$or)) {
        effect_estimate <- sprintf("%.3f (%.3f-%.3f)", result$or, result$or_lci, result$or_uci)
        effect_type <- "OR"
      } else if (!is.null(result$rr)) {
        effect_estimate <- sprintf("%.3f (%.3f-%.3f)", result$rr, result$rr_lci, result$rr_uci)
        effect_type <- "RR"
      } else {
        effect_estimate <- sprintf("%.3f", result$coefficient)
        effect_type <- "Coeff"
      }
      
      table_data <- rbind(table_data, data.frame(
        Outcome = outcome_name,
        Variable = var_name,
        Variable_Type = if(!is.null(data)) determine_variable_type(data, var_name) else "Unknown",
        Effect_Type = effect_type,
        Effect_Estimate_CI = effect_estimate,
        P_value = sprintf("%.6f", result$p_value),
        Significant = ifelse(result$p_value <= univariate_results$alpha_used, "✓", "✗"),
        N = result$n_total,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(table_data)
}

#' Build multivariate summary table
build_multivariate_table <- function(multivariate_results, outcome_name, alpha = 0.05, data = NULL) {
  
  if (!multivariate_results$has_multivariate || is.null(multivariate_results$final_model)) {
    return(data.frame())
  }
  
  # Extract final model coefficients
  model <- multivariate_results$final_model
  
  if (inherits(model, "coxph")) {
    coef_summary <- summary(model)$coefficients
    effect_type <- "HR"
    
    table_data <- data.frame(
      Outcome = outcome_name,
      Variable = rownames(coef_summary),
      Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary), function(x) determine_variable_type(data, x)) else "Unknown",
      Effect_Type = effect_type,
      Effect_Estimate_CI = sprintf("%.3f (%.3f-%.3f)", 
                                  exp(coef_summary[, "coef"]),
                                  exp(coef_summary[, "coef"] - 1.96 * coef_summary[, "se(coef)"]),
                                  exp(coef_summary[, "coef"] + 1.96 * coef_summary[, "se(coef)"])),
      P_value = sprintf("%.6f", coef_summary[, "Pr(>|z|)"]),
      Significant = ifelse(coef_summary[, "Pr(>|z|)"] <= alpha, "✓", "✗"),
      stringsAsFactors = FALSE
    )
    
  } else if (inherits(model, "glm")) {
    coef_summary <- summary(model)$coefficients
    
    if (model$family$family == "binomial") {
      effect_type <- "OR"
    } else if (model$family$family == "poisson") {
      effect_type <- "RR"
    } else {
      effect_type <- "Coeff"
    }
    
    if (effect_type %in% c("OR", "RR")) {
      table_data <- data.frame(
        Outcome = outcome_name,
        Variable = rownames(coef_summary)[-1], # Skip intercept
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary)[-1], function(x) determine_variable_type(data, x)) else "Unknown",
        Effect_Type = effect_type,
        Effect_Estimate_CI = sprintf("%.3f (%.3f-%.3f)", 
                                    exp(coef_summary[-1, "Estimate"]),
                                    exp(coef_summary[-1, "Estimate"] - 1.96 * coef_summary[-1, "Std. Error"]),
                                    exp(coef_summary[-1, "Estimate"] + 1.96 * coef_summary[-1, "Std. Error"])),
        P_value = sprintf("%.6f", coef_summary[-1, "Pr(>|z|)"]),
        Significant = ifelse(coef_summary[-1, "Pr(>|z|)"] <= alpha, "✓", "✗"),
        stringsAsFactors = FALSE
      )
    } else {
      table_data <- data.frame(
        Outcome = outcome_name,
        Variable = rownames(coef_summary)[-1], # Skip intercept
        Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary)[-1], function(x) determine_variable_type(data, x)) else "Unknown",
        Effect_Type = effect_type,
        Effect_Estimate_CI = sprintf("%.3f", coef_summary[-1, "Estimate"]),
        P_value = sprintf("%.6f", coef_summary[-1, "Pr(>|z|)"]),
        Significant = ifelse(coef_summary[-1, "Pr(>|z|)"] <= alpha, "✓", "✗"),
        stringsAsFactors = FALSE
      )
    }
    
  } else { # lm
    coef_summary <- summary(model)$coefficients
    effect_type <- "Coeff"
    
    table_data <- data.frame(
      Outcome = outcome_name,
      Variable = rownames(coef_summary)[-1], # Skip intercept
      Variable_Type = if(!is.null(data)) sapply(rownames(coef_summary)[-1], function(x) determine_variable_type(data, x)) else "Unknown",
      Effect_Type = effect_type,
      Effect_Estimate_CI = sprintf("%.3f", coef_summary[-1, "Estimate"]),
      P_value = sprintf("%.6f", coef_summary[-1, "Pr(>|t|)"]),
      Significant = ifelse(coef_summary[-1, "Pr(>|t|)"] <= alpha, "✓", "✗"),
      stringsAsFactors = FALSE
    )
  }
  
  return(table_data)
}

#' Generate outcome-specific plots
generate_outcome_plots <- function(data, outcome_var, outcome_type, time_var, event_var) {
  
  plots <- list()
  
  if (outcome_type == "survival" && !is.null(time_var) && !is.null(event_var)) {
    # Generate Kaplan-Meier curve
    surv_obj <- Surv(data[[time_var]], data[[event_var]])
    km_fit <- survfit(surv_obj ~ 1, data = data)
    
    plots$kaplan_meier <- ggplot() +
      geom_step(aes(x = km_fit$time, y = km_fit$surv), direction = "hv") +
      geom_ribbon(aes(x = km_fit$time, ymin = km_fit$lower, ymax = km_fit$upper), 
                  alpha = 0.3, fill = "#2C275B") +
      labs(x = "Time", y = "Survival Probability", 
           title = paste("Kaplan-Meier Curve for", outcome_var)) +
      theme_minimal() +
      theme(plot.title = element_text(color = "#2C275B"))
  }
  
  # Add distribution plot
  if (outcome_type %in% c("continuous", "count")) {
    plots$distribution <- ggplot(data, aes_string(x = outcome_var)) +
      geom_histogram(fill = "#2C275B", alpha = 0.7, bins = 30) +
      labs(title = paste("Distribution of", outcome_var)) +
      theme_minimal() +
      theme(plot.title = element_text(color = "#2C275B"))
  }
  
  return(plots)
}

#' Calculate outcome summary statistics
calculate_outcome_summary_stats <- function(data, outcome_var, outcome_type) {
  
  stats <- list()
  
  if (outcome_type == "survival") {
    # Will be calculated separately for survival outcomes
    stats$type <- "survival"
  } else if (outcome_type == "binary") {
    tab <- table(data[[outcome_var]], useNA = "ifany")
    stats <- list(
      type = "binary",
      n_total = length(data[[outcome_var]]),
      n_missing = sum(is.na(data[[outcome_var]])),
      frequencies = as.list(tab),
      proportions = as.list(prop.table(tab))
    )
  } else if (outcome_type %in% c("continuous", "count")) {
    stats <- list(
      type = outcome_type,
      n_total = length(data[[outcome_var]]),
      n_missing = sum(is.na(data[[outcome_var]])),
      mean = mean(data[[outcome_var]], na.rm = TRUE),
      median = median(data[[outcome_var]], na.rm = TRUE),
      sd = sd(data[[outcome_var]], na.rm = TRUE),
      min = min(data[[outcome_var]], na.rm = TRUE),
      max = max(data[[outcome_var]], na.rm = TRUE),
      q25 = quantile(data[[outcome_var]], 0.25, na.rm = TRUE),
      q75 = quantile(data[[outcome_var]], 0.75, na.rm = TRUE)
    )
  }
  
  return(stats)
}

#' Helper function to determine variable type from data
determine_variable_type <- function(data, variable_name) {
  if (!variable_name %in% names(data)) {
    # Check if it's a factor level (e.g., "sexMale", "raceBlack")
    base_vars <- names(data)
    for(base_var in base_vars) {
      if(startsWith(variable_name, base_var)) {
        var_data <- data[[base_var]]
        if (is.factor(var_data) || is.character(var_data)) {
          n_levels <- length(unique(var_data[!is.na(var_data)]))
          if (n_levels == 2) {
            return("Binary")
          } else {
            return("Categorical")
          }
        }
      }
    }
    return("Unknown")
  }
  
  var_data <- data[[variable_name]]
  
  # Check if it's a factor or character (categorical)
  if (is.factor(var_data) || is.character(var_data)) {
    n_levels <- length(unique(var_data[!is.na(var_data)]))
    if (n_levels == 2) {
      return("Binary")
    } else {
      return("Categorical")
    }
  }
  
  # Check if it's numeric
  if (is.numeric(var_data)) {
    # Check if it looks like count data (integers with small range)
    if (all(var_data == round(var_data), na.rm = TRUE)) {
      max_val <- max(var_data, na.rm = TRUE)
      if (max_val <= 20) {
        return("Count")
      }
    }
    return("Continuous")
  }
  
  # Check if it's logical
  if (is.logical(var_data)) {
    return("Binary")
  }
  
  return("Unknown")
}

#' Helper function to update formula by dropping a variable
update_formula_drop_variable <- function(formula, variable_to_drop) {
  
  # Convert formula to character, handling multi-line deparse results
  formula_str <- paste(deparse(formula), collapse = " ")
  
  # Remove the variable (handling various formats)
  patterns_to_remove <- c(
    paste0("\\+\\s*", variable_to_drop),
    paste0(variable_to_drop, "\\s*\\+"),
    paste0("\\s*", variable_to_drop)
  )
  
  for (pattern in patterns_to_remove) {
    formula_str <- gsub(pattern, "", formula_str, perl = TRUE)
  }
  
  # Clean up any double spaces or leading/trailing spaces
  formula_str <- gsub("\\s+", " ", formula_str)
  formula_str <- gsub("^\\s+|\\s+$", "", formula_str)
  
  # Handle case where formula becomes "outcome ~ "
  if (grepl("~\\s*$", formula_str)) {
    formula_str <- gsub("~\\s*$", "~ 1", formula_str)
  }
  
  return(as.formula(formula_str))
}

# Export the main function
cat("TEM Comprehensive Analysis function loaded successfully!\n")
cat("Usage: tem_comprehensive_analysis(data, outcomes, ...)\n") 