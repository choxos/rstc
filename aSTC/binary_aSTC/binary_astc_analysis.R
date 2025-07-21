###############################################################################
# Binary Anchored STC (Simulated Treatment Comparison) Analysis Functions
# Based on NICE DSU Technical Support Document 18
#
# This implements anchored indirect comparisons for binary outcomes using
# individual patient data (IPD) from one trial and aggregate data (AgD) from 
# another trial, where both trials share a common comparator arm.
#
# Key Reference:
# Phillippo, D.M., Ades, A.E., Dias, S., Palmer, S., Abrams, K.R., 
# Welton, N.J. NICE DSU Technical Support Document 18: Methods for
# population-adjusted indirect comparisons in submission to NICE. 2016.
###############################################################################

if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tidyr)) {install.packages("tidyr"); library(tidyr)}
if(!require(broom)) {install.packages("broom"); library(broom)}
if(!require(sandwich)) {install.packages("sandwich"); library(sandwich)}

#' Prepare IPD data for anchored STC analysis
#'
#' @param ipd_data Individual patient data from AB trial
#' @param outcome_col Name of the binary outcome column
#' @param treatment_col Name of the treatment column  
#' @param covariate_cols Vector of covariate column names
#' @param reference_arm Name of the reference treatment arm (shared comparator)
#' @return Prepared IPD data with additional columns
#' @export
prepare_ipd_for_astc <- function(ipd_data, outcome_col, treatment_col, 
                                covariate_cols, reference_arm = "A") {
  
  # Validate inputs
  if(!all(c(outcome_col, treatment_col, covariate_cols) %in% names(ipd_data))) {
    stop("Some specified columns not found in IPD data")
  }
  
  if(!reference_arm %in% ipd_data[[treatment_col]]) {
    stop("Reference arm not found in treatment column")
  }
  
  # Prepare data
  prepared_data <- ipd_data %>%
    # Create non-event column for binomial GLM
    mutate(
      !!paste0(outcome_col, "_event") := .data[[outcome_col]],
      !!paste0(outcome_col, "_nonevent") := 1 - .data[[outcome_col]]
    ) %>%
    # Set reference treatment level
    mutate(
      !!treatment_col := factor(.data[[treatment_col]], 
                               levels = c(reference_arm, 
                                        setdiff(unique(.data[[treatment_col]]), reference_arm)))
    )
  
  # Add row identifier
  prepared_data$row_id <- seq_len(nrow(prepared_data))
  
  return(prepared_data)
}

#' Prepare aggregate data for anchored STC analysis
#'
#' @param agd_data Aggregate data from AC trial
#' @param covariate_summaries Named list with mean and sd for continuous covariates
#' @param outcome_summaries Named list with events and totals for each arm
#' @param reference_arm Name of the reference treatment arm (shared comparator)
#' @return Prepared aggregate data structure
#' @export
prepare_agd_for_astc <- function(agd_data = NULL, covariate_summaries, 
                                outcome_summaries, reference_arm = "A") {
  
  # Create structured aggregate data
  agd_structure <- list(
    covariate_summaries = covariate_summaries,
    outcome_summaries = outcome_summaries,
    reference_arm = reference_arm
  )
  
  # Validate outcome summaries structure
  required_arms <- c(reference_arm, setdiff(names(outcome_summaries), reference_arm))
  if(!all(required_arms %in% names(outcome_summaries))) {
    stop("outcome_summaries must include reference arm and at least one comparator")
  }
  
  # Validate each arm has events and total
  for(arm in names(outcome_summaries)) {
    if(!all(c("events", "total") %in% names(outcome_summaries[[arm]]))) {
      stop(paste("Arm", arm, "must have 'events' and 'total' specified"))
    }
  }
  
  return(agd_structure)
}

#' Fit anchored STC model for binary outcomes
#'
#' @param prepared_ipd Prepared IPD data from prepare_ipd_for_astc()
#' @param prepared_agd Prepared aggregate data from prepare_agd_for_astc()
#' @param outcome_col Name of the binary outcome column
#' @param treatment_col Name of the treatment column
#' @param covariate_cols Vector of covariate column names for effect modification
#' @param include_interactions Logical, whether to include treatment-covariate interactions
#' @return Fitted anchored STC model object
#' @export
fit_anchored_stc_model <- function(prepared_ipd, prepared_agd, outcome_col, 
                                  treatment_col, covariate_cols, 
                                  include_interactions = TRUE) {
  
  # Center covariates on target population means
  centered_ipd <- prepared_ipd
  covariate_centers <- list()
  
  for(cov in covariate_cols) {
    if(cov %in% names(prepared_agd$covariate_summaries)) {
      center_value <- prepared_agd$covariate_summaries[[cov]]$mean
      covariate_centers[[cov]] <- center_value
      
      # Create centered covariate
      centered_col <- paste0(cov, "_centered")
      centered_ipd[[centered_col]] <- centered_ipd[[cov]] - center_value
    } else {
      warning(paste("Covariate", cov, "not found in aggregate data summaries"))
    }
  }
  
  # Build model formula
  event_col <- paste0(outcome_col, "_event")
  nonevent_col <- paste0(outcome_col, "_nonevent")
  outcome_formula <- paste0("cbind(", event_col, ", ", nonevent_col, ")")
  
  # Treatment main effect
  formula_terms <- c(treatment_col)
  
  # Add interaction terms if requested
  if(include_interactions) {
    centered_covs <- paste0(covariate_cols, "_centered")
    centered_covs <- centered_covs[paste0(covariate_cols, "_centered") %in% names(centered_ipd)]
    
    if(length(centered_covs) > 0) {
      interaction_terms <- paste0(treatment_col, ":", centered_covs)
      formula_terms <- c(formula_terms, interaction_terms)
    }
  }
  
  # Construct formula
  model_formula <- as.formula(paste(outcome_formula, "~", paste(formula_terms, collapse = " + ")))
  
  # Fit binomial GLM
  stc_model <- glm(model_formula, data = centered_ipd, family = binomial)
  
  # Store additional information
  stc_model$covariate_centers <- covariate_centers
  stc_model$prepared_agd <- prepared_agd
  stc_model$covariate_cols <- covariate_cols
  stc_model$treatment_col <- treatment_col
  stc_model$outcome_col <- outcome_col
  
  class(stc_model) <- c("astc_model", class(stc_model))
  
  return(stc_model)
}

#' Extract treatment effects from anchored STC model
#'
#' @param stc_model Fitted anchored STC model
#' @param use_robust_se Logical, whether to use sandwich (robust) standard errors
#' @return List with treatment effect estimates and standard errors
#' @export
extract_stc_treatment_effects <- function(stc_model, use_robust_se = TRUE) {
  
  # Get coefficient estimates
  coefs <- coef(stc_model)
  
  # Get variance-covariance matrix
  if(use_robust_se) {
    vcov_matrix <- vcovHC(stc_model)
  } else {
    vcov_matrix <- vcov(stc_model)
  }
  
  # Extract treatment effects (log odds ratios)
  treatment_col <- stc_model$treatment_col
  reference_arm <- stc_model$prepared_agd$reference_arm
  
  # Find treatment coefficient names
  coef_names <- names(coefs)
  treatment_coefs <- coef_names[grepl(paste0("^", treatment_col), coef_names)]
  treatment_coefs <- treatment_coefs[!grepl(":", treatment_coefs)] # Exclude interactions
  
  # Extract effects
  effects <- list()
  
  for(coef_name in treatment_coefs) {
    # Extract treatment arm name
    arm_name <- gsub(paste0("^", treatment_col), "", coef_name)
    
    effects[[arm_name]] <- list(
      log_or = coefs[coef_name],
      log_or_se = sqrt(vcov_matrix[coef_name, coef_name]),
      or = exp(coefs[coef_name]),
      or_ci_lower = exp(coefs[coef_name] - 1.96 * sqrt(vcov_matrix[coef_name, coef_name])),
      or_ci_upper = exp(coefs[coef_name] + 1.96 * sqrt(vcov_matrix[coef_name, coef_name]))
    )
  }
  
  return(effects)
}

#' Calculate treatment effect from aggregate data
#'
#' @param prepared_agd Prepared aggregate data
#' @param reference_arm Reference arm name
#' @param comparator_arm Comparator arm name
#' @return List with treatment effect estimates
#' @export
calculate_agd_treatment_effect <- function(prepared_agd, reference_arm, comparator_arm) {
  
  # Extract outcome data
  ref_data <- prepared_agd$outcome_summaries[[reference_arm]]
  comp_data <- prepared_agd$outcome_summaries[[comparator_arm]]
  
  # Calculate log odds ratio using standard formula
  log_or <- log((comp_data$events * (ref_data$total - ref_data$events)) / 
                (ref_data$events * (comp_data$total - comp_data$events)))
  
  # Calculate standard error
  log_or_se <- sqrt(1/comp_data$events + 1/(comp_data$total - comp_data$events) +
                   1/ref_data$events + 1/(ref_data$total - ref_data$events))
  
  return(list(
    log_or = log_or,
    log_or_se = log_or_se,
    or = exp(log_or),
    or_ci_lower = exp(log_or - 1.96 * log_or_se),
    or_ci_upper = exp(log_or + 1.96 * log_or_se)
  ))
}

#' Perform anchored indirect comparison
#'
#' @param stc_effects Treatment effects from STC model (from extract_stc_treatment_effects)
#' @param agd_effects Treatment effects from aggregate data (from calculate_agd_treatment_effect)
#' @param ipd_comparator_arm Name of comparator arm in IPD trial
#' @param agd_comparator_arm Name of comparator arm in aggregate data trial
#' @return Indirect comparison results
#' @export
perform_anchored_indirect_comparison <- function(stc_effects, agd_effects, 
                                                ipd_comparator_arm, agd_comparator_arm) {
  
  # Get STC effect for IPD comparator vs reference
  ipd_effect <- stc_effects[[ipd_comparator_arm]]
  
  # Calculate indirect comparison: AgD_comparator vs IPD_comparator
  # Using AgD_comparator vs Reference - IPD_comparator vs Reference
  indirect_log_or <- agd_effects$log_or - ipd_effect$log_or
  indirect_log_or_se <- sqrt(agd_effects$log_or_se^2 + ipd_effect$log_or_se^2)
  
  # Calculate OR and CI
  indirect_or <- exp(indirect_log_or)
  indirect_or_ci_lower <- exp(indirect_log_or - 1.96 * indirect_log_or_se)
  indirect_or_ci_upper <- exp(indirect_log_or + 1.96 * indirect_log_or_se)
  
  # Calculate p-value
  z_stat <- indirect_log_or / indirect_log_or_se
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(list(
    comparison = paste(agd_comparator_arm, "vs", ipd_comparator_arm),
    log_or = indirect_log_or,
    log_or_se = indirect_log_or_se,
    or = indirect_or,
    or_ci_lower = indirect_or_ci_lower,
    or_ci_upper = indirect_or_ci_upper,
    p_value = p_value,
    z_statistic = z_stat
  ))
}

#' Comprehensive anchored STC analysis with multiple models
#'
#' @param ipd_data Individual patient data
#' @param agd_covariate_summaries Covariate summaries from aggregate trial
#' @param agd_outcome_summaries Outcome summaries from aggregate trial
#' @param outcome_col Name of binary outcome column
#' @param treatment_col Name of treatment column
#' @param covariate_mapping Named list mapping IPD covariates to AgD summaries
#' @param models Named list of models with different covariate combinations
#' @param reference_arm Shared reference arm name
#' @param ipd_comparator_arm Comparator arm in IPD trial
#' @param agd_comparator_arm Comparator arm in aggregate trial
#' @param use_robust_se Use robust standard errors
#' @param study_name Study identifier
#' @param outcome_description Description of the outcome
#' @return Complete anchored STC analysis results with multiple models
#' @export
analyze_anchored_stc <- function(ipd_data, agd_covariate_summaries, agd_outcome_summaries,
                                 outcome_col, treatment_col, covariate_mapping = NULL, models = NULL,
                                 reference_arm, ipd_comparator_arm, agd_comparator_arm, 
                                 use_robust_se = TRUE, study_name = "Anchored STC Analysis",
                                 outcome_description = "Binary Outcome", 
                                 include_interactions = TRUE, use_bootstrap = FALSE, n_bootstrap = 1000) {
  
  cat("===========================================\n")
  cat("BINARY ANCHORED STC ANALYSIS\n")
  cat("===========================================\n\n")
  
  # Prepare data
  cat("Step 1: Preparing IPD data...\n")
  prepared_ipd <- prepare_ipd_for_astc(ipd_data, outcome_col, treatment_col, 
                                      names(covariate_mapping), reference_arm)
  
  cat("Step 2: Preparing aggregate data...\n")
  prepared_agd <- prepare_agd_for_astc(NULL, agd_covariate_summaries, 
                                      agd_outcome_summaries, reference_arm)
  
  # Get sample sizes
  n_ipd <- nrow(ipd_data)
  n_agd <- sum(sapply(agd_outcome_summaries, function(x) x$total))
  
  # Initialize results storage
  results <- list(
    study_characteristics = list(
      ipd_trial = paste(reference_arm, "vs", ipd_comparator_arm),
      agd_trial = paste(reference_arm, "vs", agd_comparator_arm),
      reference_arm = reference_arm,
      ipd_comparator_arm = ipd_comparator_arm,
      agd_comparator_arm = agd_comparator_arm,
      covariate_mapping = covariate_mapping,
      use_robust_se = use_robust_se,
      study_name = study_name,
      outcome_description = outcome_description
    ),
    study_info = list(
      study_name = study_name,
      outcome_description = outcome_description,
      sample_size = n_ipd,
      outcome_events = sum(ipd_data[[outcome_col]], na.rm = TRUE),
      n_ipd = n_ipd,
      n_agd = n_agd
    ),
    data_summaries = list(
      ipd_summary = prepared_ipd %>% 
        group_by(.data[[treatment_col]]) %>%
        summarise(
          n = n(),
          events = sum(.data[[outcome_col]]),
          event_rate = mean(.data[[outcome_col]]),
          .groups = "drop"
        ),
      agd_summary = data.frame(
        arm = names(agd_outcome_summaries),
        events = sapply(agd_outcome_summaries, function(x) x$events),
        total = sapply(agd_outcome_summaries, function(x) x$total),
        event_rate = sapply(agd_outcome_summaries, function(x) x$events/x$total)
      ),
      covariate_mapping = covariate_mapping
    ),
    model_results = list(),
    agd_effect = NULL,
    indirect_comparisons = list()
  )
  
  # Calculate aggregate data treatment effect once
  cat("Step 3: Calculating aggregate data treatment effect...\n")
  agd_effect <- calculate_agd_treatment_effect(prepared_agd, reference_arm, agd_comparator_arm)
  results$agd_effect <- agd_effect
  
  # Calculate enhanced effect measures for AgD trial (A vs C) - same for all models
  agd_enhanced_effect <- calculate_agd_enhanced_effect_measures(agd_effect, agd_outcome_summaries, 
                                                                reference_arm, agd_comparator_arm)
  results$agd_enhanced_effect <- agd_enhanced_effect
  
  cat("Step 4: Fitting anchored STC models...\n")
  
  # Fit each model
  for (i in seq_along(models)) {
    model_name <- names(models)[i]
    covariates <- models[[i]]
    
    cat("\nFitting model:", model_name, "\n")
    
    # Determine if naive model
    is_naive <- length(covariates) == 0 || covariates[1] == "1"
    
    # Get relevant covariates for this model
    if (is_naive) {
      model_covariates <- character(0)
    } else {
      model_covariates <- covariates
    }
    
    # Fit STC model for this covariate combination
    stc_model <- fit_anchored_stc_model(prepared_ipd, prepared_agd, outcome_col, 
                                       treatment_col, model_covariates, 
                                       include_interactions = !is_naive)
    
    # Extract treatment effects from STC
    stc_effects <- extract_stc_treatment_effects(stc_model, use_robust_se)
    
    # Calculate enhanced effect measures (RR, OR, RD, NNT)
    if (ipd_comparator_arm %in% names(stc_effects)) {
      effect <- stc_effects[[ipd_comparator_arm]]
      enhanced_effect <- calculate_enhanced_effect_measures(effect, prepared_ipd, 
                                                           treatment_col, outcome_col,
                                                           reference_arm, ipd_comparator_arm)
    } else {
      enhanced_effect <- NULL
    }
    
    # Perform indirect comparison
    indirect_comparison <- perform_anchored_indirect_comparison(stc_effects, agd_effect,
                                                              ipd_comparator_arm, agd_comparator_arm)
    
    # Calculate enhanced effect measures for indirect comparison (C vs B)
    indirect_enhanced_effect <- calculate_indirect_comparison_effect_measures(
      indirect_comparison, agd_outcome_summaries, ipd_data, treatment_col, outcome_col,
      reference_arm, ipd_comparator_arm, agd_comparator_arm
    )
    
    # Calculate bootstrap confidence intervals if requested
    bootstrap_cis <- NULL
    if (use_bootstrap) {
      cat("Calculating bootstrap CIs for model:", model_name, "\n")
      bootstrap_cis <- calculate_bootstrap_cis(
        ipd_data, agd_covariate_summaries, agd_outcome_summaries,
        outcome_col, treatment_col, covariate_mapping, model_covariates,
        reference_arm, ipd_comparator_arm, agd_comparator_arm,
        use_robust_se, n_bootstrap, include_interactions = !is_naive
      )
    }
    
    # Store comprehensive model results
    results$model_results[[model_name]] <- list(
      model = stc_model,
      covariates = model_covariates,
      stc_effects = stc_effects,
      enhanced_effect = enhanced_effect,
      indirect_comparison = indirect_comparison,
      indirect_enhanced_effect = indirect_enhanced_effect,  # New: enhanced measures for C vs B
      bootstrap_cis = bootstrap_cis,  # New: bootstrap confidence intervals
      is_naive = is_naive,
      regression_result = stc_model  # For detailed model display
    )
    
    # Display results
    if (!is.null(enhanced_effect)) {
      cat("IPD Trial Results (", ipd_comparator_arm, " vs ", reference_arm, "):\n", sep = "")
      cat("  RR (95% CI):", enhanced_effect$rr_formatted, "\n")
      cat("  OR (95% CI):", enhanced_effect$or_formatted, "\n")
      cat("  RD (95% CI):", enhanced_effect$rd_formatted, "\n")
      cat("  NNT (95% CI):", enhanced_effect$nnt_formatted, "\n")
    }
    
    cat("Indirect Comparison (", indirect_comparison$comparison, "):\n")
    cat("  OR (95% CI):", sprintf("%.3f (%.3f, %.3f)", 
                                  indirect_comparison$or, 
                                  indirect_comparison$or_ci_lower, 
                                  indirect_comparison$or_ci_upper), "\n")
    cat("  P-value:", sprintf("%.4f", indirect_comparison$p_value), "\n")
    
    # Store individual indirect comparison for this model
    results$indirect_comparisons[[model_name]] <- indirect_comparison
  }
  
  # Set primary indirect comparison (from first non-naive model or first model)
  primary_model <- names(models)[1]
  if (length(models) > 1) {
    # Find first non-naive model
    for (model_name in names(models)) {
      if (!results$model_results[[model_name]]$is_naive) {
        primary_model <- model_name
        break
      }
    }
  }
  results$indirect_comparison <- results$indirect_comparisons[[primary_model]]
  
  cat("\n===========================================\n")
  cat("ANCHORED STC ANALYSIS COMPLETED\n")
  cat("===========================================\n\n")
  
  class(results) <- "astc_analysis"
  return(results)
}

#' Calculate enhanced effect measures (RR, OR, RD, NNT) from anchored STC
#'
#' @param effect Basic treatment effect from STC model
#' @param prepared_ipd Prepared IPD data
#' @param treatment_col Treatment column name
#' @param outcome_col Outcome column name  
#' @param reference_arm Reference arm name
#' @param comparator_arm Comparator arm name
#' @return Enhanced effect measures with formatting
calculate_enhanced_effect_measures <- function(effect, prepared_ipd, treatment_col, outcome_col,
                                             reference_arm, comparator_arm) {
  
  # Calculate observed event rates in IPD trial
  ipd_summary <- prepared_ipd %>%
    group_by(.data[[treatment_col]]) %>%
    summarise(
      n = n(),
      events = sum(.data[[outcome_col]]),
      rate = mean(.data[[outcome_col]]),
      .groups = "drop"
    )
  
  ref_rate <- ipd_summary$rate[ipd_summary[[treatment_col]] == reference_arm]
  comp_rate <- ipd_summary$rate[ipd_summary[[treatment_col]] == comparator_arm]
  
  # Use OR from STC model and reference rate to calculate adjusted rates
  or <- effect$or
  
  # Calculate relative risk
  rr <- comp_rate / ref_rate
  rr_se <- sqrt((1/ipd_summary$events[ipd_summary[[treatment_col]] == reference_arm]) +
                (1/ipd_summary$events[ipd_summary[[treatment_col]] == comparator_arm]) -
                (1/ipd_summary$n[ipd_summary[[treatment_col]] == reference_arm]) -
                (1/ipd_summary$n[ipd_summary[[treatment_col]] == comparator_arm]))
  
  rr_ci_lower <- exp(log(rr) - 1.96 * rr_se)
  rr_ci_upper <- exp(log(rr) + 1.96 * rr_se)
  
  # Calculate risk difference
  rd <- comp_rate - ref_rate
  rd_se <- sqrt((ref_rate * (1 - ref_rate) / ipd_summary$n[ipd_summary[[treatment_col]] == reference_arm]) +
                (comp_rate * (1 - comp_rate) / ipd_summary$n[ipd_summary[[treatment_col]] == comparator_arm]))
  
  rd_ci_lower <- rd - 1.96 * rd_se
  rd_ci_upper <- rd + 1.96 * rd_se
  
  # Calculate NNT (only if beneficial effect, i.e., rd < 0)
  if (rd != 0) {
    nnt <- 1 / abs(rd)
    nnt_ci_lower <- 1 / abs(rd_ci_upper)
    nnt_ci_upper <- 1 / abs(rd_ci_lower)
  } else {
    nnt <- NA
    nnt_ci_lower <- NA
    nnt_ci_upper <- NA
  }
  
  # Format results
  result <- list(
    # Raw values
    rr = rr,
    rr_ci_lower = rr_ci_lower,
    rr_ci_upper = rr_ci_upper,
    rr_se = rr_se,
    
    or = effect$or,
    or_ci_lower = effect$or_ci_lower,
    or_ci_upper = effect$or_ci_upper,
    
    rd = rd,
    rd_ci_lower = rd_ci_lower,
    rd_ci_upper = rd_ci_upper,
    rd_se = rd_se,
    
    nnt = nnt,
    nnt_ci_lower = nnt_ci_lower,
    nnt_ci_upper = nnt_ci_upper,
    
    prob_treatment = comp_rate,
    prob_comparator = ref_rate,
    
    # Formatted values
    rr_formatted = sprintf("%.3f (%.3f, %.3f)", rr, rr_ci_lower, rr_ci_upper),
    or_formatted = sprintf("%.3f (%.3f, %.3f)", effect$or, effect$or_ci_lower, effect$or_ci_upper),
    rd_formatted = sprintf("%.3f (%.3f, %.3f)", rd, rd_ci_lower, rd_ci_upper),
    nnt_formatted = if (is.na(nnt)) "N/A" else sprintf("%.1f (%.1f, %.1f)", nnt, nnt_ci_lower, nnt_ci_upper),
    prob_treatment_formatted = sprintf("%.1f%%", comp_rate * 100),
    prob_comparator_formatted = sprintf("%.1f%%", ref_rate * 100)
  )
  
  return(result)
}

#' Calculate enhanced effect measures for indirect comparison (C vs B)
#'
#' @param indirect_comparison Indirect comparison result with OR
#' @param agd_outcome_summaries AgD outcome data to get baseline rates
#' @param ipd_data IPD data to get baseline rates for treatment B
#' @param treatment_col Treatment column name
#' @param outcome_col Outcome column name
#' @param reference_arm Reference arm name
#' @param ipd_comparator_arm IPD comparator arm
#' @param agd_comparator_arm AgD comparator arm
#' @return Enhanced effect measures for indirect comparison
calculate_indirect_comparison_effect_measures <- function(indirect_comparison, agd_outcome_summaries, 
                                                        ipd_data, treatment_col, outcome_col,
                                                        reference_arm, ipd_comparator_arm, agd_comparator_arm) {
  
  # Get baseline rates
  # Rate for C (from AgD trial)
  c_events <- agd_outcome_summaries[[agd_comparator_arm]]$events
  c_total <- agd_outcome_summaries[[agd_comparator_arm]]$total
  rate_c_observed <- c_events / c_total
  
  # Rate for B (from IPD trial)
  ipd_summary <- ipd_data %>%
    group_by(.data[[treatment_col]]) %>%
    summarise(
      n = n(),
      events = sum(.data[[outcome_col]]),
      rate = mean(.data[[outcome_col]]),
      .groups = "drop"
    )
  
  rate_b_observed <- ipd_summary$rate[ipd_summary[[treatment_col]] == ipd_comparator_arm]
  n_b <- ipd_summary$n[ipd_summary[[treatment_col]] == ipd_comparator_arm]
  events_b <- ipd_summary$events[ipd_summary[[treatment_col]] == ipd_comparator_arm]
  
  # Extract OR and CI from indirect comparison for this specific model
  or <- indirect_comparison$or
  or_ci_lower <- indirect_comparison$or_ci_lower
  or_ci_upper <- indirect_comparison$or_ci_upper
  
  # Calculate model-adjusted rate for C using the indirect comparison OR
  # The indirect comparison tells us: OR_CB = OR_CA / OR_BA
  # We want to estimate what rate_C would be if applied to the same population as B
  
  # Convert rate_B to odds
  odds_b <- rate_b_observed / (1 - rate_b_observed)
  
  # Apply the indirect comparison OR to get odds for C
  odds_c_model <- odds_b * or
  odds_c_model_lower <- odds_b * or_ci_lower
  odds_c_model_upper <- odds_b * or_ci_upper
  
  # Convert back to rates
  rate_c_model <- odds_c_model / (1 + odds_c_model)
  rate_c_model_lower <- odds_c_model_lower / (1 + odds_c_model_lower)
  rate_c_model_upper <- odds_c_model_upper / (1 + odds_c_model_upper)
  
  # Calculate relative risk using model-adjusted rates
  rr <- rate_c_model / rate_b_observed
  rr_ci_lower <- rate_c_model_lower / rate_b_observed
  rr_ci_upper <- rate_c_model_upper / rate_b_observed
  
  # Calculate risk difference using model-adjusted rates
  rd <- rate_c_model - rate_b_observed
  rd_ci_lower <- rate_c_model_lower - rate_b_observed
  rd_ci_upper <- rate_c_model_upper - rate_b_observed
  
  # Calculate NNT (Number Needed to Treat)
  if (rd != 0) {
    nnt <- 1 / abs(rd)
    # For CI bounds: if RD crosses 0, NNT becomes infinite
    if (rd_ci_lower * rd_ci_upper <= 0) {
      # CI crosses 0, so NNT CI includes infinity
      nnt_ci_lower <- NA
      nnt_ci_upper <- NA
    } else {
      nnt_ci_lower <- 1 / abs(rd_ci_upper)
      nnt_ci_upper <- 1 / abs(rd_ci_lower)
    }
  } else {
    nnt <- NA
    nnt_ci_lower <- NA
    nnt_ci_upper <- NA
  }
  
  # Format results
  result <- list(
    # Raw values
    rr = rr,
    rr_ci_lower = rr_ci_lower,
    rr_ci_upper = rr_ci_upper,
    
    or = or,
    or_ci_lower = or_ci_lower,
    or_ci_upper = or_ci_upper,
    
    rd = rd,
    rd_ci_lower = rd_ci_lower,
    rd_ci_upper = rd_ci_upper,
    
    nnt = nnt,
    nnt_ci_lower = nnt_ci_lower,
    nnt_ci_upper = nnt_ci_upper,
    
    rate_c_model = rate_c_model,  # Model-adjusted rate for C
    rate_c_observed = rate_c_observed,  # Observed rate for C
    rate_b = rate_b_observed,     # Observed rate for B
    
    # Formatted values
    rr_formatted = sprintf("%.3f (%.3f, %.3f)", rr, rr_ci_lower, rr_ci_upper),
    or_formatted = sprintf("%.3f (%.3f, %.3f)", or, or_ci_lower, or_ci_upper),
    rd_formatted = sprintf("%.3f (%.3f, %.3f)", rd, rd_ci_lower, rd_ci_upper),
    nnt_formatted = if (is.na(nnt)) "N/A" else sprintf("%.1f (%.1f, %.1f)", nnt, 
                                                       ifelse(is.na(nnt_ci_lower), NA, nnt_ci_lower), 
                                                       ifelse(is.na(nnt_ci_upper), NA, nnt_ci_upper)),
    rate_c_formatted = sprintf("%.1f%%", rate_c_model * 100),
    rate_b_formatted = sprintf("%.1f%%", rate_b_observed * 100),
    
    # Comparison info
    comparison = paste(agd_comparator_arm, "vs", ipd_comparator_arm)
  )
  
  return(result)
}

#' Calculate enhanced effect measures for AgD trial (A vs C)
#'
#' @param agd_effect AgD treatment effect result
#' @param agd_outcome_summaries AgD outcome data 
#' @param reference_arm Reference arm name
#' @param agd_comparator_arm AgD comparator arm name
#' @return Enhanced effect measures for AgD trial
calculate_agd_enhanced_effect_measures <- function(agd_effect, agd_outcome_summaries, 
                                                  reference_arm, agd_comparator_arm) {
  
  # Get event rates from AgD trial
  ref_events <- agd_outcome_summaries[[reference_arm]]$events
  ref_total <- agd_outcome_summaries[[reference_arm]]$total
  ref_rate <- ref_events / ref_total
  
  comp_events <- agd_outcome_summaries[[agd_comparator_arm]]$events
  comp_total <- agd_outcome_summaries[[agd_comparator_arm]]$total
  comp_rate <- comp_events / comp_total
  
  # Calculate relative risk
  rr <- comp_rate / ref_rate
  rr_se <- sqrt((1/comp_events) + (1/ref_events) - (1/comp_total) - (1/ref_total))
  rr_ci_lower <- exp(log(rr) - 1.96 * rr_se)
  rr_ci_upper <- exp(log(rr) + 1.96 * rr_se)
  
  # Calculate risk difference
  rd <- comp_rate - ref_rate
  rd_se <- sqrt((ref_rate * (1 - ref_rate) / ref_total) + 
                (comp_rate * (1 - comp_rate) / comp_total))
  rd_ci_lower <- rd - 1.96 * rd_se
  rd_ci_upper <- rd + 1.96 * rd_se
  
  # Calculate NNT
  if (rd != 0) {
    nnt <- 1 / abs(rd)
    nnt_ci_lower <- 1 / abs(rd_ci_upper)
    nnt_ci_upper <- 1 / abs(rd_ci_lower)
  } else {
    nnt <- NA
    nnt_ci_lower <- NA
    nnt_ci_upper <- NA
  }
  
  # Format results
  result <- list(
    # Raw values
    rr = rr,
    rr_ci_lower = rr_ci_lower,
    rr_ci_upper = rr_ci_upper,
    rr_se = rr_se,
    
    or = agd_effect$or,
    or_ci_lower = agd_effect$or_ci_lower,
    or_ci_upper = agd_effect$or_ci_upper,
    
    rd = rd,
    rd_ci_lower = rd_ci_lower,
    rd_ci_upper = rd_ci_upper,
    rd_se = rd_se,
    
    nnt = nnt,
    nnt_ci_lower = nnt_ci_lower,
    nnt_ci_upper = nnt_ci_upper,
    
    rate_ref = ref_rate,
    rate_comp = comp_rate,
    
    # Formatted values
    rr_formatted = sprintf("%.3f (%.3f, %.3f)", rr, rr_ci_lower, rr_ci_upper),
    or_formatted = sprintf("%.3f (%.3f, %.3f)", agd_effect$or, agd_effect$or_ci_lower, agd_effect$or_ci_upper),
    rd_formatted = sprintf("%.3f (%.3f, %.3f)", rd, rd_ci_lower, rd_ci_upper),
    nnt_formatted = if (is.na(nnt)) "N/A" else sprintf("%.1f (%.1f, %.1f)", nnt, nnt_ci_lower, nnt_ci_upper),
    rate_ref_formatted = sprintf("%.1f%%", ref_rate * 100),
    rate_comp_formatted = sprintf("%.1f%%", comp_rate * 100),
    
    # Comparison info
    comparison = paste(agd_comparator_arm, "vs", reference_arm)
  )
  
  return(result)
}

#' Print method for astc_analysis objects
#'
#' @param x astc_analysis object
#' @param ... Additional arguments
#' @export
print.astc_analysis <- function(x, ...) {
  cat("Anchored STC Analysis Results\n")
  cat("=============================\n\n")
  
  cat("Study Design:\n")
  cat("- IPD Trial:", x$study_characteristics$ipd_trial, "\n")
  cat("- AgD Trial:", x$study_characteristics$agd_trial, "\n")
  cat("- Reference Arm:", x$study_characteristics$reference_arm, "\n")
  cat("- Effect Modifiers:", paste(x$study_characteristics$covariate_cols, collapse = ", "), "\n")
  cat("- Include Interactions:", x$study_characteristics$include_interactions, "\n\n")
  
  cat("Indirect Comparison Result:\n")
  cat("- Comparison:", x$indirect_comparison$comparison, "\n")
  cat("- Odds Ratio:", sprintf("%.3f", x$indirect_comparison$or), "\n")
  cat("- 95% CI: (", sprintf("%.3f", x$indirect_comparison$or_ci_lower), 
      ", ", sprintf("%.3f", x$indirect_comparison$or_ci_upper), ")\n", sep = "")
  cat("- P-value:", sprintf("%.4f", x$indirect_comparison$p_value), "\n\n")
  
  cat("Use summary() for detailed results\n")
}

#' Summary method for astc_analysis objects
#'
#' @param object astc_analysis object
#' @param ... Additional arguments
#' @export
summary.astc_analysis <- function(object, ...) {
  cat("Anchored STC Analysis - Detailed Results\n")
  cat("========================================\n\n")
  
  # Study characteristics
  cat("STUDY CHARACTERISTICS:\n")
  cat("IPD Trial:", object$study_characteristics$ipd_trial, "\n")
  cat("AgD Trial:", object$study_characteristics$agd_trial, "\n")
  cat("Reference Arm:", object$study_characteristics$reference_arm, "\n")
  cat("Effect Modifiers:", paste(object$study_characteristics$covariate_cols, collapse = ", "), "\n")
  cat("Include Interactions:", object$study_characteristics$include_interactions, "\n")
  cat("Robust Standard Errors:", object$study_characteristics$use_robust_se, "\n\n")
  
  # Data summaries
  cat("DATA SUMMARIES:\n")
  cat("IPD Trial Summary:\n")
  print(object$data_summaries$ipd_summary)
  cat("\nAgD Trial Summary:\n")
  print(object$data_summaries$agd_summary)
  cat("\n")
  
  # STC model results
  cat("STC MODEL RESULTS:\n")
  print(summary(object$model))
  cat("\n")
  
  # Treatment effects
  cat("TREATMENT EFFECTS:\n")
  cat("STC Effects (IPD Trial):\n")
  for(arm in names(object$stc_effects)) {
    effect <- object$stc_effects[[arm]]
    cat("  ", arm, "vs Reference:\n")
    cat("    OR =", sprintf("%.3f", effect$or), 
        "(95% CI:", sprintf("%.3f", effect$or_ci_lower), "-", sprintf("%.3f", effect$or_ci_upper), ")\n")
  }
  
  cat("\nAgD Effect:\n")
  cat("  OR =", sprintf("%.3f", object$agd_effect$or), 
      "(95% CI:", sprintf("%.3f", object$agd_effect$or_ci_lower), "-", sprintf("%.3f", object$agd_effect$or_ci_upper), ")\n")
  
  # Indirect comparison
  cat("\nINDIRECT COMPARISON:\n")
  cat("Comparison:", object$indirect_comparison$comparison, "\n")
  cat("Odds Ratio:", sprintf("%.3f", object$indirect_comparison$or), "\n")
  cat("95% CI: (", sprintf("%.3f", object$indirect_comparison$or_ci_lower), 
      ", ", sprintf("%.3f", object$indirect_comparison$or_ci_upper), ")\n", sep = "")
  cat("P-value:", sprintf("%.4f", object$indirect_comparison$p_value), "\n")
  cat("Z-statistic:", sprintf("%.3f", object$indirect_comparison$z_statistic), "\n")
} 

#' Calculate bootstrap confidence intervals for anchored STC effect measures
#'
#' @param ipd_data Individual patient data 
#' @param agd_covariate_summaries Aggregate data covariate summaries
#' @param agd_outcome_summaries Aggregate data outcome summaries
#' @param outcome_col Outcome column name
#' @param treatment_col Treatment column name
#' @param covariate_mapping Covariate mapping
#' @param model_covariates Model covariates for this specific model
#' @param reference_arm Reference arm name
#' @param ipd_comparator_arm IPD comparator arm
#' @param agd_comparator_arm AgD comparator arm
#' @param use_robust_se Use robust standard errors
#' @param n_bootstrap Number of bootstrap samples
#' @param include_interactions Include treatment-covariate interactions
#' @return List with bootstrap CIs for all effect measures
#' @export
calculate_bootstrap_cis <- function(ipd_data, agd_covariate_summaries, agd_outcome_summaries,
                                   outcome_col, treatment_col, covariate_mapping, model_covariates,
                                   reference_arm, ipd_comparator_arm, agd_comparator_arm,
                                   use_robust_se = TRUE, n_bootstrap = 1000, include_interactions = TRUE) {
  
  # Store original results for comparison
  original_prepared_ipd <- prepare_ipd_for_astc(ipd_data, outcome_col, treatment_col, 
                                                   covariate_mapping, reference_arm)
  original_prepared_agd <- prepare_agd_for_astc(NULL, agd_covariate_summaries, agd_outcome_summaries, reference_arm)
  
  # Original AgD effect
  original_agd_effect <- calculate_agd_treatment_effect(original_prepared_agd, reference_arm, agd_comparator_arm)
  
  # Bootstrap results storage
  bootstrap_results <- list(
    ipd_or = numeric(n_bootstrap),
    ipd_rr = numeric(n_bootstrap), 
    ipd_rd = numeric(n_bootstrap),
    ipd_nnt = numeric(n_bootstrap),
    indirect_or = numeric(n_bootstrap),
    indirect_rr = numeric(n_bootstrap),
    indirect_rd = numeric(n_bootstrap), 
    indirect_nnt = numeric(n_bootstrap)
  )
  
  cat("Calculating bootstrap confidence intervals (", n_bootstrap, " samples)...\n")
  
  # Set up progress tracking
  progress_interval <- max(1, floor(n_bootstrap / 10))
  
  for (i in 1:n_bootstrap) {
    if (i %% progress_interval == 0) {
      cat("Bootstrap sample", i, "/", n_bootstrap, "\n")
    }
    
    tryCatch({
      # Resample IPD data with replacement (within treatment groups)
      bootstrap_ipd <- ipd_data %>%
        group_by(.data[[treatment_col]]) %>%
        slice_sample(n = n(), replace = TRUE) %>%
        ungroup()
      
      # Prepare bootstrap IPD data
      prepared_bootstrap_ipd <- prepare_ipd_for_astc(bootstrap_ipd, outcome_col, treatment_col, 
                                                        covariate_mapping, reference_arm)
      
      # Fit bootstrap STC model  
      bootstrap_stc_model <- fit_anchored_stc_model(prepared_bootstrap_ipd, original_prepared_agd,
                                                   outcome_col, treatment_col, model_covariates,
                                                   include_interactions = include_interactions)
      
      # Extract bootstrap STC effects
      bootstrap_stc_effects <- extract_stc_treatment_effects(bootstrap_stc_model, use_robust_se)
      
      if (ipd_comparator_arm %in% names(bootstrap_stc_effects)) {
        # Calculate bootstrap IPD effect measures
        bootstrap_ipd_effect <- calculate_enhanced_effect_measures(
          bootstrap_stc_effects[[ipd_comparator_arm]], prepared_bootstrap_ipd,
          treatment_col, outcome_col, reference_arm, ipd_comparator_arm
        )
        
        # Store bootstrap IPD results
        bootstrap_results$ipd_or[i] <- bootstrap_ipd_effect$or
        bootstrap_results$ipd_rr[i] <- bootstrap_ipd_effect$rr
        bootstrap_results$ipd_rd[i] <- bootstrap_ipd_effect$rd
        bootstrap_results$ipd_nnt[i] <- ifelse(is.finite(bootstrap_ipd_effect$nnt), bootstrap_ipd_effect$nnt, NA)
        
        # Calculate bootstrap indirect comparison
        bootstrap_indirect <- perform_anchored_indirect_comparison(
          bootstrap_stc_effects, original_agd_effect, ipd_comparator_arm, agd_comparator_arm
        )
        
        # Calculate bootstrap indirect effect measures
        bootstrap_indirect_effect <- calculate_indirect_comparison_effect_measures(
          bootstrap_indirect, agd_outcome_summaries, bootstrap_ipd, treatment_col, outcome_col,
          reference_arm, ipd_comparator_arm, agd_comparator_arm
        )
        
        # Store bootstrap indirect results
        bootstrap_results$indirect_or[i] <- bootstrap_indirect_effect$or
        bootstrap_results$indirect_rr[i] <- bootstrap_indirect_effect$rr
        bootstrap_results$indirect_rd[i] <- bootstrap_indirect_effect$rd
        bootstrap_results$indirect_nnt[i] <- ifelse(is.finite(bootstrap_indirect_effect$nnt), bootstrap_indirect_effect$nnt, NA)
        
        # Debug output for first few samples with actual values
        if (i <= 5) {
          cat("Sample", i, ": Indirect OR =", round(bootstrap_indirect_effect$or, 4), 
              ", RR =", round(bootstrap_indirect_effect$rr, 4), 
              ", RD =", round(bootstrap_indirect_effect$rd, 4), "\n")
        }
      }
    }, error = function(e) {
      # Skip failed bootstrap samples
      bootstrap_results$ipd_or[i] <- NA
      bootstrap_results$ipd_rr[i] <- NA
      bootstrap_results$ipd_rd[i] <- NA
      bootstrap_results$ipd_nnt[i] <- NA
      bootstrap_results$indirect_or[i] <- NA
      bootstrap_results$indirect_rr[i] <- NA
      bootstrap_results$indirect_rd[i] <- NA
      bootstrap_results$indirect_nnt[i] <- NA
    })
  }
  
  cat("Bootstrap sampling completed.\n")
  
  # Debug: Show summary of bootstrap values
  cat("DEBUG: Bootstrap values summary:\n")
  cat("Indirect OR range:", round(range(bootstrap_results$indirect_or, na.rm = TRUE), 4), "\n")
  cat("Indirect RR range:", round(range(bootstrap_results$indirect_rr, na.rm = TRUE), 4), "\n") 
  cat("Valid OR samples:", sum(!is.na(bootstrap_results$indirect_or) & is.finite(bootstrap_results$indirect_or)), "/", n_bootstrap, "\n")
  cat("Valid RR samples:", sum(!is.na(bootstrap_results$indirect_rr) & is.finite(bootstrap_results$indirect_rr)), "/", n_bootstrap, "\n")
  
  # Calculate bootstrap confidence intervals (percentile method) - FIXED VERSION
  calculate_percentile_ci <- function(x, conf_level = 0.95) {
    # Remove NAs and non-finite values only (don't filter out valid values)
    x_clean <- x[!is.na(x) & is.finite(x)]
    
    # Check if we have enough valid samples
    if (length(x_clean) < 5) {
      cat("DEBUG: Only", length(x_clean), "valid samples for CI calculation\n")
      return(c(NA, NA))
    }
    
    # Calculate percentiles (keep all finite values, including negatives and small positives)
    alpha <- 1 - conf_level
    lower_quantile <- alpha/2
    upper_quantile <- 1 - alpha/2
    
    # Use simple quantile calculation
    ci_bounds <- quantile(x_clean, probs = c(lower_quantile, upper_quantile), na.rm = TRUE)
    
    cat("DEBUG: CI calculated from", length(x_clean), "samples: (", round(ci_bounds[1], 4), ",", round(ci_bounds[2], 4), ")\n")
    return(as.numeric(ci_bounds))
  }
  
  # Add debugging before CI calculation
  cat("DEBUG: About to calculate CIs...\n")
  cat("Sample OR values (first 10):", head(bootstrap_results$indirect_or, 10), "\n")
  cat("Sample RR values (first 10):", head(bootstrap_results$indirect_rr, 10), "\n")
  
  # Calculate CIs for all measures
  result <- list(
    ipd_effect = list(
      or_ci = calculate_percentile_ci(bootstrap_results$ipd_or),
      rr_ci = calculate_percentile_ci(bootstrap_results$ipd_rr),
      rd_ci = calculate_percentile_ci(bootstrap_results$ipd_rd),
      nnt_ci = calculate_percentile_ci(bootstrap_results$ipd_nnt)
    ),
    indirect_effect = list(
      or_ci = calculate_percentile_ci(bootstrap_results$indirect_or),
      rr_ci = calculate_percentile_ci(bootstrap_results$indirect_rr),
      rd_ci = calculate_percentile_ci(bootstrap_results$indirect_rd),
      nnt_ci = calculate_percentile_ci(bootstrap_results$indirect_nnt)
    ),
    n_successful = sum(!is.na(bootstrap_results$indirect_or)),
    n_bootstrap = n_bootstrap
  )
  
  return(result)
} 