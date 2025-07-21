################################################################################
##################### Binary STC HTML Reporting Functions ####################
################################################################################
# 
# This module provides HTML reporting capabilities for binary unanchored STC 
# analysis results, following the same professional theme as TEM exploration.
#
# Main Functions:
# - generate_binary_stc_html_report(): Main HTML report generation
# - create_summary_table(): Summary table of all models
# - create_model_details_section(): Detailed model results
# - create_qba_section(): QBA results presentation
# - create_bootstrap_section(): Bootstrap results
#
# NOTE: Analysis functions (QBA, bootstrap, etc.) are imported from binary_stc_analysis.R
#
# Author: Unanchored STC Analysis Package  
# Version: 1.0
# Last Updated: 2024
################################################################################

# Required libraries
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("htmltools")) install.packages("htmltools")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("base64enc")) install.packages("base64enc")
if (!require("jsonlite")) install.packages("jsonlite")

library(knitr)
library(kableExtra)
library(DT)
library(plotly)
library(jsonlite)

# Define utility operators
`%||%` <- function(x, y) if (is.null(x)) y else x

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

library(htmltools)

################################################################################
##################### TEM-Style CSS and JavaScript ############################
################################################################################

#' Generate TEM-style CSS for Binary STC Reports
generate_binary_stc_css <- function() {
  
  css <- '<style>
/* Enhanced CSS for Binary STC HTML Reports - TEM Theme */

/* Reset and base styles */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, "Helvetica Neue", Arial, sans-serif;
    line-height: 1.6;
    color: #2c3e50;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    background-attachment: fixed;
    padding: 20px 0;
}

/* Container */
.container {
    max-width: 1200px;
    margin: 0 auto;
    background: white;
    border-radius: 15px;
    box-shadow: 0 10px 40px rgba(0, 0, 0, 0.1);
    overflow: hidden;
}

/* Header styling */
.header {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    padding: 40px 30px;
    text-align: center;
    position: relative;
    overflow: hidden;
}

.header::before {
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: url("data:image/svg+xml,%3Csvg width=\'20\' height=\'20\' viewBox=\'0 0 20 20\' xmlns=\'http://www.w3.org/2000/svg\'%3E%3Cg fill=\'%23ffffff\' fill-opacity=\'0.05\'%3E%3Ccircle cx=\'3\' cy=\'3\' r=\'3\'/%3E%3Ccircle cx=\'13\' cy=\'13\' r=\'3\'/%3E%3C/g%3E%3C/svg%3E");
    z-index: 0;
}

.header h1 {
    font-size: 2.8em;
    font-weight: 700;
    margin-bottom: 10px;
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
    z-index: 1;
    position: relative;
}

.header .subtitle {
    font-size: 1.2em;
    margin-bottom: 25px;
    opacity: 0.9;
    font-weight: 300;
    z-index: 1;
    position: relative;
}

.header .meta-info {
    display: flex;
    justify-content: center;
    flex-wrap: wrap;
    gap: 20px;
    margin-top: 25px;
    z-index: 1;
    position: relative;
}

.header .meta-item {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 1em;
    opacity: 0.95;
    background: rgba(255, 255, 255, 0.1);
    padding: 8px 15px;
    border-radius: 25px;
    backdrop-filter: blur(10px);
}

.header .meta-item i {
    font-size: 1.1em;
}

/* Navigation tabs */
.nav-tabs {
    display: flex;
    background: #f8f9fa;
    border-bottom: 2px solid #e9ecef;
    margin: 0;
    overflow-x: auto;
}

.nav-tab {
    flex: 1;
    min-width: 150px;
    padding: 18px 20px;
    background: transparent;
    border: none;
    cursor: pointer;
    font-size: 1em;
    font-weight: 600;
    color: #6c757d;
    transition: all 0.3s ease;
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
}

.nav-tab:hover {
    background: rgba(44, 39, 91, 0.1);
    color: #2C275B;
}

.nav-tab.active {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    box-shadow: 0 2px 8px rgba(44, 39, 91, 0.3);
}

.nav-tab i {
    font-size: 1.1em;
}

/* Sub-tabs styling */
.sub-tabs {
    display: flex;
    background: #f8f9fa;
    border-bottom: 2px solid #e9ecef;
    margin-bottom: 0;
    flex-wrap: wrap;
}

.sub-tab {
    padding: 12px 20px;
    background: transparent;
    border: none;
    cursor: pointer;
    font-size: 0.9em;
    font-weight: 500;
    color: #6c757d;
    transition: all 0.3s ease;
    position: relative;
    display: flex;
    align-items: center;
    gap: 6px;
    border-bottom: 3px solid transparent;
}

.sub-tab:hover {
    background: rgba(44, 39, 91, 0.05);
    color: #2C275B;
    border-bottom-color: rgba(44, 39, 91, 0.3);
}

.sub-tab.active {
    background: rgba(44, 39, 91, 0.1);
    color: #2C275B;
    border-bottom-color: #A23877;
    font-weight: 600;
}

.sub-tab i {
    font-size: 1em;
}

/* Sub-tab content */
.sub-tab-content {
    display: none;
    padding: 0;
}

.sub-tab-content.active {
    display: block;
}

/* Main tab content */
.tab-content {
    display: none;
    padding: 0;
}

.tab-content.active {
    display: block;
}

/* Content sections */
.section {
    padding: 25px 30px;
    border-bottom: 1px solid #e9ecef;
}

.section:last-child {
    border-bottom: none;
}

.section h2 {
    color: #2C275B;
    margin-bottom: 20px;
    font-size: 1.8em;
    border-bottom: 3px solid #A23877;
    padding-bottom: 10px;
}

.section h3 {
    color: #2C275B;
    margin: 25px 0 15px 0;
    font-size: 1.4em;
}

.section h4 {
    color: #A23877;
    margin: 20px 0 12px 0;
    font-size: 1.2em;
}

.section h5 {
    color: #2C275B;
    margin: 15px 0 10px 0;
    font-size: 1.1em;
}

.section p {
    margin-bottom: 15px;
    color: #555;
    line-height: 1.7;
}

/* Summary cards */
.summary-cards {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 20px;
    margin: 30px 0;
}

.summary-card {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 12px;
    padding: 25px;
    text-align: center;
    box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
    transition: transform 0.3s ease, box-shadow 0.3s ease;
    border-left: 4px solid #A23877;
}

.summary-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
}

.summary-card h4 {
    color: #2C275B;
    margin-bottom: 15px;
    font-size: 1.1em;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
}

.summary-card .value {
    font-size: 2.5em;
    font-weight: 700;
    color: #A23877;
    margin-bottom: 10px;
}

.summary-card .description {
    color: #666;
    font-size: 0.9em;
}

/* Table styling */
.analysis-table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
    background: white;
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
}

.analysis-table th {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    padding: 15px 12px;
    text-align: left;
    font-weight: 600;
    font-size: 0.9em;
    border-bottom: 2px solid #A23877;
}

.analysis-table td {
    padding: 12px;
    border-bottom: 1px solid #e9ecef;
    font-size: 0.9em;
}

.analysis-table tbody tr:hover {
    background: #f8f9fa;
}

.analysis-table tbody tr:nth-child(even) {
    background: rgba(248, 249, 250, 0.5);
}

/* Model sections */
.model-section {
    margin-bottom: 30px;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
    border: 1px solid #e9ecef;
}

.model-header {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    padding: 20px;
}

.model-info {
    background: #f8f9fa;
    padding: 15px 20px;
    border-left: 4px solid #A23877;
    font-family: "Courier New", monospace;
    font-size: 0.9em;
    color: #2C275B;
}

/* QBA section styling */
.qba-section {
    margin-bottom: 25px;
    padding: 15px;
    border-left: 4px solid #3498db;
    background: #f8f9fa;
    border-radius: 8px;
}

/* Bootstrap section styling */
.bootstrap-section {
    margin-bottom: 25px;
    padding: 15px;
    border-left: 4px solid #27ae60;
    background: #f8f9fa;
    border-radius: 8px;
}

/* Alert boxes */
.alert {
    padding: 15px 20px;
    border-radius: 8px;
    margin: 15px 0;
    border-left: 4px solid;
}

.alert-info {
    background-color: #e3f2fd;
    border-left-color: #2196f3;
    color: #1565c0;
}

.alert-warning {
    background-color: #fff3e0;
    border-left-color: #ff9800;
    color: #ef6c00;
}

.alert-success {
    background-color: #e8f5e8;
    border-left-color: #4caf50;
    color: #2e7d32;
}

/* Footer styling */
.footer {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    text-align: center;
    padding: 30px 25px;
    font-size: 0.9em;
}

.footer-content {
    max-width: 800px;
    margin: 0 auto;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 20px;
}

.footer-logo {
    height: 40px;
    width: auto;
    filter: brightness(0) invert(1);
    opacity: 0.9;
}

.footer-company {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 8px;
}

.footer-main {
    font-size: 1em;
    font-weight: 500;
}

.footer-team {
    font-size: 0.9em;
    opacity: 0.8;
}

.footer-timestamp {
    font-size: 0.85em;
    opacity: 0.7;
    margin-top: 5px;
}

.footer a {
    color: #fff;
    text-decoration: none;
    transition: opacity 0.3s ease;
}

.footer a:hover {
    opacity: 0.8;
    text-decoration: underline;
}

/* Responsive design */
@media (max-width: 768px) {
    .container {
        margin: 10px;
        border-radius: 10px;
    }
    
    .header {
        padding: 25px 20px;
    }
    
    .header h1 {
        font-size: 2.2em;
    }
    
    .header .meta-info {
        flex-direction: column;
        gap: 15px;
    }
    
    .nav-tabs {
        flex-direction: column;
    }
    
    .tab-content {
        padding: 20px;
    }
    
    .summary-cards {
        grid-template-columns: 1fr;
    }
}

/* Model sub-tabs (third level navigation) */
.model-sub-tabs {
    display: flex;
    flex-wrap: wrap;
    gap: 2px;
    background: #f8f9fa;
    padding: 10px 15px;
    border-bottom: 1px solid #e9ecef;
}

.model-sub-tab {
    background: white;
    border: 1px solid #dee2e6;
    color: #6c757d;
    padding: 8px 15px;
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s ease;
    font-size: 0.9em;
    font-weight: 500;
    display: flex;
    align-items: center;
    gap: 5px;
}

.model-sub-tab:hover {
    background: #e9ecef;
    border-color: #A23877;
    color: #495057;
}

.model-sub-tab.active {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    border-color: #A23877;
    color: white;
    font-weight: 600;
}

.model-sub-tab i {
    font-size: 0.9em;
}

/* Model sub-tab content */
.model-sub-tab-content {
    display: none;
    padding: 0;
}

.model-sub-tab-content.active {
    display: block;
}
</style>'
  
  return(css)
}

#' Generate JavaScript for Binary STC Reports (TEM-style)
generate_binary_stc_javascript <- function() {
  
  js <- '
<script>
// Tab functionality for Binary STC reports
function showTab(tabName) {
    var contents = document.querySelectorAll(".tab-content");
    for (var i = 0; i < contents.length; i++) {
        contents[i].classList.remove("active");
    }
    
    var tabs = document.querySelectorAll(".nav-tab");
    for (var i = 0; i < tabs.length; i++) {
        tabs[i].classList.remove("active");
    }
    
    var targetContent = document.getElementById(tabName);
    if (targetContent) {
        targetContent.classList.add("active");
    }
    
    // Find and activate the corresponding tab button
    var allTabs = document.querySelectorAll(".nav-tab");
    for (var i = 0; i < allTabs.length; i++) {
        var tab = allTabs[i];
        if (tab.onclick && tab.onclick.toString().indexOf(tabName) > -1) {
            tab.classList.add("active");
            break;
        }
    }
}

// Subtab functionality for Model Details
function showSubTab(subTabId) {
    var subTabs = document.querySelectorAll(".sub-tab-content");
    for (var i = 0; i < subTabs.length; i++) {
        subTabs[i].classList.remove("active");
    }
    
    var subTabButtons = document.querySelectorAll(".sub-tab");
    for (var i = 0; i < subTabButtons.length; i++) {
        subTabButtons[i].classList.remove("active");
    }
    
    var targetSubContent = document.getElementById(subTabId);
    if (targetSubContent) {
        targetSubContent.classList.add("active");
    }
    
    // Find and activate the corresponding subtab button
    var allSubTabs = document.querySelectorAll(".sub-tab");
    for (var i = 0; i < allSubTabs.length; i++) {
        var subTab = allSubTabs[i];
        if (subTab.onclick && subTab.onclick.toString().indexOf(subTabId) > -1) {
            subTab.classList.add("active");
            break;
        }
    }
}

// Model subtab functionality for individual models within outcomes
function showModelSubTab(modelSubTabId) {
    var modelSubTabs = document.querySelectorAll(".model-sub-tab-content");
    for (var i = 0; i < modelSubTabs.length; i++) {
        modelSubTabs[i].classList.remove("active");
    }
    
    var modelSubTabButtons = document.querySelectorAll(".model-sub-tab");
    for (var i = 0; i < modelSubTabButtons.length; i++) {
        modelSubTabButtons[i].classList.remove("active");
    }
    
    var targetModelSubContent = document.getElementById(modelSubTabId);
    if (targetModelSubContent) {
        targetModelSubContent.classList.add("active");
    }
    
    // Find and activate the corresponding model subtab button
    var allModelSubTabs = document.querySelectorAll(".model-sub-tab");
    for (var i = 0; i < allModelSubTabs.length; i++) {
        var modelSubTab = allModelSubTabs[i];
        if (modelSubTab.onclick && modelSubTab.onclick.toString().indexOf(modelSubTabId) > -1) {
            modelSubTab.classList.add("active");
            break;
        }
    }
}

// Initialize page
document.addEventListener("DOMContentLoaded", function() {
    var firstTab = document.querySelector(".nav-tab");
    if (firstTab) {
        firstTab.click();
    }
    
    // Load Plotly if not already loaded
    if (typeof Plotly === "undefined") {
        var script = document.createElement("script");
        script.src = "https://cdn.plot.ly/plotly-latest.min.js";
        document.head.appendChild(script);
    }
});
</script>'
  
  return(js)
}

################################################################################
##################### Summary Table Creation #####################
################################################################################

#' Create Summary Table of All STC Models
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML table with model summary
create_summary_table <- function(results) {
  
  # Extract model results
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No Model Results:</strong> No STC models were fitted.
    </div>')
  }
  
  # Create table HTML with TEM styling
  table_html <- '<div class="table-container" style="background: white; border: 1px solid #e9ecef; border-radius: 8px; margin-top: 15px;">
    <table class="analysis-table" style="width: 100%; margin: 0;">
      <thead>
        <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
          <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Model</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Type</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">RR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">OR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P(Treatment)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P(Comparator)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Covariates</th>
        </tr>
      </thead>
      <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Get model type and styling
    model_type <- ifelse(model_data$is_naive, "Naive", "Adjusted")
    row_style <- ifelse(i %% 2 == 0, "background: #f8f9fa;", "background: white;")
    type_badge_class <- ifelse(model_data$is_naive, 
                               'style="background: #6c757d; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"',
                               'style="background: #007bff; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"')
    
    # Get covariates list
    if (length(model_data$covariates) == 0) {
      covariates_text <- "None"
    } else {
      covariates_text <- paste(model_data$covariates, collapse = ", ")
    }
    
    # Format confidence intervals
    rr_ci <- sprintf("%.3f (%.3f, %.3f)", 
                     model_data$stc_result$rr, 
                     model_data$stc_result$ci_lower, 
                     model_data$stc_result$ci_upper)
    
    # Handle OR confidence intervals safely
    or_ci_lower <- if (is.null(model_data$stc_result$or_ci_lower)) NA else model_data$stc_result$or_ci_lower
    or_ci_upper <- if (is.null(model_data$stc_result$or_ci_upper)) NA else model_data$stc_result$or_ci_upper
    
    or_ci <- sprintf("%.3f (%.3f, %.3f)", 
                     model_data$stc_result$or, 
                     or_ci_lower, 
                     or_ci_upper)
    
    table_html <- paste0(table_html, '
      <tr style="', row_style, '">
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">', model_name, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;"><span ', type_badge_class, '>', model_type, '</span></td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #e74c3c; font-weight: 600;">', rr_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad; font-weight: 600;">', or_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #27ae60; font-weight: 600;">', model_data$stc_result$prob_treatment_formatted %||% format_numeric(model_data$stc_result$prob_treatment), '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #f39c12; font-weight: 600;">', model_data$stc_result$prob_comparator_formatted %||% format_numeric(model_data$stc_result$prob_comparator), '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; font-size: 0.9em;">', covariates_text, '</td>
      </tr>')
  }
  
  table_html <- paste0(table_html, '
      </tbody>
    </table>
  </div>')
  
  return(table_html)
}

################################################################################
##################### Model Details Section #####################
################################################################################

#' Create Detailed Model Results Section with Outcome-Based Subtabs
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content for detailed model results with outcome-based subtabs
create_model_details_section <- function(results) {
  
  # Get all model names for analysis
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No Model Results:</strong> No STC models were fitted.
    </div>')
  }
  
  # For now, we have one outcome (can be extended to multiple outcomes later)
  # Create outcome-based structure following TEM exploration pattern
  outcome_name <- results$study_info$outcome_description
  outcome_var <- "response"  # or extract from analysis
  
  # Generate subtab for outcome (prepared for multiple outcomes later)
  sub_tab_id <- paste0("outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
  # Create outcome subtab button
  subtabs_html <- paste0('<div class="sub-tabs">
    <button class="sub-tab active" onclick="showSubTab(\'', sub_tab_id, '\')">
      <i class="fas fa-chart-bar"></i> ', outcome_name, '
    </button>
  </div>')
  
  # Create model-specific subtabs within the outcome
  model_subtabs_html <- '<div class="model-sub-tabs" style="margin-top: 15px; padding: 0 20px;">'
  
  # Add "All Models" summary tab first
  model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab active" onclick="showModelSubTab(\'all_models_', sub_tab_id, '\')">
      <i class="fas fa-table"></i> All Models Summary
    </button>')
  
  # Add individual model tabs
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    clean_name <- gsub("_", " ", model_name)
    
    model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab" onclick="showModelSubTab(\'', model_id, '\')">
      <i class="fas fa-cog"></i> ', clean_name, '
    </button>')
  }
  
  model_subtabs_html <- paste0(model_subtabs_html, '</div>')
  
  # Create content for "All Models" summary
  all_models_content <- paste0('
    <div id="all_models_', sub_tab_id, '" class="model-sub-tab-content active">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-table"></i> Model Comparison Summary
        </h4>
        ', generate_outcome_summary_table(results), '
      </div>
    </div>')
  
  # Create content for individual models
  individual_models_content <- ""
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    
    individual_models_content <- paste0(individual_models_content, '
    <div id="', model_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-cog"></i> ', gsub("_", " ", model_name), ' - Detailed Results
        </h4>
        ', generate_individual_model_details(results, model_name), '
      </div>
    </div>')
  }
  
  # Create outcome subtab content with model subtabs
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, '</span>
            <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">Binary</span>
          </h3>
        </div>
        
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          ', model_subtabs_html, '
          ', all_models_content, '
          ', individual_models_content, '
        </div>
      </div>
    </div>')
  
  # Combine subtabs and content
  result_html <- paste0(subtabs_html, outcome_content)
  return(result_html)
}

#' Generate summary table for all models for an outcome
generate_outcome_summary_table <- function(results) {
  model_names <- names(results$model_results)
  
  # Create summary table HTML
  summary_html <- '
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr>
          <th>Model</th>
          <th>Type</th>
          <th>RR (95% CI)</th>
          <th>OR (95% CI)</th>
          <th>RD (95% CI)</th>
          <th>NNT (95% CI)</th>
          <th>Covariates</th>
        </tr>
      </thead>
      <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Get model type
    model_type <- ifelse(model_data$is_naive, "Naive", "Adjusted")
    
    # Get covariates list
    if (length(model_data$covariates) == 0) {
      covariates_text <- "None"
    } else {
      covariates_text <- paste(model_data$covariates, collapse = ", ")
    }
    
    # Color code by model type
    row_class <- ifelse(model_data$is_naive, "naive-model", "adjusted-model")
    
    summary_html <- paste0(summary_html, '
      <tr class="', row_class, '">
        <td><strong>', model_name, '</strong></td>
        <td><span class="badge badge-', ifelse(model_data$is_naive, "secondary", "primary"), '">', model_type, '</span></td>
        <td style="color: #e74c3c; font-weight: bold;">', model_data$stc_result$rr_formatted, '</td>
        <td style="color: #8e44ad; font-weight: bold;">', model_data$stc_result$or_formatted, '</td>
        <td style="color: #2980b9; font-weight: bold;">', model_data$stc_result$rd_formatted, '</td>
        <td style="color: #d35400; font-weight: bold;">', model_data$stc_result$nnt_formatted, '</td>
        <td style="font-size: 0.9em;">', covariates_text, '</td>
      </tr>')
  }
  
  summary_html <- paste0(summary_html, '
      </tbody>
    </table>')
  
  return(summary_html)
}

#' Generate detailed results for an individual model with comprehensive information
#' 
#' @param results Results object from binary_stc_analysis()
#' @param model_name Name of the model to display details for
#' @return HTML content with comprehensive model details
generate_individual_model_details <- function(results, model_name) {
  
  model_data <- results$model_results[[model_name]]
  
  if (is.null(model_data)) {
    return('<div class="alert alert-warning">Model data not available.</div>')
  }
  
  # Start comprehensive model details
  details_html <- '<div class="model-detail-sections">'
  
  # 1. Complete STC Results Table with all measures
  details_html <- paste0(details_html, '
    <div class="detail-section" style="margin-bottom: 25px;">
      <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
        <i class="fas fa-chart-line"></i> Complete STC Analysis Results
      </h5>
      <table class="analysis-table" style="width: 100%; margin-top: 10px; border: 1px solid #dee2e6;">
        <thead>
          <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
            <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Effect Measure</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Estimate</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">95% Confidence Interval</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Interpretation</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td style="padding: 10px; font-weight: 600; border-bottom: 1px solid #dee2e6;">Relative Risk (RR)</td>
            <td style="padding: 10px; text-align: center; color: #e74c3c; font-weight: 600; border-bottom: 1px solid #dee2e6;">', format_numeric(model_data$stc_result$rr, 3), '</td>
            <td style="padding: 10px; text-align: center; border-bottom: 1px solid #dee2e6;">(', format_numeric(model_data$stc_result$rr_ci_lower, 3), ', ', format_numeric(model_data$stc_result$rr_ci_upper, 3), ')</td>
            <td style="padding: 10px; font-size: 0.9em; color: #666; border-bottom: 1px solid #dee2e6;">Risk of outcome with treatment vs comparator</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600; border-bottom: 1px solid #dee2e6;">Odds Ratio (OR)</td>
            <td style="padding: 10px; text-align: center; color: #8e44ad; font-weight: 600; border-bottom: 1px solid #dee2e6;">', format_numeric(model_data$stc_result$or, 3), '</td>
            <td style="padding: 10px; text-align: center; border-bottom: 1px solid #dee2e6;">(', format_numeric(model_data$stc_result$or_ci_lower, 3), ', ', format_numeric(model_data$stc_result$or_ci_upper, 3), ')</td>
            <td style="padding: 10px; font-size: 0.9em; color: #666; border-bottom: 1px solid #dee2e6;">Odds of outcome with treatment vs comparator</td>
          </tr>
          <tr>
            <td style="padding: 10px; font-weight: 600; border-bottom: 1px solid #dee2e6;">Risk Difference (RD)</td>
            <td style="padding: 10px; text-align: center; color: #f39c12; font-weight: 600; border-bottom: 1px solid #dee2e6;">', format_numeric(model_data$stc_result$rd, 3), '</td>
            <td style="padding: 10px; text-align: center; border-bottom: 1px solid #dee2e6;">(', format_numeric(model_data$stc_result$rd_ci_lower, 3), ', ', format_numeric(model_data$stc_result$rd_ci_upper, 3), ')</td>
            <td style="padding: 10px; font-size: 0.9em; color: #666; border-bottom: 1px solid #dee2e6;">Absolute difference in outcome rates</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600; border-bottom: 1px solid #dee2e6;">Number Needed to Treat (NNT)</td>
            <td style="padding: 10px; text-align: center; color: #17a2b8; font-weight: 600; border-bottom: 1px solid #dee2e6;">', 
            ifelse(is.na(model_data$stc_result$nnt), "N/A", format_numeric(model_data$stc_result$nnt, 1)), '</td>
            <td style="padding: 10px; text-align: center; border-bottom: 1px solid #dee2e6;">',
            ifelse(is.na(model_data$stc_result$nnt_ci_lower), "N/A", 
                   paste0('(', format_numeric(model_data$stc_result$nnt_ci_lower, 1), ', ', format_numeric(model_data$stc_result$nnt_ci_upper, 1), ')')), '</td>
            <td style="padding: 10px; font-size: 0.9em; color: #666; border-bottom: 1px solid #dee2e6;">Patients needed to treat for one additional outcome</td>
          </tr>
        </tbody>
      </table>
    </div>')
  
  # 2. Treatment Probabilities with formatted percentages
  prob_treatment_display <- model_data$stc_result$prob_treatment_formatted %||% 
                           format_probability_percentage(model_data$stc_result$prob_treatment, 
                                                        model_data$stc_result$prob_treatment_ci_lower, 
                                                        model_data$stc_result$prob_treatment_ci_upper)
  prob_comparator_display <- model_data$stc_result$prob_comparator_formatted %||% 
                            format_probability_percentage(model_data$stc_result$prob_comparator, 
                                                         model_data$stc_result$prob_comparator_ci_lower, 
                                                         model_data$stc_result$prob_comparator_ci_upper)
  
  details_html <- paste0(details_html, '
    <div class="detail-section" style="margin-bottom: 25px;">
      <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
        <i class="fas fa-percentage"></i> Outcome Probabilities
      </h5>
      <div style="display: flex; gap: 20px; flex-wrap: wrap;">
        <div style="flex: 1; min-width: 250px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #27ae60;">
          <h6 style="margin: 0 0 10px 0; color: #27ae60; font-weight: 600;">
            <i class="fas fa-user-check"></i> Treatment Group
          </h6>
          <div style="font-size: 1.2em; font-weight: 600; color: #27ae60;">', prob_treatment_display, '</div>
          <div style="font-size: 0.9em; color: #666; margin-top: 5px;">Probability of outcome with treatment</div>
        </div>
        <div style="flex: 1; min-width: 250px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #fd7e14;">
          <h6 style="margin: 0 0 10px 0; color: #fd7e14; font-weight: 600;">
            <i class="fas fa-users"></i> Comparator Group
          </h6>
          <div style="font-size: 1.2em; font-weight: 600; color: #fd7e14;">', prob_comparator_display, '</div>
          <div style="font-size: 0.9em; color: #666; margin-top: 5px;">Probability of outcome with comparator</div>
        </div>
      </div>
    </div>')
  
  # 3. Covariates section (enhanced)
  if (!model_data$is_naive && length(model_data$covariates) > 0) {
    details_html <- paste0(details_html, '
      <div class="detail-section" style="margin-bottom: 25px;">
        <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
          <i class="fas fa-list"></i> Adjustment Covariates
        </h5>
        <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;">
          <div style="margin-bottom: 10px; font-weight: 600; color: #495057;">
            <i class="fas fa-info-circle"></i> Model includes adjustment for the following covariates:
          </div>
          <div style="display: flex; flex-wrap: wrap; gap: 8px;">
            ', paste0('<span style="background: #007bff; color: white; padding: 4px 8px; border-radius: 4px; font-size: 0.9em;">', model_data$covariates, '</span>', collapse = " "), '
          </div>
          <div style="margin-top: 15px; font-size: 0.9em; color: #666;">
            <strong>Note:</strong> Covariates are centered to comparator trial baseline characteristics for STC analysis.
          </div>
        </div>
      </div>')
  } else {
    details_html <- paste0(details_html, '
      <div class="detail-section" style="margin-bottom: 25px;">
        <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
          <i class="fas fa-exclamation-triangle"></i> Model Type
        </h5>
        <div style="background: #fff3cd; padding: 15px; border-radius: 8px; border: 1px solid #ffeaa7; color: #856404;">
          <div style="font-weight: 600; margin-bottom: 5px;">
            <i class="fas fa-info-circle"></i> Naive (Unadjusted) Model
          </div>
          <div style="font-size: 0.9em;">
            This model does not adjust for any baseline covariates. Results may be subject to confounding bias.
          </div>
        </div>
      </div>')
  }
  
  # 4. Regression Model Details (enhanced)
  if (!is.null(model_data$regression_result)) {
    # Extract regression summary
    reg_summary <- summary(model_data$regression_result)
    
    details_html <- paste0(details_html, '
      <div class="detail-section" style="margin-bottom: 25px;">
        <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
          <i class="fas fa-calculator"></i> Logistic Regression Model
        </h5>
        
        <!-- Model Formula -->
        <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6; margin-bottom: 15px;">
          <h6 style="margin: 0 0 10px 0; color: #495057;">Model Formula:</h6>
          <div style="font-family: monospace; background: white; padding: 8px; border-radius: 4px; border: 1px solid #ccc;">
            ', as.character(formula(model_data$regression_result)), '
          </div>
        </div>
        
        <!-- Coefficients Table -->
        <div style="margin-bottom: 15px;">
          <h6 style="margin: 0 0 10px 0; color: #495057;">Regression Coefficients:</h6>
          <table class="analysis-table" style="width: 100%; border: 1px solid #dee2e6;">
            <thead>
              <tr style="background: #f8f9fa;">
                <th style="padding: 8px; text-align: left;">Variable</th>
                <th style="padding: 8px; text-align: center;">Coefficient</th>
                <th style="padding: 8px; text-align: center;">Std. Error</th>
                <th style="padding: 8px; text-align: center;">z-value</th>
                <th style="padding: 8px; text-align: center;">p-value</th>
              </tr>
            </thead>
            <tbody>')
    
    # Add coefficient rows
    coef_table <- reg_summary$coefficients
    for (i in 1:nrow(coef_table)) {
      var_name <- rownames(coef_table)[i]
      coef_val <- coef_table[i, 1]
      std_err <- coef_table[i, 2]
      z_val <- coef_table[i, 3]
      p_val <- coef_table[i, 4]
      
      row_style <- ifelse(i %% 2 == 0, "background: #f8f9fa;", "background: white;")
      p_color <- ifelse(p_val < 0.05, "color: #e74c3c; font-weight: 600;", "color: #6c757d;")
      
      details_html <- paste0(details_html, '
              <tr style="', row_style, '">
                <td style="padding: 8px; font-weight: 600;">', var_name, '</td>
                <td style="padding: 8px; text-align: center;">', format_numeric(coef_val, 4), '</td>
                <td style="padding: 8px; text-align: center;">', format_numeric(std_err, 4), '</td>
                <td style="padding: 8px; text-align: center;">', format_numeric(z_val, 3), '</td>
                <td style="padding: 8px; text-align: center; ', p_color, '">', format_p_value(p_val), '</td>
              </tr>')
    }
    
    details_html <- paste0(details_html, '
            </tbody>
          </table>
        </div>
        
        <!-- Model Diagnostics -->
        <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;">
          <h6 style="margin: 0 0 15px 0; color: #495057;">Model Diagnostics:</h6>
          <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;">
            <div>
              <strong>AIC:</strong> ', format_numeric(reg_summary$aic, 2), '
            </div>
            <div>
              <strong>Deviance:</strong> ', format_numeric(reg_summary$deviance, 2), '
            </div>
            <div>
              <strong>Null Deviance:</strong> ', format_numeric(reg_summary$null.deviance, 2), '
            </div>
            <div>
              <strong>Degrees of Freedom:</strong> ', reg_summary$df.residual, '
            </div>
          </div>
        </div>
      </div>')
  }
  
  # 5. Bootstrap Results (if available)
  if (!is.null(results$bootstrap_results[[model_name]])) {
    bootstrap_data <- results$bootstrap_results[[model_name]]
    
    if (!is.null(bootstrap_data$rr) && length(bootstrap_data$rr) > 0) {
      bootstrap_mean <- mean(bootstrap_data$rr, na.rm = TRUE)
      bootstrap_ci <- quantile(bootstrap_data$rr, c(0.025, 0.975), na.rm = TRUE)
      bootstrap_bias <- bootstrap_mean - model_data$stc_result$rr
      
      details_html <- paste0(details_html, '
        <div class="detail-section" style="margin-bottom: 25px;">
          <h5 style="color: #A23877; margin-bottom: 15px; border-bottom: 2px solid #A23877; padding-bottom: 5px;">
            <i class="fas fa-random"></i> Bootstrap Analysis
          </h5>
          <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;">
            <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745;">
              <h6 style="margin: 0 0 10px 0; color: #28a745;">Bootstrap Mean</h6>
              <div style="font-size: 1.2em; font-weight: 600;">', format_numeric(bootstrap_mean, 3), '</div>
            </div>
            <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8;">
              <h6 style="margin: 0 0 10px 0; color: #17a2b8;">Bootstrap 95% CI</h6>
              <div style="font-size: 1.1em; font-weight: 600;">(', format_numeric(bootstrap_ci[1], 3), ', ', format_numeric(bootstrap_ci[2], 3), ')</div>
            </div>
            <div style="background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #', 
                 ifelse(abs(bootstrap_bias) > 0.1, 'e74c3c', '28a745'), ';">
              <h6 style="margin: 0 0 10px 0; color: #', ifelse(abs(bootstrap_bias) > 0.1, 'e74c3c', '28a745'), ';">Bias</h6>
              <div style="font-size: 1.2em; font-weight: 600;">', format_numeric(bootstrap_bias, 4), '</div>
            </div>
          </div>
        </div>')
    }
  }
  
  details_html <- paste0(details_html, '</div>')
  
  return(details_html)
}

################################################################################
##################### QBA Section #####################
################################################################################

#' Create QBA Results Section with Outcome-Based Subtabs
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content for QBA results with outcome-based subtabs
create_qba_section <- function(results) {
  
  if (length(results$qba_results) == 0) {
    return('<div class="alert alert-info">
      <strong>No QBA Results:</strong> No quantitative bias analysis was performed.
    </div>')
  }
  
  # Create outcome-based subtab structure
  outcome_name <- results$study_info$outcome_description
  sub_tab_id <- paste0("qba_outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
  # Generate subtab button for outcome
  subtabs_html <- paste0('<div class="sub-tabs">
    <button class="sub-tab active" onclick="showSubTab(\'', sub_tab_id, '\')">
      <i class="fas fa-chart-bar"></i> ', outcome_name, '
    </button>
  </div>')
  
  # Get model names for QBA
  model_names <- names(results$qba_results)
  
  # Create model-specific subtabs within the outcome
  model_subtabs_html <- '<div class="model-sub-tabs" style="margin-top: 15px; padding: 0 20px;">'
  
  # Add "All Models" summary tab first
  model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab active" onclick="showModelSubTab(\'all_qba_models_', sub_tab_id, '\')">
      <i class="fas fa-table"></i> All Models QBA Summary
    </button>')
  
  # Add individual model tabs
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("qba_model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    clean_name <- gsub("_", " ", model_name)
    
    model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab" onclick="showModelSubTab(\'', model_id, '\')">
      <i class="fas fa-balance-scale"></i> ', clean_name, '
    </button>')
  }
  
  model_subtabs_html <- paste0(model_subtabs_html, '</div>')
  
  # Create content for "All Models" summary
  all_models_content <- paste0('
    <div id="all_qba_models_', sub_tab_id, '" class="model-sub-tab-content active">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-table"></i> QBA Summary Across All Models
        </h4>
        ', generate_qba_summary_table(results), '
      </div>
    </div>')
  
  # Create content for individual models
  individual_models_content <- ""
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("qba_model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    
    individual_models_content <- paste0(individual_models_content, '
    <div id="', model_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-balance-scale"></i> ', gsub("_", " ", model_name), ' - QBA Detailed Analysis
        </h4>
        ', generate_individual_qba_results(results, model_name), '
      </div>
    </div>')
  }
  
  # Generate subtab content with QBA results for all models
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, ' - QBA Analysis</span>
            <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">Bias Assessment</span>
          </h3>
        </div>
        
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          ', model_subtabs_html, '
          ', all_models_content, '
          ', individual_models_content, '
        </div>
      </div>
    </div>')
  
  return(paste0(subtabs_html, outcome_content))
}

################################################################################
##################### Bootstrap Section #####################
################################################################################

#' Create Bootstrap Results Section with Outcome-Based Subtabs
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content for bootstrap results with outcome-based subtabs
create_bootstrap_section <- function(results) {
  
  if (length(results$bootstrap_results) == 0) {
    return('<div class="alert alert-info">
      <strong>No Bootstrap Results:</strong> No bootstrap analysis was performed.
    </div>')
  }
  
  # Create outcome-based subtab structure
  outcome_name <- results$study_info$outcome_description
  sub_tab_id <- paste0("bootstrap_outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
  # Generate subtab button for outcome
  subtabs_html <- paste0('<div class="sub-tabs">
    <button class="sub-tab active" onclick="showSubTab(\'', sub_tab_id, '\')">
      <i class="fas fa-chart-bar"></i> ', outcome_name, '
    </button>
  </div>')
  
  # Get model names for Bootstrap
  model_names <- names(results$bootstrap_results)
  
  # Create model-specific subtabs within the outcome
  model_subtabs_html <- '<div class="model-sub-tabs" style="margin-top: 15px; padding: 0 20px;">'
  
  # Add "All Models" summary tab first
  model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab active" onclick="showModelSubTab(\'all_bootstrap_models_', sub_tab_id, '\')">
      <i class="fas fa-table"></i> All Models Bootstrap Summary
    </button>')
  
  # Add individual model tabs
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("bootstrap_model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    clean_name <- gsub("_", " ", model_name)
    
    model_subtabs_html <- paste0(model_subtabs_html, '
    <button class="model-sub-tab" onclick="showModelSubTab(\'', model_id, '\')">
      <i class="fas fa-sync-alt"></i> ', clean_name, '
    </button>')
  }
  
  model_subtabs_html <- paste0(model_subtabs_html, '</div>')
  
  # Create content for "All Models" summary
  all_models_content <- paste0('
    <div id="all_bootstrap_models_', sub_tab_id, '" class="model-sub-tab-content active">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-chart-histogram"></i> Bootstrap Validation Summary
        </h4>
        ', generate_bootstrap_summary_table(results), '
      </div>
    </div>')
  
  # Create content for individual models
  individual_models_content <- ""
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_id <- paste0("bootstrap_model_", gsub("[^A-Za-z0-9]", "_", model_name), "_", sub_tab_id)
    
    individual_models_content <- paste0(individual_models_content, '
    <div id="', model_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-sync-alt"></i> ', gsub("_", " ", model_name), ' - Bootstrap Detailed Results
        </h4>
        ', generate_individual_bootstrap_results(results, model_name), '
      </div>
    </div>')
  }
  
  # Generate subtab content with bootstrap results for all models
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, ' - Bootstrap Validation</span>
            <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">Uncertainty Assessment</span>
          </h3>
        </div>
        
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          ', model_subtabs_html, '
          ', all_models_content, '
          ', individual_models_content, '
        </div>
      </div>
    </div>')
  
  return(paste0(subtabs_html, outcome_content))
}

################################################################################
##################### Data Summary Section #####################
################################################################################

#' Create Data Summary Section with Subtabs
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content for data summary with subtabs
create_data_summary_section <- function(results) {
  
  if (is.null(results$data_summary)) {
    return('<div class="alert alert-info">
      <strong>No Data Summary:</strong> Data summary information is not available.
    </div>')
  }
  
  # Define subtab sections
  subtab_sections <- c("baseline", "covariates", "outcome")
  subtab_names <- c("Baseline Characteristics", "Covariate Mapping", "Outcome Summary")
  
  # Generate subtab buttons
  subtabs_html <- '<div class="sub-tabs">'
  for (i in seq_along(subtab_sections)) {
    section <- subtab_sections[i]
    section_name <- subtab_names[i]
    sub_tab_id <- paste0("data_", section)
    active_class <- if (i == 1) " active" else ""
    
    # Get icon for section
    section_icon <- switch(section, 
      "baseline" = "user-friends", 
      "covariates" = "exchange-alt", 
      "outcome" = "target",
      "database")  # default case
    
    subtabs_html <- paste0(subtabs_html, '
      <button class="sub-tab', active_class, '" onclick="showSubTab(\'', sub_tab_id, '\')">
        <i class="fas fa-', section_icon, '"></i> ', section_name, '
      </button>')
  }
  subtabs_html <- paste0(subtabs_html, '</div>')
  
  # Generate baseline characteristics content
  baseline_html <- '
    <div id="data_baseline" class="sub-tab-content active">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Baseline Comparator Characteristics</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">'
  
  # Create baseline characteristics table
  if (!is.null(results$data_summary$baseline_comparator)) {
    baseline_data <- results$data_summary$baseline_comparator
    baseline_html <- paste0(baseline_html, '
            <table class="table table-striped table-hover">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>Variable</th>
                  <th>Value</th>
                  <th>Description</th>
                </tr>
              </thead>
              <tbody>')
    
    for (i in 1:nrow(baseline_data)) {
      row <- baseline_data[i, ]
      baseline_html <- paste0(baseline_html, '
                <tr>
                  <td><strong>', row[[1]], '</strong></td>
                  <td>', row[[2]], '</td>
                  <td>', if(ncol(row) > 2) row[[3]] else "Baseline characteristic", '</td>
                </tr>')
    }
    
    baseline_html <- paste0(baseline_html, '
              </tbody>
            </table>')
  } else {
    baseline_html <- paste0(baseline_html, '
            <div class="alert alert-warning">
              <strong>No baseline characteristics available</strong>
            </div>')
  }
  
  baseline_html <- paste0(baseline_html, '
          </div>
        </div>
      </div>
    </div>')
  
  # Generate covariate mapping content
  covariates_html <- '
    <div id="data_covariates" class="sub-tab-content">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Covariate Mapping</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">'
  
  # Create covariate mapping table
  if (!is.null(results$data_summary$covariate_mapping)) {
    covariates_html <- paste0(covariates_html, '
            <table class="table table-striped table-hover">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>IPD Variable</th>
                  <th>Comparator Variable</th>
                  <th>Mapping Type</th>
                </tr>
              </thead>
              <tbody>')
    
    mapping <- results$data_summary$covariate_mapping
    for (i in seq_along(mapping)) {
      var_name <- names(mapping)[i]
      mapped_name <- mapping[[i]]
      covariates_html <- paste0(covariates_html, '
                <tr>
                  <td><strong>', var_name, '</strong></td>
                  <td>', mapped_name, '</td>
                  <td>Direct mapping</td>
                </tr>')
    }
    
    covariates_html <- paste0(covariates_html, '
              </tbody>
            </table>')
  } else {
    covariates_html <- paste0(covariates_html, '
            <div class="alert alert-warning">
              <strong>No covariate mapping information available</strong>
            </div>')
  }
  
  covariates_html <- paste0(covariates_html, '
            <div class="alert alert-info" style="margin-top: 15px;">
              <strong><i class="fas fa-info-circle"></i> Note:</strong>
              Covariate mapping shows how variables from the IPD are matched to comparator trial characteristics for centering.
            </div>
          </div>
        </div>
      </div>
    </div>')
  
  # Generate outcome summary content
  outcome_html <- paste0('
    <div id="data_outcome" class="sub-tab-content">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Outcome Summary</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">
            <table class="table table-striped table-hover">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>Metric</th>
                  <th>Value</th>
                  <th>Description</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td><strong>Sample Size</strong></td>
                  <td>', results$study_info$sample_size, '</td>
                  <td>Total number of patients analyzed</td>
                </tr>
                <tr>
                  <td><strong>Outcome Events</strong></td>
                  <td>', results$study_info$outcome_events, '</td>
                  <td>Number of patients with binary outcome</td>
                </tr>
                <tr>
                  <td><strong>Event Rate</strong></td>
                  <td>', format_numeric(results$study_info$outcome_events / results$study_info$sample_size * 100, 1), '%</td>
                  <td>Percentage of patients with outcome</td>
                </tr>
                <tr>
                  <td><strong>Models Fitted</strong></td>
                  <td>', length(results$model_results), '</td>
                  <td>Number of STC models analyzed</td>
                </tr>
              </tbody>
            </table>
            <div class="alert alert-success" style="margin-top: 15px;">
              <strong><i class="fas fa-check-circle"></i> Analysis Summary:</strong>
              Binary unanchored STC analysis completed with ', length(results$model_results), ' models fitted across ', results$study_info$sample_size, ' patients.
            </div>
          </div>
        </div>
      </div>
    </div>')
  
  # Combine all content
  result_html <- paste0(subtabs_html, baseline_html, covariates_html, outcome_html)
  return(result_html)
}

################################################################################
##################### Forest Plot Section #####################
################################################################################

#' Create STC forest plot for binary outcomes
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content with forest plot
create_stc_forest_plot <- function(results) {
  
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No Model Results:</strong> No STC models were fitted for forest plot.
    </div>')
  }
  
  # Extract data for forest plot
  model_data <- list()
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_result <- results$model_results[[model_name]]
    
    model_data[[i]] <- list(
      model = model_name,
      type = ifelse(model_result$is_naive, "Naive", "Adjusted"),
      rr = model_result$stc_result$rr,
      ci_lower = model_result$stc_result$ci_lower,
      ci_upper = model_result$stc_result$ci_upper,
      or = model_result$stc_result$or,
      or_ci_lower = model_result$stc_result$or_ci_lower %||% NA,
      or_ci_upper = model_result$stc_result$or_ci_upper %||% NA
    )
  }
  
  # Create forest plot using plotly (simplified version)
  forest_plot_html <- '
    <div class="forest-plot-container" style="margin: 20px 0;">
      <h4 style="color: #2C275B; text-align: center; margin-bottom: 20px;">
        <i class="fas fa-chart-line"></i> Binary STC Forest Plot - Relative Risk
      </h4>
      
      <div id="forest-plot-rr" style="width: 100%; height: 400px; margin: 20px 0;"></div>
      
      <script>
        // Forest plot data
        var forestData = ['
  
  # Add data points
  for (i in seq_along(model_data)) {
    model <- model_data[[i]]
    forest_plot_html <- paste0(forest_plot_html, 
      if (i > 1) ', ' else '',
      '{
        y: ["', model$model, '"],
        x: [', model$rr, '],
        error_x: {
          type: "data",
          array: [', model$ci_upper - model$rr, '],
          arrayminus: [', model$rr - model$ci_lower, ']
        },
        type: "scatter",
        mode: "markers",
        marker: {
          color: "', if (model$type == "Naive") "#6c757d" else "#007bff", '",
          size: 10
        },
        name: "', model$model, '"
      }')
  }
  
  forest_plot_html <- paste0(forest_plot_html, '
        ];
        
        var layout = {
          title: "",
          xaxis: {
            title: "Relative Risk",
            showgrid: true,
            zeroline: true,
            zerolinecolor: "red",
            zerolinewidth: 2
          },
          yaxis: {
            title: "",
            autorange: "reversed"
          },
          showlegend: false,
          margin: { t: 30, b: 50, l: 120, r: 50 }
        };
        
        Plotly.newPlot("forest-plot-rr", forestData, layout, {responsive: true});
      </script>
    </div>')
  
  return(forest_plot_html)
}

################################################################################
##################### QBA Summary Functions #####################
################################################################################

#' Generate QBA summary table across all models
generate_qba_summary_table <- function(results) {
  
  model_names <- names(results$qba_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No QBA results available.</div>')
  }
  
  # Create summary table
  summary_html <- '
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr>
          <th>Model</th>
          <th>Original RR</th>
          <th>E-value</th>
          <th>Selection Bias</th>
          <th>Confounding Bias</th>
          <th>Misclassification</th>
        </tr>
      </thead>
      <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    qba_data <- results$qba_results[[model_name]]
    
    # Get original estimate
    original_rr <- results$model_results[[model_name]]$stc_result$rr
    
    # Calculate E-value (simplified)
    evalue <- if (original_rr >= 1) {
      original_rr + sqrt(original_rr * (original_rr - 1))
    } else {
      (1/original_rr) + sqrt((1/original_rr) * ((1/original_rr) - 1))
    }
    
    # Get bias estimates (simplified)
    selection_result <- "Not assessed"
    confounding_result <- "Not assessed"
    misclass_result <- "Not assessed"
    
    if (!is.null(qba_data) && length(qba_data) > 0) {
      # Extract bias results if available
      for (scenario_name in names(qba_data)) {
        scenario_data <- qba_data[[scenario_name]]
        if (grepl("selection", tolower(scenario_name), ignore.case = TRUE)) {
          selection_result <- "Assessed"
        } else if (grepl("confound", tolower(scenario_name), ignore.case = TRUE)) {
          confounding_result <- "Assessed"
        } else if (grepl("misclass", tolower(scenario_name), ignore.case = TRUE)) {
          misclass_result <- "Assessed"
        }
      }
    }
    
    summary_html <- paste0(summary_html, '
      <tr>
        <td><strong>', model_name, '</strong></td>
        <td style="color: #e74c3c; font-weight: bold;">', format_numeric(original_rr), '</td>
        <td style="color: #8e44ad; font-weight: bold;">', format_numeric(evalue), '</td>
        <td>', selection_result, '</td>
        <td>', confounding_result, '</td>
        <td>', misclass_result, '</td>
      </tr>')
  }
  
  summary_html <- paste0(summary_html, '
      </tbody>
    </table>')
  
  return(summary_html)
}

#' Generate individual QBA results for a specific model with comprehensive analysis
#' 
#' @param results Results object from binary_stc_analysis()
#' @param model_name Name of the model to display QBA results for
#' @return HTML content with detailed QBA results
generate_individual_qba_results <- function(results, model_name) {
  
  qba_data <- results$qba_results[[model_name]]
  
  if (is.null(qba_data) || length(qba_data) == 0) {
    return('<div class="alert alert-warning">
      <strong>No QBA results available for ', model_name, '</strong>
    </div>')
  }
  
  # Get original results for comparison
  original_rr <- results$model_results[[model_name]]$stc_result$rr
  original_ci_lower <- results$model_results[[model_name]]$stc_result$rr_ci_lower
  original_ci_upper <- results$model_results[[model_name]]$stc_result$rr_ci_upper
  
  # Start QBA results HTML
  qba_html <- '<div class="qba-detailed-results">'
  
  # 1. E-value Analysis (Enhanced)
  evalue <- if (original_rr >= 1) {
    original_rr + sqrt(original_rr * (original_rr - 1))
  } else {
    (1/original_rr) + sqrt((1/original_rr) * ((1/original_rr) - 1))
  }
  
  # E-value for CI bound
  evalue_ci <- if (original_ci_lower >= 1) {
    original_ci_lower + sqrt(original_ci_lower * (original_ci_lower - 1))
  } else {
    (1/original_ci_upper) + sqrt((1/original_ci_upper) * ((1/original_ci_upper) - 1))
  }
  
  qba_html <- paste0(qba_html, '
    <div class="qba-section" style="margin-bottom: 25px; background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #6f42c1;">
      <h5 style="color: #6f42c1; margin-bottom: 15px;">
        <i class="fas fa-shield-alt"></i> E-value Analysis for Unmeasured Confounding
      </h5>
      <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 15px; margin-bottom: 15px;">
        <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
          <h6 style="margin: 0 0 10px 0; color: #6f42c1;">E-value (Point Estimate)</h6>
          <div style="font-size: 1.3em; font-weight: 600; color: #6f42c1;">', format_numeric(evalue, 2), '</div>
          <div style="font-size: 0.9em; color: #666; margin-top: 5px;">Based on RR = ', format_numeric(original_rr, 3), '</div>
        </div>
        <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
          <h6 style="margin: 0 0 10px 0; color: #6f42c1;">E-value (CI Bound)</h6>
          <div style="font-size: 1.3em; font-weight: 600; color: #6f42c1;">', format_numeric(evalue_ci, 2), '</div>
          <div style="font-size: 0.9em; color: #666; margin-top: 5px;">Conservative estimate</div>
        </div>
      </div>
      <div style="background: #e7f3ff; padding: 15px; border-radius: 6px; border: 1px solid #b3d7ff;">
        <strong>Interpretation:</strong> An unmeasured confounder would need to be associated with both the treatment and outcome with relative risk ≥ <strong>', format_numeric(evalue, 2), '</strong> to explain away the observed association.
        ', ifelse(evalue >= 2, 
                 'This suggests <strong style="color: #28a745;">moderate to strong robustness</strong> to unmeasured confounding.',
                 'This suggests <strong style="color: #ffc107;">limited robustness</strong> to unmeasured confounding.'), '
      </div>
    </div>')
  
  # 2. QBA Scenario Results (Enhanced with actual analysis)
  scenario_counter <- 0
  for (scenario_name in names(qba_data)) {
    scenario_result <- qba_data[[scenario_name]]
    scenario_counter <- scenario_counter + 1
    
    # Extract scenario details if available
    if (is.list(scenario_result)) {
      
      qba_html <- paste0(qba_html, '
        <div class="qba-section" style="margin-bottom: 25px; background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #e74c3c;">
          <h5 style="color: #e74c3c; margin-bottom: 15px;">
            <i class="fas fa-exclamation-triangle"></i> Scenario ', scenario_counter, ': ', scenario_name, '
          </h5>')
      
      # Show bias parameters if available
      if (!is.null(scenario_result$bias_parameters)) {
        bias_params <- scenario_result$bias_parameters
        
        qba_html <- paste0(qba_html, '
          <div style="margin-bottom: 15px;">
            <h6 style="color: #495057; margin-bottom: 10px;">Bias Parameters:</h6>
            <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
              <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px;">
                ', ifelse(!is.null(bias_params$selection_bias), 
                         paste0('<div><strong>Selection Bias:</strong> ', format_numeric(bias_params$selection_bias, 3), '</div>'), ''), '
                ', ifelse(!is.null(bias_params$information_bias), 
                         paste0('<div><strong>Information Bias:</strong> ', format_numeric(bias_params$information_bias, 3), '</div>'), ''), '
                ', ifelse(!is.null(bias_params$confounding_rr), 
                         paste0('<div><strong>Confounding RR:</strong> ', format_numeric(bias_params$confounding_rr, 3), '</div>'), ''), '
              </div>
            </div>
          </div>')
      }
      
      # Show bias-adjusted results if available
      if (!is.null(scenario_result$adjusted_rr)) {
        adjusted_rr <- scenario_result$adjusted_rr
        adjusted_ci_lower <- scenario_result$adjusted_ci_lower %||% NA
        adjusted_ci_upper <- scenario_result$adjusted_ci_upper %||% NA
        
        # Calculate percent change
        percent_change <- ((adjusted_rr - original_rr) / original_rr) * 100
        
        qba_html <- paste0(qba_html, '
          <div style="margin-bottom: 15px;">
            <h6 style="color: #495057; margin-bottom: 10px;">Bias-Adjusted Results:</h6>
            <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;">
              <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
                <div style="font-size: 0.9em; color: #666; margin-bottom: 5px;">Original RR</div>
                <div style="font-size: 1.2em; font-weight: 600; color: #6c757d;">', format_numeric(original_rr, 3), '</div>
                <div style="font-size: 0.8em; color: #999;">(', format_numeric(original_ci_lower, 3), ', ', format_numeric(original_ci_upper, 3), ')</div>
              </div>
              <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
                <div style="font-size: 0.9em; color: #666; margin-bottom: 5px;">Bias-Adjusted RR</div>
                <div style="font-size: 1.2em; font-weight: 600; color: #e74c3c;">', format_numeric(adjusted_rr, 3), '</div>
                ', ifelse(!is.na(adjusted_ci_lower) && !is.na(adjusted_ci_upper), 
                         paste0('<div style="font-size: 0.8em; color: #999;">(', format_numeric(adjusted_ci_lower, 3), ', ', format_numeric(adjusted_ci_upper, 3), ')</div>'), ''), '
              </div>
              <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
                <div style="font-size: 0.9em; color: #666; margin-bottom: 5px;">Change</div>
                <div style="font-size: 1.2em; font-weight: 600; color: ', 
                     ifelse(abs(percent_change) > 20, '#e74c3c', ifelse(abs(percent_change) > 10, '#ffc107', '#28a745')), ';">
                  ', ifelse(percent_change > 0, '+', ''), format_numeric(percent_change, 1), '%
                </div>
              </div>
            </div>
          </div>')
        
        # Impact assessment
        impact_color <- ifelse(abs(percent_change) > 20, '#e74c3c', ifelse(abs(percent_change) > 10, '#ffc107', '#28a745'))
        impact_text <- ifelse(abs(percent_change) > 20, 'High Impact', ifelse(abs(percent_change) > 10, 'Moderate Impact', 'Low Impact'))
        
        qba_html <- paste0(qba_html, '
          <div style="background: ', ifelse(abs(percent_change) > 20, '#ffe6e6', ifelse(abs(percent_change) > 10, '#fff3cd', '#e8f5e8')), '; 
                      padding: 15px; border-radius: 6px; border: 1px solid ', impact_color, ';">
            <strong style="color: ', impact_color, ';">Impact Assessment: ', impact_text, '</strong><br>
            ', ifelse(abs(percent_change) > 20, 
                     'This bias scenario substantially changes the treatment effect estimate, suggesting high vulnerability to this type of bias.',
                     ifelse(abs(percent_change) > 10,
                            'This bias scenario moderately changes the treatment effect estimate.',
                            'This bias scenario has minimal impact on the treatment effect estimate, suggesting robustness to this type of bias.')), '
          </div>')
      } else {
        # Generic QBA scenario without specific results
        qba_html <- paste0(qba_html, '
          <div style="background: #e7f3ff; padding: 15px; border-radius: 6px; border: 1px solid #b3d7ff;">
            <strong>Bias Analysis Status:</strong> This scenario evaluates the potential impact of ', tolower(scenario_name), ' on the treatment effect estimate.
            ', ifelse(is.character(scenario_result), scenario_result, 'Analysis completed with scenario-specific parameters.'), '
          </div>')
      }
      
      qba_html <- paste0(qba_html, '</div>')
      
    } else {
      # Simple scenario result
      qba_html <- paste0(qba_html, '
        <div class="qba-section" style="margin-bottom: 20px; background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #6c757d;">
          <h5 style="color: #6c757d; margin-bottom: 10px;">
            <i class="fas fa-info-circle"></i> ', scenario_name, '
          </h5>
          <div style="background: #e7f3ff; padding: 15px; border-radius: 6px; border: 1px solid #b3d7ff;">
            ', ifelse(is.character(scenario_result), scenario_result, 'Bias analysis scenario evaluated.'), '
          </div>
        </div>')
    }
  }
  
  # 3. Overall QBA Summary
  qba_html <- paste0(qba_html, '
    <div class="qba-section" style="margin-bottom: 20px; background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 4px solid #17a2b8;">
      <h5 style="color: #17a2b8; margin-bottom: 15px;">
        <i class="fas fa-clipboard-check"></i> QBA Summary for ', model_name, '
      </h5>
      <div style="background: white; padding: 15px; border-radius: 6px; border: 1px solid #dee2e6;">
        <ul style="margin: 0; padding-left: 20px;">
          <li style="margin: 8px 0;"><strong>E-value:</strong> ', format_numeric(evalue, 2), ' (', 
          ifelse(evalue >= 2, 'moderate-strong robustness', 'limited robustness'), ' to unmeasured confounding)</li>
          <li style="margin: 8px 0;"><strong>Scenarios Evaluated:</strong> ', length(qba_data), ' bias scenarios</li>
          <li style="margin: 8px 0;"><strong>Original RR:</strong> ', format_numeric(original_rr, 3), ' (95% CI: ', 
          format_numeric(original_ci_lower, 3), ', ', format_numeric(original_ci_upper, 3), ')</li>
        </ul>
      </div>
    </div>')
  
  qba_html <- paste0(qba_html, '</div>')
  
  return(qba_html)
}

################################################################################
##################### Bootstrap Summary Functions #####################
################################################################################

#' Generate bootstrap summary table across all models
generate_bootstrap_summary_table <- function(results) {
  
  model_names <- names(results$bootstrap_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No bootstrap results available.</div>')
  }
  
  # Create summary table
  summary_html <- '
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr>
          <th>Model</th>
          <th>Original RR</th>
          <th>Bootstrap Mean</th>
          <th>Bootstrap 95% CI</th>
          <th>Bias</th>
          <th>Coverage</th>
        </tr>
      </thead>
      <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    bootstrap_data <- results$bootstrap_results[[model_name]]
    
    if (!is.null(bootstrap_data) && !is.null(bootstrap_data$rr)) {
      original_rr <- results$model_results[[model_name]]$stc_result$rr
      bootstrap_mean <- mean(bootstrap_data$rr, na.rm = TRUE)
      bootstrap_ci <- quantile(bootstrap_data$rr, c(0.025, 0.975), na.rm = TRUE)
      bias <- bootstrap_mean - original_rr
      
      # Simple coverage assessment
      coverage <- if (original_rr >= bootstrap_ci[1] && original_rr <= bootstrap_ci[2]) "Yes" else "No"
      
      summary_html <- paste0(summary_html, '
        <tr>
          <td><strong>', model_name, '</strong></td>
          <td style="color: #e74c3c; font-weight: bold;">', format_numeric(original_rr), '</td>
          <td style="color: #27ae60; font-weight: bold;">', format_numeric(bootstrap_mean), '</td>
          <td>(', format_numeric(bootstrap_ci[1]), ', ', format_numeric(bootstrap_ci[2]), ')</td>
          <td style="color: ', if(abs(bias) > 0.1) '#e74c3c' else '#27ae60', ';">', format_numeric(bias), '</td>
          <td style="color: ', if(coverage == "Yes") '#27ae60' else '#e74c3c', ';">', coverage, '</td>
        </tr>')
    } else {
      summary_html <- paste0(summary_html, '
        <tr>
          <td><strong>', model_name, '</strong></td>
          <td colspan="5" style="text-align: center; color: #6c757d;">Bootstrap failed</td>
        </tr>')
    }
  }
  
  summary_html <- paste0(summary_html, '
      </tbody>
    </table>')
  
  return(summary_html)
}

#' Generate individual bootstrap results for a specific model
generate_individual_bootstrap_results <- function(results, model_name) {
  
  bootstrap_data <- results$bootstrap_results[[model_name]]
  
  if (is.null(bootstrap_data)) {
    return('<div class="alert alert-warning">
      <strong>No bootstrap results available for ', model_name, '</strong>
    </div>')
  }
  
  # Create detailed bootstrap results
  bootstrap_html <- '<div class="bootstrap-detailed-results">'
  
  if (!is.null(bootstrap_data$rr) && length(bootstrap_data$rr) > 0) {
    bootstrap_stats <- list(
      mean = mean(bootstrap_data$rr, na.rm = TRUE),
      median = median(bootstrap_data$rr, na.rm = TRUE),
      sd = sd(bootstrap_data$rr, na.rm = TRUE),
      ci = quantile(bootstrap_data$rr, c(0.025, 0.975), na.rm = TRUE)
    )
    
    bootstrap_html <- paste0(bootstrap_html, '
      <div class="bootstrap-section">
        <h5 style="color: #A23877;">Bootstrap Statistics</h5>
        <table class="analysis-table" style="width: 100%; margin-top: 10px;">
          <tbody>
            <tr>
              <td><strong>Bootstrap Mean</strong></td>
              <td>', format_numeric(bootstrap_stats$mean), '</td>
            </tr>
            <tr>
              <td><strong>Bootstrap Median</strong></td>
              <td>', format_numeric(bootstrap_stats$median), '</td>
            </tr>
            <tr>
              <td><strong>Bootstrap SD</strong></td>
              <td>', format_numeric(bootstrap_stats$sd), '</td>
            </tr>
            <tr>
              <td><strong>Bootstrap 95% CI</strong></td>
              <td>(', format_numeric(bootstrap_stats$ci[1]), ', ', format_numeric(bootstrap_stats$ci[2]), ')</td>
            </tr>
          </tbody>
        </table>
      </div>')
    
    # Add convergence information if available
    if (!is.null(bootstrap_data$convergence_rate)) {
      bootstrap_html <- paste0(bootstrap_html, '
        <div class="bootstrap-section" style="margin-top: 20px;">
          <h5 style="color: #A23877;">Convergence Information</h5>
          <p>Bootstrap convergence rate: <strong>', format_numeric(bootstrap_data$convergence_rate * 100, 1), '%</strong></p>
        </div>')
    }
  } else {
    bootstrap_html <- paste0(bootstrap_html, '
      <div class="alert alert-warning">
        Bootstrap analysis failed to converge for this model.
      </div>')
  }
  
  bootstrap_html <- paste0(bootstrap_html, '</div>')
  
  return(bootstrap_html)
}

################################################################################
##################### Footer Section #####################
################################################################################

#' Generate HTML footer with Cytel branding
#' 
#' @return HTML content for footer
generate_binary_stc_footer <- function() {
  
  # Cytel logo SVG (inline, optimized for footer)
  cytel_logo_svg <- '<svg class="footer-logo" viewBox="0 0 2400 1000" xmlns="http://www.w3.org/2000/svg">
    <path d="M2297.92 569.9V0h-142.4v557.4l.2.66c-.08 14.06.44 28.2 2.02 42.5A348 348 0 0 0 2174.7 676c31.96 92.44 143.66 128.6 225.3 74.98-62.26-55.54-99.2-85.8-102.1-181.06zM1036.96 217.24v523.08c-.42 71.4-45.02 140.32-101.94 180.74l119.54 78.9c66.88-56.4 112.52-137.36 122.6-229 1.6-14.28 2.1-28.44 2.02-42.5l.22-.66V217.26zm393.66 352.2-.96-4.28v-206.8h90.92l36.62-142.74h-127.54V0h-142.4v557.08l.2.66c-.08 14.08.44 28.2 2.02 42.5a349 349 0 0 0 17.24 76.24c32.04 91.96 143.08 128 224.36 74.58-56.28-48.42-99.4-85.88-100.46-181.6M1994 568c-25.8 47.88-90.72 90-145.84 90q-1.52 0-2.96-.06c-86.62-2.44-162.98-75.54-163.48-156.4-.64-106.52 82.14-162.56 164.44-170.88 27.98-2.74 73.86 11.86 102.46 36.4L1734.46 514.7a104 104 0 0 0 142.86 27l224.24-149.2-4.64-10.4a278 278 0 0 0-70.52-95.82 275.2 275.2 0 0 0-181.62-68.4 276 276 0 0 0-205.56 92.08c-53.02 59.32-77.4 135.96-68.58 215.8 14.3 129.72 118.94 232.3 248.8 243.92q12.8 1.14 25.4 1.14c100.28 0 191.18-53.56 239.92-140.26l-90.8-62.56zM473.46 602.46a210.4 210.4 0 0 1-128.2 39c-108.48-2.9-198.74-89.96-205.48-198.5-7.64-123.3 89.88-225.68 211.26-225.68 81.56 0 152.32 46.28 187.68 114.04l113.72-75.08C591.88 148.04 475.58 75.44 342.5 77.66 152.34 80.82-2.04 240.06.02 430.64c2.08 191.06 157.26 345.3 348.4 345.3 129.34 0 242.16-70.7 302.28-175.6l-2.38-1.62c-53.04-36.56-122.4-33.62-174.88 3.72zm526.7 28.08c-81.2 53.6-192.38 17.64-224.4-74.38a348 348 0 0 1-17.2-76.08 360 360 0 0 1-2-42.48l-.22-.66V217.28h142.4v232.16c1.28 91.7 42.92 138.7 101.42 181.12z" fill="currentColor"/>
  </svg>'
  
  footer <- paste0(
    '
    </div>

    <div class="footer">
        <div class="footer-content">
            ', cytel_logo_svg, '
            <div class="footer-company">
                <div class="footer-main">
                    Developed by <a href="https://cytel.com" target="_blank">Cytel</a> | Comparative Effectiveness Team
                </div>
                <div class="footer-team">
                    Unanchored STC Analysis Package | Binary STC Module
                </div>
            </div>
            <div class="footer-timestamp">
                Report created on ', format(Sys.time(), "%B %d, %Y at %H:%M %Z"), '
            </div>
        </div>
    </div>

</body>
</html>'
  )
  
  return(footer)
}

################################################################################
##################### Main HTML Report Generation #####################
################################################################################

#' Generate comprehensive HTML report for binary unanchored STC analysis
#'
#' @param results List containing analysis results from binary_stc_analysis()
#' @param project_name Name of the project/study for the report subfolder
#' @param output_dir Base directory to save the report (will create timestamped subfolder)
#' @param output_file Optional custom filename (if NULL, will use default naming)
#' @param title Title for the HTML report
#' @return Path to generated HTML file
generate_binary_stc_html_report <- function(results, 
                                           project_name = "Clinical_Study",
                                           output_dir = "reports",
                                           output_file = NULL,
                                           title = "Binary Unanchored STC Analysis Report") {
  
  # Create date stamp and report directory
  date_stamp <- format(Sys.time(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create subfolder with format: Unanchored_STC_[Project_Name]_Date
  project_folder <- paste0("Unanchored_STC_", gsub("[^A-Za-z0-9_]", "_", project_name), "_", date_stamp)
  report_dir <- file.path(output_dir, project_folder)
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate filename if not provided
  if (is.null(output_file)) {
    outcome_summary <- "Binary_Outcomes"
    output_file <- sprintf("Binary_STC_Analysis_%s_%s.html", 
                          outcome_summary, timestamp)
  }
  
  # Full path for the output file
  full_output_path <- file.path(report_dir, output_file)
  
  # Generate CSS and JavaScript
  css_styles <- generate_binary_stc_css()
  js_scripts <- generate_binary_stc_javascript()
  
  # Create HTML content using TEM structure
  html_content <- paste0(
    '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>', title, '</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
    ', css_styles, '
    ', js_scripts, '
</head>
<body>

<div class="container">
    <!-- Header Section -->
    <div class="header">
        <h1>', title, '</h1>
        <div class="subtitle">Comprehensive Binary Unanchored STC Analysis with QBA</div>
        <div class="meta-info">
            <div class="meta-item">
                <i class="fas fa-calendar"></i>
                <span>Generated: ', format(Sys.time(), "%B %d, %Y at %H:%M"), '</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-chart-line"></i>
                <span>', length(results$model_results), ' Models Analyzed</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-users"></i>
                <span>', results$study_info$sample_size, ' Patients</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-target"></i>
                <span>', results$study_info$outcome_events, ' Events</span>
            </div>
        </div>
    </div>

    <!-- Navigation Tabs -->
    <div class="nav-tabs">
        <button class="nav-tab active" onclick="showTab(\'overview\')">
            <i class="fas fa-home"></i> Overview
        </button>
        <button class="nav-tab" onclick="showTab(\'results\')">
            <i class="fas fa-chart-bar"></i> STC Results
        </button>
        <button class="nav-tab" onclick="showTab(\'models\')">
            <i class="fas fa-cogs"></i> Model Details
        </button>',
        if (length(results$qba_results) > 0) {
          '<button class="nav-tab" onclick="showTab(\'qba\')">
              <i class="fas fa-shield-alt"></i> QBA Analysis
          </button>'
        } else { '' },
        if (length(results$bootstrap_results) > 0) {
          '<button class="nav-tab" onclick="showTab(\'bootstrap\')">
              <i class="fas fa-sync"></i> Bootstrap
          </button>'
        } else { '' }, '
        <button class="nav-tab" onclick="showTab(\'data\')">
            <i class="fas fa-database"></i> Data Summary
        </button>
    </div>

    <!-- Overview Tab -->
    <div id="overview" class="tab-content active">
        <div class="section">
            <h2>Executive Summary</h2>
            
            <!-- Summary Cards -->
            <div class="summary-cards">
                <div class="summary-card">
                    <h4><i class="fas fa-flask"></i> Study Analysis</h4>
                    <div class="value">', results$study_info$study_name, '</div>
                    <div class="description">', results$study_info$outcome_description, '</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-users"></i> Sample Size</h4>
                    <div class="value">', results$study_info$sample_size, '</div>
                    <div class="description">Patients analyzed</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-bullseye"></i> Event Rate</h4>
                    <div class="value">', round(results$study_info$outcome_events/results$study_info$sample_size*100, 1), '%</div>
                    <div class="description">Outcome events</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-chart-line"></i> Models</h4>
                    <div class="value">', length(results$model_results), '</div>
                    <div class="description">STC models fitted</div>
                </div>
            </div>
            
            <!-- Model Summary Table -->
            <h3>Model Summary</h3>
            ', create_summary_table(results), '
            
            <!-- Forest Plot -->
            ', create_stc_forest_plot(results), '
        </div>
    </div>

    <!-- STC Results Tab -->
    <div id="results" class="tab-content">
        <div class="section">
            <h2>STC Analysis Results</h2>
            
            <!-- Create outcome-based subtabs for STC results -->
            ', create_stc_results_section(results), '
        </div>
    </div>

    <!-- Model Details Tab -->
    <div id="models" class="tab-content">
        <div class="section">
            <h2>Detailed Model Results</h2>
            ', create_model_details_section(results), '
        </div>
    </div>',

    # QBA Tab (conditional)
    if (length(results$qba_results) > 0) {
      paste0('
    <!-- QBA Tab -->
    <div id="qba" class="tab-content">
        <div class="section">
            <h2>Quantitative Bias Analysis</h2>
            ', create_qba_section(results), '
        </div>
    </div>')
    } else { '' },

    # Bootstrap Tab (conditional)
    if (length(results$bootstrap_results) > 0) {
      paste0('
    <!-- Bootstrap Tab -->
    <div id="bootstrap" class="tab-content">
        <div class="section">
            <h2>Bootstrap Validation</h2>
            ', create_bootstrap_section(results), '
        </div>
    </div>')
    } else { '' }, '

    <!-- Data Summary Tab -->
    <div id="data" class="tab-content">
        <div class="section">
            <h2>Data Summary</h2>
            ', create_data_summary_section(results), '
        </div>
    </div>

    ', generate_binary_stc_footer())
  
  # Write HTML to file
  writeLines(html_content, full_output_path)
  
  # Print success message
  cat("Binary STC HTML report generated successfully!\n")
  cat("File saved to:", full_output_path, "\n")
  cat("Report folder:", report_dir, "\n")
  
  return(full_output_path)
}

################################################################################
##################### STC Results Section with Outcome Subtabs #####################
################################################################################

#' Create STC Results Section with Outcome-Based Subtabs
#' 
#' Creates STC results section with outcome-based subtabs including summary table,
#' interactive forest plot, and detailed results
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content for STC results with outcome-based subtabs
create_stc_results_section <- function(results) {
  
  # Get all model names for analysis
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No STC Results:</strong> No STC models were fitted.
    </div>')
  }
  
  # For now, we have one outcome (can be extended to multiple outcomes later)
  outcome_name <- results$study_info$outcome_description
  outcome_var <- "response"  # or extract from analysis
  
  # Generate subtab for outcome (prepared for multiple outcomes later)
  sub_tab_id <- paste0("stc_outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
  # Create outcome subtab button
  subtabs_html <- paste0('<div class="sub-tabs">
    <button class="sub-tab active" onclick="showSubTab(\'', sub_tab_id, '\')">
      <i class="fas fa-chart-bar"></i> ', outcome_name, '
    </button>
  </div>')
  
  # Create result-type subtabs within the outcome
  result_subtabs_html <- '<div class="model-sub-tabs" style="margin-top: 15px; padding: 0 20px;">'
  
  # Add different result views
  result_subtabs_html <- paste0(result_subtabs_html, '
    <button class="model-sub-tab active" onclick="showModelSubTab(\'summary_', sub_tab_id, '\')">
      <i class="fas fa-table"></i> Summary Table
    </button>
    <button class="model-sub-tab" onclick="showModelSubTab(\'forest_', sub_tab_id, '\')">
      <i class="fas fa-chart-line"></i> Forest Plot
    </button>
    <button class="model-sub-tab" onclick="showModelSubTab(\'detailed_', sub_tab_id, '\')">
      <i class="fas fa-list-alt"></i> Detailed Results
    </button>')
  
  result_subtabs_html <- paste0(result_subtabs_html, '</div>')
  
  # Create content for Summary Table
  summary_content <- paste0('
    <div id="summary_', sub_tab_id, '" class="model-sub-tab-content active">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-table"></i> STC Analysis Summary
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Comparative effectiveness results across all fitted models with confidence intervals.</p>
        ', create_summary_table(results), '
      </div>
    </div>')
  
  # Create content for Forest Plot
  forest_content <- paste0('
    <div id="forest_', sub_tab_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-chart-line"></i> Interactive Forest Plot
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Interactive visualization of treatment effects with model selection controls and high-resolution download options.</p>
        ', create_enhanced_stc_forest_plot(results), '
      </div>
    </div>')
  
  # Create content for Detailed Results
  detailed_content <- paste0('
    <div id="detailed_', sub_tab_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-list-alt"></i> Comprehensive Results
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Complete statistical results including all effect measures and confidence intervals.</p>
        ', generate_comprehensive_results_table(results), '
      </div>
    </div>')
  
  # Create outcome subtab content
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, ' - STC Results</span>
            <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">Binary Outcome</span>
          </h3>
        </div>
        
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          ', result_subtabs_html, '
          ', summary_content, '
          ', forest_content, '
          ', detailed_content, '
        </div>
      </div>
    </div>')
  
  return(paste0(subtabs_html, outcome_content))
}

################################################################################
##################### Enhanced Forest Plot #####################
################################################################################

#' Create Enhanced STC Forest Plot with Interactive Controls
#' 
#' Creates an enhanced forest plot with model selection controls and download options
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content with enhanced forest plot
create_enhanced_stc_forest_plot <- function(results) {
  
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No Model Results:</strong> No STC models were fitted for forest plot.
    </div>')
  }
  
  # Create model selection controls
  controls_html <- '
    <div class="forest-plot-controls" style="background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 1px solid #dee2e6;">
      <div style="display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;">
        <div>
          <h5 style="margin: 0 0 10px 0; color: #495057;">
            <i class="fas fa-cogs"></i> Plot Configuration
          </h5>
          <div style="display: flex; align-items: center; gap: 15px; flex-wrap: wrap;">
            <label style="font-weight: 500; color: #6c757d; display: flex; align-items: center; gap: 5px;">
              <input type="checkbox" id="include_bootstrap" style="margin: 0;"> Include Bootstrap CIs
            </label>
            <label style="font-weight: 500; color: #6c757d; display: flex; align-items: center; gap: 5px;">
              Effect Measure:
              <select id="effect_measure" style="margin-left: 5px; padding: 3px 8px; border: 1px solid #ced4da; border-radius: 4px;">
                <option value="rr">Relative Risk (RR)</option>
                <option value="or">Odds Ratio (OR)</option>
                <option value="rd">Risk Difference (RD)</option>
              </select>
            </label>
          </div>
        </div>
        <div style="display: flex; gap: 10px;">
          <button onclick="updateForestPlot()" style="
            background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
            color: white;
            border: none;
            padding: 8px 16px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            display: flex;
            align-items: center;
            gap: 5px;
          ">
            <i class="fas fa-sync-alt"></i> Update Plot
          </button>
          <button onclick="downloadForestPlot()" style="
            background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%);
            color: white;
            border: none;
            padding: 8px 16px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            display: flex;
            align-items: center;
            gap: 5px;
          ">
            <i class="fas fa-download"></i> PNG (300 DPI)
          </button>
        </div>
      </div>
    </div>'
  
  # Create model selection checkboxes
  model_controls_html <- '
    <div class="model-selection" style="background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 1px solid #dee2e6;">
      <h5 style="margin: 0 0 15px 0; color: #495057;">
        <i class="fas fa-check-square"></i> Select Models to Display
      </h5>
      <div style="display: flex; flex-wrap: wrap; gap: 15px;">'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    clean_name <- gsub("_", " ", model_name)
    
    model_controls_html <- paste0(model_controls_html, '
        <label style="font-weight: 500; color: #6c757d; display: flex; align-items: center; gap: 8px; padding: 5px 10px; background: white; border: 1px solid #dee2e6; border-radius: 6px; cursor: pointer;">
          <input type="checkbox" id="model_', model_name, '" checked style="margin: 0;"> 
          <span>', clean_name, '</span>
        </label>')
  }
  
  model_controls_html <- paste0(model_controls_html, '
      </div>
    </div>')
  
  # Generate plotly forest plot with enhanced features
  forest_plot_html <- create_plotly_forest_plot_enhanced(results)
  
  return(paste0(controls_html, model_controls_html, forest_plot_html))
}

################################################################################
##################### Enhanced Plotly Forest Plot #####################
################################################################################

#' Create Enhanced Plotly Forest Plot with Better Styling and Controls
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML content with plotly forest plot
create_plotly_forest_plot_enhanced <- function(results) {
  
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No model results available for forest plot.</div>')
  }
  
  # Helper function to safely extract numeric values
  safe_numeric <- function(value, default = NA) {
    if (is.null(value) || is.na(value) || !is.numeric(value)) {
      return(default)
    }
    return(as.numeric(value))
  }
  
  # Helper function to create JSON safely
  safe_json <- function(data_list) {
    # Convert all values to numeric, replacing NULL/NA with null in JSON
    clean_list <- lapply(data_list, function(x) {
      if (is.null(x) || is.na(x)) return(NULL)
      if (!is.numeric(x)) return(NULL)
      return(as.numeric(x))
    })
    return(toJSON(clean_list, auto_unbox = TRUE, null = "null"))
  }
  
  # Prepare forest plot data with validation
  model_data_js <- ""
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Extract and validate data for different measures
    rr_data <- list(
      estimate = safe_numeric(model_data$stc_result$rr),
      ci_lower = safe_numeric(model_data$stc_result$ci_lower %||% model_data$stc_result$rr_ci_lower),
      ci_upper = safe_numeric(model_data$stc_result$ci_upper %||% model_data$stc_result$rr_ci_upper)
    )
    
    or_data <- list(
      estimate = safe_numeric(model_data$stc_result$or),
      ci_lower = safe_numeric(model_data$stc_result$or_ci_lower),
      ci_upper = safe_numeric(model_data$stc_result$or_ci_upper)
    )
    
    rd_data <- list(
      estimate = safe_numeric(model_data$stc_result$rd),
      ci_lower = safe_numeric(model_data$stc_result$rd_ci_lower),
      ci_upper = safe_numeric(model_data$stc_result$rd_ci_upper)
    )
    
    # Add bootstrap data if available
    bootstrap_rr <- NULL
    if (!is.null(results$bootstrap_results[[model_name]])) {
      bootstrap_data <- results$bootstrap_results[[model_name]]
      if (!is.null(bootstrap_data$rr) && length(bootstrap_data$rr) > 0) {
        bootstrap_ci <- quantile(bootstrap_data$rr, c(0.025, 0.975), na.rm = TRUE)
        if (!any(is.na(bootstrap_ci))) {
          bootstrap_rr <- list(
            ci_lower = safe_numeric(bootstrap_ci[1]),
            ci_upper = safe_numeric(bootstrap_ci[2])
          )
        }
      }
    }
    
    # Build model data JSON
    model_data_js <- paste0(model_data_js, 
      if (i > 1) ",\n" else "",
      '      "', model_name, '": {
        "name": "', gsub("_", " ", model_name), '",
        "type": "', ifelse(model_data$is_naive, "Naive", "Adjusted"), '",
        "rr": ', safe_json(rr_data), ',
        "or": ', safe_json(or_data), ',
        "rd": ', safe_json(rd_data), ',
        "bootstrap_rr": ', ifelse(is.null(bootstrap_rr), "null", safe_json(bootstrap_rr)), '
      }')
  }
  
  # Create the plotly forest plot HTML with JavaScript
  forest_plot_html <- paste0('
    <div class="forest-plot-container" style="background: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;">
      <div id="forest-plot" style="width: 100%; height: 500px;"></div>
    </div>
    
    <script>
    // Load Plotly if not already loaded
    if (typeof Plotly === "undefined") {
      var script = document.createElement("script");
      script.src = "https://cdn.plot.ly/plotly-latest.min.js";
      document.head.appendChild(script);
    }
    
    // Model data for forest plot
    var modelData = {
', model_data_js, '
    };
    
    console.log("Forest plot model data:", modelData); // Debug logging
    
    // Function to create forest plot
    function createForestPlot(measure = "rr", includeBootstrap = false, selectedModels = null) {
      if (typeof Plotly === "undefined") {
        console.error("Plotly is not loaded");
        return;
      }
      
      var data = [];
      var yLabels = [];
      var yPos = 0;
      
      // Get selected models or all models
      var modelsToShow = selectedModels || Object.keys(modelData);
      
      // Reference line data
      var refLineX = (measure === "rd") ? 0 : 1;
      
      // Process models in reverse order for plotting
      var modelsReversed = modelsToShow.slice().reverse();
      
      for (var j = 0; j < modelsReversed.length; j++) {
        var modelName = modelsReversed[j];
        var model = modelData[modelName];
        
        if (!model || !model[measure]) {
          console.warn("Missing model data for:", modelName, measure);
          continue;
        }
        
        var measureData = model[measure];
        
        // Validate measure data
        if (!measureData || 
            measureData.estimate === null || measureData.estimate === undefined ||
            measureData.ci_lower === null || measureData.ci_lower === undefined ||
            measureData.ci_upper === null || measureData.ci_upper === undefined) {
          console.warn("Invalid measure data for:", modelName, measure, measureData);
          continue;
        }
        
        yPos++;
        yLabels.push(model.name + " (" + model.type + ")");
        
        // Main estimate with CI
        data.push({
          x: [measureData.ci_lower, measureData.estimate, measureData.ci_upper],
          y: [yPos, yPos, yPos],
          mode: "markers+lines",
          type: "scatter",
          line: {
            color: model.type === "Naive" ? "#6c757d" : "#007bff", 
            width: 3
          },
          marker: {
            color: model.type === "Naive" ? "#6c757d" : "#007bff",
            size: [0, 12, 0],
            symbol: ["line-ew", "diamond", "line-ew"],
            line: {color: "white", width: 2}
          },
          name: model.name,
          showlegend: false,
          hovertemplate: "<b>" + model.name + "</b><br>" +
                        measure.toUpperCase() + ": " + measureData.estimate.toFixed(3) + "<br>" +
                        "95% CI: (" + measureData.ci_lower.toFixed(3) + ", " + measureData.ci_upper.toFixed(3) + ")<br>" +
                        "Type: " + model.type + "<extra></extra>"
        });
        
        // Bootstrap CI if requested and available
        if (includeBootstrap && model.bootstrap_rr && measure === "rr" && 
            model.bootstrap_rr.ci_lower !== null && model.bootstrap_rr.ci_upper !== null) {
          data.push({
            x: [model.bootstrap_rr.ci_lower, model.bootstrap_rr.ci_upper],
            y: [yPos, yPos],
            mode: "lines",
            type: "scatter",
            line: {color: "#28a745", width: 2, dash: "dash"},
            name: model.name + " (Bootstrap)",
            showlegend: false,
            hovertemplate: "<b>" + model.name + " (Bootstrap)</b><br>" +
                          "Bootstrap 95% CI: (" + model.bootstrap_rr.ci_lower.toFixed(3) + ", " + 
                          model.bootstrap_rr.ci_upper.toFixed(3) + ")<extra></extra>"
          });
        }
      }
      
      // Add reference line
      if (yPos > 0) {
        data.push({
          x: [refLineX, refLineX],
          y: [0.5, yPos + 0.5],
          mode: "lines",
          type: "scatter",
          line: {color: "#dc3545", width: 2, dash: "dash"},
          name: "No Effect",
          showlegend: false,
          hoverinfo: "skip"
        });
      }
      
      // Create layout
      var layout = {
        title: {
          text: "<b>Forest Plot - " + measure.toUpperCase() + "</b>",
          font: {size: 18, color: "#2C275B"}
        },
        xaxis: {
          title: measure.toUpperCase() + " (95% Confidence Interval)",
          showgrid: true,
          gridcolor: "#f0f0f0",
          tickformat: measure === "rd" ? ".3f" : ".2f"
        },
        yaxis: {
          title: "",
          tickvals: Array.from({length: yPos}, (_, i) => i + 1),
          ticktext: yLabels,
          showgrid: true,
          gridcolor: "#f0f0f0"
        },
        plot_bgcolor: "#ffffff",
        paper_bgcolor: "#ffffff",
        margin: {l: 200, r: 50, t: 80, b: 60},
        height: Math.max(300, 150 + (yPos * 40)),
        hovermode: "closest"
      };
      
      // Create config
      var config = {
        responsive: true,
        toImageButtonOptions: {
          format: "png",
          filename: "forest_plot_binary_stc",
          height: Math.max(300, 150 + (yPos * 40)),
          width: 1000,
          scale: 3
        },
        modeBarButtonsToAdd: [{
          name: "Download PNG (300 DPI)",
          icon: Plotly.Icons.camera,
          click: function(gd) {
            Plotly.downloadImage(gd, {
              format: "png",
              width: 1200,
              height: Math.max(300, 150 + (yPos * 40)),
              scale: 3,
              filename: "forest_plot_binary_stc_300dpi"
            });
          }
        }],
        displayModeBar: true,
        displaylogo: false
      };
      
      console.log("Creating forest plot with", data.length, "traces"); // Debug
      Plotly.newPlot("forest-plot", data, layout, config);
    }
    
    // Function to update forest plot based on controls
    function updateForestPlot() {
      var measureSelect = document.getElementById("effect_measure");
      var bootstrapCheck = document.getElementById("include_bootstrap");
      
      if (!measureSelect || !bootstrapCheck) {
        console.error("Forest plot controls not found");
        return;
      }
      
      var measure = measureSelect.value;
      var includeBootstrap = bootstrapCheck.checked;
      
      // Get selected models
      var selectedModels = [];
      Object.keys(modelData).forEach(function(modelName) {
        var checkbox = document.getElementById("model_" + modelName);
        if (checkbox && checkbox.checked) {
          selectedModels.push(modelName);
        }
      });
      
      console.log("Updating forest plot:", measure, includeBootstrap, selectedModels);
      createForestPlot(measure, includeBootstrap, selectedModels);
    }
    
    // Function to download forest plot
    function downloadForestPlot() {
      var gd = document.getElementById("forest-plot");
      if (!gd || typeof Plotly === "undefined") {
        console.error("Cannot download: Plotly or plot element not found");
        return;
      }
      
      Plotly.downloadImage(gd, {
        format: "png",
        width: 1200,
        height: gd.layout ? gd.layout.height : 600,
        scale: 3,
        filename: "forest_plot_binary_stc_300dpi"
      });
    }
    
    // Initialize forest plot when page loads
    function initializeForestPlot() {
      if (typeof Plotly !== "undefined") {
        console.log("Initializing forest plot");
        createForestPlot();
      } else {
        // Wait for Plotly to load
        setTimeout(initializeForestPlot, 100);
      }
    }
    
    // Start initialization
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", initializeForestPlot);
    } else {
      initializeForestPlot();
    }
    </script>')
  
  return(forest_plot_html)
}

################################################################################
##################### Comprehensive Results Table #####################
################################################################################

#' Generate Comprehensive Results Table with All Effect Measures
#' 
#' @param results Results object from binary_stc_analysis()
#' @return HTML table with comprehensive results
generate_comprehensive_results_table <- function(results) {
  
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No model results available for detailed display.</div>')
  }
  
  # Create comprehensive results table
  table_html <- '
    <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-radius: 8px; overflow-x: auto;">
      <table class="analysis-table" style="width: 100%; margin: 0;">
        <thead>
          <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
            <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Model</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Type</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">RR (95% CI)</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">OR (95% CI)</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">RD (95% CI)</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">NNT (95% CI)</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P(Treatment)</th>
            <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P(Comparator)</th>
          </tr>
        </thead>
        <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Get model type and styling
    model_type <- ifelse(model_data$is_naive, "Naive", "Adjusted")
    row_style <- ifelse(i %% 2 == 0, "background: #f8f9fa;", "background: white;")
    type_badge_class <- ifelse(model_data$is_naive, 
                               'style="background: #6c757d; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"',
                               'style="background: #007bff; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"')
    
    # Format all effect measures with CIs
    rr_formatted <- sprintf("%.3f (%.3f, %.3f)", 
                           model_data$stc_result$rr, 
                           model_data$stc_result$rr_ci_lower, 
                           model_data$stc_result$rr_ci_upper)
    
    or_formatted <- sprintf("%.3f (%.3f, %.3f)", 
                           model_data$stc_result$or, 
                           model_data$stc_result$or_ci_lower, 
                           model_data$stc_result$or_ci_upper)
    
    rd_formatted <- sprintf("%.3f (%.3f, %.3f)", 
                           model_data$stc_result$rd, 
                           model_data$stc_result$rd_ci_lower, 
                           model_data$stc_result$rd_ci_upper)
    
    nnt_formatted <- model_data$stc_result$nnt_formatted
    
    # Use formatted percentages for probabilities
    prob_treatment_display <- model_data$stc_result$prob_treatment_formatted %||% 
                             sprintf("%.1f%%", model_data$stc_result$prob_treatment * 100)
    prob_comparator_display <- model_data$stc_result$prob_comparator_formatted %||% 
                              sprintf("%.1f%%", model_data$stc_result$prob_comparator * 100)
    
    table_html <- paste0(table_html, '
      <tr style="', row_style, '">
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">', model_name, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;"><span ', type_badge_class, '>', model_type, '</span></td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #e74c3c; font-weight: 600;">', rr_formatted, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad; font-weight: 600;">', or_formatted, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #f39c12; font-weight: 600;">', rd_formatted, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #17a2b8; font-weight: 600;">', nnt_formatted, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #27ae60; font-weight: 600;">', prob_treatment_display, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #fd7e14; font-weight: 600;">', prob_comparator_display, '</td>
      </tr>')
  }
  
  table_html <- paste0(table_html, '
      </tbody>
    </table>
  </div>')
  
  return(table_html)
}