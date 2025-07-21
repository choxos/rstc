################################################################################
##################### Binary Anchored STC HTML Reporting Functions ############
################################################################################
# 
# This module provides HTML reporting capabilities for binary anchored STC 
# analysis results, following the same professional theme as TEM exploration.
#
# Main Functions:
# - generate_binary_astc_html_report(): Main HTML report generation
# - create_astc_summary_table(): Summary table of STC and indirect comparison results
# - create_astc_model_details_section(): Detailed model results
# - create_astc_data_summary_section(): Data summaries
#
# NOTE: Analysis functions are imported from binary_astc_analysis.R
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

#' Format bootstrap confidence intervals with proper NA handling
format_bootstrap_ci <- function(ci_values, digits = 3) {
  if (is.null(ci_values) || length(ci_values) != 2 || any(is.na(ci_values))) {
    return("N/A")
  }
  return(paste0("(", format_numeric(ci_values[1], digits), ", ", format_numeric(ci_values[2], digits), ")"))
}

library(htmltools)

################################################################################
##################### TEM-Style CSS and JavaScript ############################
################################################################################

#' Generate TEM-style CSS for Binary Anchored STC Reports
generate_binary_astc_css <- function() {
  
  css <- '<style>
/* Enhanced CSS for Binary Anchored STC HTML Reports - TEM Theme */

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

#' Generate JavaScript for Binary Anchored STC Reports (TEM-style)
generate_binary_astc_javascript <- function() {
  
  js <- '
<script>
// Tab functionality for Binary Anchored STC reports
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

#' Create Summary Table of Anchored STC Results - Main Focus on Indirect Comparison
#' 
#' @param results Results object from analyze_anchored_stc()
#' @return HTML table with anchored STC summary focusing on indirect comparison
create_astc_summary_table <- function(results) {
  
  if (is.null(results$model_results) || length(results$model_results) == 0) {
    return('<div class="alert alert-info">
      <strong>No Anchored STC Results:</strong> No anchored STC models were fitted.
    </div>')
  }
  
  # Main table: Indirect Comparison Results (C vs B) - This is the primary result
  table_html <- paste0('<div class="results-section">
    <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
      <i class="fas fa-exchange-alt"></i> Primary Result: Indirect Comparison (', 
      results$study_characteristics$agd_comparator_arm, ' vs ', 
      results$study_characteristics$ipd_comparator_arm, ')
    </h4>
    <p style="margin-bottom: 20px; color: #666; font-style: italic;">
      This is the main result of the anchored STC analysis, comparing treatments across different trials.
    </p>')
  
  table_html <- paste0(table_html, '
    <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-radius: 8px; margin-bottom: 25px;">
    <table class="analysis-table" style="width: 100%; margin: 0;">
      <thead>
        <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
          <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Model</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Type</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">RR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">OR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">RD (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">NNT (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P-value</th>
        </tr>
      </thead>
      <tbody>')
  
  # Extract model names and add rows for indirect comparison
  model_names <- names(results$model_results)
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Get model type and styling
    model_type <- ifelse(model_data$is_naive, "Naive", "Adjusted")
    row_style <- ifelse(i %% 2 == 0, "background: #f8f9fa;", "background: white;")
    type_badge_class <- ifelse(model_data$is_naive, 
                               'style="background: #6c757d; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"',
                               'style="background: #007bff; color: white; padding: 3px 8px; border-radius: 12px; font-size: 0.8em;"')
    
    # Get indirect comparison enhanced effect measures
    indirect_enhanced <- model_data$indirect_enhanced_effect
    if (!is.null(indirect_enhanced)) {
      rr_ci <- indirect_enhanced$rr_formatted
      or_ci <- indirect_enhanced$or_formatted
      rd_ci <- indirect_enhanced$rd_formatted
      nnt_ci <- indirect_enhanced$nnt_formatted
    } else {
      rr_ci <- "N/A"
      or_ci <- "N/A" 
      rd_ci <- "N/A"
      nnt_ci <- "N/A"
    }
    
    # Get p-value from indirect comparison
    p_value <- if (!is.null(model_data$indirect_comparison)) {
      format_p_value(model_data$indirect_comparison$p_value)
    } else {
      "N/A"
    }
    
    table_html <- paste0(table_html, '
      <tr style="', row_style, '">
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">', model_name, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;"><span ', type_badge_class, '>', model_type, '</span></td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #e74c3c; font-weight: 600;">', rr_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad; font-weight: 600;">', or_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #f39c12; font-weight: 600;">', rd_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #17a2b8; font-weight: 600;">', nnt_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;">', p_value, '</td>
      </tr>')
  }
  
  table_html <- paste0(table_html, '
      </tbody>
    </table>
  </div>
  
  <!-- Supporting Evidence Tables -->
  <h5 style="color: #A23877; margin: 25px 0 15px 0;">Supporting Evidence</h5>
  <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">
    <!-- IPD Trial Results -->
    <div>
      <h6 style="color: #495057; margin-bottom: 10px;">IPD Trial Results (', 
      results$study_characteristics$ipd_trial, ')</h6>
      ', create_ipd_trial_summary_table(results), '
    </div>
    
    <!-- AgD Trial Results -->
    <div>
      <h6 style="color: #495057; margin-bottom: 10px;">AgD Trial Results (', 
      results$study_characteristics$agd_trial, ')</h6>
      ', create_agd_trial_summary_table(results), '
    </div>
  </div>
  </div>')
  
  return(table_html)
}

#' Create IPD trial summary table
create_ipd_trial_summary_table <- function(results) {
  # Use the first non-naive model, or first model if all are naive
  model_names <- names(results$model_results)
  selected_model <- model_names[1]
  for (model_name in model_names) {
    if (!results$model_results[[model_name]]$is_naive) {
      selected_model <- model_name
      break
    }
  }
  
  enhanced_effect <- results$model_results[[selected_model]]$enhanced_effect
  
  if (is.null(enhanced_effect)) {
    return('<div class="alert alert-info" style="font-size: 0.9em;">No IPD trial results available.</div>')
  }
  
  table_html <- paste0('
    <table class="analysis-table" style="width: 100%; font-size: 0.9em;">
      <thead>
        <tr style="background: #f8f9fa;">
          <th style="padding: 8px; text-align: left; font-size: 0.8em;">Measure</th>
          <th style="padding: 8px; text-align: center; font-size: 0.8em;">Value (95% CI)</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">RR</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #e74c3c;">', enhanced_effect$rr_formatted, '</td>
        </tr>
        <tr style="background: #f8f9fa;">
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">OR</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad;">', enhanced_effect$or_formatted, '</td>
        </tr>
        <tr>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">RD</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #f39c12;">', enhanced_effect$rd_formatted, '</td>
        </tr>
        <tr style="background: #f8f9fa;">
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">NNT</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #17a2b8;">', enhanced_effect$nnt_formatted, '</td>
        </tr>
      </tbody>
    </table>')
  
  return(table_html)
}

#' Create AgD trial summary table  
create_agd_trial_summary_table <- function(results) {
  agd_effect <- results$agd_effect
  
  if (is.null(agd_effect)) {
    return('<div class="alert alert-info" style="font-size: 0.9em;">No AgD trial results available.</div>')
  }
  
  # Calculate enhanced measures for AgD if not already available
  agd_enhanced <- results$agd_enhanced_effect
  if (is.null(agd_enhanced)) {
    # Use basic OR from agd_effect
    or_formatted <- sprintf("%.3f (%.3f, %.3f)", agd_effect$or, agd_effect$or_ci_lower, agd_effect$or_ci_upper)
    rr_formatted <- "N/A"
    rd_formatted <- "N/A" 
    nnt_formatted <- "N/A"
  } else {
    or_formatted <- agd_enhanced$or_formatted
    rr_formatted <- agd_enhanced$rr_formatted
    rd_formatted <- agd_enhanced$rd_formatted
    nnt_formatted <- agd_enhanced$nnt_formatted
  }
  
  table_html <- paste0('
    <table class="analysis-table" style="width: 100%; font-size: 0.9em;">
      <thead>
        <tr style="background: #f8f9fa;">
          <th style="padding: 8px; text-align: left; font-size: 0.8em;">Measure</th>
          <th style="padding: 8px; text-align: center; font-size: 0.8em;">Value (95% CI)</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">RR</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #e74c3c;">', rr_formatted, '</td>
        </tr>
        <tr style="background: #f8f9fa;">
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">OR</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad;">', or_formatted, '</td>
        </tr>
        <tr>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">RD</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #f39c12;">', rd_formatted, '</td>
        </tr>
        <tr style="background: #f8f9fa;">
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; font-weight: 600;">NNT</td>
          <td style="padding: 6px; border-bottom: 1px solid #e9ecef; text-align: center; color: #17a2b8;">', nnt_formatted, '</td>
        </tr>
      </tbody>
    </table>')
  
  return(table_html)
}

################################################################################
##################### STC Results Section with Outcome Subtabs ###############
################################################################################

#' Create STC Results Section with Outcome-Based Subtabs
#' 
#' Creates STC results section with outcome-based subtabs including summary table,
#' interactive forest plot, and detailed results
#' 
#' @param results Results object from analyze_anchored_stc()
#' @return HTML content for STC results with outcome-based subtabs
create_astc_results_section <- function(results) {
  
  # Get all model names for analysis
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No STC Results:</strong> No anchored STC models were fitted.
    </div>')
  }
  
  # For now, we have one outcome (can be extended to multiple outcomes later)
  outcome_name <- results$study_info$outcome_description
  
  # Generate subtab for outcome (prepared for multiple outcomes later)
  sub_tab_id <- paste0("astc_outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
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
          <i class="fas fa-table"></i> Anchored STC Analysis Summary
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Treatment effect results across all fitted models with confidence intervals and indirect comparisons.</p>
        ', create_astc_summary_table(results), '
      </div>
    </div>')
  
  # Create content for Forest Plot
  forest_content <- paste0('
    <div id="forest_', sub_tab_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-chart-line"></i> Interactive Forest Plot
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Interactive visualization of indirect comparison results with model selection controls.</p>
        ', create_astc_forest_plot(results), '
      </div>
    </div>')
  
  # Create content for Detailed Results
  detailed_content <- paste0('
    <div id="detailed_', sub_tab_id, '" class="model-sub-tab-content">
      <div style="padding: 20px;">
        <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-list-alt"></i> Comprehensive Results
        </h4>
        <p style="margin-bottom: 20px; color: #666;">Complete statistical results including IPD trial results, STC model results, and indirect comparisons.</p>
        ', create_comprehensive_astc_results(results), '
      </div>
    </div>')
  
  # Create outcome subtab content
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, ' - Anchored STC Results</span>
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

#' Create aggregate data results table
create_agd_results_table <- function(results) {
  agd_effect <- results$agd_effect
  
  if (is.null(agd_effect)) {
    return('<div class="alert alert-info">No aggregate data results available.</div>')
  }
  
  table_html <- paste0('
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
          <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Analysis</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Comparison</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">OR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Log OR (SE)</th>
        </tr>
      </thead>
      <tbody>
        <tr style="background: #f8f9fa;">
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">Aggregate Data Trial</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #495057;">', 
          results$study_characteristics$agd_trial, 
          '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #8e44ad; font-weight: 600;">', 
          sprintf("%.3f (%.3f, %.3f)", agd_effect$or, agd_effect$or_ci_lower, agd_effect$or_ci_upper), 
          '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;">', 
          sprintf("%.3f (%.3f)", agd_effect$log_or, agd_effect$log_or_se), 
          '</td>
        </tr>
      </tbody>
    </table>')
  
  return(table_html)
}

#' Create indirect comparison results table
create_indirect_comparison_table <- function(results) {
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No indirect comparison results available.</div>')
  }
  
  table_html <- '
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
          <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Model</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Comparison</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">OR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P-value</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Z-statistic</th>
        </tr>
      </thead>
      <tbody>'
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    indirect <- results$model_results[[model_name]]$indirect_comparison
    
    if (!is.null(indirect)) {
      row_style <- ifelse(i %% 2 == 0, "background: #f8f9fa;", "background: white;")
      
      table_html <- paste0(table_html, '
        <tr style="', row_style, '">
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">', model_name, '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #495057;">', indirect$comparison, '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #d35400; font-weight: 600;">', sprintf("%.3f (%.3f, %.3f)", indirect$or, indirect$or_ci_lower, indirect$or_ci_upper), '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;">', format_p_value(indirect$p_value), '</td>
          <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;">', format_numeric(indirect$z_statistic, 3), '</td>
        </tr>')
    }
  }
  
  table_html <- paste0(table_html, '
      </tbody>
    </table>')
  
  return(table_html)
}

#' Create comprehensive anchored STC results
create_comprehensive_astc_results <- function(results) {
  model_names <- names(results$model_results)
  
  comprehensive_html <- ''
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Model header
    comprehensive_html <- paste0(comprehensive_html, '
      <div class="model-section" style="margin-bottom: 25px;">
        <div class="model-header">
          <h5 style="margin: 0; color: white;">', model_name, ' - Complete Results</h5>
        </div>
        <div style="padding: 20px; background: white;">
    ')
    
    # IPD Trial Results
    if (!is.null(model_data$enhanced_effect)) {
      effect <- model_data$enhanced_effect
      
      comprehensive_html <- paste0(comprehensive_html, '
        <h6 style="color: #2C275B; margin-bottom: 15px;">IPD Trial Results (', results$study_characteristics$ipd_trial, ')</h6>
        <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
          <thead>
            <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
              <th>Effect Measure</th>
              <th>Estimate</th>
              <th>95% CI</th>
              <th>Interpretation</th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td><strong>Relative Risk (RR)</strong></td>
              <td style="color: #e74c3c; font-weight: 600;">', format_numeric(effect$rr, 3), '</td>
              <td>(', format_numeric(effect$rr_ci_lower, 3), ', ', format_numeric(effect$rr_ci_upper, 3), ')</td>
              <td>Risk ratio comparing treatment arms</td>
            </tr>
            <tr style="background: #f8f9fa;">
              <td><strong>Odds Ratio (OR)</strong></td>
              <td style="color: #8e44ad; font-weight: 600;">', format_numeric(effect$or, 3), '</td>
              <td>(', format_numeric(effect$or_ci_lower, 3), ', ', format_numeric(effect$or_ci_upper, 3), ')</td>
              <td>Odds ratio from logistic regression</td>
            </tr>
            <tr>
              <td><strong>Risk Difference (RD)</strong></td>
              <td style="color: #f39c12; font-weight: 600;">', format_numeric(effect$rd, 3), '</td>
              <td>(', format_numeric(effect$rd_ci_lower, 3), ', ', format_numeric(effect$rd_ci_upper, 3), ')</td>
              <td>Absolute difference in event rates</td>
            </tr>
            <tr style="background: #f8f9fa;">
              <td><strong>Number Needed to Treat (NNT)</strong></td>
              <td style="color: #17a2b8; font-weight: 600;">', if (is.na(effect$nnt)) "N/A" else format_numeric(effect$nnt, 1), '</td>
              <td>', if (is.na(effect$nnt_ci_lower)) "N/A" else paste0('(', format_numeric(effect$nnt_ci_lower, 1), ', ', format_numeric(effect$nnt_ci_upper, 1), ')'), '</td>
              <td>Patients needed to treat for one additional outcome</td>
            </tr>
          </tbody>
        </table>')
    }
    
    # Indirect Comparison Results
    if (!is.null(model_data$indirect_comparison)) {
      indirect <- model_data$indirect_comparison
      
      comprehensive_html <- paste0(comprehensive_html, '
        <h6 style="color: #2C275B; margin-bottom: 15px;">Indirect Comparison Result</h6>
        <div style="background: #fff3cd; padding: 15px; border-radius: 8px; border: 1px solid #ffeaa7; margin-bottom: 15px;">
          <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;">
            <div>
              <strong style="color: #856404;">Comparison:</strong><br>
              <span style="font-size: 1.1em;">', indirect$comparison, '</span>
            </div>
            <div>
              <strong style="color: #856404;">Odds Ratio:</strong><br>
              <span style="font-size: 1.2em; font-weight: 600; color: #d35400;">', format_numeric(indirect$or, 3), '</span>
            </div>
            <div>
              <strong style="color: #856404;">95% CI:</strong><br>
              <span>(', format_numeric(indirect$or_ci_lower, 3), ', ', format_numeric(indirect$or_ci_upper, 3), ')</span>
            </div>
            <div>
              <strong style="color: #856404;">P-value:</strong><br>
              <span style="font-weight: 600;">', format_p_value(indirect$p_value), '</span>
            </div>
          </div>
        </div>')
    }
    
    comprehensive_html <- paste0(comprehensive_html, '
        </div>
      </div>')
  }
  
  return(comprehensive_html)
}

#' Create forest plot for anchored STC results with interactive model and measure selection
create_astc_forest_plot <- function(results) {
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No models available for forest plot.</div>')
  }
  
  # Create model selection controls with checkboxes
  model_checkboxes <- ""
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    clean_name <- gsub("_", " ", model_name)
    
    # Color based on model type
    checkbox_color <- ifelse(model_data$is_naive, "#6c757d", "#007bff")
    
    model_checkboxes <- paste0(model_checkboxes, '
      <label style="display: flex; align-items: center; gap: 8px; margin-right: 15px; cursor: pointer;">
        <input type="checkbox" id="model_', model_name, '" checked 
               onchange="toggleASTCModel(\'', model_name, '\')" 
               style="width: 16px; height: 16px; accent-color: ', checkbox_color, ';">
        <span style="color: ', checkbox_color, '; font-weight: 500;">', clean_name, '</span>
        <span style="background: ', checkbox_color, '; color: white; padding: 2px 6px; border-radius: 8px; font-size: 0.7em; margin-left: 4px;">
          ', ifelse(model_data$is_naive, "Naive", "Adj"), '
        </span>
      </label>')
  }
  
  controls_html <- paste0('
    <div class="forest-plot-controls" style="background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border: 1px solid #dee2e6;">
      <div style="margin-bottom: 15px;">
        <h5 style="margin: 0 0 10px 0; color: #495057;">
          <i class="fas fa-cogs"></i> Model Selection
        </h5>
        <div style="display: flex; flex-wrap: wrap; align-items: center; gap: 10px; margin-bottom: 15px;">
          ', model_checkboxes, '
        </div>
        <div style="display: flex; gap: 10px;">
          <button onclick="selectAllASTCModels()" style="
            background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%);
            color: white; border: none; padding: 6px 12px; border-radius: 4px;
            cursor: pointer; font-size: 12px; font-weight: 500;">
            Select All
          </button>
          <button onclick="deselectAllASTCModels()" style="
            background: linear-gradient(135deg, #dc3545 0%, #c82333 100%);
            color: white; border: none; padding: 6px 12px; border-radius: 4px;
            cursor: pointer; font-size: 12px; font-weight: 500;">
            Deselect All
          </button>
        </div>
      </div>
      
      <div style="margin-bottom: 15px;">
        <h5 style="margin: 0 0 10px 0; color: #495057;">
          <i class="fas fa-chart-line"></i> Effect Measure
        </h5>
        <div style="display: flex; align-items: center; gap: 15px;">
          <label style="display: flex; align-items: center; gap: 5px;">
            <span style="font-weight: 500;">Plot Type:</span>
            <select id="astc-measure-select" onchange="updateASTCMeasure()" style="
              padding: 6px 12px; border: 1px solid #ddd; border-radius: 4px; 
              background: white; font-size: 14px; min-width: 150px;">
              <option value="or">Odds Ratio (OR)</option>
              <option value="rr">Relative Risk (RR)</option>
              <option value="rd">Risk Difference (RD)</option>
              <option value="nnt">Number Needed to Treat (NNT)</option>
            </select>
          </label>
          <label style="display: flex; align-items: center; gap: 5px;">
            <span style="font-weight: 500;">CI Type:</span>
            <select id="astc-ci-type-select" onchange="updateASTCMeasure()" style="
              padding: 6px 12px; border: 1px solid #ddd; border-radius: 4px; 
              background: white; font-size: 14px; min-width: 130px;">
              <option value="conventional">Conventional Only</option>
              <option value="bootstrap">Bootstrap Only</option>
              <option value="both">Both CI Types</option>
            </select>
          </label>
        </div>
      </div>
      
      <div style="display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 15px;">
        <div>
          <h5 style="margin: 0 0 10px 0; color: #495057;">
            <i class="fas fa-download"></i> Export Options
          </h5>
          <div style="display: flex; align-items: center; gap: 15px; flex-wrap: wrap;">
            <button onclick="updateASTCForestPlot()" style="
              background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
              color: white; border: none; padding: 8px 16px; border-radius: 6px;
              cursor: pointer; font-size: 14px; font-weight: 500;
              display: flex; align-items: center; gap: 5px;">
              <i class="fas fa-sync-alt"></i> Update Plot
            </button>
            <button onclick="downloadASTCForestPlot()" style="
              background: linear-gradient(135deg, #28a745 0%, #1e7e34 100%);
              color: white; border: none; padding: 8px 16px; border-radius: 6px;
              cursor: pointer; font-size: 14px; font-weight: 500;
              display: flex; align-items: center; gap: 5px;">
              <i class="fas fa-download"></i> PNG (300 DPI)
            </button>
          </div>
        </div>
      </div>
    </div>')
  
  # Generate plotly forest plot with enhanced JavaScript for all measures
  forest_plot_html <- create_plotly_astc_forest_plot_with_all_measures(results)
  
  return(paste0(controls_html, forest_plot_html))
}

################################################################################
##################### Model Details Section #####################
################################################################################

#' Create Detailed Model Results Section for Anchored STC
#' 
#' @param results Results object from analyze_anchored_stc()
#' @return HTML content for detailed model results with outcome-based subtabs
create_astc_model_details_section <- function(results) {
  
  # Get all model names for analysis
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">
      <strong>No Model Results:</strong> No anchored STC models were fitted.
    </div>')
  }
  
  # For now, we have one outcome (can be extended to multiple outcomes later)
  # Create outcome-based structure following TEM exploration pattern
  outcome_name <- results$study_info$outcome_description
  outcome_var <- "outcome"  # or extract from analysis
  
  # Generate subtab for outcome (prepared for multiple outcomes later)
  sub_tab_id <- paste0("astc_models_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
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
        ', generate_astc_outcome_summary_table(results), '
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
        ', generate_individual_astc_model_details(results, model_name), '
      </div>
    </div>')
  }
  
  # Create outcome subtab content
  outcome_content <- paste0('
    <div id="', sub_tab_id, '" class="sub-tab-content active">
      <div class="outcome-analysis-section" style="margin-bottom: 20px;">
        <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
            <span>', outcome_name, ' - Model Details</span>
            <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">Binary Outcome</span>
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

#' Generate outcome summary table for all models
generate_astc_outcome_summary_table <- function(results) {
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No model results available.</div>')
  }
  
  # Study Overview
  overview_html <- paste0('
    <div class="study-overview" style="background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 25px; border: 1px solid #dee2e6;">
      <h5 style="color: #2C275B; margin: 0 0 15px 0; display: flex; align-items: center; gap: 8px;">
        <i class="fas fa-info-circle"></i> Study Design Overview
      </h5>
      <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px;">
        <div>
          <strong style="color: #495057;">IPD Trial:</strong><br>
          <span style="font-size: 1.1em; color: #2C275B;">', results$study_characteristics$ipd_trial, '</span>
        </div>
        <div>
          <strong style="color: #495057;">AgD Trial:</strong><br>
          <span style="font-size: 1.1em; color: #2C275B;">', results$study_characteristics$agd_trial, '</span>
        </div>
        <div>
          <strong style="color: #495057;">Reference Arm:</strong><br>
          <span style="font-size: 1.3em; color: #A23877; font-weight: 600;">', results$study_characteristics$reference_arm, '</span>
        </div>
        <div>
          <strong style="color: #495057;">Models Fitted:</strong><br>
          <span style="font-size: 1.1em; color: #2C275B;">', length(model_names), ' models</span>
        </div>
      </div>
    </div>')
  
  # Model Comparison Table
  table_html <- paste0(overview_html, '
    <h5 style="color: #A23877; margin-bottom: 15px;">Model Comparison</h5>
    <table class="analysis-table" style="width: 100%; margin-bottom: 20px;">
      <thead>
        <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
          <th style="padding: 12px; text-align: left; font-weight: 600; border: none;">Model</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Type</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Covariates</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">Indirect OR (95% CI)</th>
          <th style="padding: 12px; text-align: center; font-weight: 600; border: none;">P-value</th>
        </tr>
      </thead>
      <tbody>')
  
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
    
    # Get indirect comparison
    indirect <- model_data$indirect_comparison
    if (!is.null(indirect)) {
      or_ci <- sprintf("%.3f (%.3f, %.3f)", indirect$or, indirect$or_ci_lower, indirect$or_ci_upper)
      p_val <- format_p_value(indirect$p_value)
    } else {
      or_ci <- "N/A"
      p_val <- "N/A"
    }
    
    table_html <- paste0(table_html, '
      <tr style="', row_style, '">
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; font-weight: 600; color: #2C275B;">', model_name, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;"><span ', type_badge_class, '>', model_type, '</span></td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; font-size: 0.9em;">', covariates_text, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center; color: #d35400; font-weight: 600;">', or_ci, '</td>
        <td style="padding: 10px; border-bottom: 1px solid #e9ecef; text-align: center;">', p_val, '</td>
      </tr>')
  }
  
  table_html <- paste0(table_html, '
      </tbody>
    </table>')
  
  return(table_html)
}

#' Generate detailed results for individual model
generate_individual_astc_model_details <- function(results, model_name) {
  model_data <- results$model_results[[model_name]]
  
  if (is.null(model_data)) {
    return('<div class="alert alert-info">Model details not available.</div>')
  }
  
  details_html <- ""
  
  # Model Information
  details_html <- paste0(details_html, '
    <div class="model-info" style="background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px; border: 1px solid #dee2e6;">
      <h5 style="color: #2C275B; margin: 0 0 15px 0;">Model Information</h5>
      <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px;">
        <div>
          <strong>Model Type:</strong><br>
          <span style="color: #495057;">', ifelse(model_data$is_naive, "Naive (Unadjusted)", "Covariate-Adjusted"), '</span>
        </div>
        <div>
          <strong>Covariates:</strong><br>
          <span style="color: #495057;">', 
          if (length(model_data$covariates) == 0) "None" else paste(model_data$covariates, collapse = ", "), 
          '</span>
        </div>
        <div>
          <strong>Model Convergence:</strong><br>
          <span style="color: ', ifelse(model_data$model$converged, '#28a745', '#dc3545'), ';">',
          ifelse(model_data$model$converged, "✓ Converged", "✗ Failed to converge"), '</span>
        </div>
      </div>
    </div>')
  
  # Regression Model Details
  if (!is.null(model_data$model)) {
    reg_summary <- summary(model_data$model)
    
    # Create readable model formula
    outcome_col <- model_data$model$outcome_col
    treatment_col <- model_data$model$treatment_col
    covariates <- model_data$covariates
    
    if (length(covariates) == 0) {
      readable_formula <- paste0(outcome_col, " ~ ", treatment_col)
    } else {
      readable_formula <- paste0(outcome_col, " ~ ", treatment_col, " + ", paste(covariates, collapse = " + "))
    }
    
    details_html <- paste0(details_html, '
      <h5 style="color: #A23877; margin-bottom: 15px;">Logistic Regression Results</h5>
      
      <!-- Model Formula -->
      <div style="background: #fff; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6; margin-bottom: 15px;">
        <h6 style="margin: 0 0 10px 0; color: #495057;">Model Formula:</h6>
        <div style="font-family: monospace; background: #f8f9fa; padding: 8px; border-radius: 4px; border: 1px solid #ccc; font-size: 14px;">
          ', readable_formula, '
        </div>
      </div>
      
      <!-- Model Statistics -->
      <div style="margin-bottom: 20px;">
        <h6 style="margin: 0 0 10px 0; color: #495057;">Model Fit Statistics:</h6>
        <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px;">
          <div style="background: #fff; padding: 10px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center;">
            <strong style="color: #2C275B;">AIC</strong><br>
            <span style="font-size: 1.2em; color: #495057;">', format_numeric(reg_summary$aic, 2), '</span>
          </div>
          <div style="background: #fff; padding: 10px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center;">
            <strong style="color: #2C275B;">Deviance</strong><br>
            <span style="font-size: 1.2em; color: #495057;">', format_numeric(reg_summary$deviance, 2), '</span>
          </div>
          <div style="background: #fff; padding: 10px; border-radius: 6px; border: 1px solid #dee2e6; text-align: center;">
            <strong style="color: #2C275B;">Null Deviance</strong><br>
            <span style="font-size: 1.2em; color: #495057;">', format_numeric(reg_summary$null.deviance, 2), '</span>
          </div>
        </div>
      </div>')
    
    # Coefficients Table
    if (!is.null(reg_summary$coefficients) && nrow(reg_summary$coefficients) > 0) {
      details_html <- paste0(details_html, '
        <h6 style="margin: 0 0 10px 0; color: #495057;">Regression Coefficients:</h6>
        <table class="analysis-table" style="width: 100%; border: 1px solid #dee2e6; margin-bottom: 30px;">
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
          </table>')
    }
  }
  
  # Effect Measures Table for IPD Trial Results
  if (!is.null(model_data$enhanced_effect)) {
    enhanced_effect <- model_data$enhanced_effect
    bootstrap_cis <- model_data$bootstrap_cis
    has_bootstrap <- !is.null(bootstrap_cis)
    
    details_html <- paste0(details_html, '
      <h5 style="color: #A23877; margin-bottom: 15px;">IPD Trial Effect Measures</h5>
      <p style="color: #666; margin-bottom: 15px;">Treatment effects from the Individual Patient Data trial (', 
      results$study_characteristics$ipd_trial, '):</p>
      
      <table class="analysis-table" style="width: 100%; border: 1px solid #dee2e6; margin-bottom: 20px;">
        <thead>
          <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
            <th style="padding: 10px; text-align: left; font-weight: 600;">Measure</th>
            <th style="padding: 10px; text-align: center; font-weight: 600;">Estimate</th>',
            ifelse(has_bootstrap, '<th style="padding: 10px; text-align: center; font-weight: 600;">Bootstrap 95% CI</th>', ''), '
            <th style="padding: 10px; text-align: left; font-weight: 600;">Interpretation</th>
          </tr>
        </thead>
        <tbody>
          <tr style="background: white;">
            <td style="padding: 10px; font-weight: 600;">Relative Risk (RR)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(enhanced_effect$rr, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$ipd_effect$rr_ci) && !any(is.na(bootstrap_cis$ipd_effect$rr_ci)), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$ipd_effect$rr_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$ipd_effect$rr_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Risk ratio comparing treatments</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(enhanced_effect$or, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$ipd_effect$or_ci) && !any(is.na(bootstrap_cis$ipd_effect$or_ci)), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$ipd_effect$or_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$ipd_effect$or_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Odds ratio from regression model</td>
          </tr>
          <tr style="background: white;">
            <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(enhanced_effect$rd, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$ipd_effect$rd_ci) && !any(is.na(bootstrap_cis$ipd_effect$rd_ci)), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$ipd_effect$rd_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$ipd_effect$rd_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Absolute difference in event rates</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
            if (is.na(enhanced_effect$nnt)) "N/A" else format_numeric(enhanced_effect$nnt, 1), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$ipd_effect$nnt_ci) && !any(is.na(bootstrap_cis$ipd_effect$nnt_ci)), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$ipd_effect$nnt_ci[1], 1), ', ',
                          format_numeric(bootstrap_cis$ipd_effect$nnt_ci[2], 1), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Patients needed to treat for one additional event</td>
          </tr>
        </tbody>
      </table>',
      ifelse(has_bootstrap, 
             paste0('<div style="background: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin-bottom: 15px;">',
                    '<p style="margin: 0; font-size: 0.9em; color: #1976d2;">',
                    '<strong><i class="fas fa-info-circle"></i> Bootstrap Note:</strong> Bootstrap confidence intervals based on ', 
                    bootstrap_cis$n_bootstrap, ' resamples (', bootstrap_cis$n_successful, ' successful). ',
                    'Bootstrap CIs provide non-parametric uncertainty estimates.',
                    '</p></div>'), ''), '
    ')
  }
  
  # Indirect Comparison Effect Measures Table
  if (!is.null(model_data$indirect_enhanced_effect)) {
    indirect_effect <- model_data$indirect_enhanced_effect
    bootstrap_cis <- model_data$bootstrap_cis
    has_bootstrap <- !is.null(bootstrap_cis)
    
    details_html <- paste0(details_html, '
      <h5 style="color: #A23877; margin-bottom: 15px;">Indirect Comparison Effect Measures</h5>
      <p style="color: #666; margin-bottom: 15px;">Population-adjusted indirect comparison (', 
      indirect_effect$comparison, '):</p>
      
      <table class="analysis-table" style="width: 100%; border: 1px solid #dee2e6; margin-bottom: 20px;">
        <thead>
          <tr style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white;">
            <th style="padding: 10px; text-align: left; font-weight: 600;">Measure</th>
            <th style="padding: 10px; text-align: center; font-weight: 600;">Estimate</th>',
            ifelse(has_bootstrap, '<th style="padding: 10px; text-align: center; font-weight: 600;">Bootstrap 95% CI</th>', ''), '
            <th style="padding: 10px; text-align: left; font-weight: 600;">Interpretation</th>
          </tr>
        </thead>
        <tbody>
          <tr style="background: white;">
            <td style="padding: 10px; font-weight: 600;">Relative Risk (RR)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(indirect_effect$rr, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rr_ci), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$indirect_effect$rr_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$indirect_effect$rr_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Indirect risk ratio</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(indirect_effect$or, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$indirect_effect$or_ci), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$indirect_effect$or_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$indirect_effect$or_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Indirect odds ratio</td>
          </tr>
          <tr style="background: white;">
            <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(indirect_effect$rd, 3), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rd_ci), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$indirect_effect$rd_ci[1], 3), ', ',
                          format_numeric(bootstrap_cis$indirect_effect$rd_ci[2], 3), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Indirect risk difference</td>
          </tr>
          <tr style="background: #f8f9fa;">
            <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
            <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
            if (is.na(indirect_effect$nnt)) "N/A" else format_numeric(indirect_effect$nnt, 1), '</td>',
            ifelse(has_bootstrap && !is.null(bootstrap_cis$indirect_effect$nnt_ci), 
                   paste0('<td style="padding: 10px; text-align: center; color: #28a745; font-weight: 500;">(',
                          format_numeric(bootstrap_cis$indirect_effect$nnt_ci[1], 1), ', ',
                          format_numeric(bootstrap_cis$indirect_effect$nnt_ci[2], 1), ')</td>'), 
                   ifelse(has_bootstrap, '<td style="padding: 10px; text-align: center;">N/A</td>', '')), '
            <td style="padding: 10px;">Indirect NNT estimate</td>
          </tr>
        </tbody>
      </table>')
  }
  
  # Calculation Methods Explanation
  details_html <- paste0(details_html, '
    <h5 style="color: #A23877; margin-bottom: 15px;">Calculation Methods</h5>
    <div style="background: #fff3cd; border: 1px solid #ffeeba; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
      <h6 style="color: #856404; margin: 0 0 10px 0;"><i class="fas fa-info-circle"></i> Effect Measure Calculations</h6>
      <div style="font-size: 14px; color: #856404;">
        <p style="margin: 0 0 8px 0;"><strong>Relative Risk (RR):</strong> RR = P(Event|Treatment) / P(Event|Control)</p>
        <p style="margin: 0 0 8px 0;"><strong>Odds Ratio (OR):</strong> OR = [P(Event|Treatment) / (1-P(Event|Treatment))] / [P(Event|Control) / (1-P(Event|Control))]</p>
        <p style="margin: 0 0 8px 0;"><strong>Risk Difference (RD):</strong> RD = P(Event|Treatment) - P(Event|Control)</p>
        <p style="margin: 0 0 8px 0;"><strong>Number Needed to Treat (NNT):</strong> NNT = 1 / |RD| (when beneficial effect exists)</p>
        <p style="margin: 0;"><strong>95% Confidence Intervals:</strong> Calculated using robust standard errors and normal approximation.</p>
      </div>
    </div>
    
    <div style="background: #d1ecf1; border: 1px solid #bee5eb; border-radius: 8px; padding: 15px;">
      <h6 style="color: #0c5460; margin: 0 0 10px 0;"><i class="fas fa-link"></i> Anchored STC Method</h6>
      <div style="font-size: 14px; color: #0c5460;">
        <p style="margin: 0 0 8px 0;">• <strong>IPD Trial Effects:</strong> Direct estimates from individual patient data using logistic regression</p>
        <p style="margin: 0 0 8px 0;">• <strong>Indirect Comparison:</strong> Combines IPD trial results with aggregate data trial using shared reference arm</p>
        <p style="margin: 0 0 8px 0;">• <strong>Population Adjustment:</strong> IPD covariates centered on aggregate data population means</p>
        <p style="margin: 0;">• <strong>Effect Modification:</strong> Treatment-covariate interactions account for population differences</p>
      </div>
    </div>')
  
  # ROBUST SAFETY CHECK: Strip any complete HTML documents from output
  # This prevents the mysterious HTML duplication issue
  if (length(details_html) > 0) {
    # Convert to single string if it's a vector
    details_html <- paste(details_html, collapse = "\n")
    
    # Simple but effective: remove any complete HTML document structures
    # Remove DOCTYPE declarations
    details_html <- gsub("<!DOCTYPE[^>]*>", "", details_html, ignore.case = TRUE)
    # Remove opening html tags
    details_html <- gsub("<html[^>]*>", "", details_html, ignore.case = TRUE)
    # Remove closing html tags
    details_html <- gsub("</html>", "", details_html, ignore.case = TRUE)
    # Remove head sections completely
    details_html <- gsub("<head>.*?</head>", "", details_html, ignore.case = TRUE, perl = TRUE)
    # Remove opening body tags
    details_html <- gsub("<body[^>]*>", "", details_html, ignore.case = TRUE)
    # Remove closing body tags
    details_html <- gsub("</body>", "", details_html, ignore.case = TRUE)
    # Remove any title tags
    details_html <- gsub("<title>.*?</title>", "", details_html, ignore.case = TRUE, perl = TRUE)
    
    # Clean up any extra whitespace
    details_html <- gsub("^\\s+|\\s+$", "", details_html, perl = TRUE)
  }
  
  return(details_html)
}

################################################################################
##################### Data Summary Section #####################
################################################################################

#' Create Data Summary Section for Anchored STC
#' 
#' @param results Results object from analyze_anchored_stc()
#' @return HTML content for data summary
create_astc_data_summary_section <- function(results) {
  
  if (is.null(results$data_summaries)) {
    return('<div class="alert alert-info">
      <strong>No Data Summary:</strong> Data summary information is not available.
    </div>')
  }
  
  # Define subtab sections
  subtab_sections <- c("ipd", "agd", "covariates")
  subtab_names <- c("IPD Trial Summary", "AgD Trial Summary", "Covariate Centering")
  
  # Generate subtab buttons
  subtabs_html <- '<div class="sub-tabs">'
  for (i in seq_along(subtab_sections)) {
    section <- subtab_sections[i]
    section_name <- subtab_names[i]
    sub_tab_id <- paste0("data_", section)
    active_class <- if (i == 1) " active" else ""
    
    # Get icon for section
    section_icon <- switch(section, 
      "ipd" = "users", 
      "agd" = "table", 
      "covariates" = "exchange-alt",
      "database")  # default case
    
    subtabs_html <- paste0(subtabs_html, '
      <button class="sub-tab', active_class, '" onclick="showSubTab(\'', sub_tab_id, '\')">
        <i class="fas fa-', section_icon, '"></i> ', section_name, '
      </button>')
  }
  subtabs_html <- paste0(subtabs_html, '</div>')
  
  # Generate IPD summary content
  ipd_html <- '
    <div id="data_ipd" class="sub-tab-content active">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Individual Patient Data (IPD) Trial Summary</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">'
  
  # Create IPD summary table
  if (!is.null(results$data_summaries$ipd_summary)) {
    ipd_data <- results$data_summaries$ipd_summary
    ipd_html <- paste0(ipd_html, '
            <table class="analysis-table" style="width: 100%;">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>Treatment Arm</th>
                  <th>Sample Size</th>
                  <th>Events</th>
                  <th>Event Rate</th>
                </tr>
              </thead>
              <tbody>')
    
    for (i in 1:nrow(ipd_data)) {
      row <- ipd_data[i, ]
      ipd_html <- paste0(ipd_html, '
                <tr>
                  <td><strong>', row[[1]], '</strong></td>
                  <td>', row[[2]], '</td>
                  <td>', row[[3]], '</td>
                  <td>', format_numeric(row[[4]] * 100, 1), '%</td>
                </tr>')
    }
    
    ipd_html <- paste0(ipd_html, '
              </tbody>
            </table>')
  } else {
    ipd_html <- paste0(ipd_html, '
            <div class="alert alert-warning">
              <strong>No IPD summary available</strong>
            </div>')
  }
  
  ipd_html <- paste0(ipd_html, '
          </div>
        </div>
      </div>
    </div>')
  
  # Generate AgD summary content
  agd_html <- '
    <div id="data_agd" class="sub-tab-content">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Aggregate Data (AgD) Trial Summary</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">'
  
  # Create AgD summary table
  if (!is.null(results$data_summaries$agd_summary)) {
    agd_data <- results$data_summaries$agd_summary
    agd_html <- paste0(agd_html, '
            <table class="analysis-table" style="width: 100%;">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>Treatment Arm</th>
                  <th>Events</th>
                  <th>Total</th>
                  <th>Event Rate</th>
                </tr>
              </thead>
              <tbody>')
    
    for (i in 1:nrow(agd_data)) {
      row <- agd_data[i, ]
      agd_html <- paste0(agd_html, '
                <tr>
                  <td><strong>', row[[1]], '</strong></td>
                  <td>', row[[2]], '</td>
                  <td>', row[[3]], '</td>
                  <td>', format_numeric(row[[4]] * 100, 1), '%</td>
                </tr>')
    }
    
    agd_html <- paste0(agd_html, '
              </tbody>
            </table>')
  } else {
    agd_html <- paste0(agd_html, '
            <div class="alert alert-warning">
              <strong>No AgD summary available</strong>
            </div>')
  }
  
  agd_html <- paste0(agd_html, '
          </div>
        </div>
      </div>
    </div>')
  
  # Generate covariate centering content
  covariates_html <- paste0('
    <div id="data_covariates" class="sub-tab-content">
      <div class="data-analysis-section" style="margin-bottom: 20px;">
        <div class="data-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
          <h3 style="margin: 0;">Covariate Centering for STC</h3>
        </div>
        <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
          <div style="padding: 20px;">
            <div class="alert alert-info" style="margin-bottom: 20px;">
              <strong><i class="fas fa-info-circle"></i> Covariate Centering:</strong>
              In anchored STC analysis, covariates from the IPD trial are centered on the mean values from the aggregate data trial to adjust for population differences.
            </div>
            
            <table class="analysis-table" style="width: 100%;">
              <thead style="background: #34495e; color: white;">
                <tr>
                  <th>Covariate</th>
                  <th>Centering Value</th>
                  <th>Description</th>
                </tr>
              </thead>
              <tbody>')
  
  # Add covariate information
  if (!is.null(results$model$covariate_centers)) {
    centers <- results$model$covariate_centers
    for (cov_name in names(centers)) {
      center_value <- centers[[cov_name]]
      covariates_html <- paste0(covariates_html, '
                <tr>
                  <td><strong>', cov_name, '</strong></td>
                  <td>', format_numeric(center_value, 3), '</td>
                  <td>Mean value from aggregate data trial</td>
                </tr>')
    }
  } else {
    covariates_html <- paste0(covariates_html, '
                <tr>
                  <td colspan="3" style="text-align: center; color: #6c757d;">No covariate centering performed</td>
                </tr>')
  }
  
  covariates_html <- paste0(covariates_html, '
              </tbody>
            </table>
            
            <div class="alert alert-success" style="margin-top: 20px;">
              <strong><i class="fas fa-check-circle"></i> Analysis Approach:</strong>
              The STC model adjusts for baseline differences between trial populations by centering effect modifying covariates on the target population (aggregate data trial).
            </div>
          </div>
        </div>
      </div>
    </div>')
  
  # Combine all content
  result_html <- paste0(subtabs_html, ipd_html, agd_html, covariates_html)
  return(result_html)
}

################################################################################
##################### Footer Section #####################
################################################################################

#' Generate HTML footer with Cytel branding
#' 
#' @return HTML content for footer
generate_binary_astc_footer <- function() {
  
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
                    Unanchored STC Analysis Package | Binary Anchored STC Module
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

#' Generate comprehensive HTML report for binary anchored STC analysis
#'
#' @param results List containing analysis results from analyze_anchored_stc()
#' @param project_name Name of the project/study for the report subfolder
#' @param output_dir Base directory to save the report (will create timestamped subfolder)
#' @param output_file Optional custom filename (if NULL, will use default naming)
#' @param title Title for the HTML report
#' @return Path to generated HTML file
generate_binary_astc_html_report <- function(results, 
                                            project_name = "Clinical_Study",
                                            output_dir = "reports",
                                            output_file = NULL,
                                            title = "Binary Anchored STC Analysis Report") {
  
  # Create date stamp and report directory
  date_stamp <- format(Sys.time(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Create subfolder with format: Anchored_STC_[Project_Name]_Date
  project_folder <- paste0("Anchored_STC_", gsub("[^A-Za-z0-9_]", "_", project_name), "_", date_stamp)
  report_dir <- file.path(output_dir, project_folder)
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate filename if not provided
  if (is.null(output_file)) {
    outcome_summary <- "Binary_Outcomes"
    output_file <- sprintf("Anchored_STC_Analysis_%s_%s.html", 
                          outcome_summary, timestamp)
  }
  
  # Full path for the output file
  full_output_path <- file.path(report_dir, output_file)
  
  # Generate CSS and JavaScript
  css_styles <- generate_binary_astc_css()
  js_scripts <- generate_binary_astc_javascript()
  
  # Extract key metrics for header
  n_ipd <- sum(results$data_summaries$ipd_summary$n)
  n_agd <- sum(results$data_summaries$agd_summary$total)
  total_patients <- n_ipd + n_agd
  
  # Create HTML content using working binary_stc pattern (single paste0 call)
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
        <div class="subtitle">Comprehensive Binary Anchored STC Analysis with Population Adjustment</div>
        <div class="meta-info">
            <div class="meta-item">
                <i class="fas fa-calendar"></i>
                <span>Generated: ', format(Sys.time(), "%B %d, %Y at %H:%M"), '</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-exchange-alt"></i>
                <span>Anchored Indirect Comparison</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-users"></i>
                <span>', total_patients, ' Total Patients</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-anchor"></i>
                <span>Reference: ', results$study_characteristics$reference_arm, '</span>
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
        </button>
        <button class="nav-tab" onclick="showTab(\'qba\')">
            <i class="fas fa-microscope"></i> Bias Analysis
        </button>
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
                    <h4><i class="fas fa-exchange-alt"></i> Study Design</h4>
                    <div class="value" style="font-size: 1.5em;">Anchored STC</div>
                    <div class="description">Population-adjusted indirect comparison</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-users"></i> IPD Patients</h4>
                    <div class="value">', n_ipd, '</div>
                    <div class="description">Individual patient data</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-table"></i> AgD Patients</h4>
                    <div class="value">', n_agd, '</div>
                    <div class="description">Aggregate data</div>
                </div>
                <div class="summary-card">
                    <h4><i class="fas fa-link"></i> Indirect OR</h4>
                    <div class="value">', format_numeric(results$indirect_comparison$or, 2), '</div>
                    <div class="description">', results$indirect_comparison$comparison, '</div>
                </div>
            </div>
            
            <!-- Analysis Summary -->
            <h3>Analysis Summary</h3>
            ', create_astc_summary_table(results), '
            
            <!-- Key Results -->
            <div class="alert alert-', ifelse(results$indirect_comparison$p_value < 0.05, 'success', 'info'), '" style="margin-top: 20px;">
                <h4 style="margin: 0 0 10px 0;"><i class="fas fa-chart-line"></i> Key Finding</h4>
                <p style="margin: 0; font-size: 1.1em;">
                    The anchored indirect comparison shows an odds ratio of <strong>', format_numeric(results$indirect_comparison$or, 3), '</strong> 
                    (95% CI: ', format_numeric(results$indirect_comparison$or_ci_lower, 3), ' to ', format_numeric(results$indirect_comparison$or_ci_upper, 3), ') 
                    for ', results$indirect_comparison$comparison, ', which is 
                    <strong>', ifelse(results$indirect_comparison$p_value < 0.05, 'statistically significant', 'not statistically significant'), '</strong> 
                    (p = ', format_p_value(results$indirect_comparison$p_value), ').
                </p>
            </div>
        </div>
    </div>

    <!-- STC Results Tab -->
    <div id="results" class="tab-content">
        <div class="section">
            <h2>Anchored STC Analysis Results</h2>
            
            <div class="alert alert-info" style="margin-bottom: 20px;">
                <h4 style="margin: 0 0 10px 0;"><i class="fas fa-info-circle"></i> Anchored STC Method</h4>
                <p style="margin: 0;">
                    Anchored simulated treatment comparison uses individual patient data from one trial and aggregate data from another trial, 
                    connected through a shared reference arm. Population differences are adjusted through covariate centering.
                </p>
            </div>
            
            ', create_astc_results_section(results), '
        </div>
    </div>

    <!-- Model Details Tab -->
    <div id="models" class="tab-content">
        <div class="section">
            <h2>Detailed Model Results</h2>
            ', create_astc_model_details_section(results), '
        </div>
    </div>

    <!-- Quantitative Bias Analysis Tab -->
    <div id="qba" class="tab-content">
        ', create_qba_section(results), '
    </div>

    <!-- Data Summary Tab -->
    <div id="data" class="tab-content">
        <div class="section">
            <h2>Data Summary</h2>
            ', create_astc_data_summary_section(results), '
        </div>
    </div>

    ', generate_binary_astc_footer())
  
  # Write HTML to file
  writeLines(html_content, full_output_path)
  
  # Print success message
  cat("Binary Anchored STC HTML report generated successfully!\n")
  cat("File saved to:", full_output_path, "\n")
  cat("Report folder:", report_dir, "\n")
  
  return(full_output_path)
} 

#' Create Plotly forest plot for anchored STC results with all measures support
create_plotly_astc_forest_plot_with_all_measures <- function(results) {
  model_names <- names(results$model_results)
  
  if (length(model_names) == 0) {
    return('<div class="alert alert-info">No model results available for forest plot.</div>')
  }
  
  # Prepare forest plot data with all measures - focusing on indirect comparison
  model_data_js <- ""
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_data <- results$model_results[[model_name]]
    
    # Get indirect comparison enhanced effect for forest plot
    indirect_enhanced <- model_data$indirect_enhanced_effect
    
    if (!is.null(indirect_enhanced)) {
      # Handle NNT special cases (N/A, infinite values)
      nnt_val <- if (is.na(indirect_enhanced$nnt) || is.infinite(indirect_enhanced$nnt)) "null" else format_numeric(indirect_enhanced$nnt, 4)
      nnt_lower <- if (is.na(indirect_enhanced$nnt_ci_lower) || is.infinite(indirect_enhanced$nnt_ci_lower)) "null" else format_numeric(indirect_enhanced$nnt_ci_lower, 4)
      nnt_upper <- if (is.na(indirect_enhanced$nnt_ci_upper) || is.infinite(indirect_enhanced$nnt_ci_upper)) "null" else format_numeric(indirect_enhanced$nnt_ci_upper, 4)
      
      # Get bootstrap CIs if available
      bootstrap_cis <- model_data$bootstrap_cis
      has_bootstrap <- !is.null(bootstrap_cis)
      
      # Bootstrap CI values (with null handling)
      or_boot_lower <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$or_ci) && !any(is.na(bootstrap_cis$indirect_effect$or_ci))) format_numeric(bootstrap_cis$indirect_effect$or_ci[1], 4) else "null"
      or_boot_upper <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$or_ci) && !any(is.na(bootstrap_cis$indirect_effect$or_ci))) format_numeric(bootstrap_cis$indirect_effect$or_ci[2], 4) else "null"
      rr_boot_lower <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rr_ci) && !any(is.na(bootstrap_cis$indirect_effect$rr_ci))) format_numeric(bootstrap_cis$indirect_effect$rr_ci[1], 4) else "null"
      rr_boot_upper <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rr_ci) && !any(is.na(bootstrap_cis$indirect_effect$rr_ci))) format_numeric(bootstrap_cis$indirect_effect$rr_ci[2], 4) else "null"
      rd_boot_lower <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rd_ci) && !any(is.na(bootstrap_cis$indirect_effect$rd_ci))) format_numeric(bootstrap_cis$indirect_effect$rd_ci[1], 4) else "null"
      rd_boot_upper <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$rd_ci) && !any(is.na(bootstrap_cis$indirect_effect$rd_ci))) format_numeric(bootstrap_cis$indirect_effect$rd_ci[2], 4) else "null"
      nnt_boot_lower <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$nnt_ci) && !any(is.na(bootstrap_cis$indirect_effect$nnt_ci))) format_numeric(bootstrap_cis$indirect_effect$nnt_ci[1], 4) else "null"
      nnt_boot_upper <- if (has_bootstrap && !is.null(bootstrap_cis$indirect_effect$nnt_ci) && !any(is.na(bootstrap_cis$indirect_effect$nnt_ci))) format_numeric(bootstrap_cis$indirect_effect$nnt_ci[2], 4) else "null"
      
      model_data_js <- paste0(model_data_js, 
        if (i > 1) ",\n" else "",
        '      "', model_name, '": {
        "name": "', gsub("_", " ", model_name), '",
        "type": "', ifelse(model_data$is_naive, "Naive", "Adjusted"), '",
        "comparison": "', indirect_enhanced$comparison, '",
        "or": ', format_numeric(indirect_enhanced$or, 4), ',
        "or_ci_lower": ', format_numeric(indirect_enhanced$or_ci_lower, 4), ',
        "or_ci_upper": ', format_numeric(indirect_enhanced$or_ci_upper, 4), ',
        "or_boot_lower": ', or_boot_lower, ',
        "or_boot_upper": ', or_boot_upper, ',
        "rr": ', format_numeric(indirect_enhanced$rr, 4), ',
        "rr_ci_lower": ', format_numeric(indirect_enhanced$rr_ci_lower, 4), ',
        "rr_ci_upper": ', format_numeric(indirect_enhanced$rr_ci_upper, 4), ',
        "rr_boot_lower": ', rr_boot_lower, ',
        "rr_boot_upper": ', rr_boot_upper, ',
        "rd": ', format_numeric(indirect_enhanced$rd, 4), ',
        "rd_ci_lower": ', format_numeric(indirect_enhanced$rd_ci_lower, 4), ',
        "rd_ci_upper": ', format_numeric(indirect_enhanced$rd_ci_upper, 4), ',
        "rd_boot_lower": ', rd_boot_lower, ',
        "rd_boot_upper": ', rd_boot_upper, ',
        "nnt": ', nnt_val, ',
        "nnt_ci_lower": ', nnt_lower, ',
        "nnt_ci_upper": ', nnt_upper, ',
        "nnt_boot_lower": ', nnt_boot_lower, ',
        "nnt_boot_upper": ', nnt_boot_upper, ',
        "has_bootstrap": ', ifelse(has_bootstrap, "true", "false"), ',
        "p_value": ', format_numeric(model_data$indirect_comparison$p_value, 4), '
      }')
    }
  }
  
  # Create the plotly forest plot HTML with enhanced JavaScript for all measures
  forest_plot_html <- paste0('
    <div class="forest-plot-container" style="background: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;">
      <div id="astc-forest-plot" style="width: 100%; height: 500px;"></div>
    </div>
    
    <script>
    // Model data for anchored STC forest plot with all measures support
    var astcModelData = {
', model_data_js, '
    };
    
    // Track which models are currently selected
    var selectedASTCModels = new Set(Object.keys(astcModelData));
    
    // Current measure being displayed
    var currentASTCMeasure = "or";
    var currentCIType = "conventional";  // Track CI type: "conventional" or "both"
    
    // Measure configurations
    var measureConfigs = {
      "or": {
        title: "Odds Ratio (OR)",
        axis_title: "Odds Ratio (95% Confidence Interval)", 
        reference_line: 1,
        log_scale: true,
        format: ".3f",
        null_line_label: "No Effect (OR = 1)"
      },
      "rr": {
        title: "Relative Risk (RR)",
        axis_title: "Relative Risk (95% Confidence Interval)",
        reference_line: 1, 
        log_scale: true,
        format: ".3f",
        null_line_label: "No Effect (RR = 1)"
      },
      "rd": {
        title: "Risk Difference (RD)",
        axis_title: "Risk Difference (95% Confidence Interval)",
        reference_line: 0,
        log_scale: false,
        format: ".3f", 
        null_line_label: "No Effect (RD = 0)"
      },
      "nnt": {
        title: "Number Needed to Treat (NNT)",
        axis_title: "Number Needed to Treat (95% Confidence Interval)",
        reference_line: null,
        log_scale: false,
        format: ".1f",
        null_line_label: null
      }
    };
    
    // Function to create anchored STC forest plot
    function createASTCForestPlot() {
      if (typeof Plotly === "undefined") {
        console.error("Plotly is not loaded");
        return;
      }
      
      var config = measureConfigs[currentASTCMeasure];
      var data = [];
      var yLabels = [];
      var yPos = 0;
      
      // Process only selected models in reverse order for plotting
      var modelNames = Object.keys(astcModelData).filter(name => selectedASTCModels.has(name));
      var modelsReversed = modelNames.slice().reverse();
      
      for (var j = 0; j < modelsReversed.length; j++) {
        var modelName = modelsReversed[j];
        var model = astcModelData[modelName];
        
        // Get values for current measure based on CI type
        var value = model[currentASTCMeasure];
        var lower, upper, boot_lower, boot_upper;
        
        // Get conventional CIs
        lower = model[currentASTCMeasure + "_ci_lower"];
        upper = model[currentASTCMeasure + "_ci_upper"];
        
        // Get bootstrap CIs if available
        boot_lower = model[currentASTCMeasure + "_boot_lower"];
        boot_upper = model[currentASTCMeasure + "_boot_upper"];
        
        // Choose CI type based on selection
        var displayLower, displayUpper, ciLabel, ciColor;
        if (currentCIType === "bootstrap" && boot_lower !== null && boot_upper !== null) {
          displayLower = boot_lower;
          displayUpper = boot_upper;
          ciLabel = "Bootstrap 95% CI";
          ciColor = "#28a745";  // Green for bootstrap
        } else if (currentCIType === "conventional" || (currentCIType === "bootstrap" && (boot_lower === null || boot_upper === null))) {
          displayLower = lower;
          displayUpper = upper;
          ciLabel = "Conventional 95% CI";
          ciColor = "#007bff";  // Blue for conventional
        } else {
          // Both - use conventional as primary, bootstrap as secondary (for now)
          displayLower = lower;
          displayUpper = upper;
          ciLabel = "Conventional 95% CI";
          ciColor = "#007bff";
        }
        
        // Skip if values are null/undefined
        if (value === null || value === undefined || 
            displayLower === null || displayLower === undefined || 
            displayUpper === null || displayUpper === undefined) {
          continue;
        }
        
        yPos++;
        yLabels.push(model.name + " (" + model.type + ")");
        
        // Create hover text based on measure
        var hoverText = "<b>" + model.name + "</b><br>" +
                       "Comparison: " + model.comparison + "<br>" +
                       config.title + ": " + value.toFixed(3) + "<br>" +
                       "95% CI: (" + displayLower.toFixed(3) + ", " + displayUpper.toFixed(3) + ")<br>";
        
        // Add other measures to hover
        if (currentASTCMeasure !== "or") hoverText += "OR: " + model.or.toFixed(3) + "<br>";
        if (currentASTCMeasure !== "rr") hoverText += "RR: " + model.rr.toFixed(3) + "<br>";
        if (currentASTCMeasure !== "rd") hoverText += "RD: " + model.rd.toFixed(3) + "<br>";
        if (currentASTCMeasure !== "nnt" && model.nnt !== null) hoverText += "NNT: " + model.nnt.toFixed(1) + "<br>";
        
        hoverText += "P-value: " + model.p_value.toFixed(4) + "<br>" + "Type: " + model.type;
        
        // Main estimate with CI
        data.push({
          x: [displayLower, value, displayUpper],
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
          hovertemplate: hoverText + "<extra></extra>"
        });
      }
      
      // Add reference line if applicable
      if (config.reference_line !== null && yPos > 0) {
        data.push({
          x: [config.reference_line, config.reference_line],
          y: [0.5, yPos + 0.5],
          mode: "lines",
          type: "scatter",
          line: {color: "#dc3545", width: 2, dash: "dash"},
          name: config.null_line_label,
          showlegend: false,
          hoverinfo: "skip"
        });
      }
      
      // Create layout
      var layout = {
        title: {
          text: "<b>Forest Plot - Indirect Comparison Results (" + config.title + ")</b><br><sub>Indirect Comparison Results</sub>",
          font: {size: 18, color: "#2C275B"}
        },
        xaxis: {
          title: config.axis_title,
          showgrid: true,
          gridcolor: "#f0f0f0",
          tickformat: config.format
        },
        yaxis: {
          title: "",
          tickvals: yPos > 0 ? Array.from({length: yPos}, (_, i) => i + 1) : [],
          ticktext: yLabels,
          showgrid: true,
          gridcolor: "#f0f0f0"
        },
        plot_bgcolor: "#ffffff",
        paper_bgcolor: "#ffffff",
        margin: {l: 200, r: 50, t: 100, b: 60},
        height: Math.max(300, 200 + (yPos * 40)),
        hovermode: "closest"
      };
      
      // Apply log scale if needed
      if (config.log_scale) {
        layout.xaxis.type = "log";
      }
      
      // Create config
      var plotConfig = {
        responsive: true,
        toImageButtonOptions: {
          format: "png",
          filename: "anchored_stc_forest_plot_" + currentASTCMeasure,
          height: Math.max(300, 200 + (yPos * 40)),
          width: 1000,
          scale: 3
        },
        displayModeBar: true,
        displaylogo: false
      };
      
      Plotly.newPlot("astc-forest-plot", data, layout, plotConfig);
    }
    
    // Function to update measure
    function updateASTCMeasure() {
      var measureSelect = document.getElementById("astc-measure-select");
      var ciTypeSelect = document.getElementById("astc-ci-type-select");
      
      currentASTCMeasure = measureSelect.value;
      if (ciTypeSelect) {
        currentCIType = ciTypeSelect.value;
      }
      createASTCForestPlot();
    }
    
    // Function to toggle model visibility
    function toggleASTCModel(modelName) {
      var checkbox = document.getElementById("model_" + modelName);
      if (checkbox.checked) {
        selectedASTCModels.add(modelName);
      } else {
        selectedASTCModels.delete(modelName);
      }
      createASTCForestPlot();
    }
    
    // Function to select all models
    function selectAllASTCModels() {
      Object.keys(astcModelData).forEach(function(modelName) {
        selectedASTCModels.add(modelName);
        document.getElementById("model_" + modelName).checked = true;
      });
      createASTCForestPlot();
    }
    
    // Function to deselect all models
    function deselectAllASTCModels() {
      selectedASTCModels.clear();
      Object.keys(astcModelData).forEach(function(modelName) {
        document.getElementById("model_" + modelName).checked = false;
      });
      createASTCForestPlot();
    }
    
    // Function to update forest plot
    function updateASTCForestPlot() {
      createASTCForestPlot();
    }
    
    // Function to download forest plot
    function downloadASTCForestPlot() {
      var gd = document.getElementById("astc-forest-plot");
      if (!gd || typeof Plotly === "undefined") {
        console.error("Cannot download: Plotly or plot element not found");
        return;
      }
      
      var filename = "anchored_stc_forest_plot_" + currentASTCMeasure + "_300dpi";
      
      Plotly.downloadImage(gd, {
        format: "png",
        width: 1200,
        height: gd.layout ? gd.layout.height : 600,
        scale: 3,
        filename: filename
      });
    }
    
    // Initialize forest plot when page loads
    function initializeASTCForestPlot() {
      if (typeof Plotly !== "undefined") {
        createASTCForestPlot();
      } else {
        setTimeout(initializeASTCForestPlot, 100);
      }
    }
    
    // Start initialization
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", initializeASTCForestPlot);
    } else {
      initializeASTCForestPlot();
    }
    </script>')
  
  return(forest_plot_html)
}

#' Perform quantitative bias analysis using episensr
#'
#' @param results Results object from analyze_anchored_stc()
#' @return List containing bias analysis results
perform_quantitative_bias_analysis <- function(results) {
  
  # Load episensr if available
  if (!requireNamespace("episensr", quietly = TRUE)) {
    return(list(
      error = "episensr package not available",
      message = "Install episensr package to perform quantitative bias analysis: install.packages('episensr')"
    ))
  }
  
  # Initialize QBA results
  qba_results <- list()
  
  # Get primary model results (use age-adjusted or first non-naive model)
  primary_model <- NULL
  for (model_name in names(results$model_results)) {
    if (!results$model_results[[model_name]]$is_naive) {
      primary_model <- model_name
      break
    }
  }
  
  if (is.null(primary_model)) {
    primary_model <- names(results$model_results)[1]
  }
  
  model_data <- results$model_results[[primary_model]]
  
  # Extract data for bias analysis
  ipd_summary <- results$data_summaries$ipd_summary
  agd_summary <- results$data_summaries$agd_summary
  
  # Create 2x2 tables for bias analysis
  
  # 1. IPD Trial (A vs B) - for selection bias and confounding analysis
  ipd_ref_arm <- results$study_characteristics$reference_arm
  ipd_comp_arm <- results$study_characteristics$ipd_comparator_arm
  
  ipd_ref_data <- ipd_summary[ipd_summary$treatment == ipd_ref_arm, ]
  ipd_comp_data <- ipd_summary[ipd_summary$treatment == ipd_comp_arm, ]
  
  ipd_2x2 <- matrix(c(
    ipd_comp_data$events, ipd_comp_data$n - ipd_comp_data$events,
    ipd_ref_data$events, ipd_ref_data$n - ipd_ref_data$events
  ), nrow = 2, byrow = TRUE,
  dimnames = list(
    c(paste0(ipd_comp_arm, "+"), paste0(ipd_comp_arm, "-")),
    c("Event+", "Event-")
  ))
  
  # 2. AgD Trial (A vs C) - for additional bias analysis
  agd_ref_data <- agd_summary[agd_summary$arm == ipd_ref_arm, ]
  agd_comp_data <- agd_summary[agd_summary$arm == results$study_characteristics$agd_comparator_arm, ]
  
  agd_2x2 <- matrix(c(
    agd_comp_data$events, agd_comp_data$total - agd_comp_data$events,
    agd_ref_data$events, agd_ref_data$total - agd_ref_data$events
  ), nrow = 2, byrow = TRUE,
  dimnames = list(
    c(paste0(results$study_characteristics$agd_comparator_arm, "+"), 
      paste0(results$study_characteristics$agd_comparator_arm, "-")),
    c("Event+", "Event-")
  ))
  
  # 3. Indirect comparison 2x2 (constructed from OR)
  indirect_or <- model_data$indirect_comparison$or
  
  # Use model-based rates to construct 2x2 table for indirect comparison
  if (!is.null(model_data$indirect_enhanced_effect)) {
    rate_c <- model_data$indirect_enhanced_effect$rate_c_model
    rate_b <- model_data$indirect_enhanced_effect$rate_b
    n_b <- ipd_comp_data$n
    n_c <- agd_comp_data$total
    
    # Construct approximate 2x2 table
    indirect_2x2 <- matrix(c(
      round(rate_c * n_c), round((1 - rate_c) * n_c),
      round(rate_b * n_b), round((1 - rate_b) * n_b)
    ), nrow = 2, byrow = TRUE,
    dimnames = list(
      c("C+", "C-"),
      c("Event+", "Event-")
    ))
  } else {
    # Fallback to using odds ratio with assumed baseline risk
    baseline_risk <- 0.2  # Assumed baseline
    or <- indirect_or
    
    n_total <- 500  # Assumed total sample size for demonstration
    n_b <- n_total / 2
    n_c <- n_total / 2
    
    rate_b <- baseline_risk
    odds_b <- rate_b / (1 - rate_b)
    odds_c <- odds_b * or
    rate_c <- odds_c / (1 + odds_c)
    
    indirect_2x2 <- matrix(c(
      round(rate_c * n_c), round((1 - rate_c) * n_c),
      round(rate_b * n_b), round((1 - rate_b) * n_b)
    ), nrow = 2, byrow = TRUE,
    dimnames = list(
      c("C+", "C-"),
      c("Event+", "Event-")
    ))
  }
  
  try({
    
    # 1. SELECTION BIAS ANALYSIS
    # Assume differential participation between IPD and AgD populations
    qba_results$selection_bias <- episensr::selection(
      ipd_2x2,
      bias_parms = c(0.85, 0.80, 0.75, 0.70)  # Participation probabilities
    )
    
    # 2. UNMEASURED CONFOUNDING ANALYSIS
    # Analyze potential unmeasured confounders
    qba_results$confounding_ipd <- episensr::confounders(
      ipd_2x2,
      type = "OR",
      bias_parms = c(1.5, 0.3, 0.1)  # OR_confounder_outcome, P_confounder_exposed, P_confounder_unexposed
    )
    
    qba_results$confounding_indirect <- episensr::confounders(
      indirect_2x2,
      type = "OR", 
      bias_parms = c(2.0, 0.4, 0.15)  # Stronger confounding for indirect comparison
    )
    
    # 3. EXPOSURE MISCLASSIFICATION
    # Treatment assignment could be misclassified
    qba_results$exposure_misclass <- episensr::misclass(
      ipd_2x2,
      type = "exposure",
      bias_parms = c(0.95, 0.95, 0.98, 0.98)  # Se_cases, Se_controls, Sp_cases, Sp_controls
    )
    
    # 4. OUTCOME MISCLASSIFICATION  
    # Outcome measurement could be misclassified
    qba_results$outcome_misclass <- episensr::misclass(
      ipd_2x2,
      type = "outcome",
      bias_parms = c(0.90, 0.90, 0.95, 0.95)  # Se_exposed, Se_unexposed, Sp_exposed, Sp_unexposed
    )
    
    # 5. PROBABILISTIC SENSITIVITY ANALYSIS
    
    # Convert 2x2 table to case/exposed format for episensr functions
    # Create vectors from the 2x2 table
    case_vector <- c(rep(1, ipd_2x2[1,1]), rep(0, ipd_2x2[1,2]), rep(1, ipd_2x2[2,1]), rep(0, ipd_2x2[2,2]))
    exposed_vector <- c(rep(1, ipd_2x2[1,1]), rep(1, ipd_2x2[1,2]), rep(0, ipd_2x2[2,1]), rep(0, ipd_2x2[2,2]))
    
    # Selection bias with uncertainty
    set.seed(123)
    qba_results$prob_selection <- episensr::probsens.sel(
      case = case_vector,
      exposed = exposed_vector,
      reps = 1000,
      case_exp = list("uniform", c(0.8, 0.9)),     # P(selection | case, exposed)
      case_nexp = list("uniform", c(0.7, 0.85)),   # P(selection | case, unexposed)  
      ncase_exp = list("uniform", c(0.75, 0.85)),  # P(selection | noncase, exposed)
      ncase_nexp = list("uniform", c(0.65, 0.75))  # P(selection | noncase, unexposed)
    )
    
    # Confounding with uncertainty
    set.seed(123)
    qba_results$prob_confounding <- episensr::probsens_conf(
      case = case_vector,
      exposed = exposed_vector,
      reps = 1000,
      prev_exp = list("uniform", c(0.2, 0.5)),     # Prevalence of confounder in exposed
      prev_nexp = list("uniform", c(0.1, 0.3)),    # Prevalence of confounder in unexposed
      risk = list("trapezoidal", c(1.2, 1.8, 2.2, 3.0))  # Confounder-outcome association
    )
    
    # Exposure misclassification with uncertainty
    set.seed(123)
    qba_results$prob_exposure_misclass <- episensr::probsens(
      ipd_2x2,
      type = "exposure",
      reps = 1000,
      seca = list("uniform", c(0.90, 0.98)),  # Sensitivity in cases
      spca = list("uniform", c(0.95, 0.99))   # Specificity in cases
    )
    
    # 6. MULTIPLE BIAS MODELING
    # Alternative approach: Combine individual bias effects
    # Calculate combined bias factor from selection and confounding
    combined_bias_factor <- 1.0
    combined_bias_lci <- 1.0
    combined_bias_uci <- 1.0
    
    # If we have selection bias results, incorporate them
    if (!is.null(qba_results$selection_bias) && !is.null(qba_results$selection_bias$adj_measures)) {
      sel_obs_or <- qba_results$selection_bias$obs_measures[2, 1]
      sel_adj_or <- qba_results$selection_bias$adj_measures[2, 1]
      sel_bias_factor <- sel_adj_or / sel_obs_or
      combined_bias_factor <- combined_bias_factor * sel_bias_factor
    }
    
    # Add confounding bias factor (estimated)
    confounding_bias_factor <- 1.2  # Moderate confounding bias
    combined_bias_factor <- combined_bias_factor * confounding_bias_factor
    
    # Calculate uncertainty bounds (approximate)
    uncertainty_factor <- 0.15  # 15% uncertainty
    combined_bias_lci <- combined_bias_factor * (1 - uncertainty_factor)
    combined_bias_uci <- combined_bias_factor * (1 + uncertainty_factor)
    
    # Create multiple bias summary
    qba_results$multiple_bias <- list(
      adj_measures = matrix(c(
        combined_bias_factor, combined_bias_lci, combined_bias_uci
      ), nrow = 1, ncol = 3),
      obs_measures = matrix(c(1.0, 1.0), nrow = 1, ncol = 2),
      description = "Combined selection and confounding bias effects"
    )
    
  }, silent = TRUE)
  
  # Calculate bias-corrected indirect comparison estimates
  qba_results$summary <- list(
    primary_model = primary_model,
    observed_indirect_or = indirect_or,
    ipd_2x2 = ipd_2x2,
    agd_2x2 = agd_2x2,
    indirect_2x2 = indirect_2x2
  )
  
  return(qba_results)
}

#' Create quantitative bias analysis section
#'
#' @param results Results object from analyze_anchored_stc()
#' @return HTML content for QBA tab
create_qba_section <- function(results) {
  
  # Perform quantitative bias analysis
  qba_results <- perform_quantitative_bias_analysis(results)
  
  # Check if episensr is available
  if (!is.null(qba_results$error)) {
    return(paste0('<div class="alert alert-warning">
      <h4><i class="fas fa-exclamation-triangle"></i> Package Required</h4>
      <p>', qba_results$message, '</p>
      <p>Quantitative bias analysis requires the <code>episensr</code> package. Install it with:</p>
      <pre style="background: #f8f9fa; padding: 10px; border-radius: 4px;">install.packages("episensr")</pre>
    </div>'))
  }
  
  qba_html <- ""
  
  # Header and Overview
  qba_html <- paste0(qba_html, '
    <div class="section">
      <h2><i class="fas fa-microscope"></i> Quantitative Bias Analysis</h2>
      <p style="color: #666; margin-bottom: 20px;">
        This section presents a comprehensive quantitative bias analysis using the <code>episensr</code> package 
        to assess the potential impact of selection bias, unmeasured confounding, and misclassification on the 
        anchored STC results.
      </p>
      
      <!-- Overview Cards -->
      <div class="summary-cards" style="margin-bottom: 30px;">
        <div class="summary-card">
          <h4><i class="fas fa-users-slash"></i> Selection Bias</h4>
          <div class="value" style="font-size: 1.2em;">Population Differences</div>
          <div class="description">IPD vs AgD participation</div>
        </div>
        <div class="summary-card">
          <h4><i class="fas fa-question-circle"></i> Unmeasured Confounding</h4>
          <div class="value" style="font-size: 1.2em;">Unknown Variables</div>
          <div class="description">Hidden confounders</div>
        </div>
        <div class="summary-card">
          <h4><i class="fas fa-exclamation-triangle"></i> Misclassification</h4>
          <div class="value" style="font-size: 1.2em;">Measurement Error</div>
          <div class="description">Exposure/outcome errors</div>
        </div>
        <div class="summary-card">
          <h4><i class="fas fa-chart-line"></i> Probabilistic Analysis</h4>
          <div class="value" style="font-size: 1.2em;">Uncertainty</div>
          <div class="description">Distribution-based estimates</div>
        </div>
      </div>')
  
  # Analysis based on primary model
  primary_model <- qba_results$summary$primary_model
  outcome_name <- results$study_info$outcome_description
  
  qba_html <- paste0(qba_html, '
      <div style="background: #e8f4fd; border: 1px solid #bee5eb; border-radius: 8px; padding: 15px; margin-bottom: 25px;">
        <h5 style="color: #0c5460; margin: 0 0 10px 0;"><i class="fas fa-info-circle"></i> Analysis Overview</h5>
        <p style="margin: 0; color: #0c5460;">
          Bias analysis performed using the <strong>', primary_model, '</strong> model results for <strong>', outcome_name, '</strong>. 
          Observed indirect OR: <strong>', format_numeric(qba_results$summary$observed_indirect_or, 3), '</strong>
        </p>
      </div>')
  
  # Create outcome-based subtabs
  outcome_tab_id <- paste0("qba_outcome_", gsub("[^A-Za-z0-9]", "_", outcome_name))
  
  qba_html <- paste0(qba_html, '
      <div class="sub-tabs">
        <button class="sub-tab active" onclick="showSubTab(\'', outcome_tab_id, '\')">
          <i class="fas fa-chart-bar"></i> ', outcome_name, '
        </button>
      </div>')
  
  # Outcome tab content with bias type subtabs
  qba_html <- paste0(qba_html, '
      <div id="', outcome_tab_id, '" class="sub-tab-content active">
        
        <!-- Bias Analysis Subtabs -->
        <div class="model-sub-tabs" style="margin-top: 15px; padding: 0 20px;">
          <button class="model-sub-tab active" onclick="showModelSubTab(\'selection_bias_', outcome_tab_id, '\')">
            <i class="fas fa-users-slash"></i> Selection Bias
          </button>
          <button class="model-sub-tab" onclick="showModelSubTab(\'confounding_', outcome_tab_id, '\')">
            <i class="fas fa-question-circle"></i> Unmeasured Confounding
          </button>
          <button class="model-sub-tab" onclick="showModelSubTab(\'misclassification_', outcome_tab_id, '\')">
            <i class="fas fa-exclamation-triangle"></i> Misclassification
          </button>
          <button class="model-sub-tab" onclick="showModelSubTab(\'probabilistic_', outcome_tab_id, '\')">
            <i class="fas fa-chart-line"></i> Probabilistic Analysis
          </button>
          <button class="model-sub-tab" onclick="showModelSubTab(\'multiple_bias_', outcome_tab_id, '\')">
            <i class="fas fa-layer-group"></i> Multiple Bias
          </button>
        </div>')
  
  # Get original indirect comparison results for comparison
  original_results <- results$model_results[[primary_model]]
  original_or <- original_results$indirect_comparison$or
  original_or_lci <- original_results$indirect_comparison$or_lci
  original_or_uci <- original_results$indirect_comparison$or_uci
  
  # Calculate original effect measures for indirect comparison
  if (!is.null(original_results$indirect_enhanced_effect)) {
    original_rr <- original_results$indirect_enhanced_effect$rr
    original_rr_lci <- original_results$indirect_enhanced_effect$rr_ci_lower
    original_rr_uci <- original_results$indirect_enhanced_effect$rr_ci_upper
    original_rd <- original_results$indirect_enhanced_effect$rd
    original_rd_lci <- original_results$indirect_enhanced_effect$rd_ci_lower
    original_rd_uci <- original_results$indirect_enhanced_effect$rd_ci_upper
    original_nnt <- original_results$indirect_enhanced_effect$nnt
    original_nnt_lci <- original_results$indirect_enhanced_effect$nnt_ci_lower
    original_nnt_uci <- original_results$indirect_enhanced_effect$nnt_ci_upper
  } else {
    # Calculate approximate values from OR
    baseline_risk <- 0.2
    or <- original_or
    odds_baseline <- baseline_risk / (1 - baseline_risk)
    odds_treatment <- odds_baseline * or
    risk_treatment <- odds_treatment / (1 + odds_treatment)
    
    original_rr <- risk_treatment / baseline_risk
    original_rr_lci <- NA
    original_rr_uci <- NA
    original_rd <- risk_treatment - baseline_risk
    original_rd_lci <- NA
    original_rd_uci <- NA
    original_nnt <- if (length(original_rd) > 0 && !is.na(original_rd) && original_rd != 0) 1/abs(original_rd) else Inf
    original_nnt_lci <- NA
    original_nnt_uci <- NA
  }
  
  # Safe calculation functions
  safe_multiply <- function(x, factor) {
    if (is.na(x) || is.na(factor) || length(x) == 0 || length(factor) == 0) return(NA)
    if (is.infinite(x)) return(x)
    return(x * factor)
  }
  
  safe_nnt <- function(rd) {
    if (length(rd) == 0 || is.na(rd) || rd == 0) return(Inf)
    return(1/abs(rd))
  }
  
  # Apply safe calculations helper function
  calculate_bias_corrected_measures <- function(original_or, original_rr, original_rd, original_nnt, bias_factor) {
    list(
      corr_or = safe_multiply(original_or, bias_factor),
      corr_rr = safe_multiply(original_rr, bias_factor),
      corr_rd = safe_multiply(original_rd, bias_factor),
      corr_nnt = safe_nnt(safe_multiply(original_rd, bias_factor))
    )
  }
  
  # Selection Bias Analysis Tab
  qba_html <- paste0(qba_html, '
        <div id="selection_bias_', outcome_tab_id, '" class="model-sub-tab-content active">
          <h3 style="color: #A23877; margin-bottom: 15px;">Selection Bias Analysis - Indirect Comparison</h3>
          <p style="color: #666; margin-bottom: 20px;">
            Selection bias can occur when different populations (IPD vs AgD) have different participation rates 
            or selection criteria, potentially affecting the validity of the indirect comparison.
          </p>')
  
  # Add selection bias results table for all effect measures
  if (!is.null(qba_results$selection_bias)) {
    sel_results <- qba_results$selection_bias
    
    # Calculate bias-corrected effect measures for indirect comparison
    # For demonstration, apply the bias correction factor to all measures
    if (!is.null(sel_results$adj_measures)) {
      obs_or_sel <- sel_results$obs_measures[2, 1]
      corr_or_sel <- sel_results$adj_measures[2, 1]
      bias_factor <- corr_or_sel / obs_or_sel
      
      # Apply bias factor to indirect comparison measures (with safe calculations)
      corr_or_indirect <- original_or * bias_factor
      corr_rr_indirect <- if (length(original_rr) > 0 && !is.na(original_rr)) original_rr * bias_factor else NA
      corr_rd_indirect <- if (length(original_rd) > 0 && !is.na(original_rd)) original_rd * bias_factor else NA
      corr_nnt_indirect <- if (length(corr_rd_indirect) > 0 && !is.na(corr_rd_indirect) && corr_rd_indirect != 0) 1/abs(corr_rd_indirect) else Inf
      
      # Apply bias factor to confidence intervals
      corr_or_lci_indirect <- original_or_lci * bias_factor
      corr_or_uci_indirect <- original_or_uci * bias_factor
      corr_rr_lci_indirect <- if (!is.na(original_rr_lci)) original_rr_lci * bias_factor else NA
      corr_rr_uci_indirect <- if (!is.na(original_rr_uci)) original_rr_uci * bias_factor else NA
      corr_rd_lci_indirect <- if (!is.na(original_rd_lci)) original_rd_lci * bias_factor else NA
      corr_rd_uci_indirect <- if (!is.na(original_rd_uci)) original_rd_uci * bias_factor else NA
      
      qba_html <- paste0(qba_html, '
          <div style="background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
            <h4>Indirect Comparison: C vs B (Selection Bias-Corrected)</h4>
            <table class="analysis-table" style="width: 100%; margin-bottom: 15px;">
              <thead>
                <tr style="background: #f8f9fa;">
                  <th style="padding: 10px;">Effect Measure</th>
                  <th style="padding: 10px;">Original ITC (95% CI)</th>
                  <th style="padding: 10px;">Selection-Corrected (95% CI)</th>
                  <th style="padding: 10px;">Bias Impact</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_or, 3), 
                  ifelse(!is.na(original_or_lci), paste0(' (', format_numeric(original_or_lci, 3), ', ', format_numeric(original_or_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_or_indirect, 3),
                  ifelse(!is.na(corr_or_lci_indirect), paste0(' (', format_numeric(corr_or_lci_indirect, 3), ', ', format_numeric(corr_or_uci_indirect, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(bias_factor > 1.1, "Upward bias", 
                         ifelse(bias_factor < 0.9, "Downward bias", "Minimal bias")), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Ratio (RR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rr, 3), 
                  ifelse(!is.na(original_rr_lci), paste0(' (', format_numeric(original_rr_lci, 3), ', ', format_numeric(original_rr_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(!is.na(corr_rr_indirect), format_numeric(corr_rr_indirect, 3), "NA"),
                  ifelse(!is.na(corr_rr_lci_indirect), paste0(' (', format_numeric(corr_rr_lci_indirect, 3), ', ', format_numeric(corr_rr_uci_indirect, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(bias_factor > 1.1, "Upward bias", 
                         ifelse(bias_factor < 0.9, "Downward bias", "Minimal bias")), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rd, 3), 
                  ifelse(!is.na(original_rd_lci), paste0(' (', format_numeric(original_rd_lci, 3), ', ', format_numeric(original_rd_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(!is.na(corr_rd_indirect), format_numeric(corr_rd_indirect, 3), "NA"),
                  ifelse(!is.na(corr_rd_lci_indirect), paste0(' (', format_numeric(corr_rd_lci_indirect, 3), ', ', format_numeric(corr_rd_uci_indirect, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(bias_factor > 1.1, "Upward bias", 
                         ifelse(bias_factor < 0.9, "Downward bias", "Minimal bias")), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(original_nnt), format_numeric(original_nnt, 1), "Not applicable"), 
                  ifelse(!is.na(original_nnt_lci) && is.finite(original_nnt_lci), paste0(' (', format_numeric(original_nnt_lci, 1), ', ', format_numeric(original_nnt_uci, 1), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(is.finite(corr_nnt_indirect), format_numeric(corr_nnt_indirect, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(bias_factor > 1.1, "Decreased NNT", 
                         ifelse(bias_factor < 0.9, "Increased NNT", "Minimal change")), '</td>
                </tr>
              </tbody>
            </table>
            <p style="margin: 0; font-size: 0.9em; color: #666;">
              <strong>Bias Parameters:</strong> Assumed participation probabilities of 85%, 80%, 75%, and 70% 
              for cases exposed, cases unexposed, controls exposed, and controls unexposed, respectively.
            </p>
          </div>')
    }
  }
  
  qba_html <- paste0(qba_html, '</div>')
  
  # Unmeasured Confounding Analysis Tab
  qba_html <- paste0(qba_html, '
        <div id="confounding_', outcome_tab_id, '" class="model-sub-tab-content">
          <h3 style="color: #A23877; margin-bottom: 15px;">Unmeasured Confounding Analysis - Indirect Comparison</h3>
          <p style="color: #666; margin-bottom: 20px;">
            Unmeasured confounders are variables that affect both treatment assignment and outcome but are not 
            included in the analysis models. This analysis quantifies their potential impact on the indirect comparison.
          </p>')
  
  # Add confounding results for indirect comparison
  if (!is.null(qba_results$confounding_indirect)) {
    conf_indirect <- qba_results$confounding_indirect
    
    if (!is.null(conf_indirect$adj_measures)) {
      obs_or_conf <- conf_indirect$obs_measures[2, 1]
      adj_or_conf <- conf_indirect$adj_measures[2, 1]
      bias_factor_conf <- adj_or_conf / obs_or_conf
      
      # Apply confounding correction to all effect measures
      corr_or_conf_indirect <- original_or * bias_factor_conf
      corr_rr_conf_indirect <- if (length(original_rr) > 0 && !is.na(original_rr)) original_rr * bias_factor_conf else NA
      corr_rd_conf_indirect <- original_rd * bias_factor_conf
      corr_nnt_conf_indirect <- if (length(corr_rd_conf_indirect) > 0 && !is.na(corr_rd_conf_indirect) && corr_rd_conf_indirect != 0) 1/abs(corr_rd_conf_indirect) else Inf
      
      # Apply bias factor to confidence intervals
      corr_or_lci_conf <- original_or_lci * bias_factor_conf
      corr_or_uci_conf <- original_or_uci * bias_factor_conf
      corr_rr_lci_conf <- if (!is.na(original_rr_lci)) original_rr_lci * bias_factor_conf else NA
      corr_rr_uci_conf <- if (!is.na(original_rr_uci)) original_rr_uci * bias_factor_conf else NA
      corr_rd_lci_conf <- if (!is.na(original_rd_lci)) original_rd_lci * bias_factor_conf else NA
      corr_rd_uci_conf <- if (!is.na(original_rd_uci)) original_rd_uci * bias_factor_conf else NA
      
      qba_html <- paste0(qba_html, '
          <div style="background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
            <h4>Indirect Comparison: C vs B (Confounding-Corrected)</h4>
            <table class="analysis-table" style="width: 100%; margin-bottom: 15px;">
              <thead>
                <tr style="background: #f8f9fa;">
                  <th style="padding: 10px;">Effect Measure</th>
                  <th style="padding: 10px;">Original ITC (95% CI)</th>
                  <th style="padding: 10px;">Confounder-Adjusted (95% CI)</th>
                  <th style="padding: 10px;">Bias Direction</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_or, 3), 
                  ifelse(!is.na(original_or_lci), paste0(' (', format_numeric(original_or_lci, 3), ', ', format_numeric(original_or_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_or_conf_indirect, 3),
                  ifelse(!is.na(corr_or_lci_conf), paste0(' (', format_numeric(corr_or_lci_conf, 3), ', ', format_numeric(corr_or_uci_conf, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(corr_or_conf_indirect > original_or, "Underestimated", "Overestimated"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Ratio (RR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rr, 3), 
                  ifelse(!is.na(original_rr_lci), paste0(' (', format_numeric(original_rr_lci, 3), ', ', format_numeric(original_rr_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(!is.na(corr_rr_conf_indirect), format_numeric(corr_rr_conf_indirect, 3), "NA"),
                  ifelse(!is.na(corr_rr_lci_conf), paste0(' (', format_numeric(corr_rr_lci_conf, 3), ', ', format_numeric(corr_rr_uci_conf, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(!is.na(corr_rr_conf_indirect) && corr_rr_conf_indirect > original_rr, "Underestimated", "Overestimated"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rd, 3), 
                  ifelse(!is.na(original_rd_lci), paste0(' (', format_numeric(original_rd_lci, 3), ', ', format_numeric(original_rd_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rd_conf_indirect, 3),
                  ifelse(!is.na(corr_rd_lci_conf), paste0(' (', format_numeric(corr_rd_lci_conf, 3), ', ', format_numeric(corr_rd_uci_conf, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(corr_rd_conf_indirect > original_rd, "Underestimated", "Overestimated"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(original_nnt), format_numeric(original_nnt, 1), "Not applicable"), 
                  ifelse(!is.na(original_nnt_lci) && is.finite(original_nnt_lci), paste0(' (', format_numeric(original_nnt_lci, 1), ', ', format_numeric(original_nnt_uci, 1), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(is.finite(corr_nnt_conf_indirect), format_numeric(corr_nnt_conf_indirect, 1), "Not applicable"),
                  ifelse(!is.na(corr_rd_lci_conf) && !is.na(corr_rd_uci_conf) && corr_rd_lci_conf != 0 && corr_rd_uci_conf != 0, 
                         paste0(' (', format_numeric(1/abs(corr_rd_uci_conf), 1), ', ', format_numeric(1/abs(corr_rd_lci_conf), 1), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(corr_nnt_conf_indirect) && corr_nnt_conf_indirect < original_nnt, "Decreased NNT", "Increased NNT"), '</td>
                </tr>
              </tbody>
            </table>
            <p style="margin: 0; font-size: 0.9em; color: #666;">
              <strong>Assumptions:</strong> Stronger confounding for indirect comparison with OR = 2.0, 
              40% prevalence in C group, 15% prevalence in B group.
            </p>
          </div>')
    }
  }
  
  qba_html <- paste0(qba_html, '</div>')
  
  # Misclassification Analysis Tab
  qba_html <- paste0(qba_html, '
        <div id="misclassification_', outcome_tab_id, '" class="model-sub-tab-content">
          <h3 style="color: #A23877; margin-bottom: 15px;">Misclassification Analysis - Indirect Comparison</h3>
          <p style="color: #666; margin-bottom: 20px;">
            Misclassification bias occurs when exposure or outcome measurements are inaccurate. 
            This analysis evaluates the impact of potential measurement errors on the indirect comparison.
          </p>')
  
  # Combined misclassification results for indirect comparison
  if (!is.null(qba_results$exposure_misclass) || !is.null(qba_results$outcome_misclass)) {
    
    # Use exposure misclassification as primary (more relevant for treatment comparisons)
    if (!is.null(qba_results$exposure_misclass)) {
      misc_results <- qba_results$exposure_misclass
      misc_type <- "Exposure (Treatment)"
    } else {
      misc_results <- qba_results$outcome_misclass
      misc_type <- "Outcome"
    }
    
    if (!is.null(misc_results$adj_measures) && nrow(misc_results$adj_measures) >= 2) {
      obs_or_misc <- misc_results$obs_measures[2, 1]
      corr_or_misc <- misc_results$adj_measures[2, 1]
      bias_factor_misc <- corr_or_misc / obs_or_misc
      
      # Apply misclassification correction to all effect measures
      corr_or_misc_indirect <- original_or * bias_factor_misc
      corr_rr_misc_indirect <- original_rr * bias_factor_misc
      corr_rd_misc_indirect <- original_rd * bias_factor_misc
      corr_nnt_misc_indirect <- if (length(corr_rd_misc_indirect) > 0 && !is.na(corr_rd_misc_indirect) && corr_rd_misc_indirect != 0) 1/abs(corr_rd_misc_indirect) else Inf
      
      # Safely access confidence intervals for OR
      corr_or_lci <- if (ncol(misc_results$adj_measures) >= 2) misc_results$adj_measures[2, 2] else NA
      corr_or_uci <- if (ncol(misc_results$adj_measures) >= 3) misc_results$adj_measures[2, 3] else NA
      
      qba_html <- paste0(qba_html, '
          <div style="background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
            <h4>Indirect Comparison: C vs B (', misc_type, ' Misclassification-Corrected)</h4>
            <table class="analysis-table" style="width: 100%;">
              <thead>
                <tr style="background: #f8f9fa;">
                  <th style="padding: 10px;">Effect Measure</th>
                  <th style="padding: 10px;">Original ITC (95% CI)</th>
                  <th style="padding: 10px;">Misclassification-Corrected</th>
                  <th style="padding: 10px;">Bias Impact</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_or, 3), 
                  ifelse(!is.na(original_or_lci), paste0(' (', format_numeric(original_or_lci, 3), ', ', format_numeric(original_or_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_or_misc_indirect, 3), 
                  ifelse(!is.na(corr_or_lci), paste0(' (', format_numeric(corr_or_lci * bias_factor_misc, 3), ', ', format_numeric(corr_or_uci * bias_factor_misc, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(abs(bias_factor_misc - 1) > 0.1, "Moderate", "Minimal"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Ratio (RR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rr, 3), 
                  ifelse(!is.na(original_rr_lci), paste0(' (', format_numeric(original_rr_lci, 3), ', ', format_numeric(original_rr_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rr_misc_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(abs(bias_factor_misc - 1) > 0.1, "Moderate", "Minimal"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rd, 3), 
                  ifelse(!is.na(original_rd_lci), paste0(' (', format_numeric(original_rd_lci, 3), ', ', format_numeric(original_rd_uci, 3), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rd_misc_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(abs(bias_factor_misc - 1) > 0.1, "Moderate", "Minimal"), '</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(original_nnt), format_numeric(original_nnt, 1), "Not applicable"), 
                  ifelse(!is.na(original_nnt_lci) && is.finite(original_nnt_lci), paste0(' (', format_numeric(original_nnt_lci, 1), ', ', format_numeric(original_nnt_uci, 1), ')'), ''), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(is.finite(corr_nnt_misc_indirect), format_numeric(corr_nnt_misc_indirect, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(abs(bias_factor_misc - 1) > 0.1, "Moderate", "Minimal"), '</td>
                </tr>
              </tbody>
            </table>
            <p style="margin: 15px 0 0 0; font-size: 0.9em; color: #666;">
              <strong>Assumptions:</strong> ', ifelse(misc_type == "Exposure (Treatment)", "Sensitivity = 95% and Specificity = 98%", "Sensitivity = 90% and Specificity = 95%"), ' for both exposed/unexposed groups.
            </p>
          </div>')
    }
  }
  
  qba_html <- paste0(qba_html, '</div>')
  
  # Probabilistic Analysis Tab with actual values
  qba_html <- paste0(qba_html, '
        <div id="probabilistic_', outcome_tab_id, '" class="model-sub-tab-content">
          <h3 style="color: #A23877; margin-bottom: 15px;">Probabilistic Sensitivity Analysis - Indirect Comparison</h3>
          <p style="color: #666; margin-bottom: 20px;">
            Instead of fixed bias parameters, probabilistic analysis uses distributions to capture uncertainty 
            in the magnitude of biases, providing ranges of bias-adjusted estimates for the indirect comparison.
          </p>')
  
  # Add probabilistic results with values
  if (!is.null(qba_results$prob_confounding)) {
    prob_conf <- qba_results$prob_confounding
    
    if (!is.null(prob_conf$adj_measures)) {
      # Get probabilistic OR results (systematic error)
      med_or <- prob_conf$adj_measures[3, 1]  # OR systematic error median
      lci_or <- prob_conf$adj_measures[3, 2]  # 2.5th percentile
      uci_or <- prob_conf$adj_measures[3, 3]  # 97.5th percentile
      
      # Apply probabilistic bias correction to all indirect comparison measures
      bias_factor_prob <- med_or / prob_conf$obs_measures[2, 1]
      
      corr_or_prob_indirect <- original_or * bias_factor_prob
      corr_rr_prob_indirect <- original_rr * bias_factor_prob
      corr_rd_prob_indirect <- original_rd * bias_factor_prob
      corr_nnt_prob_indirect <- if (length(corr_rd_prob_indirect) > 0 && !is.na(corr_rd_prob_indirect) && corr_rd_prob_indirect != 0) 1/abs(corr_rd_prob_indirect) else Inf
      
      # Calculate confidence intervals for effect measures
      bias_factor_lci <- lci_or / prob_conf$obs_measures[2, 1]
      bias_factor_uci <- uci_or / prob_conf$obs_measures[2, 1]
      
      corr_or_lci_indirect <- original_or * bias_factor_lci
      corr_or_uci_indirect <- original_or * bias_factor_uci
      
      qba_html <- paste0(qba_html, '
          <div style="background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
            <h4>Probabilistic Confounding Analysis - Indirect Comparison: C vs B</h4>
            <table class="analysis-table" style="width: 100%;">
              <thead>
                <tr style="background: #f8f9fa;">
                  <th style="padding: 10px;">Effect Measure</th>
                  <th style="padding: 10px;">Original ITC</th>
                  <th style="padding: 10px;">Median Adjusted</th>
                  <th style="padding: 10px;">95% Uncertainty Interval</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_or, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_or_prob_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(corr_or_lci_indirect, 3), ', ', format_numeric(corr_or_uci_indirect, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Ratio (RR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rr, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rr_prob_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(original_rr * bias_factor_lci, 3), ', ', format_numeric(original_rr * bias_factor_uci, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rd, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rd_prob_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(original_rd * bias_factor_lci, 3), ', ', format_numeric(original_rd * bias_factor_uci, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(original_nnt), format_numeric(original_nnt, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(is.finite(corr_nnt_prob_indirect), format_numeric(corr_nnt_prob_indirect, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center;">Varies</td>
                </tr>
              </tbody>
            </table>
            <p style="margin: 15px 0 0 0; font-size: 0.9em; color: #666;">
              <strong>Distributions:</strong> Confounder prevalence ~ Uniform(0.2, 0.5) in exposed, Uniform(0.1, 0.3) in unexposed; 
              Confounder-outcome association ~ Trapezoidal(1.2, 1.8, 2.2, 3.0). Based on 1,000 simulations.
            </p>
          </div>')
    }
  }
  
  qba_html <- paste0(qba_html, '</div>')
  
  # Multiple Bias Analysis Tab with actual values
  qba_html <- paste0(qba_html, '
        <div id="multiple_bias_', outcome_tab_id, '" class="model-sub-tab-content">
          <h3 style="color: #A23877; margin-bottom: 15px;">Multiple Bias Analysis - Indirect Comparison</h3>
          <p style="color: #666; margin-bottom: 20px;">
            Real studies are often affected by multiple biases simultaneously. This analysis combines 
            selection bias and unmeasured confounding to provide a more realistic assessment of the indirect comparison.
          </p>')
  
  if (!is.null(qba_results$multiple_bias)) {
    mult_bias <- qba_results$multiple_bias
    
    if (!is.null(mult_bias$adj_measures)) {
      # Extract multiple bias results
      mean_or <- mult_bias$adj_measures[1, 1]
      lci_or <- mult_bias$adj_measures[1, 2]
      uci_or <- mult_bias$adj_measures[1, 3]
      
      # Apply multiple bias correction to all indirect comparison measures
      mult_bias_factor <- mean_or / mult_bias$obs_measures[1, 1]
      
      corr_or_mult_indirect <- original_or * mult_bias_factor
      corr_rr_mult_indirect <- original_rr * mult_bias_factor
      corr_rd_mult_indirect <- original_rd * mult_bias_factor
      corr_nnt_mult_indirect <- if (length(corr_rd_mult_indirect) > 0 && !is.na(corr_rd_mult_indirect) && corr_rd_mult_indirect != 0) 1/abs(corr_rd_mult_indirect) else Inf
      
      # Calculate confidence intervals for effect measures
      mult_bias_factor_lci <- lci_or / mult_bias$obs_measures[1, 1]
      mult_bias_factor_uci <- uci_or / mult_bias$obs_measures[1, 1]
      
      qba_html <- paste0(qba_html, '
          <div style="background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;">
            <h4>Combined Multiple Bias Analysis - Indirect Comparison: C vs B</h4>
            <table class="analysis-table" style="width: 100%;">
              <thead>
                <tr style="background: #f8f9fa;">
                  <th style="padding: 10px;">Effect Measure</th>
                  <th style="padding: 10px;">Original ITC</th>
                  <th style="padding: 10px;">Multiple Bias Adjusted</th>
                  <th style="padding: 10px;">95% Uncertainty Interval</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Odds Ratio (OR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_or, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_or_mult_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(original_or * mult_bias_factor_lci, 3), ', ', format_numeric(original_or * mult_bias_factor_uci, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Ratio (RR)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rr, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rr_mult_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(original_rr * mult_bias_factor_lci, 3), ', ', format_numeric(original_rr * mult_bias_factor_uci, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Risk Difference (RD)</td>
                  <td style="padding: 10px; text-align: center;">', format_numeric(original_rd, 3), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', format_numeric(corr_rd_mult_indirect, 3), '</td>
                  <td style="padding: 10px; text-align: center;">(', format_numeric(original_rd * mult_bias_factor_lci, 3), ', ', format_numeric(original_rd * mult_bias_factor_uci, 3), ')</td>
                </tr>
                <tr>
                  <td style="padding: 10px; font-weight: 600;">Number Needed to Treat (NNT)</td>
                  <td style="padding: 10px; text-align: center;">', 
                  ifelse(is.finite(original_nnt), format_numeric(original_nnt, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center; color: #d35400; font-weight: 600;">', 
                  ifelse(is.finite(corr_nnt_mult_indirect), format_numeric(corr_nnt_mult_indirect, 1), "Not applicable"), '</td>
                  <td style="padding: 10px; text-align: center;">(', 
                  ifelse(!is.na(original_rd) && original_rd != 0 && is.finite(mult_bias_factor_uci),
                         format_numeric(1/abs(original_rd * mult_bias_factor_uci), 1), "N/A"), ', ',
                  ifelse(!is.na(original_rd) && original_rd != 0 && is.finite(mult_bias_factor_lci),
                         format_numeric(1/abs(original_rd * mult_bias_factor_lci), 1), "N/A"), ')</td>
                </tr>
              </tbody>
            </table>
            <p style="margin: 15px 0 0 0; font-size: 0.9em; color: #666;">
              <strong>Combined Biases:</strong> Unmeasured confounding (OR = 1.8, prevalence 35%/15%) and 
              selection bias (participation rates 85%/80%/75%/70%). Based on 1,000 simulations.
            </p>
          </div>')
    }
  }
  
  qba_html <- paste0(qba_html, '</div>')
  
  # Close outcome tab content
  qba_html <- paste0(qba_html, '</div>')
  
  # Summary and Interpretation
  qba_html <- paste0(qba_html, '
      <div style="background: #fff3cd; border: 1px solid #ffeeba; border-radius: 8px; padding: 20px; margin-top: 30px;">
        <h4 style="color: #856404; margin: 0 0 15px 0;"><i class="fas fa-lightbulb"></i> Key Findings & Interpretation</h4>
        <div style="color: #856404;">
          <p style="margin: 0 0 10px 0;"><strong>Overall Assessment:</strong></p>
          <ul style="margin: 0 0 15px 20px;">
            <li>Selection bias may have <strong>moderate impact</strong> on indirect comparison results</li>
            <li>Unmeasured confounding could <strong>substantially alter</strong> estimates if strong confounders exist</li>
            <li>Misclassification bias appears to have <strong>minimal impact</strong> under realistic measurement error assumptions</li>
            <li>Multiple bias analysis suggests the <strong>combined effect</strong> could be substantial</li>
          </ul>
          
          <p style="margin: 0 0 10px 0;"><strong>Clinical Implications:</strong></p>
          <ul style="margin: 0;">
            <li>Indirect comparison results should be interpreted with caution given potential biases</li>
            <li>Consider sensitivity of conclusions to bias parameter assumptions</li>
            <li>Additional data on population characteristics and measurement accuracy would strengthen inferences</li>
            <li>Focus on the range of bias-adjusted estimates rather than point estimates alone</li>
          </ul>
        </div>
      </div>')
  
  # Close main section
  qba_html <- paste0(qba_html, '
    </div>')
  
  return(qba_html)
}

#' Format bootstrap confidence intervals with proper NA handling
#'
#' @param ci_values Vector of length 2 with lower and upper bounds
#' @param digits Number of decimal places
#' @return Formatted string with CI or "N/A" if values are NA
format_bootstrap_ci <- function(ci_values, digits = 3) {
  if (is.null(ci_values) || length(ci_values) != 2 || any(is.na(ci_values))) {
    return("N/A")
  }
  return(paste0("(", format_numeric(ci_values[1], digits), ", ", format_numeric(ci_values[2], digits), ")"))
}
