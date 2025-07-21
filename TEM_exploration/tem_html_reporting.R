# Enhanced HTML Reporting for TEM Analysis
# Author: Unanchored STC Analysis Package
# Features modern design with gradient themes and responsive layout

#' Format p-values for display
format_p_value <- function(p_value) {
  if (is.null(p_value) || is.na(p_value) || !is.numeric(p_value)) {
    return("N/A")
  }
  
  if (p_value < 0.001) {
    return("<0.001")
  } else {
    return(sprintf("%.3f", round(p_value, 3)))
  }
}

#' Generate Kaplan-Meier curve for survival outcome vs categorical/binary covariate
generate_km_curve <- function(data, time_var, event_var, covariate, outcome_name, var_name) {
  
  # Check if covariate is categorical or binary
  if (!is.factor(data[[covariate]]) && length(unique(data[[covariate]][!is.na(data[[covariate]])])) > 5) {
    return(NULL) # Skip continuous variables with >5 levels
  }
  
  tryCatch({
    # Validate that required columns exist
    if (!time_var %in% names(data)) {
      stop(paste("Time variable", time_var, "not found in data"))
    }
    if (!event_var %in% names(data)) {
      stop(paste("Event variable", event_var, "not found in data"))
    }
    if (!covariate %in% names(data)) {
      stop(paste("Covariate", covariate, "not found in data"))
    }
    
    # Ensure time variable is numeric
    time_values <- data[[time_var]]
    if (!is.numeric(time_values)) {
      # Try to convert to numeric
      time_values <- suppressWarnings(as.numeric(as.character(time_values)))
      if (all(is.na(time_values))) {
        stop(paste("Time variable", time_var, "cannot be converted to numeric"))
      }
    }
    
    # Ensure event variable is numeric (0/1)
    event_values <- data[[event_var]]
    if (!is.numeric(event_values)) {
      event_values <- suppressWarnings(as.numeric(as.character(event_values)))
      if (all(is.na(event_values))) {
        stop(paste("Event variable", event_var, "cannot be converted to numeric"))
      }
    }
    
    # Create survival object with validated numeric variables
    surv_obj <- Surv(time_values, event_values)
    
    # Fit survival curves
    fit <- survfit(surv_obj ~ data[[covariate]])
    
    # Create survival data for plotting
    surv_data <- data.frame(
      time = fit$time,
      surv = fit$surv,
      group = rep(names(fit$strata), fit$strata),
      n.risk = fit$n.risk,
      n.event = fit$n.event
    )
    
    # Clean up group names
    surv_data$group <- gsub(paste0("data\\[\\[covariate\\]\\]="), "", surv_data$group)
    
    # Generate unique plot ID
    plot_id <- paste0("km_plot_", gsub("[^A-Za-z0-9]", "_", outcome_name), "_", gsub("[^A-Za-z0-9]", "_", var_name))
    
    # Create HTML for KM plot
    km_html <- paste0('
      <div class="km-curve-container" style="margin: 20px 0; background: white; border-radius: 8px; padding: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);">
        <h5 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
          <i class="fas fa-chart-line"></i> Kaplan-Meier Survival Curves: ', outcome_name, ' by ', var_name, '
        </h5>
        <div id="', plot_id, '" style="width: 100%; height: 400px; margin: 15px 0;"></div>
        <script>
          (function() {
            var traces = [];
            var groups = ', jsonlite::toJSON(unique(surv_data$group)), ';
            var colors = ["#2C275B", "#A23877", "#28a745", "#ffc107", "#17a2b8"];
            
            for (var i = 0; i < groups.length; i++) {
              var groupData = ', jsonlite::toJSON(surv_data), '.filter(row => row.group === groups[i]);
              
              traces.push({
                x: groupData.map(row => row.time),
                y: groupData.map(row => row.surv),
                type: "scatter",
                mode: "lines",
                line: { 
                  shape: "hv", 
                  color: colors[i % colors.length],
                  width: 2 
                },
                name: groups[i],
                hovertemplate: "<b>%{fullData.name}</b><br>" +
                             "Time: %{x}<br>" +
                             "Survival: %{y:.3f}<br>" +
                             "<extra></extra>"
              });
            }
            
            var layout = {
              title: {
                text: "Survival Probability Over Time",
                font: { size: 14, color: "#2C275B" }
              },
              xaxis: { 
                title: "Time", 
                zeroline: false,
                gridcolor: "#f0f0f0"
              },
              yaxis: { 
                title: "Survival Probability", 
                zeroline: false,
                range: [0, 1],
                gridcolor: "#f0f0f0"
              },
              showlegend: true,
              legend: {
                x: 0.7,
                y: 0.95,
                bgcolor: "rgba(255,255,255,0.8)",
                bordercolor: "#ddd",
                borderwidth: 1
              },
              plot_bgcolor: "rgba(0,0,0,0)",
              paper_bgcolor: "rgba(0,0,0,0)",
              margin: { l: 60, r: 40, t: 60, b: 60 }
            };
            
            var config = {
              displayModeBar: true,
              modeBarButtonsToRemove: ["lasso2d", "select2d"],
              displaylogo: false,
              toImageButtonOptions: {
                format: "png",
                filename: "survival_curve_', outcome_name, '_', var_name, '",
                height: 400,
                width: 600,
                scale: 2
              }
            };
            
            if (typeof Plotly !== "undefined") {
              Plotly.newPlot("', plot_id, '", traces, layout, config);
            }
          })();
        </script>
      </div>')
    
    return(km_html)
    
  }, error = function(e) {
    # Enhanced error information for debugging
    debug_info <- ""
    if (exists("time_var") && time_var %in% names(data)) {
      time_class <- class(data[[time_var]])
      time_sample <- paste(head(data[[time_var]], 3), collapse = ", ")
      debug_info <- paste0(" Time variable '", time_var, "' has class: ", paste(time_class, collapse = ", "), 
                          ". Sample values: ", time_sample)
    }
    
    return(paste0('<div class="alert alert-warning">
      <strong>KM Curve Error:</strong> Could not generate Kaplan-Meier curve for ', var_name, ': ', e$message, debug_info, '
    </div>'))
  })
}

#' Generate comprehensive HTML report for TEM analysis
#' 
#' @param results List containing TEM analysis results
#' @param report_title Title for the HTML report
#' @param output_dir Directory to save the report
#' @param timestamp Timestamp for the filename
#' @return Path to the generated HTML report file
generate_tem_html_report <- function(results, report_title, output_dir, timestamp) {
  
  # Create timestamped filename
  outcome_types <- unique(sapply(results$outcomes, function(x) x$outcome_type))
  outcome_summary <- paste(outcome_types, collapse = "_")
  
  report_filename <- sprintf("%s_%s_%s.html", 
                           gsub("[^A-Za-z0-9]", "_", report_title),
                           outcome_summary,
                           timestamp)
  
  report_path <- file.path(output_dir, report_filename)
  
  # Generate HTML content
  html_content <- generate_tem_html_content(results, report_title)
  
  # Write to file
  writeLines(html_content, report_path)
  
  return(report_path)
}

#' Generate complete HTML content for TEM report
generate_tem_html_content <- function(results, report_title) {
  
  # Build HTML document
  html <- paste0(
    generate_tem_html_header(report_title),
    generate_tem_html_body(results, report_title),
    generate_tem_html_footer()
  )
  
  return(html)
}

#' Generate HTML header with enhanced CSS styling
generate_tem_html_header <- function(report_title) {
  
  css_styles <- generate_tem_css()
  js_scripts <- generate_tem_javascript()
  
  header <- paste0(
    '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>', report_title, '</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css">
    ', css_styles, '
    ', js_scripts, '
</head>
<body>'
  )
  
  return(header)
}

#' Generate modern CSS with gradient theme
generate_tem_css <- function() {
  
  css <- '<style>
/* Enhanced CSS for TEM HTML Reports */

/* Reset and base styles */
* {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
}

body {
    font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, sans-serif;
    line-height: 1.6;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    color: #333;
}

.container {
    max-width: 1400px;
    margin: 20px auto;
    background: white;
    border-radius: 15px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    overflow: hidden;
    position: relative;
}

/* Header styling */
.header {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    padding: 40px 30px;
    text-align: center;
    position: relative;
}

.header::before {
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: url("data:image/svg+xml,%3Csvg width="60" height="60" viewBox="0 0 60 60" xmlns="http://www.w3.org/2000/svg"%3E%3Cg fill="none" fill-rule="evenodd"%3E%3Cg fill="%23ffffff" fill-opacity="0.05"%3E%3Ccircle cx="30" cy="30" r="4"/%3E%3C/g%3E%3C/g%3E%3C/svg%3E") repeat;
    pointer-events: none;
}

.header h1 {
    font-size: 2.8em;
    font-weight: 700;
    margin-bottom: 10px;
    position: relative;
    z-index: 1;
}

.header .subtitle {
    font-size: 1.2em;
    opacity: 0.9;
    margin-bottom: 25px;
    position: relative;
    z-index: 1;
}

.header .meta-info {
    display: flex;
    justify-content: center;
    flex-wrap: wrap;
    gap: 30px;
    position: relative;
    z-index: 1;
}

.header .meta-item {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 0.95em;
    background: rgba(255, 255, 255, 0.1);
    padding: 8px 15px;
    border-radius: 20px;
    backdrop-filter: blur(10px);
}

.header .meta-item i {
    font-size: 1.1em;
}

/* Navigation tabs */
.nav-tabs {
    display: flex;
    background: #f8f9fa;
    border-bottom: 3px solid #e9ecef;
    position: sticky;
    top: 0;
    z-index: 100;
}

.nav-tab {
    flex: 1;
    padding: 18px 20px;
    background: transparent;
    border: none;
    cursor: pointer;
    font-size: 1em;
    font-weight: 500;
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
    background: rgba(162, 56, 119, 0.1);
    color: #A23877;
}

.sub-tab.active {
    background: linear-gradient(135deg, rgba(44, 39, 91, 0.1) 0%, rgba(162, 56, 119, 0.1) 100%);
    color: #A23877;
    border-bottom-color: #A23877;
    font-weight: 600;
}

/* Tab content */
.tab-content {
    display: none;
    padding: 30px;
    min-height: 400px;
}

.tab-content.active {
    display: block;
}

.sub-tab-content {
    display: none;
}

.sub-tab-content.active {
    display: block;
}

/* Section styling */
.section {
    margin-bottom: 40px;
}

.section h2 {
    color: #2C275B;
    font-size: 2.2em;
    margin-bottom: 15px;
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

/* Significance indicators */
.significant-yes {
    color: #28a745;
    font-weight: 600;
}

.significant-no {
    color: #6c757d;
}

/* Outcome analysis sections */
.outcome-analysis-section {
    margin-bottom: 40px;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
}

.outcome-header {
    background: linear-gradient(135deg, #2C275B 0%, #A23877 100%);
    color: white;
    padding: 20px;
}

.outcome-type-badge {
    background: rgba(255, 255, 255, 0.2);
    padding: 6px 15px;
    border-radius: 20px;
    font-size: 0.8em;
    font-weight: 500;
}

.model-specification {
    background: #f8f9fa;
    padding: 15px 20px;
    border-left: 4px solid #A23877;
    font-family: "Courier New", monospace;
    font-size: 0.9em;
    color: #2C275B;
}

.table-container {
    background: white;
    border: 1px solid #e9ecef;
    border-top: none;
}

/* Selection steps styling */
.selection-step {
    background: white;
    border: 1px solid #e9ecef;
    border-radius: 8px;
    padding: 20px;
    margin: 15px 0;
    position: relative;
    transition: all 0.3s ease;
}

.selection-step:hover {
    box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
    border-color: #A23877;
}

.selection-step h5 {
    color: #2C275B;
    margin-bottom: 10px;
    display: flex;
    align-items: center;
    gap: 10px;
    font-size: 1.1em;
}

.step-action {
    color: #666;
    font-style: italic;
    margin-bottom: 10px;
}

.step-details {
    display: flex;
    flex-wrap: wrap;
    gap: 20px;
    margin-top: 15px;
}

.step-detail-item {
    background: #f8f9fa;
    border-radius: 6px;
    padding: 10px 15px;
    min-width: 120px;
}

.step-detail-item .label {
    font-size: 0.8em;
    color: #666;
    margin-bottom: 5px;
    font-weight: 500;
}

.step-detail-item .value {
    font-size: 1.1em;
    font-weight: 600;
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

/* Model recommendation sections */
.model-recommendation {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 12px;
    padding: 25px;
    margin: 25px 0;
    border-left: 4px solid #A23877;
}

.model-recommendation h4 {
    color: #2C275B;
    margin-bottom: 15px;
    display: flex;
    align-items: center;
    gap: 10px;
}

.variable-list {
    list-style: none;
    padding: 0;
}

.variable-list li {
    padding: 8px 0;
    padding-left: 25px;
    position: relative;
}

.variable-list li::before {
    content: "▶";
    color: #A23877;
    position: absolute;
    left: 0;
    top: 8px;
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
    
    .sub-tabs {
        flex-direction: column;
    }
    
    .tab-content {
        padding: 20px;
    }
    
    .summary-cards {
        grid-template-columns: 1fr;
    }
    
    .step-details {
        flex-direction: column;
    }
}

/* Print styles */
@media print {
    .nav-tabs, .sub-tabs, .footer {
        display: none;
    }
    
    .tab-content {
        display: block !important;
    }
    
    .sub-tab-content {
        display: block !important;
    }
    
    .container {
        box-shadow: none;
        margin: 0;
    }
}
</style>'
  
  return(css)
}

#' Generate JavaScript for interactive functionality
generate_tem_javascript <- function() {
  
  js <- '
<script>
// Enhanced tab functionality with sub-tab support
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
        
        // Activate first sub-tab if exists
        var firstSubTab = targetContent.querySelector(".sub-tab");
        if (firstSubTab) {
            firstSubTab.click();
        }
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

// Enhanced sub-tab functionality  
function showSubTab(subTabName) {
    var targetElement = document.getElementById(subTabName);
    if (!targetElement) return;
    
    var parentTab = targetElement.closest(".tab-content");
    if (!parentTab) return;
    
    var subContents = parentTab.querySelectorAll(".sub-tab-content");
    for (var i = 0; i < subContents.length; i++) {
        subContents[i].classList.remove("active");
    }
    
    var subTabs = parentTab.querySelectorAll(".sub-tab");
    for (var i = 0; i < subTabs.length; i++) {
        subTabs[i].classList.remove("active");
    }
    
    var targetSubContent = document.getElementById(subTabName);
    if (targetSubContent) {
        targetSubContent.classList.add("active");
    }
    
    // Find and activate the corresponding sub-tab button
    var allSubTabs = parentTab.querySelectorAll(".sub-tab");
    for (var i = 0; i < allSubTabs.length; i++) {
        var subTab = allSubTabs[i];
        if (subTab.onclick && subTab.onclick.toString().indexOf(subTabName) > -1) {
            subTab.classList.add("active");
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

#' Generate HTML body content
generate_tem_html_body <- function(results, report_title) {
  
  body <- paste0(
    generate_tem_header_section(results, report_title),
    generate_tem_navigation_tabs(),
    generate_tem_overview_tab(results),
    generate_tem_univariate_tab(results),
    generate_tem_multivariate_tab(results),
    generate_tem_consolidated_results_tab(results),
    generate_tem_methodology_tab(results)
  )
  
  return(body)
}

#' Generate header section with summary
generate_tem_header_section <- function(results, report_title) {
  
  # Calculate summary statistics
  n_outcomes <- length(results$outcomes)
  n_covariates <- results$parameters$n_covariates
  n_significant_uni <- nrow(results$univariate_table[results$univariate_table$Significant == "✓", ])
  n_significant_multi <- nrow(results$multivariate_table[results$multivariate_table$Significant == "✓", ])
  
  header <- paste0(
    '<div class="header">
        <h1>', report_title, '</h1>
        <div class="subtitle">Comprehensive Treatment Effect Modifier Analysis</div>
        <div class="meta-info">
            <div class="meta-item">
                <i class="fas fa-calendar"></i>
                <span>Generated: ', format(Sys.time(), "%B %d, %Y at %H:%M"), '</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-chart-line"></i>
                <span>', n_outcomes, ' Outcomes Analyzed</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-cogs"></i>
                <span>', n_covariates, ' Covariates Tested</span>
            </div>
            <div class="meta-item">
                <i class="fas fa-star"></i>
                <span>', n_significant_multi, ' Significant Effects</span>
            </div>
        </div>
    </div>'
  )
  
  return(header)
}

#' Generate navigation tabs
generate_tem_navigation_tabs <- function() {
  
  tabs <- '
    <div class="nav-tabs">
        <button class="nav-tab active" onclick="showTab(\'overview\')">
            <i class="fas fa-home"></i> Overview
        </button>
        <button class="nav-tab" onclick="showTab(\'univariate\')">
            <i class="fas fa-chart-bar"></i> Univariate Analysis
        </button>
        <button class="nav-tab" onclick="showTab(\'multivariate\')">
            <i class="fas fa-project-diagram"></i> Multivariate Analysis
        </button>
        <button class="nav-tab" onclick="showTab(\'consolidated\')">
            <i class="fas fa-list-alt"></i> Consolidated Results
        </button>
        <button class="nav-tab" onclick="showTab(\'methodology\')">
            <i class="fas fa-book"></i> Methodology
        </button>
    </div>'
  
  return(tabs)
}

#' Generate overview tab content
generate_tem_overview_tab <- function(results) {
  
  # Summary statistics
  outcome_types <- table(sapply(results$outcomes, function(x) x$outcome_type))
  
  # Create summary cards
  summary_cards <- paste0('
    <div class="summary-cards">
        <div class="summary-card">
            <h4><i class="fas fa-list"></i> Total Outcomes</h4>
            <div class="value">', length(results$outcomes), '</div>
            <div class="description">Analyzed across multiple types</div>
        </div>
        <div class="summary-card">
            <h4><i class="fas fa-filter"></i> Univariate Significant</h4>
            <div class="value">', nrow(results$univariate_table[results$univariate_table$Significant == "✓", ]), '</div>
            <div class="description">At α = ', results$parameters$univariate_alpha, '</div>
        </div>
        <div class="summary-card">
            <h4><i class="fas fa-check-circle"></i> Multivariate Significant</h4>
            <div class="value">', nrow(results$multivariate_table[results$multivariate_table$Significant == "✓", ]), '</div>
            <div class="description">At α = ', results$parameters$multivariate_alpha, '</div>
        </div>
        <div class="summary-card">
            <h4><i class="fas fa-cog"></i> Selection Method</h4>
            <div class="value" style="font-size: 1.5em;">', toupper(results$parameters$selection_method), '</div>
            <div class="description">Variable selection approach</div>
        </div>
    </div>')
  
  # Outcome type breakdown
  outcome_breakdown <- '<div class="section"><h3>Outcome Type Distribution</h3><ul>'
  for (type in names(outcome_types)) {
    outcome_breakdown <- paste0(outcome_breakdown, 
                               '<li><strong>', stringr::str_to_title(type), ':</strong> ', 
                               outcome_types[type], ' outcome(s)</li>')
  }
  outcome_breakdown <- paste0(outcome_breakdown, '</ul></div>')
  
  overview <- paste0(
    '<div id="overview" class="tab-content active">
        <div class="section">
            <h2>Analysis Overview</h2>
            <p>This comprehensive TEM analysis examined ', length(results$outcomes), ' outcomes using ', 
            results$parameters$n_covariates, ' covariates to identify potential treatment effect modifiers.</p>
            ', summary_cards, '
            ', outcome_breakdown, '
        </div>
    </div>'
  )
  
  return(overview)
}

#' Generate univariate analysis tab
generate_tem_univariate_tab <- function(results) {
  
  # Generate sub-tab content for outcomes
  outcome_tabs <- ""
  outcome_content <- ""
  first_outcome <- TRUE
  
  for (outcome_name in names(results$outcomes)) {
    outcome_data <- results$outcomes[[outcome_name]]
    outcome_univariate <- results$univariate_table[results$univariate_table$Outcome == outcome_name, ]
    
    # Create sub-tab button
    sub_tab_id <- paste0("uni_", gsub("[^A-Za-z0-9]", "_", outcome_name))
    active_class <- if (first_outcome) " active" else ""
    
    outcome_tabs <- paste0(outcome_tabs, '
      <button class="sub-tab', active_class, '" onclick="showSubTab(\'', sub_tab_id, '\')">
        <i class="fas fa-chart-bar"></i> ', outcome_name, '
      </button>')
    
    # Create model specification
    model_spec <- switch(outcome_data$outcome_type,
      "survival" = paste0("Cox Proportional Hazards: Surv(", 
                         if(!is.null(outcome_data$time_var)) outcome_data$time_var else "time", ", ", 
                         if(!is.null(outcome_data$event_var)) outcome_data$event_var else "event", 
                         ") ~ covariate"),
      "binary" = paste0("Logistic Regression: ", outcome_data$outcome_var, " ~ covariate"),
      "count" = paste0("Poisson Regression: ", outcome_data$outcome_var, " ~ covariate"),
      "continuous" = paste0("Linear Regression: ", outcome_data$outcome_var, " ~ covariate")
    )
    
    # Create results table
    table_html <- ""
    km_curves_html <- ""
    
    if (nrow(outcome_univariate) > 0) {
      table_html <- '<table class="analysis-table" style="width: 100%; margin-top: 15px;">
        <thead>
          <tr>
            <th>Variable</th>
            <th>Effect Type</th>
            <th>Effect Estimate (95% CI)</th>
            <th>P-value</th>
            <th>Significant</th>
            <th>N</th>
          </tr>
        </thead>
        <tbody>'
      
      # Generate KM curves for survival outcomes with categorical/binary covariates
      if (outcome_data$outcome_type == "survival" && !is.null(results$original_data)) {
        km_curves_list <- list()
        
        # Generate KM curves for categorical/binary variables
        for (i in 1:nrow(outcome_univariate)) {
          row <- outcome_univariate[i, ]
          var_name <- row$Variable
          
          # Check if variable exists in original data and is categorical/binary
          if (var_name %in% names(results$original_data)) {
            km_curve <- generate_km_curve(
              data = results$original_data,
              time_var = if(!is.null(outcome_data$time_var)) outcome_data$time_var else results$time_vars[[1]],
              event_var = if(!is.null(outcome_data$event_var)) outcome_data$event_var else results$event_vars[[1]],
              covariate = var_name,
              outcome_name = outcome_name,
              var_name = var_name
            )
            
            if (!is.null(km_curve)) {
              km_curves_list[[var_name]] <- km_curve
            }
          }
        }
        
        if (length(km_curves_list) > 0) {
          km_curves_html <- paste0('
            <div class="km-curves-section" style="margin-top: 30px;">
              <h4 style="color: #2C275B; margin-bottom: 20px; display: flex; align-items: center; gap: 8px;">
                <i class="fas fa-chart-line"></i> Kaplan-Meier Survival Curves
              </h4>
              <p style="color: #666; margin-bottom: 20px; font-size: 0.9em;">
                Survival curves are shown for categorical/binary covariates only. Continuous variables with >5 unique values are excluded from curve visualization.
              </p>
              ', paste(km_curves_list, collapse = ""), '
            </div>')
        }
      }
      
      for (i in 1:nrow(outcome_univariate)) {
        row <- outcome_univariate[i, ]
        sig_class <- if (row$Significant == "✓") "significant-yes" else "significant-no"
        
        table_html <- paste0(table_html, '
          <tr>
            <td><strong>', row$Variable, '</strong></td>
            <td>', row$Effect_Type, '</td>
            <td>', row$Effect_Estimate_CI, '</td>
            <td>', format_p_value(as.numeric(row$P_value)), '</td>
            <td class="', sig_class, '">', row$Significant, '</td>
            <td>', row$N, '</td>
          </tr>')
      }
      
      table_html <- paste0(table_html, '
        </tbody>
      </table>')
    } else {
      table_html <- paste0('<div class="alert alert-info" style="margin-top: 15px;">
        <strong>No significant associations found</strong> for this outcome at α = ', results$parameters$univariate_alpha, '.
      </div>')
    }
    
    # Create sub-tab content
    active_content_class <- if (first_outcome) " active" else ""
    
    outcome_content <- paste0(outcome_content, '
      <div id="', sub_tab_id, '" class="sub-tab-content', active_content_class, '">
        <div class="outcome-analysis-section" style="margin-bottom: 20px;">
          <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
            <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
              <span>', outcome_name, '</span>
              <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">', 
              stringr::str_to_title(outcome_data$outcome_type), '</span>
            </h3>
          </div>
          <div class="model-specification" style="background: #f8f9fa; padding: 12px 20px; border-left: 4px solid #A23877; font-family: monospace; font-size: 0.9em; color: #2C275B;">
            <strong>Model:</strong> ', model_spec, '
          </div>
          <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
            <div style="padding: 20px;">
              ', table_html, '
              ', km_curves_html, '
            </div>
          </div>
        </div>
      </div>')
    
    first_outcome <- FALSE
  }
  
  if (outcome_tabs == "") {
    outcome_content <- '<div class="alert alert-info">
      <strong>No Results:</strong> No univariate associations were detected for any outcome. 
      This may indicate insufficient statistical power or lack of associations at the specified significance level (α = ' + 
      results$parameters$univariate_alpha + ').
    </div>'
  }
  
  univariate <- paste0(
    '<div id="univariate" class="tab-content">
        <div class="section">
            <h2>Univariate Analysis Results</h2>
            <p>Individual covariate effects for each outcome. Each analysis tests one covariate against one outcome (α = ', results$parameters$univariate_alpha, ').</p>
            
            <div class="sub-tabs">
                ', outcome_tabs, '
            </div>
            
            ', outcome_content, '
        </div>
    </div>'
  )
  
  return(univariate)
}

#' Generate multivariate analysis tab with sub-tabs and backward selection steps
generate_tem_multivariate_tab <- function(results) {
  
  # Generate sub-tab content for outcomes
  outcome_tabs <- ""
  outcome_content <- ""
  first_outcome <- TRUE
  
  for (outcome_name in names(results$outcomes)) {
    outcome_data <- results$outcomes[[outcome_name]]
    
    # Get multivariate results for this outcome
    outcome_multivariate <- results$multivariate_table[results$multivariate_table$Outcome == outcome_name, ]
    
    # Create sub-tab button
    sub_tab_id <- paste0("multi_", gsub("[^A-Za-z0-9]", "_", outcome_name))
    active_class <- if (first_outcome) " active" else ""
    
    outcome_tabs <- paste0(outcome_tabs, '
      <button class="sub-tab', active_class, '" onclick="showSubTab(\'', sub_tab_id, '\')">
        <i class="fas fa-project-diagram"></i> ', outcome_name, '
      </button>')
    
    # Create model specification
    model_spec <- ""
    if (nrow(outcome_multivariate) > 0) {
      # Get variables included in the model
      included_vars <- unique(outcome_multivariate$Variable)
      var_list <- paste(included_vars, collapse = " + ")
      
      model_spec <- switch(outcome_data$outcome_type,
        "survival" = paste0("Cox Proportional Hazards: Surv(", 
                           if(!is.null(outcome_data$time_var)) outcome_data$time_var else "time", ", ", 
                           if(!is.null(outcome_data$event_var)) outcome_data$event_var else "event", 
                           ") ~ ", var_list),
        "binary" = paste0("Logistic Regression: ", outcome_data$outcome_var, " ~ ", var_list),
        "count" = paste0("Poisson Regression: ", outcome_data$outcome_var, " ~ ", var_list),
        "continuous" = paste0("Linear Regression: ", outcome_data$outcome_var, " ~ ", var_list)
      )
    } else {
      model_spec <- "No multivariate model fitted (no variables met univariate threshold)"
    }
    
    # Create results table HTML
    table_html <- ""
    if (nrow(outcome_multivariate) > 0) {
      table_html <- '<table class="analysis-table" style="width: 100%; margin-top: 15px;">
        <thead>
          <tr>
            <th>Variable</th>
            <th>Effect Type</th>
            <th>Effect Estimate (95% CI)</th>
            <th>P-value</th>
            <th>Significant</th>
          </tr>
        </thead>
        <tbody>'
      
      for (i in 1:nrow(outcome_multivariate)) {
        row <- outcome_multivariate[i, ]
        sig_class <- if (row$Significant == "✓") "significant-yes" else "significant-no"
        
        table_html <- paste0(table_html, '
          <tr>
            <td><strong>', row$Variable, '</strong></td>
            <td>', row$Effect_Type, '</td>
            <td>', row$Effect_Estimate_CI, '</td>
            <td>', format_p_value(as.numeric(row$P_value)), '</td>
            <td class="', sig_class, '">', row$Significant, '</td>
          </tr>')
      }
      
      table_html <- paste0(table_html, '
        </tbody>
      </table>')
    } else {
      table_html <- paste0('<div class="alert alert-info" style="margin-top: 15px;">
        <strong>No multivariate model fitted</strong> - no variables met the univariate significance threshold (α = ', results$parameters$univariate_alpha, ').
      </div>')
    }
    
    # Generate backward selection steps HTML
    selection_steps_html <- ""
    if (!is.null(outcome_data$multivariate_results) && !is.null(outcome_data$multivariate_results$selection_steps) && length(outcome_data$multivariate_results$selection_steps) > 0) {
      selection_steps_html <- paste0('
        <div class="selection-steps-section" style="margin-top: 30px;">
          <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
            <i class="fas fa-list-ol"></i> Backward Selection Process
          </h4>
          <p style="color: #666; margin-bottom: 20px; font-size: 0.9em;">
            Step-by-step backward elimination process (α = ', results$parameters$multivariate_alpha, '). Variables with highest p-values are eliminated until all remaining variables meet the significance threshold.
          </p>')
      
      for (i in 1:length(outcome_data$multivariate_results$selection_steps)) {
        step <- outcome_data$multivariate_results$selection_steps[[i]]
        
        # Create step details
        step_details <- '<div class="step-details">'
        
        if (!is.null(step$aic) && !is.na(step$aic)) {
          step_details <- paste0(step_details, '
            <div class="step-detail-item">
              <div class="label">AIC</div>
              <div class="value">', round(step$aic, 2), '</div>
            </div>')
        }
        
        if (!is.null(step$p_value) && !is.na(step$p_value)) {
          step_details <- paste0(step_details, '
            <div class="step-detail-item">
              <div class="label">P-value</div>
              <div class="value">', format_p_value(step$p_value), '</div>
            </div>')
        }
        
        if (!is.null(step$variable) && !is.na(step$variable) && step$variable != "") {
          step_details <- paste0(step_details, '
            <div class="step-detail-item">
              <div class="label">Variable Removed</div>
              <div class="value">', step$variable, '</div>
            </div>')
        }
        
        if (!is.null(step$remaining_vars) && length(step$remaining_vars) > 0) {
          step_details <- paste0(step_details, '
            <div class="step-detail-item">
              <div class="label">Remaining Variables</div>
              <div class="value">', length(step$remaining_vars), '</div>
            </div>')
        }
        
        step_details <- paste0(step_details, '</div>')
        
        selection_steps_html <- paste0(selection_steps_html, '
          <div class="selection-step">
            <h5>
              <i class="fas fa-step-forward"></i> Step ', i, ': ', if(!is.null(step$step)) step$step else paste0("Selection Step ", i), '
            </h5>
            <div class="step-action">', if(!is.null(step$action)) step$action else "Variable selection step", '</div>
            ', step_details, '
            
            <!-- Full regression table for this step -->
            ', if(!is.null(step$regression_table) && nrow(step$regression_table) > 0) {
              paste0('<div class="step-regression-table" style="margin-top: 15px;">
                <h6 style="color: #2C275B; margin-bottom: 10px;"><i class="fas fa-table"></i> Model Coefficients</h6>
                <div class="table-responsive">
                  <table class="table table-sm table-bordered">
                    <thead class="table-header">
                      <tr>
                        <th>Variable</th>
                        <th>Type</th>
                        ', if("HR_CI" %in% names(step$regression_table)) '<th>HR (95% CI)</th>' else 
                           if("OR_CI" %in% names(step$regression_table)) '<th>OR (95% CI)</th>' else 
                           if("RR_CI" %in% names(step$regression_table)) '<th>RR (95% CI)</th>' else 
                           '<th>Coefficient</th>', '
                        <th>P-value</th>
                      </tr>
                    </thead>
                    <tbody>',
                      paste(apply(step$regression_table, 1, function(row) {
                        effect_value <- if("HR_CI" %in% names(step$regression_table)) row["HR_CI"] else 
                                       if("OR_CI" %in% names(step$regression_table)) row["OR_CI"] else 
                                       if("RR_CI" %in% names(step$regression_table)) row["RR_CI"] else 
                                       row["Coefficient"]
                        paste0('<tr>
                          <td><strong>', row["Variable"], '</strong></td>
                          <td><span class="badge badge-info">', row["Variable_Type"], '</span></td>
                          <td>', effect_value, '</td>
                          <td>', format_p_value(as.numeric(row["P_value"])), '</td>
                        </tr>')
                      }), collapse = ''), '
                    </tbody>
                  </table>
                </div>
              </div>')
            } else {
              ''
            }, '
          </div>')
      }
      
      selection_steps_html <- paste0(selection_steps_html, '
        </div>')
    } else if (nrow(outcome_multivariate) > 0) {
      selection_steps_html <- paste0('
        <div class="alert alert-info" style="margin-top: 20px;">
          <strong>Selection Steps:</strong> All variables entered the final model without requiring backward elimination (all p-values ≤ α = ', results$parameters$multivariate_alpha, ').
        </div>')
    }
    
    # Create sub-tab content
    active_content_class <- if (first_outcome) " active" else ""
    
    outcome_content <- paste0(outcome_content, '
      <div id="', sub_tab_id, '" class="sub-tab-content', active_content_class, '">
        <div class="outcome-analysis-section" style="margin-bottom: 20px;">
          <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
            <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
              <span>', outcome_name, '</span>
              <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">', 
              stringr::str_to_title(outcome_data$outcome_type), '</span>
            </h3>
          </div>
          <div class="model-specification" style="background: #f8f9fa; padding: 12px 20px; border-left: 4px solid #A23877; font-family: monospace; font-size: 0.9em; color: #2C275B;">
            <strong>Final Model:</strong> ', model_spec, '
          </div>
          <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
            <div style="padding: 20px;">
              ', table_html, '
              ', selection_steps_html, '
            </div>
          </div>
        </div>
      </div>')
    
    first_outcome <- FALSE
  }
  
  if (outcome_tabs == "") {
    outcome_content <- '<div class="alert alert-info">
      <strong>No Results:</strong> No multivariate models were fitted. 
      This occurs when no covariates meet the univariate significance threshold (α = ' + 
      results$parameters$univariate_alpha + ') for any outcome.
    </div>'
  }
  
  multivariate <- paste0(
    '<div id="multivariate" class="tab-content">
        <div class="section">
            <h2>Multivariate Analysis Results</h2>
            <p>Adjusted effect estimates from final multivariate models using ', stringr::str_to_title(results$parameters$selection_method), ' selection (α = ', results$parameters$multivariate_alpha, ').</p>
            
            <div class="sub-tabs">
                ', outcome_tabs, '
            </div>
            
            ', outcome_content, '
        </div>
    </div>'
  )
  
  return(multivariate)
}

#' Generate consolidated results tab with base-case and sensitivity model recommendations
generate_tem_consolidated_results_tab <- function(results) {
  
  # Generate sub-tab content for outcomes
  outcome_tabs <- ""
  outcome_content <- ""
  first_outcome <- TRUE
  
  for (outcome_name in names(results$outcomes)) {
    outcome_data <- results$outcomes[[outcome_name]]
    
    # Create sub-tab button
    sub_tab_id <- paste0("cons_", gsub("[^A-Za-z0-9]", "_", outcome_name))
    active_class <- if (first_outcome) " active" else ""
    
    outcome_tabs <- paste0(outcome_tabs, '
      <button class="sub-tab', active_class, '" onclick="showSubTab(\'', sub_tab_id, '\')">
        <i class="fas fa-clipboard-list"></i> ', outcome_name, '
      </button>')
    
    # Variable selection summary
    all_vars <- names(outcome_data$univariate_results$results)
    significant_univariate <- outcome_data$univariate_results$significant_vars
    tested_vars <- length(all_vars)
    significant_uni_count <- length(significant_univariate)
    
    # Get multivariate significant variables
    outcome_multivariate <- results$multivariate_table[results$multivariate_table$Outcome == outcome_name & results$multivariate_table$Significant == "✓", ]
    significant_multivariate <- unique(outcome_multivariate$Variable)
    significant_multi_count <- length(significant_multivariate)
    
    # Determine base-case and sensitivity variables
    base_case_vars <- significant_multivariate  # Variables significant in final multivariate
    univariate_only_vars <- setdiff(significant_univariate, significant_multivariate)  # Significant only in univariate
    sensitivity_vars <- c(base_case_vars, univariate_only_vars)  # Base-case + univariate-only
    
    # Create final results summary tables
    final_univariate_table <- ""
    if (significant_uni_count > 0) {
      outcome_uni_results <- results$univariate_table[results$univariate_table$Outcome == outcome_name & results$univariate_table$Significant == "✓", ]
      
      final_univariate_table <- '<table class="analysis-table" style="width: 100%; margin-top: 15px;">
        <thead>
          <tr>
            <th>Variable</th>
            <th>Effect Type</th>
            <th>Effect Estimate (95% CI)</th>
            <th>P-value</th>
            <th>N</th>
          </tr>
        </thead>
        <tbody>'
      
      for (i in 1:nrow(outcome_uni_results)) {
        row <- outcome_uni_results[i, ]
        final_univariate_table <- paste0(final_univariate_table, '
          <tr>
            <td><strong>', row$Variable, '</strong></td>
            <td>', row$Effect_Type, '</td>
            <td>', row$Effect_Estimate_CI, '</td>
            <td>', format_p_value(as.numeric(row$P_value)), '</td>
            <td>', row$N, '</td>
          </tr>')
      }
      
      final_univariate_table <- paste0(final_univariate_table, '
        </tbody>
      </table>')
    } else {
      final_univariate_table <- paste0('<div class="alert alert-info">No variables were significant in univariate analysis (α = ', results$parameters$univariate_alpha, ').</div>')
    }
    
    # Final multivariate table
    final_multivariate_table <- ""
    if (significant_multi_count > 0) {
      final_multivariate_table <- '<table class="analysis-table" style="width: 100%; margin-top: 15px;">
        <thead>
          <tr>
            <th>Variable</th>
            <th>Effect Type</th>
            <th>Effect Estimate (95% CI)</th>
            <th>P-value</th>
          </tr>
        </thead>
        <tbody>'
      
      for (i in 1:nrow(outcome_multivariate)) {
        row <- outcome_multivariate[i, ]
        final_multivariate_table <- paste0(final_multivariate_table, '
          <tr>
            <td><strong>', row$Variable, '</strong></td>
            <td>', row$Effect_Type, '</td>
            <td>', row$Effect_Estimate_CI, '</td>
            <td>', format_p_value(as.numeric(row$P_value)), '</td>
          </tr>')
      }
      
      final_multivariate_table <- paste0(final_multivariate_table, '
        </tbody>
      </table>')
    } else {
      final_multivariate_table <- paste0('<div class="alert alert-info">No variables remained significant in multivariate analysis (α = ', results$parameters$multivariate_alpha, ').</div>')
    }
    
    # Model recommendations
    base_case_html <- ""
    sensitivity_html <- ""
    
    if (length(base_case_vars) > 0) {
      base_case_list <- paste0('<ul class="variable-list">', 
                              paste0('<li>', base_case_vars, '</li>', collapse = ''), 
                              '</ul>')
      base_case_html <- paste0('
        <div class="model-recommendation">
          <h4><i class="fas fa-star"></i> Base-Case Model Variables</h4>
          <p style="margin-bottom: 15px; color: #666;">Variables that remained significant in the final multivariate model (α = ', results$parameters$multivariate_alpha, '):</p>
          ', base_case_list, '
        </div>')
    } else {
      base_case_html <- '
        <div class="model-recommendation">
          <h4><i class="fas fa-star"></i> Base-Case Model Variables</h4>
          <div class="alert alert-warning">
            <strong>No base-case variables identified:</strong> No variables remained significant in multivariate analysis.
          </div>
        </div>'
    }
    
    if (length(sensitivity_vars) > 0) {
      if (length(univariate_only_vars) > 0) {
        sensitivity_list <- paste0('<ul class="variable-list">', 
                                  paste0('<li><strong>', base_case_vars, '</strong> (multivariate significant)</li>', collapse = ''), 
                                  paste0('<li>', univariate_only_vars, ' (univariate only)</li>', collapse = ''), 
                                  '</ul>')
        sensitivity_desc <- paste0('Base-case variables plus variables significant only in univariate analysis (', length(univariate_only_vars), ' additional variable(s)):')
      } else {
        sensitivity_list <- paste0('<ul class="variable-list">', 
                                  paste0('<li>', sensitivity_vars, '</li>', collapse = ''), 
                                  '</ul>')
        sensitivity_desc <- 'Same as base-case model (no additional univariate-only variables):'
      }
      
      sensitivity_html <- paste0('
        <div class="model-recommendation">
          <h4><i class="fas fa-shield-alt"></i> Sensitivity Model Variables</h4>
          <p style="margin-bottom: 15px; color: #666;">', sensitivity_desc, '</p>
          ', sensitivity_list, '
        </div>')
    } else {
      sensitivity_html <- '
        <div class="model-recommendation">
          <h4><i class="fas fa-shield-alt"></i> Sensitivity Model Variables</h4>
          <div class="alert alert-warning">
            <strong>No sensitivity variables identified:</strong> No variables were significant in either analysis.
          </div>
        </div>'
    }
    
    # Create sub-tab content
    active_content_class <- if (first_outcome) " active" else ""
    
    outcome_content <- paste0(outcome_content, '
      <div id="', sub_tab_id, '" class="sub-tab-content', active_content_class, '">
        <div class="outcome-analysis-section" style="margin-bottom: 20px;">
          <div class="outcome-header" style="background: linear-gradient(135deg, #2C275B 0%, #A23877 100%); color: white; padding: 15px 20px; border-radius: 8px 8px 0 0; margin-bottom: 0;">
            <h3 style="margin: 0; display: flex; justify-content: space-between; align-items: center;">
              <span>', outcome_name, '</span>
              <span class="outcome-type-badge" style="background: rgba(255,255,255,0.2); padding: 4px 12px; border-radius: 15px; font-size: 0.8em; font-weight: 500;">', 
              stringr::str_to_title(outcome_data$outcome_type), '</span>
            </h3>
          </div>
          
          <div class="table-container" style="background: white; border: 1px solid #e9ecef; border-top: none; border-radius: 0 0 8px 8px;">
            <div style="padding: 20px;">
              
              <!-- Analysis Summary -->
              <div class="analysis-summary" style="background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 25px;">
                <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
                  <i class="fas fa-chart-pie"></i> Analysis Summary
                </h4>
                <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 20px;">
                  <div class="summary-stat">
                    <div class="value" style="font-size: 1.5em; font-weight: 700; color: #A23877;">', tested_vars, '</div>
                    <div class="label" style="color: #666; font-size: 0.9em;">Variables Tested</div>
                  </div>
                  <div class="summary-stat">
                    <div class="value" style="font-size: 1.5em; font-weight: 700; color: #A23877;">', significant_uni_count, '</div>
                    <div class="label" style="color: #666; font-size: 0.9em;">Univariate Significant</div>
                  </div>
                  <div class="summary-stat">
                    <div class="value" style="font-size: 1.5em; font-weight: 700; color: #A23877;">', significant_multi_count, '</div>
                    <div class="label" style="color: #666; font-size: 0.9em;">Multivariate Significant</div>
                  </div>
                  <div class="summary-stat">
                    <div class="value" style="font-size: 1.5em; font-weight: 700; color: #A23877;">', length(sensitivity_vars), '</div>
                    <div class="label" style="color: #666; font-size: 0.9em;">Sensitivity Variables</div>
                  </div>
                </div>
              </div>
              
              <!-- Final Results -->
              <div class="subsection" style="margin-bottom: 30px;">
                <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
                  <i class="fas fa-check-circle"></i> Final Univariate Results
                </h4>
                <p style="color: #666; margin-bottom: 15px; font-size: 0.9em;">
                  Variables with significant univariate associations (α = ', results$parameters$univariate_alpha, '):
                </p>
                ', final_univariate_table, '
              </div>
              
              <div class="subsection" style="margin-bottom: 30px;">
                <h4 style="color: #2C275B; margin-bottom: 15px; display: flex; align-items: center; gap: 8px;">
                  <i class="fas fa-project-diagram"></i> Final Multivariate Results
                </h4>
                <p style="color: #666; margin-bottom: 15px; font-size: 0.9em;">
                  Variables with significant adjusted effects (α = ', results$parameters$multivariate_alpha, '):
                </p>
                ', final_multivariate_table, '
              </div>
              
              <!-- Model Recommendations -->
              <div class="subsection">
                <h4 style="color: #2C275B; margin-bottom: 20px; display: flex; align-items: center; gap: 8px;">
                  <i class="fas fa-lightbulb"></i> Model Recommendations
                </h4>
                
                ', base_case_html, '
                ', sensitivity_html, '
                
                <div class="alert alert-info" style="margin-top: 20px;">
                  <strong><i class="fas fa-info-circle"></i> Recommendation:</strong> Use the base-case model for primary analysis. 
                  Consider the sensitivity model to assess the robustness of findings and explore potential confounders 
                  that may only show effects in univariate analysis.
                </div>
              </div>
              
            </div>
          </div>
        </div>
      </div>')
    
    first_outcome <- FALSE
  }
  
  if (outcome_tabs == "") {
    outcome_content <- '<div class="alert alert-info">
      <strong>No Results:</strong> No outcomes were analyzed. Please check your data and analysis parameters.
    </div>'
  }
  
  consolidated <- paste0(
    '<div id="consolidated" class="tab-content">
        <div class="section">
            <h2>Consolidated Results</h2>
            <p>Final analysis summaries with base-case and sensitivity model recommendations for each outcome. Base-case models include variables significant in multivariate analysis; sensitivity models add variables significant only in univariate analysis.</p>
            
            <div class="sub-tabs">
                ', outcome_tabs, '
            </div>
            
            ', outcome_content, '
        </div>
    </div>'
  )
  
  return(consolidated)
}

#' Generate methodology tab
generate_tem_methodology_tab <- function(results) {
  
  methodology <- paste0(
    '<div id="methodology" class="tab-content">
        <div class="section">
            <h2>Analysis Methodology</h2>
            
            <div class="methodology-section">
                <h3>Statistical Approach</h3>
                <ul class="variable-list">
                    <li><strong>Univariate Analysis:</strong> Each covariate tested individually against each outcome using appropriate regression models</li>
                    <li><strong>Multivariate Analysis:</strong> ', stringr::str_to_title(results$parameters$selection_method), ' variable selection starting with all univariate-significant variables</li>
                    <li><strong>Significance Thresholds:</strong> α = ', results$parameters$univariate_alpha, ' for univariate screening, α = ', results$parameters$multivariate_alpha, ' for multivariate retention</li>
                    <li><strong>P-value Formatting:</strong> Values rounded to 3 decimal places, with "&lt;0.001" for very small p-values</li>
                </ul>
            </div>
            
            <div class="methodology-section">
                <h3>Outcome-Specific Models</h3>
                <ul class="variable-list">
                    <li><strong>Survival Outcomes:</strong> Cox proportional hazards models reporting hazard ratios (HR) with 95% confidence intervals</li>
                    <li><strong>Binary Outcomes:</strong> Logistic regression models reporting odds ratios (OR) with 95% confidence intervals</li>
                    <li><strong>Count Outcomes:</strong> Poisson regression models reporting rate ratios (RR) with 95% confidence intervals</li>
                    <li><strong>Continuous Outcomes:</strong> Linear regression models reporting coefficients with 95% confidence intervals</li>
                </ul>
            </div>
            
            <div class="methodology-section">
                <h3>Enhanced Visualizations</h3>
                <ul class="variable-list">
                    <li><strong>Kaplan-Meier Curves:</strong> Interactive survival curves generated for categorical/binary covariates in survival outcomes</li>
                    <li><strong>Variable Exclusion:</strong> Continuous variables with >5 unique values excluded from survival curve visualization</li>
                    <li><strong>Interactive Elements:</strong> Plotly-based graphics with hover information and export capabilities</li>
                    <li><strong>Selection Steps:</strong> Detailed backward elimination process showing AIC values, p-values, and variable removal sequence</li>
                </ul>
            </div>
            
            <div class="methodology-section">
                <h3>Model Recommendations</h3>
                <ul class="variable-list">
                    <li><strong>Base-Case Model:</strong> Includes variables that remained significant in final multivariate analysis (α = ', results$parameters$multivariate_alpha, ')</li>
                    <li><strong>Sensitivity Model:</strong> Base-case variables plus those significant only in univariate analysis for robustness assessment</li>
                    <li><strong>Clinical Application:</strong> Base-case for primary analysis, sensitivity for exploring potential confounders</li>
                    <li><strong>Statistical Rationale:</strong> Balances Type I error control with comprehensive covariate assessment</li>
                </ul>
            </div>
            
            <div class="methodology-section">
                <h3>Quality Control</h3>
                <ul class="variable-list">
                    <li><strong>Missing Data:</strong> Variables with >50% missing data excluded from analysis</li>
                    <li><strong>Model Convergence:</strong> Non-convergent models identified and excluded with appropriate warnings</li>
                    <li><strong>Effect Estimates:</strong> All point estimates accompanied by 95% confidence intervals</li>
                    <li><strong>Multiple Testing:</strong> Results interpreted considering exploratory nature and multiple comparisons</li>
                </ul>
            </div>
            
            <div class="methodology-section">
                <h3>Software and Implementation</h3>
                <ul class="variable-list">
                    <li><strong>R Environment:</strong> Statistical analysis conducted in R statistical computing platform</li>
                    <li><strong>Core Packages:</strong> survival (Cox regression), stats (GLM), stringr (text processing)</li>
                    <li><strong>Visualization:</strong> Plotly for interactive Kaplan-Meier curves and survival visualization</li>
                    <li><strong>Reporting:</strong> Custom HTML generation with responsive design and gradient themes</li>
                    <li><strong>Navigation:</strong> Tab-based interface with sub-tabs for organized outcome-specific results</li>
                </ul>
            </div>
        </div>
    </div>'
  )
  
  return(methodology)
}

#' Generate HTML footer
generate_tem_html_footer <- function() {
  
  # Cytel logo SVG (inline, optimized for footer)
  cytel_logo_svg <- '<svg class="footer-logo" viewBox="0 0 2400 1000" xmlns="http://www.w3.org/2000/svg">
    <path d="M2297.92 569.9V0h-142.4v557.4l.2.66c-.08 14.06.44 28.2 2.02 42.5A348 348 0 0 0 2174.7 676c31.96 92.44 143.66 128.6 225.3 74.98-62.26-55.54-99.2-85.8-102.1-181.06zM1036.96 217.24v523.08c-.42 71.4-45.02 140.32-101.94 180.74l119.54 78.9c66.88-56.4 112.52-137.36 122.6-229 1.6-14.28 2.1-28.44 2.02-42.5l.22-.66V217.26zm393.66 352.2-.96-4.28v-206.8h90.92l36.62-142.74h-127.54V0h-142.4v557.08l.2.66c-.08 14.08.44 28.2 2.02 42.5a349 349 0 0 0 17.24 76.24c32.04 91.96 143.08 128 224.36 74.58-56.28-48.42-99.4-85.88-100.46-181.6M1994 568c-25.8 47.88-90.72 90-145.84 90q-1.52 0-2.96-.06c-86.62-2.44-162.98-75.54-163.48-156.4-.64-106.52 82.14-162.56 164.44-170.88 27.98-2.74 73.86 11.86 102.46 36.4L1734.46 514.7a104 104 0 0 0 142.86 27l224.24-149.2-4.64-10.4a278 278 0 0 0-70.52-95.82 275.2 275.2 0 0 0-181.62-68.4 276 276 0 0 0-205.56 92.08c-53.02 59.32-77.4 135.96-68.58 215.8 14.3 129.72 118.94 232.3 248.8 243.92q12.8 1.14 25.4 1.14c100.28 0 191.18-53.56 239.92-140.26l-90.8-62.56zM473.46 602.46a210.4 210.4 0 0 1-128.2 39c-108.48-2.9-198.74-89.96-205.48-198.5-7.64-123.3 89.88-225.68 211.26-225.68 81.56 0 152.32 46.28 187.68 114.04l113.72-75.08C591.88 148.04 475.58 75.44 342.5 77.66 152.34 80.82-2.04 240.06.02 430.64c2.08 191.06 157.26 345.3 348.4 345.3 129.34 0 242.16-70.7 302.28-175.6l-2.38-1.62c-53.04-36.56-122.4-33.62-174.88 3.72zm526.7 28.08c-81.2 53.6-192.38 17.64-224.4-74.38a348 348 0 0 1-17.2-76.08 360 360 0 0 1-2-42.48l-.22-.66V217.28h142.4v232.16c1.28 91.7 42.92 138.7 101.42 181.12z" fill="currentColor"/>
  </svg>'
  
  footer <- paste0(
    '<div class="footer">
        <div class="footer-content">
            ', cytel_logo_svg, '
            <div class="footer-company">
                <div class="footer-main">
                    Developed by <a href="https://cytel.com" target="_blank">Cytel</a> | Comparative Effectiveness Team
                </div>
                <div class="footer-team">
                    Unanchored STC Analysis Package | TEM Comprehensive Analysis Module
                </div>
            </div>
            <div class="footer-timestamp">
                Report created on ', format(Sys.time(), "%B %d, %Y at %H:%M %Z"), '
            </div>
        </div>
    </div>
</div>
</body>
</html>'
  )
  
  return(footer)
}

# Export functions
cat("TEM HTML Reporting functions loaded successfully!\n") 
cat("TEM HTML Reporting functions loaded successfully!\n") 