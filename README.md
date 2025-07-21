# Comprehensive R Package for Simulated Treatment Comparison (STC)

A complete R package for conducting simulated treatment comparison (STC) analysis with integrated reporting capabilities. This package supports both anchored and unanchored STC methodologies with beautiful HTML report generation.

## Overview

Simulated Treatment Comparison (STC) is a statistical methodology for indirect treatment comparisons:

- **Anchored STC**: Comparisons with a shared control arm between studies
- **Unanchored STC**: Comparisons between single-arm studies without shared control

## Package Structure

### Core Methodologies

#### Treatment Effect Modifier (TEM) Exploration
**Location**: `TEM_exploration/`

Analysis to identify baseline characteristics that influence treatment effects, crucial for population adjustment in STC.

- `tem_comprehensive_analysis.R` - Core TEM analysis functions
- `tem_html_reporting.R` - HTML report generation
- `example_tem_analysis.Rmd` & `example_tem_analysis.R` - Example analysis

#### Unanchored STC (uSTC)
**Location**: `uSTC/`

##### Binary Unanchored STC
**Location**: `uSTC/binary_uSTC/`

- `binary_stc_analysis.R` - Binary outcome STC analysis
- `binary_stc_html_reporting.R` - HTML reporting for binary outcomes
- `example_binary_stc_analysis.Rmd` & `example_binary_stc_analysis.R` - Examples

##### Survival Unanchored STC
**Location**: `uSTC/survival_uSTC/`

- `survival_stc_analysis.R` - Survival outcome STC analysis with flexsurv
- `survival_stc_html_reporting.R` - HTML reporting for survival outcomes
- `example_survival_stc_analysis.Rmd` & `example_survival_stc_analysis.R` - Examples

#### Anchored STC (aSTC)
**Location**: `aSTC/`

##### Binary Anchored STC
**Location**: `aSTC/binary_aSTC/`

- `binary_astc_analysis.R` - Binary anchored STC analysis
- `binary_astc_html_reporting.R` - HTML reporting
- `example_binary_astc_analysis.R` & `example_binary_astc_analysis.Rmd` - Examples

##### Survival Anchored STC
**Location**: `aSTC/survival_aSTC/`

- `survival_astc_analysis.R` - Survival anchored STC analysis
- `survival_astc_html_reporting.R` - HTML reporting
- `example_survival_astc_analysis.R` & `example_survival_astc_analysis.Rmd` - Examples

### Advanced Methodologies (Develop Branch)

#### NORTA Unanchored STC
**Location**: `advanced_methods/NORTA_uSTC/`

Implementation of the NORTA (NORmal To Anything) algorithm for unanchored STC with correlated binary covariates, based on Ren et al. (2023).

**Key Features:**
- Uses Normal copula for generating correlated binary variables
- Preserves both marginal distributions and correlation structure
- Bootstrap uncertainty quantification
- Comprehensive validation tools

**Files:**
- `norta_ustc_analysis.R` - Core NORTA STC functions
- `example_norta_ustc_analysis.Rmd` & `example_norta_ustc_analysis.R` - Comprehensive examples

#### Multivariate Normal Simulation STC
**Location**: `advanced_methods/MVN_simulation_STC/`

Implementation of non-linear STC methodology based on Ishak et al. (2015) that addresses the fundamental bias problem in traditional STC methods.

**Key Innovation:**
- Addresses the mathematical issue that f(E[X]) ≠ E[f(X)] for non-linear functions
- Samples individual patient profiles from multivariate normal distribution
- Provides bias-free estimates for non-linear outcomes

**Applications:**
- Survival analysis (exponential, Weibull, Gamma distributions)
- Count data (Poisson regression)
- Binary outcomes with extreme probabilities
- Any model with exponential/log transformations

**Files:**
- `mvn_simulation_stc_analysis.R` - Core MVN simulation functions
- `example_mvn_simulation_stc_analysis.Rmd` & `example_mvn_simulation_stc_analysis.R` - Comprehensive examples

## Key Features

### Core Analysis Capabilities
- **Distribution Selection**: Automatic selection using AIC with flexsurv
- **Pseudo-IPD Reconstruction**: Multiple methods including Guyot algorithm
- **Covariate Centering**: Automated data preparation
- **Bootstrap Analysis**: Robust uncertainty quantification
- **Quantitative Bias Analysis (QBA)**: Sensitivity analysis for unmeasured confounding

### Advanced Features (Develop Branch)
- **Non-Linear Bias Correction**: MVN simulation addresses systematic bias in traditional methods
- **Correlated Covariate Handling**: NORTA algorithm preserves realistic correlation structures
- **Regulatory-Ready**: Methods suitable for health technology assessment submissions
- **Comprehensive Validation**: Built-in quality metrics and bias assessment

### Reporting Features
- **HTML Reports**: Beautiful, interactive reports with plots and tables
- **Multiple Formats**: Both R Markdown and pure R script examples
- **Reproducible Research**: All analyses are fully documented and reproducible

## Installation and Usage

### Basic Installation
```r
# Clone the repository
git clone https://github.com/your-repo/rstc.git

# Load required packages
source("package_requirements.R")  # Will be created
```

### Advanced Methodologies
```r
# Switch to develop branch for advanced methods
git checkout develop

# Source advanced methodology functions
source("advanced_methods/NORTA_uSTC/norta_ustc_analysis.R")
source("advanced_methods/MVN_simulation_STC/mvn_simulation_stc_analysis.R")
```

## Quick Start Examples

### Basic TEM Analysis
```r
source("TEM_exploration/tem_comprehensive_analysis.R")
results <- run_tem_analysis(data, covariates, outcomes)
```

### Basic Unanchored STC
```r
source("uSTC/binary_uSTC/binary_stc_analysis.R")
results <- binary_unanchored_stc_analysis(ipd_data, agd_summary)
```

### Advanced NORTA Analysis (Develop Branch)
```r
source("advanced_methods/NORTA_uSTC/norta_ustc_analysis.R")
results <- norta_unanchored_stc_analysis(ipd_data, agd_summary, covariates)
```

### Advanced MVN Simulation (Develop Branch)
```r
source("advanced_methods/MVN_simulation_STC/mvn_simulation_stc_analysis.R")
results <- run_complete_mvn_stc_analysis("atrial_fib")
```

## Dependencies

### Core Dependencies
- `tidyverse` - Data manipulation and visualization
- `survival` - Survival analysis
- `flexsurv` - Parametric survival modeling
- `boot` - Bootstrap methods
- `knitr` - Report generation
- `kableExtra` - Enhanced tables

### Advanced Method Dependencies (Develop Branch)
- `copula` - NORTA algorithm implementation
- `MASS` - Multivariate normal simulation
- `mvtnorm` - Multivariate normal distributions

## Methodology References

### Core Methods
- NICE DSU TSD 18: Methods for population-adjusted indirect comparisons
- Phillippo et al. (2018): Methods for Network Meta-Analysis
- Guyot et al. (2012): Enhanced secondary analysis of survival data

### Advanced Methods (Develop Branch)
- **NORTA Method**: Ren et al. (2023): "Comprehensive simulation study of unanchored simulated treatment comparison approach"
- **MVN Simulation**: Ishak et al. (2015): "Simulated Treatment Comparison of Time-To-Event (And Other Non-Linear) Outcomes", Value in Health Journal

## Development Roadmap

### Current Status
- ✅ Core STC methodologies (anchored and unanchored)
- ✅ TEM exploration framework
- ✅ HTML report generation
- ✅ Advanced NORTA implementation (develop branch)
- ✅ Advanced MVN simulation (develop branch)

### Future Enhancements
- 🔄 Integration of advanced methods into main branch
- 📋 Additional outcome types (ordinal, count)
- 📊 Interactive dashboards
- 📚 Comprehensive vignettes
- 🧪 Extensive unit testing

## Contributing

This package follows a structured development approach:

- **Main Branch**: Stable, production-ready methodologies
- **Develop Branch**: Advanced methodologies and experimental features
- **Feature Branches**: Individual feature development

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use this package in your research, please cite:

```
R Package for Comprehensive Simulated Treatment Comparison (STC) Analysis
Version 1.0
Available at: https://github.com/your-repo/rstc
```

## Contact

For questions, bug reports, or feature requests, please open an issue on GitHub or contact the development team.

---

**Note**: The advanced methodologies (NORTA and MVN simulation) are currently available in the `develop` branch and represent cutting-edge approaches for addressing limitations in traditional STC methods. These methods are particularly valuable for regulatory submissions and health technology assessments requiring the highest methodological standards.