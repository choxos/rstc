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

#### Machine Learning STC Methods
**Location**: `advanced_methods/ML_STC/`

Comprehensive implementation of state-of-the-art machine learning methods for causal inference and heterogeneous treatment effect estimation in STC analysis.

**Tree-Based Methods** (`tree_based/`):
- **Causal Forests**: Honest estimation with variable importance and subgroup discovery
- **BART**: Bayesian Additive Regression Trees with natural uncertainty quantification  
- **XGBoost/LightGBM**: High-performance gradient boosting with hyperparameter tuning

**Meta-Learning Approaches** (`meta_learning/`):
- **T-Learner**: Separate models for treatment and control groups
- **X-Learner**: Handles imbalanced treatment groups via counterfactual imputation
- **R-Learner**: Direct optimization for causal objectives with theoretical optimality
- **Double Machine Learning**: Cross-fitting for valid inference under weak assumptions

**Specialized Causal Methods** (`specialized_causal/`):
- **TMLE with Super Learner**: Doubly robust estimation with targeted bias reduction
- **Causal Neural Networks**: TarNet and CFR for complex non-linear relationships with balanced representations

**Advanced Ensemble Methods** (`ensemble_methods/`):
- **AIPW with ML**: Doubly robust estimation using flexible ML for propensity scores and outcomes
- **GANs for Causal Inference**: GANITE and CausalGAN for counterfactual generation

**Key ML-STC Features:**
- Heterogeneous treatment effect estimation
- Target population predictions for STC
- Robust inference with confidence intervals
- Model diagnostics and sensitivity analysis
- Support for binary and continuous outcomes
- Integration with Python for neural networks/GANs
- Comprehensive utility functions and examples

**Files:**
- `utils/ml_stc_utils.R` - Core utility functions for all ML methods
- `examples/comprehensive_ml_stc_example.Rmd` & `.R` - Complete demonstration with realistic clinical data
- Individual implementation files for each method category

## Methodological Innovation

This package represents the **first comprehensive implementation** of machine learning methods specifically designed for STC analysis. Traditional STC methods assume homogeneous treatment effects and linear relationships, which may not hold in real-world clinical settings. Our ML-based approaches address these limitations by:

### Heterogeneous Treatment Effects
- **Individual-level predictions**: Estimate personalized treatment effects rather than population averages
- **Subgroup identification**: Discover patient subgroups with differential treatment responses
- **Precision medicine**: Support personalized treatment decisions based on patient characteristics

### Robust Causal Inference
- **Doubly robust methods**: Protection against model misspecification in either propensity score or outcome models
- **Cross-fitting**: Reduce overfitting bias through sample splitting techniques
- **Uncertainty quantification**: Provide valid confidence intervals under weaker assumptions

### Advanced Modeling Capabilities
- **Non-linear relationships**: Capture complex interactions between covariates and outcomes
- **High-dimensional data**: Handle scenarios with many covariates relative to sample size
- **Deep learning integration**: Leverage neural networks for complex confounding patterns

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
- **Machine Learning Integration**: Comprehensive ML methods for causal inference and heterogeneous treatment effects
- **Doubly Robust Estimation**: AIPW, DML, and TMLE provide protection against model misspecification
- **Neural Network Capabilities**: Deep learning methods for complex confounding patterns
- **Heterogeneity Analysis**: Individual-level treatment effect estimation and subgroup discovery
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

# Source ML-based STC methods
source("advanced_methods/ML_STC/utils/ml_stc_utils.R")
source("advanced_methods/ML_STC/tree_based/causal_forests_stc.R")
source("advanced_methods/ML_STC/meta_learning/double_ml_stc.R")
source("advanced_methods/ML_STC/ensemble_methods/aipw_stc.R")
# ... (see examples for complete sourcing)
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

### Machine Learning STC Methods (Develop Branch)
```r
# Causal Forests for heterogeneous treatment effects
source("advanced_methods/ML_STC/tree_based/causal_forests_stc.R")
cf_results <- causal_forest_stc_analysis(data, "outcome", "treatment", covariates)

# Double Machine Learning with cross-fitting
source("advanced_methods/ML_STC/meta_learning/double_ml_stc.R")
dml_results <- double_ml_stc_analysis(data, "outcome", "treatment", covariates)

# AIPW with machine learning (doubly robust)
source("advanced_methods/ML_STC/ensemble_methods/aipw_stc.R")
aipw_results <- aipw_stc_analysis(data, "outcome", "treatment", covariates, 
                                  ps_method = "rf", outcome_method = "rf")

# Comprehensive ML comparison
source("advanced_methods/ML_STC/examples/comprehensive_ml_stc_example.R")
# Run complete analysis with all methods
```

## Method Selection Guide

### When to Use Each ML Method

| **Use Case** | **Recommended Methods** | **Key Benefits** |
|--------------|------------------------|------------------|
| **Robust estimation** | AIPW, Double ML, TMLE | Doubly robust properties |
| **Heterogeneity discovery** | Causal Forests, Neural Networks | Subgroup identification |
| **High interpretability** | Tree-based methods, T-Learner | Clear variable importance |
| **Complex confounding** | Neural Networks, GANs | Non-linear relationships |
| **Uncertainty quantification** | BART, TMLE | Bayesian/targeted inference |
| **Imbalanced treatment groups** | X-Learner, AIPW | Handles group imbalance |
| **High-dimensional data** | Regularized methods, Super Learner | Manages many covariates |
| **Small sample sizes** | BART, regularized methods | Conservative estimates |

### Decision Tree for Method Selection

```
┌─ Robustness Priority? ─── Yes ──→ AIPW, Double ML, TMLE
│
├─ Heterogeneity Focus? ─── Yes ──→ Causal Forests, Meta-learners
│
├─ Interpretability Need? ── Yes ──→ Tree-based methods, Linear meta-learners
│
├─ Complex Relationships? ── Yes ──→ Neural Networks, GANs, BART
│
└─ Standard Analysis ──────────────→ Start with AIPW or Double ML
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

#### Machine Learning Dependencies
- `randomForest` - Random forest implementations
- `xgboost` - Gradient boosting methods
- `glmnet` - Elastic net regularization
- `grf` - Generalized random forests (Causal Forests)
- `BART` / `bartCause` - Bayesian additive regression trees
- `SuperLearner` - Ensemble learning framework
- `tmle` - Targeted maximum likelihood estimation
- `reticulate` - Python integration (for neural networks/GANs)

#### Python Dependencies (Optional, via reticulate)
- `tensorflow` - Deep learning framework
- `numpy` - Numerical computing
- `scikit-learn` - Machine learning library
- `pandas` - Data manipulation

## Methodology References

### Core Methods
- NICE DSU TSD 18: Methods for population-adjusted indirect comparisons
- Phillippo et al. (2018): Methods for Network Meta-Analysis
- Guyot et al. (2012): Enhanced secondary analysis of survival data

### Advanced Methods (Develop Branch)
- **NORTA Method**: Ren et al. (2023): "Comprehensive simulation study of unanchored simulated treatment comparison approach"
- **MVN Simulation**: Ishak et al. (2015): "Simulated Treatment Comparison of Time-To-Event (And Other Non-Linear) Outcomes", Value in Health Journal

#### Machine Learning Methods
- **Causal Forests**: Wager & Athey (2018): "Estimation and Inference of Heterogeneous Treatment Effects using Random Forests", Journal of the American Statistical Association
- **BART**: Chipman et al. (2010): "BART: Bayesian additive regression trees", Annals of Applied Statistics
- **Double ML**: Chernozhukov et al. (2018): "Double/debiased machine learning for treatment and structural parameters", Econometrics Journal
- **TMLE**: van der Laan & Rose (2011): "Targeted Learning: Causal Inference for Observational and Experimental Data"
- **AIPW**: Robins et al. (1994): "Estimation of regression coefficients when some regressors are not always observed"
- **TarNet**: Shalit et al. (2017): "Estimating individual treatment effect: generalization bounds and algorithms", ICML
- **GANITE**: Yoon et al. (2018): "GANITE: Estimation of individualized treatment effects using generative adversarial nets", ICLR

## Development Roadmap

### Current Status
- ✅ Core STC methodologies (anchored and unanchored)
- ✅ TEM exploration framework
- ✅ HTML report generation
- ✅ Advanced NORTA implementation (develop branch)
- ✅ Advanced MVN simulation (develop branch)
- ✅ **Machine Learning STC Methods (develop branch)**
  - ✅ Tree-based methods (Causal Forests, BART, XGBoost)
  - ✅ Meta-learning approaches (T/X/R-Learner, Double ML)
  - ✅ Specialized causal methods (TMLE, Neural Networks)
  - ✅ Advanced ensemble methods (AIPW, GANs)
  - ✅ Comprehensive utilities and examples

### Future Enhancements
- 🔄 Integration of advanced methods into main branch
- 📋 Additional outcome types (ordinal, count)
- 📊 Interactive dashboards and Shiny applications
- 📚 Comprehensive vignettes and tutorials
- 🧪 Extensive unit testing and validation studies
- 🎯 Benchmark studies comparing ML methods
- 📦 CRAN-ready package structure

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

**Note**: The advanced methodologies (NORTA, MVN simulation, and comprehensive ML methods) are currently available in the `develop` branch and represent cutting-edge approaches for addressing limitations in traditional STC methods. The machine learning implementations provide the first comprehensive framework for heterogeneous treatment effect estimation in STC analysis. These methods are particularly valuable for regulatory submissions and health technology assessments requiring the highest methodological standards.