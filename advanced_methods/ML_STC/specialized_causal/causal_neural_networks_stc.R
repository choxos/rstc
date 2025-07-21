################################################################################
##################### Causal Neural Networks for STC Analysis ##################
################################################################################
#
# This module implements causal neural networks for STC analysis, including:
# 1. TarNet (Treatment-Agnostic Representation Network)
# 2. Counterfactual Regression (CFR) Networks
# 3. Balanced Neural Network approaches
#
# Author: Advanced STC Methods Package
# Date: 2024
#
# Dependencies: reticulate, torch (optional), keras (via reticulate)
# Python dependencies: tensorflow, numpy, scikit-learn

# Load required libraries
if (!requireNamespace("reticulate", quietly = TRUE)) {
  warning("reticulate package not available. Python-based neural networks will not work.")
}

# Global variables for Python environment
python_env_setup <- FALSE

#' Setup Python Environment for Causal Neural Networks
#' 
#' Sets up the Python environment with required packages for causal neural networks
#' 
#' @param conda_env Character string specifying conda environment name (default: "causal_ml")
#' @param install_packages Logical, whether to install required Python packages
#' @param python_path Optional path to Python executable
#' @return List with setup status and environment information
setup_python_env <- function(conda_env = "causal_ml", install_packages = FALSE, 
                            python_path = NULL) {
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(list(status = "failed", message = "reticulate package not available"))
  }
  
  tryCatch({
    if (!is.null(python_path)) {
      reticulate::use_python(python_path, required = TRUE)
    } else {
      # Try to use conda environment
      if (conda_env %in% reticulate::conda_list()$name) {
        reticulate::use_condaenv(conda_env, required = TRUE)
      } else {
        # Use default Python
        reticulate::use_python(reticulate::py_discover_config()$python, required = TRUE)
      }
    }
    
    # Install packages if requested
    if (install_packages) {
      reticulate::py_install(c("tensorflow", "numpy", "scikit-learn", "pandas"))
    }
    
    # Test imports
    reticulate::py_run_string("
import tensorflow as tf
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import pandas as pd
")
    
    python_env_setup <<- TRUE
    return(list(status = "success", message = "Python environment setup complete",
                python_version = reticulate::py_config()$version))
    
  }, error = function(e) {
    return(list(status = "failed", message = paste("Setup failed:", e$message)))
  })
}

#' TarNet Implementation for STC Analysis
#' 
#' Implements Treatment-Agnostic Representation Network for heterogeneous treatment effects
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional target population data for prediction
#' @param representation_dim Dimension of shared representation layer
#' @param hidden_layers Vector specifying hidden layer sizes
#' @param learning_rate Learning rate for optimization
#' @param batch_size Batch size for training
#' @param epochs Number of training epochs
#' @param validation_split Proportion of data for validation
#' @param alpha_reg Regularization strength for representation balance
#' @param seed Random seed for reproducibility
#' @return List containing trained model, predictions, and diagnostics
tarnet_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                               target_population = NULL, representation_dim = 50,
                               hidden_layers = c(100, 50), learning_rate = 0.001,
                               batch_size = 32, epochs = 100, validation_split = 0.2,
                               alpha_reg = 0.1, seed = 123) {
  
  set.seed(seed)
  
  # Check Python environment
  if (!python_env_setup) {
    env_status <- setup_python_env()
    if (env_status$status == "failed") {
      stop("Python environment setup failed: ", env_status$message)
    }
  }
  
  # Prepare data
  cat("Preparing data for TarNet analysis...\n")
  X <- as.matrix(data[, covariate_cols, drop = FALSE])
  y <- data[[outcome_col]]
  t <- data[[treatment_col]]
  
  # Standardize features
  scaler_X <- list(center = apply(X, 2, mean), scale = apply(X, 2, sd))
  X_scaled <- scale(X, center = scaler_X$center, scale = scaler_X$scale)
  
  # Create TarNet model in Python
  reticulate::py_run_string(sprintf("
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np

# Set seeds
tf.random.set_seed(%d)
np.random.seed(%d)

class TarNet(keras.Model):
    def __init__(self, input_dim, representation_dim, hidden_layers, alpha_reg=0.1):
        super(TarNet, self).__init__()
        self.alpha_reg = alpha_reg
        
        # Shared representation network
        self.representation_layers = []
        self.representation_layers.append(layers.Dense(representation_dim, activation='relu', 
                                                     input_shape=(input_dim,)))
        
        # Treatment-specific outcome networks
        self.control_layers = []
        self.treated_layers = []
        
        for units in hidden_layers:
            self.control_layers.append(layers.Dense(units, activation='relu'))
            self.treated_layers.append(layers.Dense(units, activation='relu'))
        
        # Output layers
        self.control_output = layers.Dense(1, activation='linear')
        self.treated_output = layers.Dense(1, activation='linear')
        
    def call(self, inputs, training=None):
        x, t = inputs
        
        # Shared representation
        phi = self.representation_layers[0](x)
        
        # Treatment-specific networks
        y0_hat = phi
        y1_hat = phi
        
        for layer in self.control_layers:
            y0_hat = layer(y0_hat)
        for layer in self.treated_layers:
            y1_hat = layer(y1_hat)
            
        y0_hat = self.control_output(y0_hat)
        y1_hat = self.treated_output(y1_hat)
        
        return y0_hat, y1_hat, phi

# Custom loss function
def tarnet_loss(y_true, y_pred, t, phi, alpha_reg):
    y0_pred, y1_pred = y_pred
    
    # Factual loss
    y_pred_factual = t * y1_pred + (1 - t) * y0_pred
    factual_loss = tf.reduce_mean(tf.square(y_true - tf.squeeze(y_pred_factual)))
    
    # Representation balance loss (simplified IPM)
    phi_treated = tf.boolean_mask(phi, tf.equal(t, 1))
    phi_control = tf.boolean_mask(phi, tf.equal(t, 0))
    
    if tf.shape(phi_treated)[0] > 0 and tf.shape(phi_control)[0] > 0:
        balance_loss = tf.reduce_mean(tf.square(
            tf.reduce_mean(phi_treated, axis=0) - tf.reduce_mean(phi_control, axis=0)
        ))
    else:
        balance_loss = 0.0
    
    total_loss = factual_loss + alpha_reg * balance_loss
    return total_loss, factual_loss, balance_loss
", seed, seed))
  
  # Convert data to Python
  reticulate::py$X_train <- X_scaled
  reticulate::py$y_train <- y
  reticulate::py$t_train <- as.numeric(t)
  
  # Create and train model
  cat("Training TarNet model...\n")
  reticulate::py_run_string(sprintf("
# Model parameters
input_dim = X_train.shape[1]
representation_dim = %d
hidden_layers = %s
learning_rate = %f
batch_size = %d
epochs = %d
validation_split = %f
alpha_reg = %f

# Create model
model = TarNet(input_dim, representation_dim, hidden_layers, alpha_reg)

# Optimizer
optimizer = keras.optimizers.Adam(learning_rate=learning_rate)

# Training loop
train_losses = []
val_losses = []

# Split data
n_val = int(len(X_train) * validation_split)
indices = np.random.permutation(len(X_train))
val_indices = indices[:n_val]
train_indices = indices[n_val:]

X_train_split = X_train[train_indices]
y_train_split = y_train[train_indices]
t_train_split = t_train[train_indices]

X_val = X_train[val_indices]
y_val = y_train[val_indices]
t_val = t_train[val_indices]

# Convert to tensors
X_train_tensor = tf.constant(X_train_split, dtype=tf.float32)
y_train_tensor = tf.constant(y_train_split, dtype=tf.float32)
t_train_tensor = tf.constant(t_train_split, dtype=tf.float32)

X_val_tensor = tf.constant(X_val, dtype=tf.float32)
y_val_tensor = tf.constant(y_val, dtype=tf.float32)
t_val_tensor = tf.constant(t_val, dtype=tf.float32)

for epoch in range(epochs):
    # Training
    with tf.GradientTape() as tape:
        y0_pred, y1_pred, phi = model([X_train_tensor, t_train_tensor], training=True)
        loss, factual_loss, balance_loss = tarnet_loss(
            y_train_tensor, (y0_pred, y1_pred), t_train_tensor, phi, alpha_reg
        )
    
    gradients = tape.gradient(loss, model.trainable_variables)
    optimizer.apply_gradients(zip(gradients, model.trainable_variables))
    
    # Validation
    y0_val_pred, y1_val_pred, phi_val = model([X_val_tensor, t_val_tensor], training=False)
    val_loss, val_factual_loss, val_balance_loss = tarnet_loss(
        y_val_tensor, (y0_val_pred, y1_val_pred), t_val_tensor, phi_val, alpha_reg
    )
    
    train_losses.append(float(loss))
    val_losses.append(float(val_loss))
    
    if epoch %% 10 == 0:
        print(f'Epoch {epoch}: Train Loss = {loss:.4f}, Val Loss = {val_loss:.4f}')

# Final predictions
y0_final, y1_final, phi_final = model([tf.constant(X_train, dtype=tf.float32), 
                                     tf.constant(t_train, dtype=tf.float32)], training=False)

# Convert back to numpy
y0_pred_np = y0_final.numpy().flatten()
y1_pred_np = y1_final.numpy().flatten()
phi_np = phi_final.numpy()
", representation_dim, paste0("[", paste(hidden_layers, collapse = ", "), "]"),
learning_rate, batch_size, epochs, validation_split, alpha_reg))
  
  # Extract results
  y0_pred <- reticulate::py$y0_pred_np
  y1_pred <- reticulate::py$y1_pred_np
  phi <- reticulate::py$phi_np
  train_losses <- reticulate::py$train_losses
  val_losses <- reticulate::py$val_losses
  
  # Calculate treatment effects
  tau_pred <- y1_pred - y0_pred
  
  # Predictions for target population if provided
  target_predictions <- NULL
  if (!is.null(target_population)) {
    cat("Making predictions for target population...\n")
    X_target <- as.matrix(target_population[, covariate_cols, drop = FALSE])
    X_target_scaled <- scale(X_target, center = scaler_X$center, scale = scaler_X$scale)
    
    reticulate::py$X_target <- X_target_scaled
    reticulate::py_run_string("
t_target = np.zeros(len(X_target))  # Dummy treatment for prediction
y0_target, y1_target, phi_target = model([tf.constant(X_target, dtype=tf.float32), 
                                        tf.constant(t_target, dtype=tf.float32)], training=False)
y0_target_np = y0_target.numpy().flatten()
y1_target_np = y1_target.numpy().flatten()
tau_target_np = y1_target_np - y0_target_np
")
    
    target_predictions <- list(
      y0_pred = reticulate::py$y0_target_np,
      y1_pred = reticulate::py$y1_target_np,
      tau_pred = reticulate::py$tau_target_np,
      ate_estimate = mean(reticulate::py$tau_target_np)
    )
  }
  
  # Calculate ATE
  ate_estimate <- mean(tau_pred)
  
  # Model diagnostics
  diagnostics <- list(
    representation_balance = calculate_representation_balance(phi, t),
    training_history = data.frame(
      epoch = 1:length(train_losses),
      train_loss = train_losses,
      val_loss = val_losses
    ),
    prediction_quality = list(
      factual_rmse = sqrt(mean((y - (t * y1_pred + (1-t) * y0_pred))^2)),
      treatment_overlap = sum(t == 1) / length(t)
    )
  )
  
  cat("TarNet analysis completed.\n")
  cat(sprintf("Estimated ATE: %.4f\n", ate_estimate))
  cat(sprintf("Representation balance score: %.4f\n", diagnostics$representation_balance$imbalance_score))
  
  return(list(
    method = "TarNet",
    ate_estimate = ate_estimate,
    individual_effects = tau_pred,
    y0_pred = y0_pred,
    y1_pred = y1_pred,
    representations = phi,
    target_predictions = target_predictions,
    diagnostics = diagnostics,
    model_params = list(
      representation_dim = representation_dim,
      hidden_layers = hidden_layers,
      learning_rate = learning_rate,
      alpha_reg = alpha_reg
    )
  ))
}

#' Counterfactual Regression (CFR) Implementation for STC Analysis
#' 
#' Implements Counterfactual Regression with domain adaptation for causal inference
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable  
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional target population data for prediction
#' @param representation_dim Dimension of shared representation layer
#' @param hidden_layers Vector specifying hidden layer sizes
#' @param learning_rate Learning rate for optimization
#' @param batch_size Batch size for training
#' @param epochs Number of training epochs
#' @param alpha_reg Regularization strength for domain adaptation
#' @param distance_metric Distance metric for representation learning ('mmd', 'wasserstein')
#' @param seed Random seed for reproducibility
#' @return List containing trained model, predictions, and diagnostics
cfr_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                            target_population = NULL, representation_dim = 50,
                            hidden_layers = c(100, 50), learning_rate = 0.001,
                            batch_size = 32, epochs = 100, alpha_reg = 1.0,
                            distance_metric = "mmd", seed = 123) {
  
  set.seed(seed)
  
  # Check Python environment
  if (!python_env_setup) {
    env_status <- setup_python_env()
    if (env_status$status == "failed") {
      stop("Python environment setup failed: ", env_status$message)
    }
  }
  
  # Prepare data
  cat("Preparing data for CFR analysis...\n")
  X <- as.matrix(data[, covariate_cols, drop = FALSE])
  y <- data[[outcome_col]]
  t <- data[[treatment_col]]
  
  # Standardize features
  scaler_X <- list(center = apply(X, 2, mean), scale = apply(X, 2, sd))
  X_scaled <- scale(X, center = scaler_X$center, scale = scaler_X$scale)
  
  # Create CFR model in Python
  reticulate::py_run_string(sprintf("
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np

# Set seeds
tf.random.set_seed(%d)
np.random.seed(%d)

class CFR(keras.Model):
    def __init__(self, input_dim, representation_dim, hidden_layers, distance_metric='mmd'):
        super(CFR, self).__init__()
        self.distance_metric = distance_metric
        
        # Shared representation network
        self.representation_net = keras.Sequential([
            layers.Dense(representation_dim, activation='relu', input_shape=(input_dim,)),
            layers.Dense(representation_dim, activation='relu')
        ])
        
        # Hypothesis networks for each treatment
        self.h0_net = keras.Sequential()
        self.h1_net = keras.Sequential()
        
        for units in hidden_layers:
            self.h0_net.add(layers.Dense(units, activation='relu'))
            self.h1_net.add(layers.Dense(units, activation='relu'))
        
        self.h0_net.add(layers.Dense(1, activation='linear'))
        self.h1_net.add(layers.Dense(1, activation='linear'))
        
    def call(self, inputs, training=None):
        x, t = inputs
        
        # Get representation
        phi = self.representation_net(x, training=training)
        
        # Get potential outcomes
        y0_pred = self.h0_net(phi, training=training)
        y1_pred = self.h1_net(phi, training=training)
        
        return y0_pred, y1_pred, phi

def mmd_loss(phi_treated, phi_control, kernel='rbf', sigma=1.0):
    '''Maximum Mean Discrepancy loss'''
    if tf.shape(phi_treated)[0] == 0 or tf.shape(phi_control)[0] == 0:
        return tf.constant(0.0)
    
    # RBF kernel
    def rbf_kernel(x, y, sigma):
        x_norm = tf.reduce_sum(tf.square(x), axis=1, keepdims=True)
        y_norm = tf.reduce_sum(tf.square(y), axis=1, keepdims=True)
        dist = x_norm + tf.transpose(y_norm) - 2.0 * tf.matmul(x, tf.transpose(y))
        return tf.exp(-dist / (2.0 * sigma**2))
    
    K_tt = rbf_kernel(phi_treated, phi_treated, sigma)
    K_cc = rbf_kernel(phi_control, phi_control, sigma)
    K_tc = rbf_kernel(phi_treated, phi_control, sigma)
    
    mmd = tf.reduce_mean(K_tt) + tf.reduce_mean(K_cc) - 2.0 * tf.reduce_mean(K_tc)
    return mmd

def cfr_loss(y_true, y_pred, t, phi, alpha_reg, distance_metric='mmd'):
    y0_pred, y1_pred = y_pred
    
    # Factual loss
    y_pred_factual = t * y1_pred + (1 - t) * y0_pred
    factual_loss = tf.reduce_mean(tf.square(y_true - tf.squeeze(y_pred_factual)))
    
    # Domain adaptation loss
    phi_treated = tf.boolean_mask(phi, tf.equal(t, 1))
    phi_control = tf.boolean_mask(phi, tf.equal(t, 0))
    
    if distance_metric == 'mmd':
        domain_loss = mmd_loss(phi_treated, phi_control)
    else:  # Simplified Wasserstein (just mean difference)
        if tf.shape(phi_treated)[0] > 0 and tf.shape(phi_control)[0] > 0:
            domain_loss = tf.reduce_mean(tf.square(
                tf.reduce_mean(phi_treated, axis=0) - tf.reduce_mean(phi_control, axis=0)
            ))
        else:
            domain_loss = 0.0
    
    total_loss = factual_loss + alpha_reg * domain_loss
    return total_loss, factual_loss, domain_loss
", seed, seed))
  
  # Convert data to Python
  reticulate::py$X_train <- X_scaled
  reticulate::py$y_train <- y
  reticulate::py$t_train <- as.numeric(t)
  
  # Create and train model
  cat("Training CFR model...\n")
  reticulate::py_run_string(sprintf("
# Model parameters
input_dim = X_train.shape[1]
representation_dim = %d
hidden_layers = %s
learning_rate = %f
batch_size = %d
epochs = %d
alpha_reg = %f
distance_metric = '%s'

# Create model
model = CFR(input_dim, representation_dim, hidden_layers, distance_metric)

# Optimizer
optimizer = keras.optimizers.Adam(learning_rate=learning_rate)

# Training loop
train_losses = []
factual_losses = []
domain_losses = []

# Convert to tensors
X_tensor = tf.constant(X_train, dtype=tf.float32)
y_tensor = tf.constant(y_train, dtype=tf.float32)
t_tensor = tf.constant(t_train, dtype=tf.float32)

for epoch in range(epochs):
    with tf.GradientTape() as tape:
        y0_pred, y1_pred, phi = model([X_tensor, t_tensor], training=True)
        loss, factual_loss, domain_loss = cfr_loss(
            y_tensor, (y0_pred, y1_pred), t_tensor, phi, alpha_reg, distance_metric
        )
    
    gradients = tape.gradient(loss, model.trainable_variables)
    optimizer.apply_gradients(zip(gradients, model.trainable_variables))
    
    train_losses.append(float(loss))
    factual_losses.append(float(factual_loss))
    domain_losses.append(float(domain_loss))
    
    if epoch %% 10 == 0:
        print(f'Epoch {epoch}: Total Loss = {loss:.4f}, Factual = {factual_loss:.4f}, Domain = {domain_loss:.4f}')

# Final predictions
y0_final, y1_final, phi_final = model([X_tensor, t_tensor], training=False)

# Convert back to numpy
y0_pred_np = y0_final.numpy().flatten()
y1_pred_np = y1_final.numpy().flatten()
phi_np = phi_final.numpy()
", representation_dim, paste0("[", paste(hidden_layers, collapse = ", "), "]"),
learning_rate, batch_size, epochs, alpha_reg, distance_metric))
  
  # Extract results
  y0_pred <- reticulate::py$y0_pred_np
  y1_pred <- reticulate::py$y1_pred_np
  phi <- reticulate::py$phi_np
  train_losses <- reticulate::py$train_losses
  factual_losses <- reticulate::py$factual_losses
  domain_losses <- reticulate::py$domain_losses
  
  # Calculate treatment effects
  tau_pred <- y1_pred - y0_pred
  
  # Predictions for target population if provided
  target_predictions <- NULL
  if (!is.null(target_population)) {
    cat("Making predictions for target population...\n")
    X_target <- as.matrix(target_population[, covariate_cols, drop = FALSE])
    X_target_scaled <- scale(X_target, center = scaler_X$center, scale = scaler_X$scale)
    
    reticulate::py$X_target <- X_target_scaled
    reticulate::py_run_string("
t_target = np.zeros(len(X_target))  # Dummy treatment for prediction
y0_target, y1_target, phi_target = model([tf.constant(X_target, dtype=tf.float32), 
                                        tf.constant(t_target, dtype=tf.float32)], training=False)
y0_target_np = y0_target.numpy().flatten()
y1_target_np = y1_target.numpy().flatten()
tau_target_np = y1_target_np - y0_target_np
")
    
    target_predictions <- list(
      y0_pred = reticulate::py$y0_target_np,
      y1_pred = reticulate::py$y1_target_np,
      tau_pred = reticulate::py$tau_target_np,
      ate_estimate = mean(reticulate::py$tau_target_np)
    )
  }
  
  # Calculate ATE
  ate_estimate <- mean(tau_pred)
  
  # Model diagnostics
  diagnostics <- list(
    representation_balance = calculate_representation_balance(phi, t),
    training_history = data.frame(
      epoch = 1:length(train_losses),
      total_loss = train_losses,
      factual_loss = factual_losses,
      domain_loss = domain_losses
    ),
    prediction_quality = list(
      factual_rmse = sqrt(mean((y - (t * y1_pred + (1-t) * y0_pred))^2)),
      treatment_overlap = sum(t == 1) / length(t)
    )
  )
  
  cat("CFR analysis completed.\n")
  cat(sprintf("Estimated ATE: %.4f\n", ate_estimate))
  cat(sprintf("Final domain loss: %.4f\n", tail(domain_losses, 1)))
  
  return(list(
    method = "CFR",
    ate_estimate = ate_estimate,
    individual_effects = tau_pred,
    y0_pred = y0_pred,
    y1_pred = y1_pred,
    representations = phi,
    target_predictions = target_predictions,
    diagnostics = diagnostics,
    model_params = list(
      representation_dim = representation_dim,
      hidden_layers = hidden_layers,
      learning_rate = learning_rate,
      alpha_reg = alpha_reg,
      distance_metric = distance_metric
    )
  ))
}

#' Calculate Representation Balance
#' 
#' Evaluates the balance of learned representations between treatment groups
#' 
#' @param phi Matrix of learned representations
#' @param t Treatment assignment vector
#' @return List with balance metrics
calculate_representation_balance <- function(phi, t) {
  # Split representations by treatment
  phi_treated <- phi[t == 1, , drop = FALSE]
  phi_control <- phi[t == 0, , drop = FALSE]
  
  if (nrow(phi_treated) == 0 || nrow(phi_control) == 0) {
    return(list(
      imbalance_score = Inf,
      mean_difference = NA,
      standardized_difference = NA
    ))
  }
  
  # Calculate mean differences
  mean_treated <- colMeans(phi_treated)
  mean_control <- colMeans(phi_control)
  mean_diff <- mean_treated - mean_control
  
  # Standardized mean differences
  pooled_sd <- sqrt((apply(phi_treated, 2, var) + apply(phi_control, 2, var)) / 2)
  std_diff <- mean_diff / (pooled_sd + 1e-8)
  
  # Overall imbalance score (mean absolute standardized difference)
  imbalance_score <- mean(abs(std_diff))
  
  return(list(
    imbalance_score = imbalance_score,
    mean_difference = mean_diff,
    standardized_difference = std_diff
  ))
}

#' Neural Network STC Analysis Wrapper
#' 
#' Unified interface for neural network-based STC methods
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param method Neural network method ("tarnet" or "cfr")
#' @param target_population Optional target population data for prediction
#' @param ... Additional parameters passed to specific methods
#' @return Results from the specified neural network method
neural_network_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                                       method = "tarnet", target_population = NULL, ...) {
  
  # Validate inputs
  if (!method %in% c("tarnet", "cfr")) {
    stop("Method must be 'tarnet' or 'cfr'")
  }
  
  # Check required columns
  missing_cols <- setdiff(c(outcome_col, treatment_col, covariate_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Call appropriate method
  if (method == "tarnet") {
    return(tarnet_stc_analysis(data, outcome_col, treatment_col, covariate_cols,
                              target_population, ...))
  } else {
    return(cfr_stc_analysis(data, outcome_col, treatment_col, covariate_cols,
                           target_population, ...))
  }
}

# Message when loading
cat("Causal Neural Networks for STC Analysis loaded successfully.\n")
cat("Available functions:\n")
cat("- setup_python_env(): Setup Python environment\n")
cat("- tarnet_stc_analysis(): TarNet implementation\n")
cat("- cfr_stc_analysis(): Counterfactual Regression implementation\n")
cat("- neural_network_stc_analysis(): Unified interface\n")
cat("- calculate_representation_balance(): Balance evaluation\n\n")
cat("Note: Requires Python environment with TensorFlow.\n")
cat("Run setup_python_env() first if not already configured.\n\n") 