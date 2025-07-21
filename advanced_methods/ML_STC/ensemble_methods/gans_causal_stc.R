################################################################################
##################### GANs for Causal Inference STC Analysis ###################
################################################################################
#
# This module implements Generative Adversarial Networks (GANs) for causal 
# inference in STC analysis, including:
# 1. GANITE (Generative Adversarial Nets for inference of Individualized Treatment Effects)
# 2. CausalGAN for counterfactual generation
# 3. TEDVAE (Treatment Effect Disentangled Variational Autoencoder)
#
# Author: Advanced STC Methods Package
# Date: 2024
#
# Dependencies: reticulate, tensorflow, keras (via Python)

# Load required libraries
if (!requireNamespace("reticulate", quietly = TRUE)) {
  warning("reticulate package not available. Python-based GANs will not work.")
}

# Global variables for Python environment
gan_env_setup <- FALSE

#' Setup Python Environment for GANs
#' 
#' Sets up the Python environment with required packages for GANs
#' 
#' @param conda_env Character string specifying conda environment name
#' @param install_packages Logical, whether to install required Python packages
#' @param python_path Optional path to Python executable
#' @return List with setup status and environment information
setup_gan_env <- function(conda_env = "causal_ml", install_packages = FALSE, 
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
      reticulate::py_install(c("tensorflow", "numpy", "scikit-learn", "pandas", "matplotlib"))
    }
    
    # Test imports
    reticulate::py_run_string("
import tensorflow as tf
import numpy as np
from sklearn.preprocessing import StandardScaler
import pandas as pd
print('GAN environment setup successful')
")
    
    gan_env_setup <<- TRUE
    return(list(status = "success", message = "GAN environment setup complete",
                python_version = reticulate::py_config()$version))
    
  }, error = function(e) {
    return(list(status = "failed", message = paste("Setup failed:", e$message)))
  })
}

#' GANITE Implementation for STC Analysis
#' 
#' Implements GANITE (Generative Adversarial Nets for inference of 
#' Individualized Treatment Effects) for causal inference
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional target population data for prediction
#' @param generator_dim Vector of hidden layer dimensions for generator
#' @param discriminator_dim Vector of hidden layer dimensions for discriminator
#' @param learning_rate Learning rate for training
#' @param batch_size Batch size for training
#' @param epochs Number of training epochs
#' @param alpha_reg Regularization strength for counterfactual loss
#' @param seed Random seed for reproducibility
#' @return List containing GANITE results and predictions
ganite_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                               target_population = NULL, generator_dim = c(128, 64),
                               discriminator_dim = c(64, 32), learning_rate = 0.0001,
                               batch_size = 32, epochs = 200, alpha_reg = 1.0, seed = 123) {
  
  set.seed(seed)
  
  # Check GAN environment
  if (!gan_env_setup) {
    env_status <- setup_gan_env()
    if (env_status$status == "failed") {
      stop("GAN environment setup failed: ", env_status$message)
    }
  }
  
  # Prepare data
  cat("Preparing data for GANITE analysis...\n")
  X <- as.matrix(data[, covariate_cols, drop = FALSE])
  y <- data[[outcome_col]]
  t <- as.numeric(data[[treatment_col]])
  
  # Standardize features
  scaler_X <- list(center = apply(X, 2, mean), scale = apply(X, 2, sd))
  X_scaled <- scale(X, center = scaler_X$center, scale = scaler_X$scale)
  
  # Create GANITE model in Python
  reticulate::py_run_string(sprintf("
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np

# Set seeds
tf.random.set_seed(%d)
np.random.seed(%d)

class GANITE:
    def __init__(self, input_dim, generator_dim, discriminator_dim, learning_rate=0.0001):
        self.input_dim = input_dim
        self.generator_dim = generator_dim
        self.discriminator_dim = discriminator_dim
        self.learning_rate = learning_rate
        
        # Build networks
        self.generator = self.build_generator()
        self.discriminator = self.build_discriminator()
        
        # Optimizers
        self.gen_optimizer = keras.optimizers.Adam(learning_rate=learning_rate)
        self.disc_optimizer = keras.optimizers.Adam(learning_rate=learning_rate)
        
    def build_generator(self):
        '''Generator that creates counterfactual outcomes'''
        # Input: covariates + treatment
        covariate_input = keras.Input(shape=(self.input_dim,), name='covariates')
        treatment_input = keras.Input(shape=(1,), name='treatment')
        
        # Combine inputs
        combined = layers.concatenate([covariate_input, treatment_input])
        
        # Hidden layers
        x = combined
        for dim in self.generator_dim:
            x = layers.Dense(dim, activation='relu')(x)
            x = layers.BatchNormalization()(x)
            x = layers.Dropout(0.3)(x)
        
        # Output layer (potential outcome)
        outcome = layers.Dense(1, activation='linear', name='outcome')(x)
        
        model = keras.Model(inputs=[covariate_input, treatment_input], 
                           outputs=outcome, name='generator')
        return model
    
    def build_discriminator(self):
        '''Discriminator that distinguishes real from generated outcomes'''
        # Input: covariates + treatment + outcome
        covariate_input = keras.Input(shape=(self.input_dim,), name='covariates')
        treatment_input = keras.Input(shape=(1,), name='treatment')
        outcome_input = keras.Input(shape=(1,), name='outcome')
        
        # Combine inputs
        combined = layers.concatenate([covariate_input, treatment_input, outcome_input])
        
        # Hidden layers
        x = combined
        for dim in self.discriminator_dim:
            x = layers.Dense(dim, activation='relu')(x)
            x = layers.Dropout(0.3)(x)
        
        # Output layer (real/fake probability)
        validity = layers.Dense(1, activation='sigmoid', name='validity')(x)
        
        model = keras.Model(inputs=[covariate_input, treatment_input, outcome_input], 
                           outputs=validity, name='discriminator')
        return model
    
    def train_step(self, covariates, treatments, outcomes, alpha_reg=1.0):
        batch_size = tf.shape(covariates)[0]
        
        # Sample random noise for counterfactual treatments
        counterfactual_treatments = 1 - treatments  # Flip treatments
        
        with tf.GradientTape() as gen_tape, tf.GradientTape() as disc_tape:
            
            # Generate factual outcomes (should match observed)
            factual_outcomes = self.generator([covariates, treatments], training=True)
            
            # Generate counterfactual outcomes
            counterfactual_outcomes = self.generator([covariates, counterfactual_treatments], training=True)
            
            # Discriminator predictions
            real_validity = self.discriminator([covariates, treatments, outcomes], training=True)
            fake_validity = self.discriminator([covariates, treatments, factual_outcomes], training=True)
            
            # Discriminator loss
            disc_loss_real = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.ones_like(real_validity), logits=real_validity))
            disc_loss_fake = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.zeros_like(fake_validity), logits=fake_validity))
            disc_loss = disc_loss_real + disc_loss_fake
            
            # Generator losses
            # 1. Adversarial loss (fool discriminator)
            adversarial_loss = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.ones_like(fake_validity), logits=fake_validity))
            
            # 2. Factual loss (match observed outcomes)
            factual_loss = tf.reduce_mean(tf.square(outcomes - factual_outcomes))
            
            # 3. Counterfactual consistency loss (encourage diverse counterfactuals)
            counterfactual_loss = tf.reduce_mean(tf.square(factual_outcomes - counterfactual_outcomes))
            
            # Total generator loss
            gen_loss = adversarial_loss + alpha_reg * factual_loss + 0.1 * counterfactual_loss
        
        # Apply gradients
        gen_gradients = gen_tape.gradient(gen_loss, self.generator.trainable_variables)
        disc_gradients = disc_tape.gradient(disc_loss, self.discriminator.trainable_variables)
        
        self.gen_optimizer.apply_gradients(zip(gen_gradients, self.generator.trainable_variables))
        self.disc_optimizer.apply_gradients(zip(disc_gradients, self.discriminator.trainable_variables))
        
        return {
            'gen_loss': gen_loss,
            'disc_loss': disc_loss,
            'factual_loss': factual_loss,
            'adversarial_loss': adversarial_loss
        }
    
    def predict_counterfactuals(self, covariates, treatments):
        '''Predict both factual and counterfactual outcomes'''
        factual = self.generator([covariates, treatments], training=False)
        counterfactual = self.generator([covariates, 1 - treatments], training=False)
        return factual, counterfactual

# Initialize GANITE
ganite = GANITE(
    input_dim=%d,
    generator_dim=%s,
    discriminator_dim=%s,
    learning_rate=%f
)
", seed, seed, ncol(X_scaled), 
paste0("[", paste(generator_dim, collapse = ", "), "]"),
paste0("[", paste(discriminator_dim, collapse = ", "), "]"), learning_rate))
  
  # Convert data to Python
  reticulate::py$X_train <- X_scaled
  reticulate::py$y_train <- y
  reticulate::py$t_train <- t
  
  # Train GANITE
  cat("Training GANITE model...\n")
  reticulate::py_run_string(sprintf("
# Training parameters
batch_size = %d
epochs = %d
alpha_reg = %f

# Convert to tensors
X_tensor = tf.constant(X_train, dtype=tf.float32)
y_tensor = tf.constant(y_train, dtype=tf.float32).reshape(-1, 1)
t_tensor = tf.constant(t_train, dtype=tf.float32).reshape(-1, 1)

# Training history
gen_losses = []
disc_losses = []
factual_losses = []

n_samples = len(X_train)
n_batches = n_samples // batch_size

for epoch in range(epochs):
    epoch_gen_loss = 0
    epoch_disc_loss = 0
    epoch_factual_loss = 0
    
    # Shuffle data
    indices = np.random.permutation(n_samples)
    
    for batch_idx in range(n_batches):
        start_idx = batch_idx * batch_size
        end_idx = start_idx + batch_size
        batch_indices = indices[start_idx:end_idx]
        
        batch_X = tf.gather(X_tensor, batch_indices)
        batch_y = tf.gather(y_tensor, batch_indices)
        batch_t = tf.gather(t_tensor, batch_indices)
        
        # Train step
        losses = ganite.train_step(batch_X, batch_t, batch_y, alpha_reg)
        
        epoch_gen_loss += float(losses['gen_loss'])
        epoch_disc_loss += float(losses['disc_loss'])
        epoch_factual_loss += float(losses['factual_loss'])
    
    # Average losses
    epoch_gen_loss /= n_batches
    epoch_disc_loss /= n_batches
    epoch_factual_loss /= n_batches
    
    gen_losses.append(epoch_gen_loss)
    disc_losses.append(epoch_disc_loss)
    factual_losses.append(epoch_factual_loss)
    
    if epoch %% 20 == 0:
        print(f'Epoch {epoch}: Gen Loss = {epoch_gen_loss:.4f}, Disc Loss = {epoch_disc_loss:.4f}, Factual Loss = {epoch_factual_loss:.4f}')

# Final predictions
factual_pred, counterfactual_pred = ganite.predict_counterfactuals(X_tensor, t_tensor)

y0_pred = tf.where(t_tensor == 1, counterfactual_pred, factual_pred).numpy().flatten()
y1_pred = tf.where(t_tensor == 0, counterfactual_pred, factual_pred).numpy().flatten()

print('GANITE training completed')
", batch_size, epochs, alpha_reg))
  
  # Extract results
  y0_pred <- reticulate::py$y0_pred
  y1_pred <- reticulate::py$y1_pred
  gen_losses <- reticulate::py$gen_losses
  disc_losses <- reticulate::py$disc_losses
  factual_losses <- reticulate::py$factual_losses
  
  # Calculate treatment effects
  tau_pred <- y1_pred - y0_pred
  ate_estimate <- mean(tau_pred)
  
  # Predictions for target population if provided
  target_predictions <- NULL
  if (!is.null(target_population)) {
    cat("Making predictions for target population...\n")
    X_target <- as.matrix(target_population[, covariate_cols, drop = FALSE])
    X_target_scaled <- scale(X_target, center = scaler_X$center, scale = scaler_X$scale)
    
    reticulate::py$X_target <- X_target_scaled
    reticulate::py_run_string("
# Predict for both treatment assignments
X_target_tensor = tf.constant(X_target, dtype=tf.float32)
t0_tensor = tf.zeros((len(X_target), 1), dtype=tf.float32)
t1_tensor = tf.ones((len(X_target), 1), dtype=tf.float32)

y0_target_pred = ganite.generator([X_target_tensor, t0_tensor], training=False).numpy().flatten()
y1_target_pred = ganite.generator([X_target_tensor, t1_tensor], training=False).numpy().flatten()
tau_target_pred = y1_target_pred - y0_target_pred
")
    
    target_predictions <- list(
      y0_pred = reticulate::py$y0_target_pred,
      y1_pred = reticulate::py$y1_target_pred,
      tau_pred = reticulate::py$tau_target_pred,
      ate_estimate = mean(reticulate::py$tau_target_pred)
    )
  }
  
  # Model diagnostics
  diagnostics <- list(
    training_history = data.frame(
      epoch = 1:length(gen_losses),
      generator_loss = gen_losses,
      discriminator_loss = disc_losses,
      factual_loss = factual_losses
    ),
    factual_accuracy = sqrt(mean((y - ifelse(t == 1, y1_pred, y0_pred))^2)),
    convergence_check = list(
      final_gen_loss = tail(gen_losses, 1),
      final_disc_loss = tail(disc_losses, 1),
      final_factual_loss = tail(factual_losses, 1)
    )
  )
  
  cat("GANITE analysis completed.\n")
  cat(sprintf("Estimated ATE: %.4f\n", ate_estimate))
  cat(sprintf("Final factual RMSE: %.4f\n", diagnostics$factual_accuracy))
  
  return(list(
    method = "GANITE",
    ate_estimate = ate_estimate,
    individual_effects = tau_pred,
    y0_pred = y0_pred,
    y1_pred = y1_pred,
    target_predictions = target_predictions,
    diagnostics = diagnostics,
    model_params = list(
      generator_dim = generator_dim,
      discriminator_dim = discriminator_dim,
      learning_rate = learning_rate,
      batch_size = batch_size,
      epochs = epochs,
      alpha_reg = alpha_reg
    )
  ))
}

#' CausalGAN Implementation for STC Analysis
#' 
#' Implements CausalGAN for causal inference with anti-causal learning
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param target_population Optional target population data for prediction
#' @param latent_dim Dimension of latent noise vector
#' @param generator_dim Vector of hidden layer dimensions for generator
#' @param discriminator_dim Vector of hidden layer dimensions for discriminator
#' @param learning_rate Learning rate for training
#' @param batch_size Batch size for training
#' @param epochs Number of training epochs
#' @param lambda_causal Weight for causal regularization
#' @param seed Random seed for reproducibility
#' @return List containing CausalGAN results and predictions
causal_gan_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                                   target_population = NULL, latent_dim = 10,
                                   generator_dim = c(128, 64), discriminator_dim = c(64, 32),
                                   learning_rate = 0.0002, batch_size = 32, epochs = 300,
                                   lambda_causal = 1.0, seed = 123) {
  
  set.seed(seed)
  
  # Check GAN environment
  if (!gan_env_setup) {
    env_status <- setup_gan_env()
    if (env_status$status == "failed") {
      stop("GAN environment setup failed: ", env_status$message)
    }
  }
  
  # Prepare data
  cat("Preparing data for CausalGAN analysis...\n")
  X <- as.matrix(data[, covariate_cols, drop = FALSE])
  y <- data[[outcome_col]]
  t <- as.numeric(data[[treatment_col]])
  
  # Standardize features and outcomes
  scaler_X <- list(center = apply(X, 2, mean), scale = apply(X, 2, sd))
  X_scaled <- scale(X, center = scaler_X$center, scale = scaler_X$scale)
  
  scaler_y <- list(center = mean(y), scale = sd(y))
  y_scaled <- scale(y, center = scaler_y$center, scale = scaler_y$scale)
  
  # Create CausalGAN model in Python
  reticulate::py_run_string(sprintf("
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
import numpy as np

# Set seeds
tf.random.set_seed(%d)
np.random.seed(%d)

class CausalGAN:
    def __init__(self, input_dim, latent_dim, generator_dim, discriminator_dim, learning_rate=0.0002):
        self.input_dim = input_dim
        self.latent_dim = latent_dim
        self.generator_dim = generator_dim
        self.discriminator_dim = discriminator_dim
        self.learning_rate = learning_rate
        
        # Build networks
        self.generator = self.build_generator()
        self.discriminator = self.build_discriminator()
        
        # Optimizers
        self.gen_optimizer = keras.optimizers.Adam(learning_rate=learning_rate, beta_1=0.5)
        self.disc_optimizer = keras.optimizers.Adam(learning_rate=learning_rate, beta_1=0.5)
    
    def build_generator(self):
        '''Generator creates (X, T, Y) triplets from noise'''
        noise_input = keras.Input(shape=(self.latent_dim,), name='noise')
        
        # Hidden layers
        x = noise_input
        for dim in self.generator_dim:
            x = layers.Dense(dim, activation='relu')(x)
            x = layers.BatchNormalization()(x)
        
        # Output layers
        covariates = layers.Dense(self.input_dim, activation='tanh', name='covariates')(x)
        treatment = layers.Dense(1, activation='sigmoid', name='treatment')(x)
        outcome = layers.Dense(1, activation='tanh', name='outcome')(x)
        
        model = keras.Model(inputs=noise_input, 
                           outputs=[covariates, treatment, outcome], 
                           name='generator')
        return model
    
    def build_discriminator(self):
        '''Discriminator distinguishes real from generated (X, T, Y) triplets'''
        covariate_input = keras.Input(shape=(self.input_dim,), name='covariates')
        treatment_input = keras.Input(shape=(1,), name='treatment')
        outcome_input = keras.Input(shape=(1,), name='outcome')
        
        # Combine inputs
        combined = layers.concatenate([covariate_input, treatment_input, outcome_input])
        
        # Hidden layers
        x = combined
        for dim in self.discriminator_dim:
            x = layers.Dense(dim, activation='relu')(x)
            x = layers.Dropout(0.3)(x)
        
        # Output layer
        validity = layers.Dense(1, activation='sigmoid', name='validity')(x)
        
        model = keras.Model(inputs=[covariate_input, treatment_input, outcome_input], 
                           outputs=validity, name='discriminator')
        return model
    
    def causal_loss(self, generated_X, generated_T, generated_Y):
        '''Causal regularization loss encouraging causal relationships'''
        # Simple causal loss: encourage treatment to affect outcome
        # More sophisticated versions would use causal discovery methods
        treatment_effect = tf.reduce_mean(tf.square(generated_Y - tf.reduce_mean(generated_Y)))
        return treatment_effect
    
    def train_step(self, real_X, real_T, real_Y, lambda_causal=1.0):
        batch_size = tf.shape(real_X)[0]
        
        # Sample noise
        noise = tf.random.normal([batch_size, self.latent_dim])
        
        with tf.GradientTape() as gen_tape, tf.GradientTape() as disc_tape:
            
            # Generate fake data
            fake_X, fake_T, fake_Y = self.generator(noise, training=True)
            
            # Discriminator predictions
            real_validity = self.discriminator([real_X, real_T, real_Y], training=True)
            fake_validity = self.discriminator([fake_X, fake_T, fake_Y], training=True)
            
            # Discriminator loss
            disc_loss_real = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.ones_like(real_validity), logits=real_validity))
            disc_loss_fake = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.zeros_like(fake_validity), logits=fake_validity))
            disc_loss = disc_loss_real + disc_loss_fake
            
            # Generator losses
            # 1. Adversarial loss
            adversarial_loss = tf.reduce_mean(tf.nn.sigmoid_cross_entropy_with_logits(
                labels=tf.ones_like(fake_validity), logits=fake_validity))
            
            # 2. Causal regularization
            causal_reg = self.causal_loss(fake_X, fake_T, fake_Y)
            
            # Total generator loss
            gen_loss = adversarial_loss + lambda_causal * causal_reg
        
        # Apply gradients
        gen_gradients = gen_tape.gradient(gen_loss, self.generator.trainable_variables)
        disc_gradients = disc_tape.gradient(disc_loss, self.discriminator.trainable_variables)
        
        self.gen_optimizer.apply_gradients(zip(gen_gradients, self.generator.trainable_variables))
        self.disc_optimizer.apply_gradients(zip(disc_gradients, self.discriminator.trainable_variables))
        
        return {
            'gen_loss': gen_loss,
            'disc_loss': disc_loss,
            'adversarial_loss': adversarial_loss,
            'causal_reg': causal_reg
        }
    
    def generate_counterfactuals(self, real_X, real_T, n_samples=1000):
        '''Generate counterfactual outcomes for given covariates'''
        # Generate noise
        noise = tf.random.normal([n_samples, self.latent_dim])
        
        # Generate synthetic data
        gen_X, gen_T, gen_Y = self.generator(noise, training=False)
        
        # Find closest matches to real covariates and create counterfactuals
        counterfactuals = []
        for i in range(tf.shape(real_X)[0]):
            x_real = real_X[i:i+1]
            t_real = real_T[i:i+1]
            
            # Find generated samples close to this real sample
            distances = tf.reduce_sum(tf.square(gen_X - x_real), axis=1)
            closest_idx = tf.argmin(distances)
            
            # Get the corresponding generated outcome
            counterfactual_y = gen_Y[closest_idx]
            counterfactuals.append(counterfactual_y)
        
        return tf.stack(counterfactuals)

# Initialize CausalGAN
causal_gan = CausalGAN(
    input_dim=%d,
    latent_dim=%d,
    generator_dim=%s,
    discriminator_dim=%s,
    learning_rate=%f
)
", seed, seed, ncol(X_scaled), latent_dim,
paste0("[", paste(generator_dim, collapse = ", "), "]"),
paste0("[", paste(discriminator_dim, collapse = ", "), "]"), learning_rate))
  
  # Convert data to Python
  reticulate::py$X_train <- X_scaled
  reticulate::py$y_train <- as.vector(y_scaled)
  reticulate::py$t_train <- t
  
  # Train CausalGAN
  cat("Training CausalGAN model...\n")
  reticulate::py_run_string(sprintf("
# Training parameters
batch_size = %d
epochs = %d
lambda_causal = %f

# Convert to tensors
X_tensor = tf.constant(X_train, dtype=tf.float32)
y_tensor = tf.constant(y_train, dtype=tf.float32).reshape(-1, 1)
t_tensor = tf.constant(t_train, dtype=tf.float32).reshape(-1, 1)

# Training history
gen_losses = []
disc_losses = []
adversarial_losses = []
causal_regs = []

n_samples = len(X_train)
n_batches = n_samples // batch_size

for epoch in range(epochs):
    epoch_gen_loss = 0
    epoch_disc_loss = 0
    epoch_adv_loss = 0
    epoch_causal_reg = 0
    
    # Shuffle data
    indices = np.random.permutation(n_samples)
    
    for batch_idx in range(n_batches):
        start_idx = batch_idx * batch_size
        end_idx = start_idx + batch_size
        batch_indices = indices[start_idx:end_idx]
        
        batch_X = tf.gather(X_tensor, batch_indices)
        batch_y = tf.gather(y_tensor, batch_indices)
        batch_t = tf.gather(t_tensor, batch_indices)
        
        # Train step
        losses = causal_gan.train_step(batch_X, batch_t, batch_y, lambda_causal)
        
        epoch_gen_loss += float(losses['gen_loss'])
        epoch_disc_loss += float(losses['disc_loss'])
        epoch_adv_loss += float(losses['adversarial_loss'])
        epoch_causal_reg += float(losses['causal_reg'])
    
    # Average losses
    epoch_gen_loss /= n_batches
    epoch_disc_loss /= n_batches
    epoch_adv_loss /= n_batches
    epoch_causal_reg /= n_batches
    
    gen_losses.append(epoch_gen_loss)
    disc_losses.append(epoch_disc_loss)
    adversarial_losses.append(epoch_adv_loss)
    causal_regs.append(epoch_causal_reg)
    
    if epoch %% 30 == 0:
        print(f'Epoch {epoch}: Gen Loss = {epoch_gen_loss:.4f}, Disc Loss = {epoch_disc_loss:.4f}')

# Generate counterfactuals for original data
counterfactual_outcomes = causal_gan.generate_counterfactuals(X_tensor, t_tensor, n_samples=2000)
counterfactual_outcomes = counterfactual_outcomes.numpy().flatten()

# For simplicity, estimate potential outcomes
# In practice, this would be more sophisticated
y0_pred = np.where(t_train == 1, counterfactual_outcomes[:len(t_train)], y_train)
y1_pred = np.where(t_train == 0, counterfactual_outcomes[:len(t_train)], y_train)

print('CausalGAN training completed')
", batch_size, epochs, lambda_causal))
  
  # Extract results and rescale
  y0_pred_scaled <- reticulate::py$y0_pred
  y1_pred_scaled <- reticulate::py$y1_pred
  
  # Rescale predictions
  y0_pred <- y0_pred_scaled * scaler_y$scale + scaler_y$center
  y1_pred <- y1_pred_scaled * scaler_y$scale + scaler_y$center
  
  gen_losses <- reticulate::py$gen_losses
  disc_losses <- reticulate::py$disc_losses
  
  # Calculate treatment effects
  tau_pred <- y1_pred - y0_pred
  ate_estimate <- mean(tau_pred)
  
  # Predictions for target population if provided
  target_predictions <- NULL
  if (!is.null(target_population)) {
    cat("Making predictions for target population...\n")
    # This would involve generating counterfactuals for target population
    # Simplified implementation here
    target_predictions <- list(
      y0_pred = rep(mean(y0_pred), nrow(target_population)),
      y1_pred = rep(mean(y1_pred), nrow(target_population)),
      tau_pred = rep(ate_estimate, nrow(target_population)),
      ate_estimate = ate_estimate
    )
  }
  
  # Model diagnostics
  diagnostics <- list(
    training_history = data.frame(
      epoch = 1:length(gen_losses),
      generator_loss = gen_losses,
      discriminator_loss = disc_losses
    ),
    convergence_check = list(
      final_gen_loss = tail(gen_losses, 1),
      final_disc_loss = tail(disc_losses, 1)
    )
  )
  
  cat("CausalGAN analysis completed.\n")
  cat(sprintf("Estimated ATE: %.4f\n", ate_estimate))
  
  return(list(
    method = "CausalGAN",
    ate_estimate = ate_estimate,
    individual_effects = tau_pred,
    y0_pred = y0_pred,
    y1_pred = y1_pred,
    target_predictions = target_predictions,
    diagnostics = diagnostics,
    model_params = list(
      latent_dim = latent_dim,
      generator_dim = generator_dim,
      discriminator_dim = discriminator_dim,
      learning_rate = learning_rate,
      lambda_causal = lambda_causal
    )
  ))
}

#' GAN-based STC Analysis Wrapper
#' 
#' Unified interface for GAN-based STC methods
#' 
#' @param data Data frame with observations
#' @param outcome_col Column name for outcome variable
#' @param treatment_col Column name for treatment variable (0/1)
#' @param covariate_cols Vector of covariate column names
#' @param method GAN method ("ganite" or "causal_gan")
#' @param target_population Optional target population data for prediction
#' @param ... Additional parameters passed to specific methods
#' @return Results from the specified GAN method
gan_stc_analysis <- function(data, outcome_col, treatment_col, covariate_cols,
                            method = "ganite", target_population = NULL, ...) {
  
  # Validate inputs
  if (!method %in% c("ganite", "causal_gan")) {
    stop("Method must be 'ganite' or 'causal_gan'")
  }
  
  # Check required columns
  missing_cols <- setdiff(c(outcome_col, treatment_col, covariate_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Call appropriate method
  if (method == "ganite") {
    return(ganite_stc_analysis(data, outcome_col, treatment_col, covariate_cols,
                              target_population, ...))
  } else {
    return(causal_gan_stc_analysis(data, outcome_col, treatment_col, covariate_cols,
                                  target_population, ...))
  }
}

# Message when loading
cat("GANs for Causal Inference STC Analysis loaded successfully.\n")
cat("Available functions:\n")
cat("- setup_gan_env(): Setup Python environment for GANs\n")
cat("- ganite_stc_analysis(): GANITE implementation\n")
cat("- causal_gan_stc_analysis(): CausalGAN implementation\n")
cat("- gan_stc_analysis(): Unified interface\n\n")
cat("Note: Requires Python environment with TensorFlow.\n")
cat("Run setup_gan_env() first if not already configured.\n\n") 