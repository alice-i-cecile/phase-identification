# Dependencies ####
source("./Tools/markov_kernels.R")

# Convolution function ####
# For convoluting the transition functions of two Markov Chains
# Generic method, with cases for discrete space Markov chains

# Cross-correlation function ####
# Also used for autocorrelation
# Generic method, with cases for discrete space Markov chains

# Factory function for making general state Markov Chains ####
factory_mc <- function(initial, transition_function, time_steps){
    
    # Make a MarkovKernel object
    
    # Set up the data attribute
    
    # Save the transition function
    
    # Step through time usng the transition function
    
    # Return completed MarkovKernel object
    
}

# Linear state space models ####

# Random walk models ####

# Embedded Markov chains ####

# AR(1) models ####
# X_{t+1} = a*X_t + W_{t+1}
# W ~ \Gamma

# ARIMA models with seasonality ####

# Bilinear models ####

# SETAR (scalar self-exciting threshold autoregressive) models ####

# Gumleaf attractor models ####

# Forward and backwards recurrence time chain models ####

# Queueing models ####

# Storage models ####