# Dependencies ####


# Wrapper ####

generate_markov_kernel <- function(data, method = "empirical", ...){
    
    markov_kernel <- switch(method,
                            "discretized" = mk_discretized(data, ...),
                            "empirical" = mk_empirical(data, ...),
                            "knn" = mk_knn(data, ...),
                            "rf" = mk_rf(data, ...)
                            )
    return(markov_kernel)
    
}

# Makrob kernel creation methods ###

# Discretized Markov kernel
mk_discretized <- function(data, ...){
    
}


# Empirical Markov kernel
mk_empirical <- function(data, ...){
    
}

# K-nearest neighbours Markov kernel
mk_knn <- function(data, ...){
    
}

# Random forest Markov kernel
mk_rf <- function(data, ...){
    
}