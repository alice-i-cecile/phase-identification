# Dependencies ####

# Base Markov kernel class ####
# Uses "Reference class" object-oriented system
# http://adv-r.had.co.nz/OO-essentials.html

# Automatically detecting domain
detect_domain <- function(data){
    extract_domain <- function(col){
        if (!is.numeric(col)) { # duck typing
            return(unique(col))
        } else {
            return(c(min(col), max(col)))
        }
    }
    
    domain <- sapply(data, extract_domain)
    
    return(domain)
}

# Find a uniform random point within a domain
random_point <- function(n, domain){
    random_from_axis <- function(axis_domain){
        if (is.numeric(axis_domain) & length(axis_domain) == 2) {# duck typing, breaks for categorical numeric of length two
            return(runif(n, min = axis[1], max = axis[2]))
        } else {
            return(sample(axis_domain, size = n, replace = TRUE))
        }
    }
    point <- sapply(domain, random_from_axis)
    
    return(point)
}

MarkovKernel <- setRefClass("MarkovKernel",
                            
                            
    fields = list(data = data.frame(),
                  domain = detect_domain(data), # Don't need. Solved by MCMC
                  hitting_times = NULL
    ),
    
    methods = list(
        
        # Initialization function
        # Enforces quality checks
        initialize <- function(){
            return(NULL)
        }
        
        # transition(a,b) returns the transition probability to get from a to b
        # Domain == codomain; set of all possible system states
        # This is a probability mass if the domain is discrete and a probability density if the domain is continuous
        transition <- function(a,b){
            return(NULL)
        },
        
        # random_step(a) returns a random point, as if walking 1 step from state a
        # Current implementation is lazy algorithm for continuous spaces
        # Should use MCMC instead
        random_step <- function(a, sample_size = 1000){
            
            candidate_points <- random_point(sample_size, domain)
            
            probabilities <- transition(a, candidate_points)
            
            selected_point <- sample(candidate_points, size = 1, prob = probabilities)
            
            return(selected_point)
        },
        
        
        # Returns a random point from the stationary distribution of the markov kernel
        # Allow for generation of multiple points at once for easier optimization
        # Format is identical to that received in data
        stationary_sample <- function(n=1){
            return()
        },
        
        # Hitting time method
        # Overwrite using already implemented algorithms for discrete domain Markov kernels
        # http://www.statslab.cam.ac.uk/~james/Markov/s13.pdf
        # Implemented as hitting time to a point within a certain distance in the continuous domain
        # Arguments: a,b
        hitting_time <- function(a,b, epsilon = 0, n_paths = 1000){
            
            random_path <- function(a,b){
                i <- a
                steps <- 0
                
                while (transition(a,b) > epsilon) {
                    i <- random_step(i)
                    steps <- steps + 1
                }
                
                return(steps)
            }
            
            mean_hitting_time <- mean(replicate(n_paths, random_path(a,b))) # unnecessarily poor memory usage
            
            return(mean_hitting_time)
        },
    
        # Compute hitting times between all pairs
        # Overwrite using much faster already implemented algorithms for discrete domain Markov kernels
        compute_hitting_times <- function(epsilon = 0, n_paths = 1000){
            dist_matrix <- matrix(NA, nrow(data), nrow(data))
            
            for (a in 1:nrow(data)) {
                for (b in 1:nrow(data)) {
                    dist_matrix[a,b] <- hitting_time(a,b, epsilon, n_paths)
                }
            }
            
            return(dist_matrix)
        },
        
        # Check for phi-irreducibility
        # Returns disjoint subsystems if they exist
        # Will likely need to be probabilistic?
        # Is this just the same problem again? Does it matter?
        phi_reducibility <- function(){
            return(NULL)
        }
    )    
)




# Hitting time method

# Distance is measured by direct transition probability
# Arguments: a,b,(epsilon)


# All hitting time method

# Computing distances
# distance_matrix <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
# 
# for (a in 1:nrow(data)) {
#     for (b in 1:nrow(data)) {
#         distance_matrix[a,b] <- distance_function(data[a,], data[b,], markov_kernel)
#     }
# }

# Discrete domain subclass

# Lumping method

# Continuous domain subclass

# Makrov kernel creation methods ####

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

# Wrapper ####

generate_markov_kernel <- function(data, method = "empirical", ...){
    
    # If a function is passed to the method argument, use it directly
    # Allows for customization and direct use of models
    if (is.function(method)) {
        return(method)
    }
    
    markov_kernel <- switch(method,
                            "discretized" = mk_discretized(data, ...),
                            "empirical" = mk_empirical(data, ...),
                            "knn" = mk_knn(data, ...),
                            "rf" = mk_rf(data, ...)
    )
    return(markov_kernel)
    
}