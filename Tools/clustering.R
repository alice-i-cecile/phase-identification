# Dependencies ####
source("./Tools/distance_computation.R")

# Clustering methods ####

clust_blockmodel <- function(data, markov_kernel, ...) {
    
}

clust_flow <- function(data, markov_kernel, ...) {
    
}

# Wrapper ####

find_clusters <- function(markov_kernel, 
                          method, distance_type = "hitting_time",
                          ...){
    
    clusters <- switch(method,
                            "blockmodel" = clust_blockmodel(data, markov_kernel, ...),
                            "flow" = clust_flow(data, markov_kernel, ...)
                       )
    
    return(clusters)
}


