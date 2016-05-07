# Dependencies ####
source("./Tools/distance_computation.R")

# Wrapper ####

find_clusters <- function(data, markov_kernel, 
                          method, distance_type = "hitting_time", symmetrization_method = "none",
                          ...){
    
    clusters <- switch(method,
                            "blockmodel" = clust_blockmodel(data, markov_kernel, ...),
                            "flow" = clust_flow(data, markov_kernel, ...),
                            "kmeans" = clust_kmeans(data, markov_kernel, distance_type, symmetrization_method, ...)
                       )
    
    return(clusters)
}


# Clustering methods ####

clust_blockmodel <- function(data, markov_kernel, ...) {
    
}

clust_flow <- function(data, markov_kernel, ...) {
    
}

clust_kmeans <- function(data, markov_kernel, distance_type, symmetrization_method, ...) {
    
}