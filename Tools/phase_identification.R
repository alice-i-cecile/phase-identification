# Dependencies ####
source("./Tools/markov_kernels.R")
source("./Tools/clustering.R")

# Main wrapper ####

identify_phases <- function(data, 
                            markov_kernel_method = "empirical", 
                            cluster_method = "flow", 
                            distance_type = "hitting_time", 
                            ...){
    
    markov_kernel <- generate_markov_kernel(data, method = markov_kernel_method, ...)
    
    clusters <- find_clusters(data, markov_kernel = markov_kernel, method = cluster_method, distance_type = distance_type, ...)
    
    return(clusters)
    
}