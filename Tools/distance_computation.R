# Dependencies ####

# Wrapper ####

compute_distances <- function(data, markov_kernel, distance_type = "hitting_time", symmetrization_method = "none"){
    distance_function <- switch(distance_type,
                            "direct" = dist_direct,
                            "shortest" = dist_shortest,
                            "hitting_time" = dist_hitting)
    
    # Computing distances
    distance_matrix <- matrix(NA, nrow = nrow(data), ncol=ncol(data))
    
    for (a in 1:nrow(data)) {
        for (b in 1:nrow(data)) {
            distance_matrix[a,b] <- distance_function(data[a,], data[b,], markov_kernel)
        }
    }
    
    # symmetrization
    if (symmetrization_method != "none") {
        symmetrization_function <- switch(symmetrization_method,
                                          "product" = function(x){rep.int(x[1] * x[2], 2)},
                                          "sum" = function(x,y){rep.int(x[1] + x[2], 2)}
                                          )
        
        pairs <- which(lower.tri(distance_matrix), arr.ind = TRUE)
        
        for (i in 1:nrow(pairs)){
            pair <- pairs[i,]
            symmetrical_result <- symmetrization_function(distance_matrix[pair])
            distance_matrix[pair] <- symmetrical_result
            distance_matrix[rev(pair)] <- symmetrical_result
        }
    }
    
    return(distance_matrix)

}

# Distance functions ####
dist_direct <- function(a, b, markov_kernel) {
    
}

dist_shortest <- function(a, b, markov_kernel) {
    
}

dist_hitting <- function(a, b, markov_kernel) {
    
}