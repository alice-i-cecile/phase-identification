# Libraries ####
# https://en.wikipedia.org/wiki/Random_graph
# https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf
# https://cran.r-project.org/web/packages/markovchain/vignettes/markovchainCrashIntro.pdf
library(igraph)
library(markovchain)

# Generating MCs ####

# Wrapper
generate_mc <- function(n=16, prob=1, method="random", ...){
    
    # Building adjacency matrix
    adjacency <- switch(method,
                        "complete" = complete_graph(n, ...),
                        "kpartite" =kpartite_graph(n, ...),
                        "grid" = grid_graph(n, ...),
                        "lattice" = lattice_graph(n, ...),
                        "smallworld" = smallworld_graph(n, ...),
                        "flow" = flow_graph(n, ...),
                        "hierarachical" = hierarchical_graph(n, ...)
                        )
    
    # Randomly remove edges
    adjacency[adjacency == 1] <- rbinom(sum(adjacency), 1, prob = prob)
    
    # Converting to transition matrix
    transitions <- adjacency
    transitions[adjacency == 1] <- runif(sum(adjacency))
    transitions <- transitions / rowSums(transitions)
    
    # Creating markovchain object
    mc <- new("markovchain", 
              states = paste("State", 1:n, sep = "_"),
              transitionMatrix = transitions,
              name = method)
    
    return(mc)
    
}

# Complete adjacency graph
complete_graph <- function(n){
    
    adjacency <- matrix(data = 1, nrow = n, ncol = n)
    
    return(adjacency)
}


# k-partite adjacency graph
kpartite_graph <- function(n, k_partitions = 2){
    
    # Building adjacency matrix
    adjacency <- matrix(data = 1, nrow = n, ncol = n)
    
    partitions <- data.frame(vertex = 1:n, partition = sample(k_partitions, n, replace = TRUE))

    for (i in 1:k_partitions) {
        vertex_set <- partitions[partitions$partition == i, "vertex"]
        
        edge_set <- expand.grid(vertex_set, vertex_set)
        
        for (e in 1:nrow(edge_set)) {
            a <- edge_set[e][1]
            b <- edge_set[e][2]
            
            adjacency[a,b] <- 0
        }
    }
    
    return(adjacency)
}

# Square grid adjacency graph
grid_graph <- function(n=16, prob=0.5, dimension=2, dim=NA, wrap = FALSE){
    
    # Ensure dim and n match
    # Force square if dim is not given
    if (is.na(dim)) {
        dim = rep.int(round(n ^ (1 / dimension)), dimension)
        n <- dim[1] ^ dimension
    } else {
        n <- Reduce(function(a,b){a * b}, dim)
        dimension <- length(dim)
    }
    
    # Building adjacency matrix
    adjacency <- matrix(data = 0, nrow = n, ncol = n)
    
    grid_mapping <- array(data = sample(n,n), dim = dim)
    
    
    rel_coord <- matrix(0, 2*dimension, dimension)
    
    for (i in 1:(2*dimension)) {
        direction <- ifelse(i %% 2, -1, 1)
        rel_coord[i,ceiling(i/2)] <- direction
    }
    
    wrap_coordinates <- function(position){
        
        wrap_coordinate <- function(x, maximum){
            if (x < 1) {
                return(maximum)
            } else if (x > maximum) {
                return(1)
            } else {
                return(x)
            }
        }
        
        wrapped_results <- mapply(wrap_coordinate, x = position, maximum = dim(grid_mapping))
        
        return(wrapped_results)
    }
    
    get_neighbour <- function(rel, coordinates){
        
        position <- coordinates + rel
        wrapped_position <- wrap_coordinates(position)
        
        if (all(position == wrapped_position)) {
            neighbour <- grid_mapping[position]
        } else if (wrap) {
            neighbour <- grid_mapping[wrapped_position]
        } else {
            neighbour <- NULL
        }
        
        return(neighbour)
        
    }
    
    get_neighbours <- function(coordinates){
        
        neighbours <- unlist(apply(rel_coord, MARGIN = 1, FUN = get_neighbour, coordinates = coordinates))
        
        return(neighbours)
    }
    
    for (i in grid_mapping) {
        coordinates <- which(grid_mapping == i, arr.ind = TRUE)
        neighbours <- get_neighbours(coordinates)
        
        adjacency[i, neighbours] <- 1
        
    }
    
    return(adjacency)
    
}

# Lattice adjacency graph
# https://en.wikipedia.org/wiki/Lattice_graph
lattice_graph <- function(n=16, prob=0.5, dim=2, shape = NA){
    
    
    return(adjacency)
}

# Small world adjacency graph
# https://en.wikipedia.org/wiki/Watts_and_Strogatz_model
smallworld_graph <- function(n=16){
    
    return(adjacency)
}

# Flow clustered adjacency graph
flow_graph <- function(n=16){
    
    return(adjacency)
}


# Hierarchical adjacency graph
hierarchical_graph <- function(n=16){
    
    return(adjacency)
}

# Graph modifying tools ####

# Make each outgoing path equally likely
balance_graph <- function(graph){
    
}

# Average several graphs
blend_graphs <- function(graphs, blend_type = "replace"){
    
}

# Ensure graph is strongly connected
connect_graph <- function(graph){
    
}

# Join several graphs
join_graphs <- function(graphs){
    
}
    
# Symettrize edges
symettrize_graph <- function(graph){
    
}



# Graph diagnostics ####


# Generating data from markov chains ####
