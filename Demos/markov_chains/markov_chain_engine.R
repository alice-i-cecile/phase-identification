# Libraries ####
# https://en.wikipedia.org/wiki/Random_graph
# https://cran.r-project.org/web/packages/markovchain/vignettes/an_introduction_to_markovchain_package.pdf
# https://cran.r-project.org/web/packages/markovchain/vignettes/markovchainCrashIntro.pdf
library(igraph)
library(markovchain)

# Graph modifying tools ####

# Rerandomize weights of markov chain
randomize_mc <- function(mc){
    
}

# Make each outgoing path equally likely
balance_mc <- function(mc){
    
}

# Average several graphs
blend_mc <- function(mcs, blend_type = "replace"){
    
}

# Ensure graph is strongly connected
connect_graph <- function(graph){
    
}

# Connect all nodes to themselves
selflink_graph <- function(graph){
    
}

# Add random edges to the graph
cross_link_graph <- function(graph){
    
}

# Join several graphs
join_graphs <- function(graphs, n_connections=1){
    
}

# Symettrize edges
symettrize_mc <- function(mc, method = "mean"){
    
    # Symettrize each edge
    
    # Ensure transition matrix is still valid
    
}  

# Generating MCs ####

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
grid_graph <- function(n, dimension=2, dim=NA, wrap = FALSE){
    
    # Ensure dim and n match
    # Force square if dim is not given
    if (is.na(dim)) {
        dim = rep.int(round(n ^ (1 / dimension)), dimension)
        n <- dim[1] ^ dimension
    } else {
        n <- Reduce(function(a,b){a * b}, dim)
        dimension <- length(dim)
    }
    
    adjacency <- matrix(data = 0, nrow = n, ncol = n)
    
    # Map points to an array
    grid_mapping <- array(data = sample(n), dim = dim)
    
    
    rel_coord <- matrix(0, 2*dimension, dimension)
    
    for (i in 1:(2*dimension)) {
        direction <- ifelse(i %% 2, -1, 1)
        rel_coord[i,ceiling(i/2)] <- direction
    }
    
    wrap_coordinates <- function(position){
        
        wrapped_results <- mapply(function(x, maximum){x %% maximum}, x = position, maximum = dim(grid_mapping))
        
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
    
    # Connect points to all Manhattan neighbours
    for (i in grid_mapping) {
        coordinates <- which(grid_mapping == i, arr.ind = TRUE)
        neighbours <- get_neighbours(coordinates)
        
        adjacency[i, neighbours] <- 1
        
    }
    
    return(adjacency)
    
}

# Lattice adjacency graph
# https://en.wikipedia.org/wiki/Lattice_graph
lattice_graph <- function(n, dimension=2, dim = NA){
    
    # Ensure dim and n match
    # Force square if dim is not given
    if (is.na(dim)) {
        dim = rep.int(round(n ^ (1 / dimension)), dimension)
        n <- dim[1] ^ dimension
    } else {
        n <- Reduce(function(a,b){a * b}, dim)
        dimension <- length(dim)
    }
    
    adjacency <- matrix(data = 0, nrow = n, ncol = n)
    
    # Map points to an array
    grid_mapping <- array(data = sample(n), dim = dim)
    
    # Find points that are on a common axis
    find_axis_aligned <- function(coordinates){
        
        extract_axis <- function(x, axis){
            
            # Terrible hack since R's array accessing tools are awful
            empty_before <- Reduce(paste0, rep.int(",", axis - 1))
            empty_after <- Reduce(paste0, rep.int(",", dimension - axis))
            access_string <- paste0("grid_mapping[", empty_before, x, empty_after, "]")
            
            return(eval(parse(text = access_string)))
        }
        
        axis_aligned <- sapply(coordinates, extract_axis, axis = 1:dimension)
        
        return(axis_aligned)
    }
    
    # Connect points to all others that share a row or column
    for (i in grid_mapping) {
        coordinates <- which(grid_mapping == i, arr.ind = TRUE)
        axis_aligned <- find_axis_aligned(coordinates)
        
        adjacency[i, axis_aligned] <- 1
        
    }
    
    return(adjacency)
}

# Ring lattice graph
ring_graph <- function(n, K=2){
    
    # Start with no connections
    adjacency <- matrix(data = 0, nrow = n, ncol = n)
    
    # Assign vertices to points on a ring
    ring_map <- 1:n # not randomized to make smallworld code easier
    
    # Connect adjacent points
    # K is the number of edges each vertex has
    
    num_neighbours <- round(K/2)
    
    find_neighbours <- function(i){
        distances <- setdiff(-num_neighbours:num_neighbours, 0)
        
        positions <- (i + distances) %% n
        
        neighbours <- ring_map[positions]
        
        return(neighbours)
    }
    
    for (i in 1:n) {
        
        a <- ring_map[i]
        b <- find_neighbours(i)
        
        adjacency[a,b] <- 1
        
    }
    
    return(adjacency)
}

# Small world adjacency graph
# https://en.wikipedia.org/wiki/Watts_and_Strogatz_model
# n is number of vertexes
# K is number of neighbour connections
# beta is interconnectivity
smallworld_graph <- function(n, K=2, beta = 0.5){

    # Construct a ring lattice
    adjacency <- ring_graph(n, K)
    
    # Add crosslinks
    
    # For every node n_i=n_0,\dots, n_{N-1} take every edge (n_i, n_j) with i < j, and rewire it with probability \beta. 
    # Rewiring is done by replacing (n_i, n_j) with (n_i, n_k) where k is chosen with uniform probability 
    # from all possible values that avoid self-loops (k \ne i) and link duplication
    # (there is no edge (n_i, n_{k'}) with k' = k at this point in the algorithm).
                                                                                                                                             for (i in 1:(n - 1)) {
         for (j in 1:(i - 1)) {
             if (runif(1) < beta) {
                 possible_connections <- setdiff(which(adjacency[i,] == 0), i)
                 
                 adjacency[i, sample(possible_connections, 1)] <- 1
                 adjacency[i,j] <- 0
             }
         }                                                                                                                                  }
        
    return(adjacency)
}

# Tree adjacency graph
tree_graph <- function(n){
    
    # Start with no connections
    adjacency <- matrix(data = 0, nrow = n, ncol = n)
    
    # Connect nodes one at a time
    addition_order <- sample(n)
    added <- vector()
    
    for (i in addition_order) {
        
        # Add nodes by attaching them to a single parent at random
        # Creates random tree structure
        if (length(added > 0)) {
            parent <- sample(added, 1)

            adjacency[i, parent] <- 1
            adjacency[parent, i] <- 1
            
        }
        
        added <- c(added, i)
        
    }
    
    return(adjacency)
}

# Clustered adjacency graph
# Created out of several smaller graphs
cluster_graph <- function(n, n_clusters = 3, n_connections = 1, subgraph_type="smallworld", ...){
    
    # Create clusters
    vertex_split <- sample(n_clusters, n, replace = TRUE)
    
    graph_function <- switch(subgraph_type,
                             "complete" = complete_graph,
                             "kpartite" = kpartite_graph,
                             "grid" = grid_graph,
                             "lattice" = lattice_graph,
                             "ring" = ring_graph,
                             "smallworld" = smallworld_graph,
                             "tree" = tree_graph
    )
    
    
    clusters <- mapply(graph_function, n = vertex_split, ...)
    
    # Join clusters
    adjacency <- join_graphs(clusters, n_connections)
    
    return(adjacency)
}

# Wrapper
generate_mc <- function(n=16, prob=1, graph_type="random", ...){
    
    # Building adjacency matrix
    adjacency <- switch(graph_type,
                        "complete" = complete_graph(n, ...),
                        "kpartite" = kpartite_graph(n, ...),
                        "grid" = grid_graph(n, ...),
                        "lattice" = lattice_graph(n, ...),
                        "ring" = ring_graph(n, ...),
                        "smallworld" = smallworld_graph(n, ...),
                        "tree" = tree_graph(n, ...),
                        "cluster" = cluster_graph(n, ...)
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
              name = graph_type)
    
    return(mc)
    
}


# Graph diagnostics ####


# Generating data from markov chains ####
