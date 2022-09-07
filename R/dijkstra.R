dijkstra <- function(graph, init_node){
  # Check that the arguments are correct
  stopifnot(is.numeric(init_node), init_node %in% c(graph$v1,graph$v2))
  stopifnot(is.data.frame(graph),is.numeric(graph$v1),is.numeric(graph$v2),
            is.numeric(graph$w))
  
  # Get the number of nodes
  n <- length(unique(c(graph$v1,graph$v2)))
  
  # Vector preallocation
  dist <- numeric(n)
  prev <- numeric(n)
  Q <- numeric(n)

  # Set predefined vector values
  for (v in 1:n){
    dist[v] <- Inf
    prev[v] <- 0
    Q[v] <- v
  }
  
  # Set distance to 0 for the initial node
  dist[init_node] <- 0

  while (length(Q) >= 1){
    ## Try length
    # Find the node with the minimum distance
    u <- Q[which.min(dist[Q])]
    # Delete the corresponding node index
    Q <- Q[! Q %in% c(u)]
    # Search for the neighbor nodes of u
    neigh_nodes <- c()
    nnp <- c()
    for (i in 1:length(graph[[1]])){
      if (u == graph[i,1]){
        ## THE FUNCTION WORKS DIFFERENTLY WHEN DELETING THE PRINT???
        neigh_nodes <- append(neigh_nodes, graph[i,2])
        # Save the neighbor node positions from the graph data frame
        nnp <- append(nnp, i)
      }
    }
    # Find and save the minimum distances
    p <- 0
    for (v in nnp){
      # p is the index from the nnp vector
      p <- p + 1
      alt <- dist[u] + graph[v,3]
      
      ## CHECK THE IF LOOP
      if (alt < dist[graph[v,2]]){
        dist[neigh_nodes[p]] <- alt
        prev[neigh_nodes[p]] <- u
      }
    }
  }
  
  return(dist)
}

