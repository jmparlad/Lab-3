#' Calculate the shortest path between nodes in a graph
#'
#' This function computes the shortest path between nodes in a graph using 
#' the Dijkstra algorithm.
#' For specifics about the algorithm, refer to the wikipedia page
#' \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#'
#' @param graph Data frame with the edge nodes and the edge distance
#' @param init_node Initial node to compute the distances
#' @return The vector \code{dist} containing the minimum distance from the
#' initial node to the rest of nodes of the graph..
#' @examples
#' dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), init_node = 1)
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

