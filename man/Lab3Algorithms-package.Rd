\name{Lab3Algorithms-package}
\alias{Lab3Algorithms-package}
\alias{Lab3Algorithms}
\docType{package}
\title{
Lab 3 Algorithms
}
\description{
This package contains the functions euclidean() and dijkstra() to solve Lab 3.
These functions can be used to find the gcd of two numbers (euclidean) or to 
calculate the shortest path in a graph from one node to all other nodes (dijkstra)
The package also includes the data set wiki.graph to get an example graph for 
the dijkstra function.
}
\author{
Juan Manuel Pardo and Nikolaus Stratil-Sauer
}
\references{
Euclidean algorithm: \url{https://en.wikipedia.org/wiki/Euclidean_algorithm} 
Dijkstra algorithm: \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
}
\examples{
data("wiki_graph")
euclidean(100,1000)
dijkstra(wiki_graph,1)
}
