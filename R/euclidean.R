#' Calculate the gcd of two numbers using the euclidean algorithm
#'
#' This functions calculates the greatest common denominator (gcd) of two numbers
#' using the euclidean algorithm.
#' For specifics about the algorithm, refer to the wikipedia page
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param a Number a
#' @param b Number b
#' @return The gcd of \code{a} and \code{b}.
#' @examples
#' euclidean(1000, 100)
#' euclidean(250, 55)
euclidean <- function(a,b){
  stopifnot(is.numeric(a), is.numeric(b), length(a) == 1, length(b) == 1)
  if(a>b){
    remainder1 = abs(a)
    remainder2 = abs(b)
  } else {
    remainder1 = abs(b)
    remainder2 = abs(a)
  }
  while (remainder2 > 0) {
    multiplier = 0
    while((remainder1 - (multiplier*remainder2)) >=  remainder2){
      multiplier = multiplier + 1
    }
    temp = remainder1 - (multiplier*remainder2)
    remainder1 = remainder2
    remainder2 = temp
  }
  return(remainder1)
}
