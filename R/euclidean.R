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
