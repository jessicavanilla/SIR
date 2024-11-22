simulate_sir <- function(prob = 0.125, X){
  count = 0
  while(any(X == 1)){
    X <- infection_step(X, prob)
    count <- count + 1
  }

  # calculate proportion of cells infected
  prop <- mean(X == 2)

  # create a list with the return elements
  returnList <- list(iterationNumber = count,
                     probability = prob, proportion = prop, matrix = X)
  returnList
}
