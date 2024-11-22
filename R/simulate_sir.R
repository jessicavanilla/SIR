#' Simulating a SIR infection scenario
#'
#' @param prob A float indicating probability of infection.
#' @param X A matrix of 0's, 1's, and/or 2's.
#'
#' @return A list of the number of iterations until the simulation ended, the
#'    probability of infection, the proportion of cells infected, and the matrix
#'    itself.
#' @export
#'
#' @examples
#' simulate_sir(X = infect_corners(50, 50))
#'
#' simulate_sir(prob = 0.2, X = random_infection(25, 25))
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
