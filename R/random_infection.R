#' Random infection of a matrix
#'
#' @param nrow An integer value of number of rows.
#' @param ncol An integer value of number of columns.
#' @param prob A float value of the probability of infection.
#'
#' @return A matrix of special class: SIRmatrix.
#' @export
#'
#' @examples
#' random_infection()
#'
#' random_infection(50, 50)
#'
#' random_infection(25, 25, 0.15)
random_infection <- function(nrow = 20, ncol = 20, prob = 0.15) {
  # calculate total # of cells & # of people infected
  total_cells <- nrow * ncol
  num_infected <- round(total_cells * prob)

  # use those to create vector
  initial_states <- c(rep(1, num_infected), rep(0, total_cells - num_infected))

  # shuffle the vector to randomize data
  initial_states <- sample(initial_states)

  # convert the vector to a matrix, return
  X <- matrix(initial_states, nrow = nrow, ncol = ncol)
  class(X) <- c("SIRmatrix", class(X))
  X
}
