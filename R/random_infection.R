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
