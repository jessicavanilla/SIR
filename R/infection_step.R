#' A single infection time step
#'
#' @param x A matrix of 0's, 1's, and/or 2's.
#' @param prob A float value of the probability of infection of a given cell.
#'
#' @return A matrix of special class: SIRmatrix
#' @export
#'
#' @examples
#' infection_step(infect_corners(30, 30), 0.2)
#'
#' infection_step(random_infection(30, 30), 0.1)
infection_step <- function(x, prob){
  # make a slightly bigger matrix, so we don't have to worry about the boundaries.
  nr2 = nrow(x) + 2
  nc2 = ncol(x) + 2
  x2 <- matrix(0, nrow = nr2, ncol = nc2)
  infected <- which(x == 1, arr.ind = TRUE) + 1
  ni <- nrow(infected)

  directions <- c(-1, 0, 1)
  # Don't worry about cells that are already infected or removed
  for(i in directions){
    for(j in directions){
      infect_ij <- infected
      infect_ij[, "row"] <- infect_ij[, "row"] + i
      infect_ij[, "col"] <- infect_ij[, "col"] + j
      new_inf_rows <- sample(c(TRUE, FALSE), size = ni,
                             prob = c(prob, 1-prob), replace = TRUE)
      new_infect <- infect_ij[new_inf_rows, , drop = FALSE]
      x2[new_infect] <- 1
    }
  }

  # Remove the edges from the too big matrix
  X <- x2[-c(1, nr2), -c(1, nc2)]

  # Fix the cells that are already infected or removed
  X[1 <= x] <- 2
  class(X) <- c("SIRmatrix", class(X))
  X
}
