#' Infecting the corners of a given matrix
#'
#' @param nrow An integer indicating number of rows of expected matrix
#' @param ncol An integer indicating number of columns of expected matrix
#'
#' @return A matrix of special class: SIRmatrix
#' @export
#'
#' @examples
#' infect_corners()
#'
#' infect_corners(nrow = 50, ncol = 50)
infect_corners <- function(nrow = 20, ncol = 20){
  X <- matrix(0, nrow, ncol)
  X[1, 1] <- 1
  X[1, ncol] <- 1
  X[nrow, 1] <- 1
  X[nrow, ncol] <- 1
  class(X) <- c("SIRmatrix", class(X))
  X
}
