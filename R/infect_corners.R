infect_corners <- function(nrow = 20, ncol = 20){
  X <- matrix(0, nrow, ncol)
  X[1, 1] <- 1
  X[1, ncol] <- 1
  X[nrow, 1] <- 1
  X[nrow, ncol] <- 1
  class(X) <- c("SIRmatrix", class(X))
  X
}
