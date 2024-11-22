infection_step <- function(X, prob){
  # infecting upwards (above)   -------------------------------------------------------
  # boolean matrix of those who have had close contact with the cell above
  cc_above <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                     size = (nrow(X) - 1) * ncol(X), replace = TRUE)
  cc_above <- matrix(cc_above, nrow = nrow(X) - 1, ncol = ncol(X))
  # boolean matrix of a cell that had close contact with an **infected** cell
  new_infection_above <- X[-1, ] == 1 & cc_above
  # applying the new infected cells to the original matrix
  Xabove <- X
  Xabove[-nrow(X) , ][new_infection_above] <- 1


  # infecting downwards (below) -------------------------------------------------------
  cc_below <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                     size = (nrow(X) - 1) * ncol(X), replace = TRUE)
  cc_below <- matrix(cc_below, nrow = nrow(X) - 1, ncol = ncol(X))
  new_infection_below <- X[-nrow(X), ] == 1 & cc_below
  Xbelow <- X
  Xbelow[-1 , ][new_infection_below] <- 1

  # infecting to the right      -------------------------------------------------------
  cc_right <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                     size = nrow(X) * (ncol(X) - 1), replace = TRUE)
  cc_right <- matrix(cc_right, nrow = nrow(X), ncol = ncol(X) - 1)
  new_infection_right <- X[ , -ncol(X)] == 1 & cc_right
  Xright <- X
  Xright[ , -1][new_infection_right] <- 1

  # infecting to the left       -------------------------------------------------------
  cc_left <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                    size = nrow(X) * (ncol(X) - 1), replace = TRUE)
  cc_left <- matrix(cc_left, nrow = nrow(X), ncol = ncol(X) - 1)
  new_infection_left <- X[ , -1] == 1 & cc_left
  Xleft <- X
  Xleft[ , -ncol(X)][new_infection_left] <- 1

  # infecting northeast         -------------------------------------------------------
  cc_northeast <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                         size = (nrow(X) - 1) * (ncol(X) - 1), replace = TRUE)
  cc_northeast <- matrix(cc_northeast, nrow = nrow(X) - 1, ncol = ncol(X) - 1)
  new_infection_northeast <- X[-1, -ncol(X)] == 1 & cc_northeast
  Xnortheast <- X
  Xnortheast[-nrow(X) , -1][new_infection_northeast] <- 1

  # infecting southeast         -------------------------------------------------------
  cc_southeast <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                         size = (nrow(X) - 1) * (ncol(X) - 1), replace = TRUE)
  cc_southeast <- matrix(cc_southeast, nrow = nrow(X) - 1, ncol = ncol(X) - 1)
  new_infection_southeast <- X[-nrow(X), -ncol(X)] == 1 & cc_southeast
  Xsoutheast <- X
  Xsoutheast[-1 , -1][new_infection_southeast] <- 1

  # infecting northwest         -------------------------------------------------------
  cc_northwest <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                         size = (nrow(X) - 1) * (ncol(X) - 1), replace = TRUE)
  cc_northwest <- matrix(cc_northwest, nrow = nrow(X) - 1, ncol = ncol(X) - 1)
  new_infection_northwest <- X[-1, -1] == 1 & cc_northwest
  Xnorthwest <- X
  Xnorthwest[-nrow(X) , -ncol(X)][new_infection_northwest] <- 1

  # infecting southwest         -------------------------------------------------------
  cc_southwest <- sample(c(TRUE, FALSE), prob = c(prob, 1 - prob),
                         size = (nrow(X) - 1) * (ncol(X) - 1), replace = TRUE)
  cc_southwest <- matrix(cc_southwest, nrow = nrow(X) - 1, ncol = ncol(X) - 1)
  new_infection_southwest <- X[-nrow(X), -1] == 1 & cc_southwest
  Xsouthwest <- X
  Xsouthwest[-1 , -ncol(X)][new_infection_southwest] <- 1

  # combine all matrices by addition
  directions <- list(Xbelow, Xabove, Xright, Xleft,
                     Xnortheast, Xsoutheast, Xnorthwest, Xsouthwest)
  newX <- 0
  for(direction in directions){
    newX <- newX + direction
  }

  # fix boundaries of values, return matrix
  newX[newX >= 2] <- 1 # multiple infected neighbors could've infected the same cell again
  newX[X == 1] <- 2
  class(newX) <- c("SIRmatrix", class(newX))
  newX
}
