#' Wrapper plotting function for class SIRmatrix
#'
#' @param x A matrix of 0's, 1's, and/or 2's.
#' @param title A string for the title of the plot.
#' @param ... Any additional parameters that will be passed onto image().
#'
#' @return A generated plot using image()
#' @export
#'
#' @examples
#' plot(infect_corners())
#'
#' plot(random_infection(), title = "Initial Random Infection")
plot.SIRmatrix <- function(x, title = "Infection", ...){
  if (all(x == 2)){
    newX <- apply(x, 2, rev)
    graphics::image(t(newX), col = c("gray"), axes = FALSE, asp = 1, ...)
    title(main = title)
  } else if (any(x == 2)){
    newX <- apply(x, 2, rev)
    graphics::image(t(newX), col = c("white", "red", "gray"), axes = FALSE, asp = 1, ...)
    title(main = title)
  } else {
    newX <- apply(x, 2, rev)
    graphics::image(t(newX), col = c("white", "red"), axes = FALSE, asp = 1, ...)
    title(main = title)
  }
}
