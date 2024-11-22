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
