summary.SIRmatrix <- function(object, ...){
  list(susceptible = mean(object == 0),
       infected = mean(object == 1),
       removed = mean(object == 2)
       )
}
