#' Summary statistics for a given SIRmatrix
#'
#' @param object A matrix of class SIRmatrix.
#' @param ... Any other summary statistic parameters you'd want to include.
#'
#' @return A list continained all summary statistics.
#' @export
#'
#' @examples
#' summary(infect_corners())
#'
#' summary(random_infection())
summary.SIRmatrix <- function(object, ...){
  list(susceptible = mean(object == 0),
       infected = mean(object == 1),
       removed = mean(object == 2)
       )
}
