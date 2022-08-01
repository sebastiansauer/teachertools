

#' get_grade_scheme
#'
#' Get grade scheme as numeric vector
#'
#' Returns one (of possibly several) grading scheme
#'
#' @param scheme which scheme (int)? Currently only 1 scheme is implemented
#'
#' @return scheme (numeric)
#' @export
#'
#' @examples
#' \dontrun{get_grade_scheme()}
get_grade_scheme <- function(scheme = 1){

  if (scheme == 1) grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)
}
