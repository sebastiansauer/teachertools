

#' Round HS Ansbach
#'
#' Round grades (1-5) according to HS Ansbach grades
#'
#' @param x grade vom 1.0 to 5.0
#'
#' @return vector (factor) with rounded grades
#' @export
#'
#' @examples `round_grades(x = c(1, 1.2, 1.5, 1.9, 4.1, 4.4))`
#'
#'
round_grades <- function(x, rounding_scheme = ansbach_round_scheme()) {

  rounding_scheme_vec <- rounding_scheme$breaks
  names(rounding_scheme_vec) <- rounding_scheme$grade

  grade_f <- cut(x, breaks = c(0, rounding_scheme_vec),
                 labels = names(rounding_scheme_vec),
                 right = FALSE)

  return(grade_f)
}
