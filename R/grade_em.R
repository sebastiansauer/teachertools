
#' Grade em
#'
#' This function assigns grades
#'
#' @param x student's percentage points (one row per student)
#' @param breaks thresholds for a given grade (vector of length 12)
#' @param reverse reverse grading scheme (lgl)?
#'
#' @return grades per student
#' @export
#'
#' @examples
#' \dontrun{grade_em(thresholds, values)}


grade_em <- function(x, breaks12, reverse = FALSE){
  # input: 9 threshold values, giving the grade  thresholds 4 to 1.3


  grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)  # length 11

  if (reverse) grades_scheme <- rev(grades_scheme)

  grades <- cut(x, breaks = breaks12, labels = grades_scheme)
}


