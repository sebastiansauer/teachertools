# helper functions:






#' Grade em
#'
#' This function assigns grades
#'
#' @param values student's percentage points (one row per student)
#' @param thresholds thresholds for a given grade
#'
#' @return grades per student
#' @export
#'
#' @examples
#' \dontrun{grade_em(thresholds, values)}
grade_em <- function(thresholds, values){

  grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)

  grades <- cut(values, breaks = thresholds, labels = grades_scheme)
}

