
#' Grade em
#'
#' This function assigns grades
#'
#' If no grade scheme is given, the function uses it own grade scheme,
#' which is printed when the argument `verbose` is `TRUE`.
#' The number of thresholds ("fence posts") must be equal the number of grades ("fence ways") plus 1.
#'
#' @param x student's prediction error value, such as MAE or RMSE (numeric)
#' @param thresholds thresholds for a given grade (numeric vector of length 12)
#' @param reverse reverse grading scheme (lgl)?
#' @param grades_scheme Grades scheme, eg., from 5.0 to 1.0
#'
#' @return grades per student
#' @export
#'
#' @examples
#' \dontrun{grade_em(thresholds, values)}


grade_em <- function(x,
                     thresholds,
                     reverse = FALSE,
                     grades_scheme = NULL,
                     verbose = FALSE){


  if (is.null(grades_scheme))
    grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)  # length 11
  if (verbose) {
    cat("Grading scheme used:\n")
    cat(grades_scheme)
    cat("\n")
  }

  if (reverse) grades_scheme <- rev(grades_scheme)

  grades <- cut(x, breaks = thresholds, labels = grades_scheme)
}


