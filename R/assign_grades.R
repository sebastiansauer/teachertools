#' Assign Grades
#'
#' Given a variable with points, percentage or other, a grade is
#' assigned given a grading scheme which acts as a function ascribing the
#' mapping from proints to grade
#'
#' @param d data frame
#' @param grading_scheme which grading scheme to use
#' @param var variable for which a grade should be assigned
#' @param failing_grades grades when student is failing
#' @param number_of_grade number of grades that should be in the grading_scheme
#'
#' @return data frame with grades (column) added
#' @export
#'
#' @examples
#'assign_grade(mtcars, var = "am")
#'
assign_grade <- function(
    d,
    grading_scheme = NULL,
    var = "correct_prop",
    failing_grades= c("5", "4.3", "4.7"),
    number_of_grades = 11
    ) {

  if (is.null(grading_scheme)) {
    grading_scheme <- c("5"   = 0,
                        "4"   = .45,
                        "3.7" = .50,
                        "3.3" = .55,
                        "3.0" = .60,
                        "2.7" = .66,
                        "2.3" = .70,
                        "2.0" = .75,
                        "1.7" = .80,
                        "1.3" = .85,
                        "1.0" = .90)
  }
  stopifnot(length(grading_scheme) == number_of_grades)

  d$grade_f <- cut(
    d[[var]],
    breaks = c(grading_scheme, 1.01),
    labels = names(grading_scheme),
    right = FALSE)

  # pass, if not in the failing grades!
  d$pass <- !(d$grade_f %in% failing_grades)

  d$grade <- as.numeric(as.character(d$grade_f))

  return(d)

}
