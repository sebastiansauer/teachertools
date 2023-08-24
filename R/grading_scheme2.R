#' Grading scheme 2
#'
#' Returns various grading schemes
#'
#' @param min_pass minimal percentage points to pass the exam
#' @param output_type scheme in percentage points or as proportion, choose either "percentage" or "proportion")
#'
#' @return named numeric vector of length 12
#' @export
#'
#' @examples
#' grading_scheme2()
#'5    4  3.7  3.3  3.0  2.7  2.3  2.0  1.7  1.3  1.0
#'0.00 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95
grading_scheme2 <- function(min_pass = 50, output_type = "proportion") {

  stopifnot(output_type %in% c("percentage", "proportion"))

  grade_names <- c("5" , "4", "3.7", "3.3" ,"3.0", "2.7","2.3" , "2.0",  "1.7" , "1.3", "1.0")

  grade_in_points <- (100 - min_pass) / 10

  grade_thresholds <- c(0, min_pass + 0:9 * grade_in_points)

  names(grade_thresholds) <- grade_names

  if (output_type == "proportion") grade_thresholds <- grade_thresholds / 100

  return(grade_thresholds)
}
