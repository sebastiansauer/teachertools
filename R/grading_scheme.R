#' Grading scheme
#'
#' Returns various grading schemes
#'
#' @param scheme_type which grading scheme (integer, defaults to 1)
#' @param output_type scheme in percentage points or as proportion, choose either "percentage" or "proportion")
#'
#' @return numeric named vector of varying length depending on type
#' @export
#'
#' @examples
#' grading_scheme()
#' 5   4 3.7 3.3 3.0 2.7 2.3 2.0 1.7 1.3 1.0
#' 0  50  55  60  65  70  75  80  85  90  95
grading_scheme <- function(scheme_type = 1, output_type = "proportion") {

  stopifnot(output_type %in% c("percentage", "proportion"))

  if (scheme_type == 1) scheme <- c("5" = 0,
                                     "4" = 50,
                                     "3.7" = 55,
                                     "3.3" = 60,
                                     "3.0" = 65,
                                     "2.7" = 70,
                                     "2.3" = 75,
                                     "2.0" = 80,
                                     "1.7" = 85,
                                     "1.3" = 90,
                                     "1.0" = 95)

  if (scheme_type == 2) scheme <- c("5" = 0,
                                     "4" = 40,
                                     "3.7" = 46,
                                     "3.3" = 52,
                                     "3.0" = 58,
                                     "2.7" = 64,
                                     "2.3" = 70,
                                     "2.0" = 76,
                                     "1.7" = 82,
                                     "1.3" = 88,
                                     "1.0" = 94)

  if (scheme_type == 3) scheme <- c("5" = 0,
                             "4" = 50,
                             # "3.7" = 55,
                             # "3.3" = 60,
                             "3.0" = 65,
                             # "2.7" = 70,
                             # "2.3" = 75,
                             "2.0" = 80,
                             # "1.7" = 85,
                             # "1.3" = 90,
                             "1.0" = 95)


  if (scheme_type == 4) scheme <- c("5" = 0,
                             "4" = 40,
                             # "3.7" = 46,
                             # "3.3" = 52,
                             "3.0" = 58,
                             # "2.7" = 64,
                             # "2.3" = 70,
                             "2.0" = 76,
                             # "1.7" = 82,
                             # "1.3" = 88,
                             "1.0" = 94)

  if (scheme_type == 5) scheme <- c("5" = 0,
                             "4" = 45,
                             "3.7" = 50,
                             "3.3" = 55,
                             "3.0" = 60,
                             "2.7" = 66,
                             "2.3" = 70,
                             "2.0" = 75,
                             "1.7" = 80,
                             "1.3" = 85,
                             "1.0" = 90)

  if (scheme_type == 5) scheme <- c("5" = 0,
                                    "4" = 40,
                                    "3.7" = 46,
                                    "3.3" = 51,
                                    "3.0" = 56,
                                    "2.7" = 73,
                                    "2.3" = 78,
                                    "2.0" = 86,
                                    "1.7" = 92,
                                    "1.3" = 98,
                                    "1.0" = 90)

  if (output_type == "proportion") scheme <- scheme / 100

  return(scheme)
}
