

#' HS Ansbach rounding scheme for grades
#'
#' At HS Ansbach the grading scheme is like 1, 1.3, 1.7, 2.0, etc. (but no "1.5" etc)
#'
#'
#'
#' @return tibble
#' @export
#'
#' @examples `ansbach_round_scheme()`
#'
#'
ansbach_round_scheme <- function(){


  rounding_scheme <-
    tibble::tribble(
      ~id, ~breaks, ~grade,
      1L,    1.15,      1,
      2L,     1.5,    1.3,
      3L,    1.85,    1.7,
      4L,    2.15,      2,
      5L,     2.5,    2.3,
      6L,    2.85,    2.7,
      7L,    3.15,      3,
      8L,     3.5,    3.3,
      9L,    3.85,    3.7,
      10L,    4.15,      4,
      11L,    Inf,       5
    )

  return(rounding_scheme)

}
