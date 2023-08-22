



#' grade essay
#'
#' takes a dataframe with 4 columns and grades it
#'
#' Expects the followings columns: Methodik, Formales, Inhalt, Bonus
#' (num/num/num/chr).
#'
#'
#' @param d data frame with raw grading data
#'
#' @return vector (num) with graded data
#' @importFrom dplyr ungroup
#' @export
#'
#' @examples `grade_essay(d)`
#'
grade_essay <- function(d) {



  # parse numbers:
  out <-
    d |>
    # parse numbers:
    dplyr::mutate(dplyr::across(.cols = c(Formales, Methodik, Inhalt),
                                .fns = ~ readr::parse_number(
                                  ., locale = readr::locale(decimal_mark = ",")))) |>
    # compute rowwise average:
    dplyr::rowwise() |>
    dplyr::mutate(grade_avg = base::mean(c(Formales, Methodik, Inhalt))) |>
    dplyr::ungroup() |>

    # add bonus:
    dplyr::mutate(grade_avg2 =
                    dplyr::case_when(
                      Bonus == "ja" ~ grade_avg - 0.33,
                      TRUE ~ grade_avg)) |>

    # correct for "fail anyway"
    dplyr::mutate(grade_avg3 = ifelse(Formales > 4 | Methodik > 4 | Inhalt > 4, 5, grade_avg2)) |>

    # round according to rounding scheme:
    dplyr::mutate(grade_avg4 = round_grades(grade_avg3)) |>

    # return numeric vector:
    dplyr::pull(grade_avg4)



}
