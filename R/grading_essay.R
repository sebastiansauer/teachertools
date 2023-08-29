#' grade essay HS Ansbach
#'
#' takes a dataframe with 4 columns and grades it
#'
#' Expects (at least) the following columns: Methodik, Formales, Inhalt, Bonus
#' (num/num/num/chr).
#' Methodik, Formales, Inhalt are the 3 criteria to which the essay is graded.
#' Bonus determines whether the student is awarded some bonus.
#' ATTENTION: This function is hardcoded to suit the HS Ansbach grading method!
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
grade_essay_hsan <- function(d) {
#  out <- ## Implzit return used!
    d |> # parse numbers:
    dplyr::mutate(
      dplyr::across(
        .cols = c(Formales, Methodik, Inhalt),
        .fns = ~ readr::parse_number(
            ., locale = readr::locale(decimal_mark = ",")
        )
      )
    ) |> # compute rowwise average:
    dplyr::rowwise() |>
    dplyr::mutate(
      grade_avg = base::mean(c(Formales, Methodik, Inhalt))
    ) |>
    dplyr::ungroup() |> # add bonus:
    dplyr::mutate(
      grade_avg2 = dplyr::case_when(
        Bonus == "ja" ~ grade_avg - 0.33,
        TRUE ~ grade_avg)) |> # correct for "fail anyway"
    dplyr::mutate(
      grade_avg3 = ifelse(Formales > 4 | Methodik > 4 | Inhalt > 4,
                          5,
                          grade_avg2)
    ) |> # round according to rounding scheme:
    dplyr::mutate(
      grade_avg4 = round_grades(grade_avg3)
    ) |> # return numeric vector:
    dplyr::pull(grade_avg4)
}
