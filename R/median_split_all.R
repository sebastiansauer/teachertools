
#' Median split for all variables
#'
#' Splits all variables at the median.
#' Expects numeric variables only.
#' Split variables are factors with the levels "high" and "low".
#'
#' @param data
#'
#' @return dataframe with median splitted variables as factors
#' @export
#'
#' @examples
#' median_split_all(mtcars)
median_split_all <- function(data) {

  out <-
    data |>
    dplyr::mutate(dplyr::across(tidyr::everything(), ~ ifelse(. < median(.), "high", "low"))) |>
    dplyr::mutate(dplyr::across(tidyr::everything(), factor))

  return(out)
}
