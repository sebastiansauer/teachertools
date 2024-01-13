#' Create 2x2 data set from teaching data
#'
#' This function creates a two-by-two assocation type dataset
#' from the list of teaching data sets from this package
#'
#' If `data` is set to "random" a randomly chosen data set
#' fromt the list of available data sets is chosen.
#' Alternatively, one name can be specified.
#' See `teaching_data` for details.
#' Then, the outcome variable and the first predictors are
#' split at the median.
#'
#'
#' @param data the dataset the used
#'
#' @return Dataframe with two variables are factors,
#'   where the labels are "high" and "low"
# @export
#'
#' @examples
#' two_by_two_data <- two_by_two_association("random")
two_by_two_association <- function(data = "random") {

  all_dfs <- teachertools::teaching_df()

  d <- teachertools::teaching_df(data)

  d_name <- attr(d, "df_name")

  d_path <- all_dfs |>
    dplyr::filter(datasets_names == d_name) |>
    dplyr::pull(source)

  preds_chosen <- attr(d, "preds_chosen")
  focus_pred <- attr(d, "focus_pred")
  output_var <- attr(d, "output_var")

  d2 <-
    d |>
    dplyr::select(av, uv1) |>
    na.omit() |>
    dplyr::mutate(av_bin = factor(ifelse(av > median(av), "high", "low"))) |>
    dplyr::mutate(uv_bin = factor(ifelse(uv1 > median(uv1), "high", "low"))) |>
    dplyr::select(av_bin, uv_bin)

  return(d2)
}





