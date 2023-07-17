#' Canonicalize a data frame
#'
#' @description Converts a data frame to a standard form with 1 output variable
#'     and two predictors, all with standard names (av, uv1, uv2).
#'     Currently, only 2 predictors are supported.
#'
#' @param df Data frame to be canonicalized (df)
#' @param output_var the dependent variable of a (possibly) intended model (string)
#' @param keep_only_numeric should only numeric variables be kept? (lgl), defaults to TRUE
#'
#' @return data frame canonicalized
#' @export
#'
#' @examples
#' \dontrun{canonicalize_df(my_df, my_outputvar)}
canonicalize_df <- function(df, output_var, keep_only_numeric = TRUE, verbose = TRUE) {

  assertthat::assert_that(typeof(output_var) == "character", msg = "output var must be of type character")

  # hacks:
  if ("sleep_total" %in% names(df)) df <- df |>  dplyr::select(-sleep_total)

  # rm ID cols:
  cols_wo_id <- setdiff(names(df), c("X", "...1", "ID", "id"))  # exclude ID vars
  df <- df |> dplyr::select(tidyselect::all_of(cols_wo_id))

  # rm highly correlated cols:
  col_numeric_names <- df |> dplyr::select(tidyselect::where(is.numeric)) |> names()
  col_numeric_names_flat <- stringr::str_flatten(col_numeric_names, collapse = ", ")

  redundant_cols <- caret::findCorrelation(cor(df[col_numeric_names],
                                        use = "complete.obs"),
                                    cutoff = .9,
                                    names = TRUE)

  df <- df |> dplyr::select(-tidyselect::all_of(redundant_cols))

  # keep only numeric, if desired:
  if (keep_only_numeric) {
    # get names of numeric cols:
    df <- df |> dplyr::select(tidyselect::where(is.numeric))
  }


  vars <- names(df)


  preds <- setdiff(vars, output_var)
  assertthat::assert_that(length(preds) >= 2, msg = glue::glue("There must be at least two predictors. I found only {preds}"))
  assertthat::assert_that(ncol(df) >= 3, msg = glue::glue("There must be at least 3 columns. I found these: {col_numeric_names_flat}."))
  assertthat::assert_that(output_var %in% vars, msg = glue::glue("The output variable, {output_var}, is not among the column names."))

  k <- 2

  preds_chosen_nr <- sample(1:length(preds), k)
  preds_chosen <- preds[preds_chosen_nr] # string
  pred_chosen1 <- preds_chosen[1]
  pred_chosen2 <- preds_chosen[2]
  focus_pred <- pred_chosen1

  if (verbose) message(paste0("Predictors chosen: ", stringr::str_c(preds_chosen, collapse = ", ")))
  if (verbose) message(paste0("Output variable chosen: ", output_var))

  assertthat::assert_that(typeof(preds_chosen) == "character", msg = "preds_chosen must be of type character.")
  assertthat::assert_that(is.null(df$output_var), msg = "Output variable does not exist in df.")
  assertthat::assert_that(output_var != "X", msg = "outpur_var must not be named X.")
  assertthat::assert_that(pred_chosen1 != "X", msg = "uv1 must not be named X.")
  assertthat::assert_that(pred_chosen1 != "X", msg = "uv1 must not be named X.")

  out <-
    df |>
    dplyr::rename(
      av = {{output_var}},
      uv1 = {{pred_chosen1}},
      uv2 = {{pred_chosen2}}
    ) |>
    dplyr::select(av, uv1, uv2)

  attr(out, "output_var_canonicalized") <- "av"
  attr(out, "predictors_canonicalized") <- c("uv1", "uv2")
  attr(out, "predictors_n") <- 2
  attr(out, "pred_chosen1") <- pred_chosen1
  attr(out, "pred_chosen2") <- pred_chosen2
  attr(out, "preds_chosen") <- preds_chosen
  attr(out, "focus_pred") <- focus_pred
  attr(out, "canonicalized") <- TRUE
  return(out)
}
