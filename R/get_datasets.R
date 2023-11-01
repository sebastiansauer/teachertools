

#' Get Datasets from Vincent Arel Bundock's page
#'
#' Downloads all data frames from a source page (default: incent Arel Bundock's page),
#' according to a filter (i.e., a package to filter for)
#'
#' @param d_path CSV file with data set information
#' @param d_filter Name of Package for filter for
#'
#' @return data frame with data set information
#' @export
#'
#' @examples
out <- get_datasets(d_filter = "openintro")

get_datasets <- function(d_path = NULL, d_filter = NULL) {

  if (is.null(d_path)) d_path <- "https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/datasets.csv"

  d <- read.csv(d_path)

  if (is.null(d_filter)) {
    d_out <- d
  } else {
    d_out <-
      d |>
      dplyr::filter(Package == {{d_filter}})
  }

  return(d_out)
}

