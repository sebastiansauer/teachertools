
#' try_readcsv
#'
#' tries to rad a submission csv file
#'
#' @param file path to a csv file to be read (chr)
#' @param verbose more output to be printed (lgl)?
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{try_readcsv(my_submission_file)}
try_readcsv <- function(file, verbose = FALSE) {

  # import csv file:
  x <- data.table::fread(file, header = TRUE)

  # if more than 2 columns, only select first and last one:
  if (ncol(x) > 2) {
    x <- x %>%
      dplyr::select(1, tidyselect::last_col())
  }

  if (ncol(x) == 1) stop("only 1 column!")

  names(x) <- c("id", "pred")

  x <-
    x %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                  .fns = ~ stringr::str_replace_all(., pattern = ",",
                                           replacement = ".")))

  if (verbose == TRUE) {print("Ncol: "); print(ncol(x))}
  return(x)

}
