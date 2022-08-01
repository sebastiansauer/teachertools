#' comp_error_submission
#'
#' Computes prediction error for student submissions
#'
#' Given a number of csv files with predictions (and id),
#' this function returns the prediction error (such as MAE or RMSE) for each prediction
#'
#'
#' @param path_to_submissions path  and file (CSV) with preditions (character)
#' @param verbose Print infos (lgl)?
#' @param error_fun which error fun to use (mae, rmse, ...), possibly from the tidymodels ecoverse
#' @param path_to_submissions path to submission folder with submission files (chr)
#' @param path_to_train_data  path to train data (chr)
#' @param path_to_test_data path to test data (Chr)
#' @param max_row how many rows should be prepared maximally (int)?
#' @param start_id number of the first id (int)
#' @param name_output_var name of the variable to be predicted (chr)
#' @param name_id_var name of the id variable (chr)?
#' @param name_pred_column name of the columns with the predictions (chr)?

#' @return tibble with prediction error value
#' @export
#'
#' @examples
#' \dontrun{comp_error_submissions(mypath)}
comp_error_submissions <- function(path_to_submissions = "Submissions/",
                                   verbose = TRUE,
                                   path_to_train_data,
                                   path_to_test_data,
                                   max_row = NULL,
                                   start_id = 1,
                                   name_output_var = "y",
                                   name_id_var = "id",
                                   name_pred_column = "pred",
                                   error_fun = mae) {



  if (verbose) cat("This is function `comp_error_submissions` speaking.\n")

  # Parse submissions:
  submissions <- list.files(path = path_to_submissions,
                            full.names = FALSE,
                            pattern = ".csv$|.CSV$|.Csv$|.CSv$|.csV$|.cSV$|.cSV$",
                            recursive = TRUE)

  Encoding(submissions) <- "utf8"

  if (verbose) cat(paste0("Number of CSV files to be processed: ", length(submissions), "\n"))

  # Make sure the paths are ending with a slash:
  if (!str_detect(path_to_submissions, "/$")) path_to_submissions <- str_c(path_to_submissions, "/")
  if (verbose) cat(paste0("Path to submissions is: ", path_to_submissions, "\n"))

  stopifnot(start_id > 0)




  # parse names and Matrikelnummer to df:
  d <-
    tibble::tibble(id = 1:length(submissions)) %>%
    dplyr::mutate(csv_file_name = stringr::str_conv(submissions, "utf8")) %>%
    dplyr::mutate(csv_file_name = berryFunctions::convertUmlaut(csv_file_name)) %>%
    dplyr::mutate(last_name = parse_last_names(csv_file_name),
           first_name = parse_first_names(csv_file_name)) %>%
    dplyr::mutate(matrikelnummer = parse_matrikelnummer(csv_file_name))


  # add nrow of preds to df:
  if (verbose) print("Now counting data lines per prediction data (submission) file.")
  d2 <-
    d %>%
    dplyr::mutate(npreds = purrr::map_dbl(submissions,
                            ~ R.utils::countLines(paste0(path_to_submissions, .x))) - 1)



  # add column names of pred data file to df:
  if (verbose) print("Now extracting col names from csv file with prediction data.")
  d2a <-
    d2 %>%
    dplyr::mutate(colnames_pred_file = purrr::map_chr(.x = submissions,
                                        .f = ~ data.table::fread(paste0(path_to_submissions, .x),
                                                                 nrows = 1) %>%
                                          names() %>%
                                          stringr::str_c(collapse = " - ")))


  #debug(prep_csv)
  # parse prediction data to df:
  if (verbose) print("Now starting to parse csv files with prediction data.")
  d3 <-
    d2a %>%
    #slice(1) %>%
    dplyr::mutate(data = purrr::map(
      .x = submissions,
      .f = ~ prep_csv(submission_file =  .x,
                      path_to_submissions = path_to_submissions,
                      path_to_test_data = path_to_test_data,
                      path_to_train_data = path_to_train_data,
                      max_row = max_row,
                      start_id = start_id,
                      name_output_var = name_output_var,
                      name_id_var = name_id_var,
                      name_pred_column = name_pred_column,
                      verbose = verbose)))

  if (verbose) print("Parsing of prediction data (submissions) completed.")

  if (verbose) print("Now computing test set error for each submission.")
  d4 <-
    d3 %>%
    dplyr::mutate(error_coef = purrr::map(data,
                                          ~error_fun(truth = y,
                                                     estimate = pred,
                                                     data = .x)))
  # set `error_fun <- mae` during debugging!

  options(scipen = 4)
  d5 <-
    d4 %>%
    dplyr::mutate(error_value = purrr::map_dbl(error_coef,
                                 ".estimate"))

  if (verbose) print("Computing test set error for each submission is now completed.")

  return(d5)
}
