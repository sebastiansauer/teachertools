
parse_names_raw <- function(paths) {
#' parse students names returns names and matrikelnummers of raw submissions files
#' @param name description path path to submissions raw file from moodle
#' @return sanitized data frame of individual submission. See details.
#' @export
#'


  path_subm_files <- get_and_print_sub_files(paths)

  subm_names <- get_subm_names(path_subm_files)

  subm_names <- add_matrikelnumbers(subm_names,
                                    get_matrikelnumbers(path_subm_files))

}




get_and_print_sub_files <- function(paths) {
  path_subm_files <- list.files(path = paths$subm_raw,
                                full.names = FALSE,
                                pattern = paths$csv_pattern,
                                recursive = TRUE)
  path_subm_files
}



get_subm_names <- function(files) {
  subm_names <-
    files %>%
    stringr::str_to_lower() |>
    stringr::str_match("(\\w+-?\\w*) (\\w+)(?:_\\d)") |>   # First name possibly with dash then lastname but do not capture "underscore some digits" afterwards
    tibble::as_tibble(.name_repair = "unique")

  names(subm_names) <- c("name-raw", "firstname", "lastname")

}

get_matrikelnumbers <- function(files) {
  files %>%
    stringr::str_remove(".*/") |>  # rm all path until last slash
    stringr::str_remove("\\.csv$") |>  # rm file type
    stringr::str_extract("\\d+")
}


add_matrikelnumbers <- function(names, matrikelnr) {
  names |>
    dplyr::mutate(matrikelnummer = matrikelnr)
}

