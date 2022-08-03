#' parse_last_names
#'
#' Takes submission meta data and returns student's last name
#'
#' Give a string with the student meta data, the last name of the student is returned
#'
#' @param submission string with submission data (name, student id)
#'
#' @return last name of student
#' @export
#'
#' @examples
#' \dontrun{parse_last_name(submission1)}
parse_last_names <- function(submission){
  # Parse last names


  # the right regex depends on the string to be parsed:
  # last_name <-
  #   stringr::str_to_lower(submission) %>%
  #   str_extract("^(\\S+\\s+\\S+?)_") %>%
  #   str_remove("_") %>%
  #   str_split(pattern = boundary("word")) %>%
  #   simplify() %>%
  #   tail(1)  # gets last element of string

  last_name <-
    stringr::str_to_lower(submission) %>%
    stringr::str_extract("(^.+)_(.+)_.*(\\d+)") %>%
    stringr::str_split(pattern = "_") %>%
    purrr::map_chr(~magrittr::extract(., 1))

  return(last_name)
}



#' parse_first_names
#'
#' Takes submission meta data and returns student's first name
#'
#' Give a string with the student meta data, the first name of the student is returned
#'
#' @param submission string with submission data (name, student id)
#'
#' @return first name of student
#' @export
#'
#' @examples
#' \dontrun{parse_first_names(submission1)}
parse_first_names <- function(submission){

  # Parse first names:

  # the right regex depends on the string to be parsed:
  # first_name <-
  #   stringr::str_to_lower(submission) %>%
  #   str_extract("^(\\S+\\s+\\S+?)_") %>%
  #   str_remove("_") %>%
  #   str_split(pattern = boundary("word")) %>%
  #   simplify() %>%
  #   head(1)  # gets first element of string

  first_name <-
    stringr::str_to_lower(submission) %>%
    stringr::str_extract("(^.+)_(.+)_.*(\\d+)") %>%
    stringr::str_split(pattern = "_") %>%
    purrr::map_chr(~magrittr::extract(., 2))
  return(first_name)
}



#' parse_matrikelnummer
#'
#' Takes submission meta data and returns student's matrikelnummer (ID number)
#'
#' Given a string with the student meta data, the matrikelnummer of the student is returned
#' It is assumed (per default) that the ID is a number of 8 digits
#'
#' @param submission string with submission data (name, student id)
#'
#' @return matrikelnummer of student
#' @export
#'
#' @examples
#' \dontrun{parse_matrikelnummer(submission1)}

parse_matrikelnummer <- function(submission, n_digits = 8) {

  matrikelnummer <-
    stringr::str_to_lower(submission) %>%
    stringr::str_extract("_\\d+_") %>%
    stringr::str_remove_all("_") %>%
    stringr::str_pad(width = n_digits, pad = "0", side = "left")

  # matrikelnummer <-
  #   stringr::str_to_lower(submission) %>%
  #   stringr::str_extract("(^.+)_(.+)_.*(\\d+)") %>%
  #   stringr::str_split(pattern = "_") %>%
  #   stringr::str_extract("\\d+") %>%
  #   stringr::str_pad(width = 8, pad = "0")

  return(matrikelnummer)

}

