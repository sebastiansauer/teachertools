#' lookup_name
#'
#' Looks up the name of a student given an ID (Matrikelnummer)
#'
#' Given a csv file and a student id (Matrikelnummer),
#' the name of the person (student) is returend
#'
#' @param csv_file file with ids and names of students
#' @param student_id unique ID (Matrikelnummer)
#'
#' @return name of student
#' @export
#'
#' @examples
#' \dontrun{lookup_name(my_students, "12345678")}
lookup_name <- function(csv_file, student_id){

  require(tidyverse)

  d <- readr::read_csv(csv_file)

  out <-
    d %>%
    dplyr::filter(id == {{student_id}}) %>%
    select(id)

}

