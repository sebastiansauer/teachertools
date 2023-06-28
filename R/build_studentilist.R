


#' Build tidy Studenti list
#'
#' This function takes a primuss export consisting of names, matrikelnummers and email adresses
#' of students. Then a more well-behaved csv file is constructed.
#'
#' @param inputfile input xlsx file
#' @param outputfile output csv file
#'
#' @return tidy data frame with studenti names etc.
#' @export
#'
#' @examples
#' inputfile <- "/Users/sebastiansaueruser/Google Drive/ORGA-HS-Ansbach/22-WS/AWM Studierende_Email & MATR_ Stand 24.01.23.xlsx"
#' outputfile <- "/Users/sebastiansaueruser/Google Drive/ORGA-HS-Ansbach/22-WS/studentis-awm-2022.csv"
#' build_studentilist(inputfile, outputfile)
build_studentilist <- function(inputfile, outputfile) {

  studis <- rio::import(inputfile)

  studis2 <-
    studis |>
    tidyr::separate_wider_delim(
      name_raw, delim = ", ", names = c("lastname", "firstname")) |>
    dplyr::select(-stg) |>
    dplyr::mutate(name_primuss =
                    stringr::str_c(lastname, firstname, sep = " ") |>
                    stringr::str_sub(start = 1L, end = 24L) |>
                    stringr::str_squish()
                  )

  readr::write_csv(studis2, outputfile)

  return(studis2)

}



