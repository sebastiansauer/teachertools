
#' lookup student id
#'
#' Look up a student id given the last name
#'
#' Looks up the student id given a CSV file and a last name.
#' Beware that double names may exist and hence cause problems.
#' All students (rows) with the matching last name are returned.
#' Note that the ID is returned as a character of length one (if it's a number),
#' in order to retain leading zeros of the ID. Columns are parsed as characters.
#' To that end, zeros are padded to the left side.
#'
#'
#' @param csv_file Path to file with IDs (not a tibble) and names of students (character)
#' @param lastname One word indicating the last name (character)
#' @param verbose More output? (lgl)
#' @param fun which reading fun should be used? Defaults to rio::import
#' @param pad_to to how many digits should the ID be padded with zeros (from the left side)? Defaults to 8 (integer)
#' @param strict if TRUE, NAs are returned in case of multiple (ambiguous) results
#'
#' @return student id (chracter)
#' @export
#'
#' @examples
#' studentilist_path <- "/Users/sebastiansaueruser/Google Drive/ORGA-HS-Ansbach/22-WS/studentis-awm-2022.csv"
#' studentilist <- read.csv(studentilist)
#' lookup_studentid(studentilist, "Maier")
#'
#'
lookup_studentid <- function(d, lastname, firstname = NULL,
                             verbose = FALSE,  pad_to = 8L,
                             id_col = "matrikelnr",
                             strict = FALSE) {



  out <- d

  d$id <- stringr::str_pad(as.character(out[[id_col]]), pad_to, side = "left", pad = "0")

  out2 <- out[out$lastname == lastname, ]

  if (!is.null(firstname)) out2 <- out2[out2$firstname == firstname, ]

  out3 <- out2[[id_col]]

  if (length(out3) == 0) {
    warning("Name not found. Returning NA!")
    out3 <- NA
  }
  if (length(out3) > 1) {
    warning("More than a single name matching!")
    print(out2)
    if (strict) out3 <- NA
  }


  # if (verbose) print(out3)

  return(out3)
}

