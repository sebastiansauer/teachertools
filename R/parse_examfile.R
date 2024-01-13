#' parse_examfile
#'
#' Parse r-exams exercise file
#'
#' Parses the following parts of an r-exam file: 1. metadata, 2. pre-question part,
#' 3. question, 4. solution. The metadata will be converted to regular yaml.
#'
#'
#' @param examfile path to r-exam exercise file. Can be remote or local.
#' @param verbose Print additional information?
#'
#' @return list with the parts of the exercise file

#'
#' @examples
#' \dontrun{parse_examfile(myexamfile)}
#' library(teachertools)
#' examfile_name <- "/Users/sebastiansaueruser/github-repos/rexams-exercises/exercises/sebastiansauer/de/Bayes/pigs2.Rmd"
#'
#' fileparsed <- parse_examfile(examfile_name)
#' fileparsed



parse_examfile <- function(examfile,
                           verbose = FALSE) {


  #stopifnot(file.exists(examfile))
  #ex_str <- readLines(examfile)
  ex_str <- readr::read_lines(examfile)


  # get pre-question:

  start_pos <- 1
  end_pos <- which(ex_str %in% c("Question", "Aufgabe"))
  stopifnot(end_pos > 1)
  ex_pre_question <-
    ex_str[1:(end_pos-1)]  # ends when 'Question' as new header appears


  # get question:

  start_pos <-  which(ex_str %in% c("Question", "Aufgabe"))
  end_pos <- which(ex_str %in% c("Solution", "Lösung"))
  if (end_pos < start_pos + 2) stop("Did not find Question and Solution tags properly.")
  ex_question <-
    ex_str[(start_pos+3):(end_pos-1)]



  # get solution:

  start_pos <- which(ex_str == "Solution")
  end_pos <- which(ex_str == "Meta-information")
  ex_solution <-
    ex_str[(start_pos+3):(end_pos-1)]

  # get metadata:
  start_pos <- which(ex_str == "Meta-information")

  ex_metadata <-
    ex_str[start_pos:length(ex_str)] |>
    magrittr::extract(-c(1,2))

  title <- stringr::str_extract(examfile, "([^/]+$)") |>  # The regular expression "([^/]+$)" is used to match and capture text at the end of a string that does not contain a forward slash ("/")
    stringr::str_remove("\\..{2,3}$")  # removes file suffix, assumes at least 1 character after dot, eg., "file.rmd" or "file.qmd"

  # remove backticks from metadata, as it cannot be parsed:
  ex_metadata_clean <- stringr::str_remove_all(ex_metadata, "`")
  ex_metadata_yaml <- yaml::yaml.load(ex_metadata_clean)

  ex_metadata_yaml$date <- as.character(Sys.Date())
  ex_metadata_yaml$slug <- title
  ex_metadata_yaml$title <- title


  # we need at least 2 values, otherwise yaml won't take it as a vector, which will then in turn not work in blogdown:
  if (length(ex_metadata_yaml$tags) == 1) ex_metadata_yaml$tags <-
    c(ex_metadata_yaml$tags, "stats")
  if (length(ex_metadata_yaml$categories) == 1) ex_metadata_yaml$categories <-
    c(ex_metadata_yaml$categories, format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))

  # add extype (eg., "numeric") as category:
  ex_metadata_yaml$categories <- c(ex_metadata_yaml$categories, ex_metadata_yaml$extype)


  examfile_info <-
    list(
      ex_pre_question = ex_pre_question,
      ex_question = ex_question,
      ex_solution = ex_solution,
      ex_metadata = yaml::as.yaml(ex_metadata_yaml),
      ex_metadata_yaml = ex_metadata_yaml)

  return(examfile_info)

  if (verbose) cat("Exam exercise file has been parsed.\n")

}
