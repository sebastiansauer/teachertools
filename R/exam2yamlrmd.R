#' exam2yamlrmd
#'
#' Converts r-exam File to yaml-headed RMD file
#'
#' This function calls parse_examfile() using `examfile` as input and based on the output it calls then write_yamlrmdfile().
#'
#' @param examfile path to input file in r-exam format (string)
#' @param path_output output path (string)
#' @param ex_sol_str wording for the parts of the exercise (useful for translation purposes)
#'
#' @param print_categories should the yaml field 'categories' be printed?
#' @param separate_ex_sol How should the question and solution be visually separated?
#' @param verbose talkative, yes or no? (TRUE or FALSE)
#' @param more_yaml more yaml to add which was not given in the r-exam file
#' @param header_level how many '#' should be padded in order to indicate the header leve?
#'
#' @return writes an rmd file
#' @export
#'
#' @examples
#' \dontrun{exam2yamlrmd(examfile, path_output)}
#' examfile_name <- "/Users/sebastiansaueruser/github-repos/rexams-exercises/exercises/sebastiansauer/de/Bayes/pigs2.Rmd"
#' out <- exam2yamlrmd(
#'  examfile = examfile_name,
#'  path_output = "/Users/sebastiansaueruser/Downloads")
#'
#'
#'
#'
#'

exam2yamlrmd <- function(examfile,
                         path_output = "",
                         ex_sol_str = c("Aufgabe", "Lösung", "Categories"),
                         print_categories = FALSE,
                         separate_ex_sol = rep("</br>", 10),
                         more_yaml = NULL,
                         verbose = TRUE,
                         header_level = 1){
  # This function reads a R/exams exercise file and converts it to a yaml-headed Rmd file.

  # step 1: parse the exam file
  ex_parsed <- parse_examfile(examfile = examfile)


  # step 2: write the rendered file to disk:
  out <- write_yamlrmdfile(ex_parsed,
                    path_output,
                    ex_sol_str,
                    more_yaml = more_yaml,
                    print_categories,
                    separate_ex_sol,
                    verbose = verbose,
                    header_level)

  return(out)
}



