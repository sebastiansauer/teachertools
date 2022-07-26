#' exam2yamlrmd
#'
#' Converts r-exam File to yaml-headed RMD file
#'
#' This function calls parse_examfile() and then write_yamlrmdfile().
#'
#' @param examfile input file in r-exam format
#' @param path_output output puath
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
#' \dontrun{exam2yamlrmd(ex_parsed, my_path)}

exam2yamlrmd <- function(examfile,
                         path_output,
                         ex_sol_str = c("Exercise", "Solution", "Categories"),
                         print_categories = FALSE,
                         separate_ex_sol = rep("</br>", 10),
                         verbose = TRUE,
                         header_level = 1){
  # This function reads a R/exams exercise file and converts it to a yaml-headed Rmd file.


  ex_parsed <- parse_examfile(examfile = examfile)



  write_yamlrmdfile(ex_parsed,
                    path_output,
                    ex_sol_str,
                    print_categories,
                    separate_ex_sol,
                    verbose = verbose,
                    header_level)
}



