#' write_yamlrmdfile
#'
#' Takes parts of r-exam files and writes into a yaml rmd file
#'
#' Take the parts of r-exam files which must be handed over
#' by the function parse_examfile() and then writes rmd file with yaml header.
#'
#' @param ex_parsed list output given by parse_examfile()
#' @param path_output where should the output file saved to?
#' @param ex_sol_str wording for the parts of the exercise (useful for translation purposes)
#' @param print_categories should the yaml field 'categories' be printed?
#' @param separate_ex_sol How should the question and solution be visually separated?
#' @param verbose talkative, yes or no? (TRUE or FALSE)
#' @param more_yaml more yaml to add which was not given in the r-exam file
#' @param header_level how many '#' should be padded in order to indicate the header level?
#' @param file_suffix defaults to ".Rmd" (string), other useful option: ".qmd"
#'
#' @return writes an rmd file

#'
#' @examples
#' \dontrun{write_yamlrmdfile(ex_parsed, my_path)}
#' ex_parsed <- readr::read_rds("data-raw/ex_parsed.rds")
#' out <- write_yamlrmdfile(ex_parsed)



write_yamlrmdfile <- function(ex_parsed,
                              path_output = ".",
                              ex_sol_str = c("Aufgabe", "Lösung", "Kategorien"),
                              print_categories = TRUE,
                              separate_ex_sol = rep("</br>", 10),
                              verbose = TRUE,
                              more_yaml = NULL,
                              header_level = 1,
                              file_suffix = ".Rmd"
                              ){

  # determine the number of hashed to indicate header level:
  header_level_hashes <- rep("#", header_level) |>  stringr::str_c(collapse = "")


  # more yaml to be added?
  ex_parsed$ex_metadata <- stringr::str_c(ex_parsed$ex_metadata, more_yaml, collapse = "\n")


  # now build the yaml-rmd file:
  yamlrmdfile <-
    # metadata:
    c("---", ex_parsed$ex_metadata, "---", "", "",
      # pre-question:
      ex_parsed$ex_pre_question, "",
      # header for "Exercise":
      stringr::str_c(c(header_level_hashes, " ", ex_sol_str[1]), collapse = ""), "",
      # exercise:
      ex_parsed$ex_question, "",
      # separation between exercise and solution:
      separate_ex_sol, "",
      # solution:
      stringr::str_c(c(header_level_hashes, " ", ex_sol_str[2]), collapse = ""), "",
      ex_parsed$ex_solution, "")

  # add "categories" if desired:
  if (print_categories) {
    part_categories <-
      c("", "---", "",
        paste0(ex_sol_str[3], ": "),
        "",
        stringr::str_c(yaml::as.yaml(ex_parsed$ex_metadata_yaml$categories), collapse = ""))

    yamlrmdfile <-
      c(yamlrmdfile, part_categories)
    # yamlrmdfile <-
    #   c(yamlrmdfile,"\n", paste0("- ", ex_parsed$ex_metadata_yaml$extype,"\n"))
    }

  # build path:
  path_output_ex <- paste0(path_output,"/", ex_parsed$ex_metadata_yaml$title)

  # add filename
  filename_output <-
    paste0(path_output_ex, "/", ex_parsed$ex_metadata_yaml$title, file_suffix)

  if (!file.exists(path_output_ex))
    dir.create(path = path_output_ex)

  writeLines(text = yamlrmdfile, con = filename_output)

  if (verbose) message(paste0("Yaml-Md-Exercise file has been written to output dir: ",filename_output, "\n"))

  return(yamlrmdfile)
}
