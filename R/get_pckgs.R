
#' Get List of R Packages Ised in a Project
#'
#' Parses all files ending with "qmd", "R" and "Rmd".
#' Returns a string of length n with all n packages identified.
#' Note that only calls with "library()" and "require()" will be detected.
#'
#' @param pattern (chr)
#' @param recursive (lgl)
#'
#' @return character vector with identified R packages in the project
#' @export
#'
#' @examples
#' get_pckgs()
  get_pckgs <- function(pattern = "\\.qmd$|\\.R$|\\.Rmd$",
                        recursive = TRUE) {

    list.files(pattern = pattern, recursive = recursive) |>
      purrr::map(readLines) |>
      stringr::str_extract_all("library\\(([^)]+)\\)|require\\(([^)]+)\\)") |>
      unlist() |>
      unique()|>
      sort()
  }
