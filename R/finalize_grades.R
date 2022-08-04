
#' finalize grades
#'
#' Given the predictive error per student, compute the individual grades.
#'
#' @param d_error df with error per student. Output of `comp_error_submissions`
#' @param thresholds  thresholds for the grades
#' @param grades_particulars individual changes to the grades (df)
#' @param save_to_disk save as `d_grades.csv` and `d_grades.rds`, if TRUE (lgl)
#' @param verbose more output? (lgl)
#' @param ... more options eg., for grade_em which is called internally
#'
#' @return df with grdades (notenliste)
#' @export
#'
#' @examples
#' \dontrun{finalize_grades(my_d_error, my_grades_templates, my_thresholds,)}
finalize_grades <- function(d_error,
                            thresholds,
                            grades_particulars = NULL,
                            save_to_disk = FALSE,
                            verbose = FALSE,
                            ...){



  # Add metadata (attributes) as columns, drop list columns:
  if (verbose) cat("Add metadata (attributes) as columns.\n")
  d_error2 <-
    d_error %>%
    dplyr::mutate(comments_to_student = purrr::map_chr(data, ~ attr(., which = "comments_to_student"))) %>%
    dplyr::mutate(failed = purrr::map_lgl(data, ~ attr(., which = "failed"))) %>%
    dplyr::mutate(na_prop = purrr::map_dbl(data, ~ attr(., which = "na_prop"))) %>%
    dplyr::select(id, last_name, first_name, npreds, error_value, comments_to_student, failed, na_prop)

  # Grade the students:
  if (verbose) cat("Grade em.\n")
  d_grades <-
    d_error2 %>%
    dplyr::mutate(grade_f = grade_em(x = error_value,
                              thresholds = thresholds,
                              reverse = TRUE)) %>%
    dplyr::mutate(grade = as.numeric(as.character(grade_f)))


  if (is.null(grades_particulars)) {
    grades_particulars <-
      tibble::tribble(
        ~id, ~grade_change, ~comment,
        "", 0, ""
      )

  }

  # Add individual "ad-hoc" changes to computed grade:
  if (verbose) cat("Add ad-hoc changes to computed grade.\n")
  d_grades2 <-
    d_grades %>%
    left_join(grades_particulars) %>%
    rename(grade_old = grade) %>%
    dplyr::mutate(grade_change = tidyr::replace_na(grade_change, 0)) %>%
    dplyr::mutate(grade = grade_old + grade_change) %>%
    dplyr::mutate(grade_f = as.factor(grade)) %>%
    dplyr::mutate(comment = paste0("Error value in test sample: ", round(error_value, 3), ". ")) %>%
    dplyr::mutate(comment = purrr::map2_chr(comment, comments_to_student, paste0)) %>%
    dplyr::select(-c(comments_to_student))


  # Save to disk:
  if (save_to_disk) {
    readr::write_csv(d_grades2, file = paste0(results_path, "d_grades.rds"))
    readr::write_rds(d_grades2, file = paste0(results_path, "d_grades.rds"))
  }

  return(d_grades2)
}



