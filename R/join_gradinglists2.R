
#' Join two grading lists DEPRECATEAD
#'
#' Join Moodle grading list with Institution's grading list
#'
#' Based on the Moodle csv file, a grading list has been created.
#' This function now merges the Moodle grading list with a XLSX template file
#' from the institution. Merging (joining) is performed by based on surname
#' and first names of the student. Joining errors are reported. Note that
#' names are no unique nor save way to join lists.
#' ATTENTION: This function does not parse the participants excel file,
#' but expects it as a cleaned data fraem. Use `join_gradinglist()`
#' if you need to get it parsed.
#'
#' @param moodle_grading_d data frame with results of Moodle grading
#' @param course_d data frame without grades from university
#' @param comments comments passed to each student (separate comments are not supported)
#' @param grades_var variable in Moodle table holding the grades of the students
#'
#' @return  data frame for grading based on the institution name list
#' @export
#'
#' @examples
#'  \dontrun{join_gradinglists(d1, d2)}
join_gradinglists2 <- function(moodle_grading_d, course_d, grades_var = "grade") {


  # make sure name of grading var is "grades":
  names(moodle_grading_d)[which(names(moodle_grading_d) == grades_var)] <- "grades"

    if ("bewertung" %in% names(moodle_grading_d)) moodle_grading_d$bewertung <- NULL




  results_df <-
    course_d %>%
    dplyr::left_join({moodle_grading_d %>%
        dplyr::select(bewertung = grades, nachname, vorname, comments, score)},
        by = c("nachname", "vorname")) %>%
    dplyr::select(1:4, bewertung, bemerkung = comments)

  if (nrow(moodle_grading_d) != nrow(course_d)){
    warning("Row numbers do not match. You will need to carefully theck the discrepancies.")

    anti_results_df <-
      results_df %>%
      dplyr::select(nachname, vorname, bewertung) %>%
      dplyr::anti_join(course_d)

    anti_results_df2 <-
      course_d %>%
      dplyr::select(nachname, vorname) %>%
      dplyr::anti_join(select(moodle_grading_d, vorname, nachname, bewertung = grades))

    anti_results_df3 <-
      anti_results_df %>%
      dplyr::bind_rows(anti_results_df2)

    print("The following names could NOT be matched and need be treated manually:\n")

    print(anti_results_df3$nachname)
    cat("\n")

  }

  return(results_df)


}
