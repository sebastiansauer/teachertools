
#' Join two grading lists
#'
#' Join moodle grading list with Instituation's grading list
#'
#' Based on the Moodle csv file, a grading list has been created.
#' This function now merges the moodle grading list with a XLSX template file
#' from the institatution. Merging (joining) is performed by based on surname
#' and first names of the student. Joining errors are reported.
#'
#' @param moodle_grading_d data frame with results of moodle grading
#' @param course_file path to XLSX file from institatution
#' @param comments comments passed to each student (separate comments are not supported)
#'
#' @return  data frame for grading based on the instutation name list
#' @export
#'
#' @examples
#'  \dontrun{join_gradinglists(d1, d2)}
join_gradinglists <- function(moodle_grading_d, course_file, comments = NULL) {

  participants_results_path <- course_file

  participants_results_df <- rio::import(participants_results_path,
                                         skip  = 20)  %>% # skip the first few lines
    janitor::remove_empty("rows") %>%
    janitor::clean_names() %>%
    dplyr::rename(nachname = "name") %>%
    dplyr::select(-bewertung)


  results_df <-
    participants_results_df %>%
    left_join({moodle_grading_d %>%
        select(-bewertung, bewertung = grades)}, by = c("nachname", "vorname") ) %>%
    select(1:4, bewertung) %>%
    mutate(bemerkung = comments)

  if (nrow(moodle_grading_d) != nrow(participants_results_df)){
    warning("Row numbers do not match. You will need to carefully theck the discrepancies.")

    anti_results_df <-
      results_df %>%
      select(nachname, vorname, bewertung) %>%
      anti_join(participants_results_df)

    anti_results_df2 <-
      participants_results_df %>%
      select(nachname, vorname) %>%
      anti_join(select(moodle_grading_d, vorname, nachname, bewertung = grades))

    anti_results_df3 <-
      anti_results_df %>%
      bind_rows(anti_results_df2)

    print("The following names could NOT be matched and need be treated manually:\n")

    print(anti_results_df3$nachname)
    cat("\n")

  }

  return(results_df)


}
