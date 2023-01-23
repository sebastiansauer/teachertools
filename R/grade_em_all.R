#' Grade em all
#'
#' Assigns grades to exams
#'
#' This function assigns a grade to each student (row)
#' drawing upon the exam items in the input csv moodle file.
#' Some preprocssing is being done, such as cleaning the col names.
#'
#' @param data_path_moodle_csv A csv file, possible coming from moodle with exam items. See details.
#' @param grading_scheme Defines thresholds for grades, defaults to  `c(51, 55, 60, 65, 70, 75, 80, 85, 90, 95)`
#' @param verbose if true, additional processing infos are printed (lgl, defaults to TRUE)
#' @param n_bonus_points Be kind to your students (int, defaults to zero)
#' @param comments comments passed on to each student (chr, defaults to NULL)
#' @param suppress_details suppresses the output of additional columns (lgl, defaults to TRUE)
#' @param ... additional arguments, currently not used
#'
#' @return csv file with grades
#' @export
#'
#' @examples
#' \dontrun{grade_em_all(data.csv)}
#'
#' data_path_moodle_csv <- "/Users/sebastiansaueruser/Google Drive/Lehre/Lehre_AKTUELL/2022-SoSe/QM2/Prüfung/Ergebnisse/raw-data/AWM-QM2-SoSe22-Prüfungsaufgaben-Bewertungen (2).csv"
#' my_grading_scheme <-  c(45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
#' out <- grade_em_all(data_path_moodle_csv, my_grading_scheme)
#'
grade_em_all <- function(data_path_moodle_csv,
                         grading_scheme = NULL,
                         verbose = TRUE,
                         n_bonus_points = 0,
                         suppress_details = TRUE,
                         suppress_warnings = TRUE,
                         comments = NULL,
                         ...){


  out_raw <- grade_percentage_points(
    data_path_moodle_csv = data_path_moodle_csv,
    verbose = verbose,
    n_bonus_points = n_bonus_points)


  if (is.null(grading_scheme)) grading_scheme <- c(51, 55, 60, 65, 70, 75, 80, 85, 90, 95)

  stopifnot(length(grading_scheme) == 10)

  # grade:
  out <-
    out_raw |>
    dplyr::mutate(grades =
                    dplyr::case_when(bewertung >= grading_scheme[10] ~ 1,
                                     bewertung >= grading_scheme[9] ~ 1.3,
                                     bewertung >= grading_scheme[8] ~ 1.7,
                                     bewertung >= grading_scheme[7] ~ 2.0,
                                     bewertung >= grading_scheme[6] ~ 2.3,
                                     bewertung >= grading_scheme[5] ~ 2.7,
                                     bewertung >= grading_scheme[4] ~ 3.0,
                                     bewertung >= grading_scheme[3] ~ 3.3,
                                     bewertung >= grading_scheme[2] ~ 3.7,
                                     bewertung >= grading_scheme[1] ~ 4.0,
                                     bewertung < grading_scheme[1] ~ 5)) |>
  dplyr::mutate(pass =  ifelse(grades <= 4, TRUE, FALSE))


  # some clean-up:
  out <-
    out |>
    dplyr::filter(nachname != "Gesamtdurchschnitt") |>
    dplyr::arrange(nachname) |>
    dplyr::mutate(comments = comments)

  if (!suppress_details){
    out <-
      out |>
      dplyr::select(nachname, vorname, e_mail_adresse, score,
                    n_correct, grades, pass, comments)
  }

  return(out)

}
