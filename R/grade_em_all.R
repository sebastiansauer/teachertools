#' Grade em all
#'
#' Assigns grades to exams
#'
#' This function assigns a grade to each student (row)
#' drawing upon the exam items in the input csv moodle file.
#' Some preprocssing is being done, such as cleaning the col names.
#'
#' @param data_path_moodle_csv A csv file, possible coming from moodle with exam items. See details.
#' @param grading_scheme Defines thresholds for grades
#' @param verbose if true, additional processing infos are printed
#' @param n_bonus_points Be kind to your students
#' @param comments comments passed on to each student
#'
#' @return csv file with grades
#' @export
#'
#' @examples
#' \dontrun{grade_em_all(data.csv)}
#'
grade_em_all <- function(data_path_moodle_csv, grading_scheme = NULL, verbose = TRUE, n_bonus_points = 0, comments = NULL){


  grade_percentage_points <- function(data_path_moodle_csv, verbose, n_bonus_points) {

  d_raw <- readr::read_csv(data_path_moodle_csv)
  d2 <- janitor::clean_names(d_raw)

  item_names_raw <-  names(d2) %>%
    purrr::keep(stringr::str_detect(., "f_\\d+_\\d"))

  n_items <-
    item_names_raw %>% length()

  item_names <- paste0("q", sprintf("%02d", 1:n_items))

  first_item_pos <-
    names(d2) %>%
    stringr::str_detect(., "^f_1_") %>%
    which()

  names(d2)[first_item_pos:(first_item_pos + n_items - 1)] <- item_names

  d3 <-
    d2 %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches("q\\d\\d"),
                                ~ readr::parse_number(., locale = readr::locale(decimal_mark = ","))))

  d3 <-
    d3 %>%
    dplyr::mutate(bewertung = readr::parse_number(bewertung_10_00),
                  bewertung = bewertung / 10)

  d4 <-
    d3 %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches("q\\d\\d"),
                                .fns = ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(bewertung = tidyr::replace_na(bewertung, 0))

  d4_small <-
    d4 %>%
    dplyr::select(dplyr::matches("q\\d{2}"), "bewertung")

  stopifnot(sum(is.na(d4_small)) == 0)

  d5 <-
    d4 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bewertung_check = sum(dplyr::c_across(q01:q20), na.rm = TRUE) * 10) %>%
    dplyr::relocate(bewertung_check, .after = bewertung) %>%
    dplyr::mutate(check_if_similar = abs(bewertung - bewertung_check) < .5, .after = bewertung) %>%
    dplyr::ungroup()

  strange_names <- c("Gesamtdurchschnitt")

  d6 <-
    d5 %>%
    dplyr::filter(!(nachname %in% c(strange_names))) %>%
    dplyr::filter(!is.na(nachname))

  stopifnot(dplyr::filter(d6, nachname == "Sauer") %>% nrow() == 0)

  one_correct_perc_points <- 100 / n_items

  bonus <- (n_bonus_points * one_correct_perc_points)

  d6b <-
    d6 %>%
    dplyr::mutate(bewertung_mit_bonus = bewertung + bonus) %>%
    dplyr::relocate(bewertung_mit_bonus, .after = bewertung) %>%
    dplyr::mutate(bewertung_ohne_bonus = bewertung,
                  bewertung = bewertung_mit_bonus)


  one_correct <- 100 / n_items

  d6c <-
    d6b %>%
    dplyr::mutate(n_correct = round(bewertung / one_correct)) %>%
    dplyr::relocate(n_correct, .after = bewertung)

  return(d6c)

  }


  d6c <- grade_percentage_points(data_path_moodle_csv = data_path_moodle_csv,
                                 verbose = verbose,
                                 n_bonus_points = n_bonus_points)


  if (is.null(grading_scheme)) grading_scheme <- c(51, 55, 60, 65, 70, 75, 80, 85, 90, 95)

  stopifnot(length(grading_scheme) == 10)

  # grades_df <-
  #   tibble::tibble(
  #     # equidistant thresholds:
  #     thresholds = c(
  #                    seq(from = 51, to = 95,
  #                        length.out = 10)),
  #     # more milder thresholds:
  #     thresholds2 = grading_scheme,
  #     grades = c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1)) %>%
  #   dplyr::mutate(grade_id = nrow(.):1 - 1) %>%
  #   #dplyr::mutate(thresholds = round(thresholds)) %>%
  #   tibble::rownames_to_column() %>%
  #   #dplyr::select(thresholds_points = "thresholds2",
  #   #             grades) %>%
  #   dplyr::filter(grades != 0.7)

  # d7 <-
  #   d6c %>%
  #   # counts how many thresholds are surpassed by the student:
  #   dplyr::mutate(grade_id_old = purrr::map_dbl(bewertung,
  #                                               .f = ~ {`>`(grades_df$thresholds, .x) %>% sum()} )) %>%
  #   dplyr::mutate(grade_id = purrr::map_dbl(bewertung,
  #                                           .f = ~ {`>`(grades_df$thresholds2, .x) %>% sum()} ))

  d7 <-
    d6c %>%
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
                                     bewertung < grading_scheme[1] ~ 5))


  if (verbose) cat("case_when grading used")

  # d8 <-
  #   d7 %>%
  #   dplyr::left_join(grades_df, by  = "grade_id")

  d8 <-
    d7 %>%
    dplyr::filter(nachname != "Gesamtdurchschnitt") %>%
    dplyr::arrange(nachname)

  d9 <-
    d8 %>%
    dplyr::mutate(pass =  dplyr::ifelse(grades <= 4, TRUE, FALSE)) %>%
    dplyr::mutate(comments = comments)

  out <-
    d9 %>%
    dplyr::select(nachname, vorname, e_mail_adresse, bewertung,
           n_correct, grades, pass, comments)

  return(out)

}
