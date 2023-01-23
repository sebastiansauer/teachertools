#' Grade percentage points
#'
#' Assigns percentage points to test results
#'
#' @param data_path_moodle_csv  path to csv file with moodle test results (stringr)
#' @param verbose print additional information (lgf, defaults to TRUE)
#' @param n_bonus_points  how many bonus points should be granted (integer, defaults to zero)
#'
#' @return tibble with percentage points grades
#'
#' @examples
#' \dontrun{grade_percentage_points(my_csv_file)}
grade_percentage_points <- function(data_path_moodle_csv,
                                    verbose = TRUE,
                                    # score_var,
                                    n_bonus_points = 0) {

  #if (is.null(score_var)) stop("Argument `score_var` must not be empty.")

  d_raw <- readr::read_csv(data_path_moodle_csv)
  d2 <- janitor::clean_names(d_raw)

  item_names_raw <-  names(d2) %>%
    purrr::keep(stringr::str_detect(., "f_\\d+"))

  # how many columns do we have?
  n_items <-
    item_names_raw %>% length()

  # assign friendly column names:s
  item_names <- paste0("q", sprintf("%02d", 1:n_items))


  # replace "-" with NA
  d2 <-
    d2 |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                .fns = ~ dplyr::na_if(., "-")))

  # following the Moodle naming scheme, that's quite hacky:
  first_item_pos <-
    names(d2) %>%
    stringr::str_detect(., "^f_1_") %>%
    which()

  names(d2)[first_item_pos:(first_item_pos + n_items - 1)] <- item_names

  d3 <-
    d2 %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("q\\d\\d"),
        ~ readr::parse_number(.,
                              locale = readr::locale(decimal_mark = ","))))

  d3 <-
    d3 %>%
    dplyr::mutate(score = readr::parse_number(score))

  d4 <-
    d3 %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches("q\\d\\d"),
                                .fns = ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(score = tidyr::replace_na(score, 0))

  d4_small <-
    d4 %>%
    dplyr::select(dplyr::matches("q\\d{2}"), "score")

  stopifnot(sum(is.na(d4_small)) == 0)

  d5 <-
    d4 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(score_check = sum(dplyr::c_across(q01:q20), na.rm = TRUE) * 10) %>%
    dplyr::relocate(score_check, .after = score) %>%
    dplyr::mutate(check_if_similar = abs(score - score_check) < .5, .after = score) %>%
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
    dplyr::mutate(score_mit_bonus = score + bonus) %>%
    dplyr::relocate(score_mit_bonus, .after = score) %>%
    dplyr::mutate(score_ohne_bonus = score,
                  score = score_mit_bonus)


  one_correct <- 100 / n_items

  d6c <-
    d6b %>%
    dplyr::mutate(n_correct = round(score / one_correct)) %>%
    dplyr::relocate(n_correct, .after = score)

  return(d6c)

}

