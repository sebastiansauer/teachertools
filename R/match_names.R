
#' Match names
#'
#' Check which names from persons in two tibbles match
#'
#' Assume there are two lists of names (each consisting of first names(s) and)
#' last name. You would like to know which names do match and which do not.
#' This function return a tibble showing the matched and non-matched names.
#' The function excepts in both data frames (df1, df2)
#' the columns "nachname" and "vorname".
#' To match the names in the two dataframes, the first and last names are
#' concatenated, and (optionally) stubbed to the first n characters
#' (only for df1 at them moment). If you do not want the stub the length of the name,
#' leave the value to its default, ie to -1.
#' The function returns a df with four columns:
#' name1 (concatenated name from df1), name2, name (names from both data frames),
#' match (TRUE or FALSE).
#' Note that there's not check for capitalization.
#'
#' @param df1 first data frame
#' @param df2 second data frame
#' @param first_n_chrs take only the first n chrs from df1 joint name, default -1
#'
#' @return data frame with four columns, name1, name2, name, match (T or F)
#' @export
#'
#' @examples
#' match_names(data.frame(vorname = "John", nachname = "Doe"), data.frame(vorname = "John", nachname = "Doe"))
#'  A tibble: 1 × 4
#' name1    name     name2    is_match
#' <glue>   <glue>   <glue>   <lgl>
#'   1 Doe John Doe John Doe John TRUE
#'
match_names <- function(df1, df2, first_n_chrs = -1){

  names_df1 <-
    tibble::tibble(
      name1 = glue::glue("{df2$nachname} {df2$vorname}"),
      name1_firstn = name1 |> stringr::str_sub(1, first_n_chrs)) |>
    mutate(name1_firstn = stringr::str_trim(name1_firstn))

  if ("Gesamtdurchschnitt" %in% names_df1$name1) names_df1 <-
      dplyr::filter(names_df1, name1 != "Gesamtdurchschnitt")

  names_df2 <-
    tibble::tibble(
      name2 = glue::glue("{df1$nachname} {df1$vorname}")) |>
    mutate(name2 = stringr::str_trim(name2))

  if ("Gesamtdurchschnitt" %in% names_df2$name2) names_df2 <-
    dplyr::filter(names_df2, name2 != "Gesamtdurchschnitt")

  names_matched <-
    names_df1 |>
    dplyr::full_join(names_df2 %>% select(name2), by = c("name1_firstn" = "name2")) |>
    dplyr::rename(name = name1_firstn)|>
    dplyr::mutate(is_match =
                    ifelse(is.na(name1), FALSE, TRUE))

  name_only_df1 <- setdiff(names_df1$name1_firstn, names_df2$name2)
  name_only_df2 <- setdiff(names_df2$name2, names_df1$name1_firstn)


  if ("Gesamtdurchschnitt" %in% names_matched$name)
    names_matched <- dplyr::filter(names_matched, name1 != "Gesamtdurchschnitt")

  attr(names_matched, "name_only_in_df1") <- name_only_df1
  attr(names_matched, "name_only_in_df2") <- name_only_df2

  return(names_matched)
}

