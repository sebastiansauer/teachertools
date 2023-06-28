

#' Plot Notenspiegel
#'
#' Plot Grade Distribution (Notenspiegel).
#'
#' Plot distribution of grades, a simple wrapper around ggplot2
#' Annotations are currently in German language
#'
#' @param d data frame containing grades
#' @param plot_title Title to be assigned to the plot
#' @param var variable holding the grades to be plotted
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{plot_notenspiegel(grades_data)}
plot_notenspiegel <- function(d, plot_title = "Notenverteilung", var = "grade"){

  grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1.0)

  # d <-
  #   d %>%
  #   rename(Note = dplyr::all_of(var))

  names(d)[which(names(d) == var)] <- "Note"

  notenschnitt <- round(mean(d[["Note"]], na.rm = TRUE), 1)

  bestehensquote <- round(sum(d$pass[d$pass == TRUE], na.rm = TRUE) / nrow(d), 2)

  data_count <-
    d %>%
    dplyr::count(Note) |>
    dplyr::mutate(bestanden = ifelse(Note > 4, FALSE, TRUE))

  data_count %>%
    ggplot2::ggplot(aes(x = Note, y = n)) +
    ggplot2::geom_col(aes(fill = bestanden)) +
    ggplot2::scale_x_continuous(breaks = grades_scheme)+
    ggplot2::geom_vline(xintercept = notenschnitt, linetype = "dashed") +
    ggplot2::geom_text(aes(label = n)) +
    ggplot2::labs(x = "Noten",
                  y = "Anzahl",
                  title = plot_title,
                  subtitle = paste0("Mittelwert: ", notenschnitt, "; Bestehensquote: ", bestehensquote),
                  caption = paste0("n = ", nrow(d))) +
    ggplot2::annotate("label", x = notenschnitt, y = max(data_count$n), label = paste0("MW: ", notenschnitt))

}

