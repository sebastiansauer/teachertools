

#' Plot Notenspiegel
#'
#' Plot Grade Distribution (Notenspiegel).
#'
#' Plot distribution of grades, a simple wrapper around ggplot2
#' Annotations are currently in German language
#'
#' IF `show_prop_avg` is TRUE, it is expected that a column by the name of `prop` is available which holds the percentage of points achieved by student
#'
#' @param d data frame containing grades
#' @param plot_title Title to be assigned to the plot
#' @param var variable holding the grades to be plotted
#' @param show_prop_avg Should the mean percentage value be shown (defaults to FALSE)
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{plot_notenspiegel(grades_data)}
plot_notenspiegel <- function(d, plot_title = "Notenverteilung",
                              var = "grade",
                              show_prop_avg = FALSE){

  grades_scheme <- c(5, 4, 3.7, 3.3, 3.0, 2.7, 2.3, 2, 1.7, 1.3, 1.0)

  # d <-
  #   d %>%
  #   rename(Note = dplyr::all_of(var))

  names(d)[which(names(d) == var)] <- "Note"

  notenschnitt <- round(mean(d[["Note"]], na.rm = TRUE), 1)

  if (is.null(nrow(d[d$Note <= 4, ]))) bestehensquote <- 1
  else bestehensquote <-
    round(nrow(d[d$Note <= 4, ]) / base::nrow(d), 2)
  grade_mean <-
    round(mean(d$Note, na.rm = TRUE), 2)
  grade_sd <-
    round(sd(d$Note, na.rm = TRUE), 2)

  if (show_prop_avg) prop_avg <- round(mean(d$prop, na.rm = TRUE), 0)
  else prop_avg <- NA

  d$bestanden <- ifelse(d$Note <= 4, TRUE, FALSE)

  bestehensquote <- round(sum(d$bestanden[d$bestanden == TRUE], na.rm = TRUE) / nrow(d), 2)

  data_count <-
    d %>%
    dplyr::count(Note) |>
    dplyr::mutate(bestanden = ifelse(Note > 4, FALSE, TRUE))

  data_count %>%
    ggplot2::ggplot(aes(x = Note, y = n)) +
    ggplot2::geom_col(aes(fill = bestanden)) +
    ggplot2::scale_x_continuous(breaks = grades_scheme)+
    ggplot2::geom_vline(xintercept = notenschnitt, linetype = "dashed") +
    ggplot2::geom_label(aes(label = n)) +
    ggplot2::labs(x = "Noten",
                  y = "Anzahl",
                  title = plot_title,
                  subtitle = paste0("Mittelwert: ", notenschnitt,
                                    "; Bestehensquote: ", bestehensquote,
                                    "\nMittlere Punktzahl (%): ", prop_avg),
                  caption = paste0("n = ", nrow(d))) +
    ggplot2::annotate("label", x = notenschnitt, y = max(data_count$n), label = paste0("MW: ", notenschnitt)) +
    ggplot2::theme_minimal() +
    ggplot2::annotate("segment",
                      x = grade_mean-grade_sd, xend = grade_mean + grade_sd,
                      y = 0, yend = 0,
                      size = 3,
                      color = "grey20",
                      #fill = "grey20",
                      alpha = .9) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::annotate("label",
                      x = grade_mean,
                      y = 0,
                      label  = paste0("SD: ", grade_sd))

}

