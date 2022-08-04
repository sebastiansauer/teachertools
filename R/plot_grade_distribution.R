

#' plot_grade_distribution
#'
#' Plots Distribution of Grades
#'
#' @param d_grades df with grades, particularly a column `grade` containing the grades
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{plot_grade_distribution(d_grades)}
plot_grade_distribution <- function(d_grades){

  bestehensquote <-
    base::round(base::nrow(d_grades[d_grades$grade <= 4, ]) / base::nrow(d_grades), 2)
  grade_mean <-
    base::round(base::mean(d_grades$grade, na.rm = TRUE), 2)

  grades_p_hist <-
    d_grades %>%
    ggplot2::ggplot(aes(x = grade)) +
    ggplot2::geom_bar() +
    ggplot2::geom_vline(xintercept = grade_mean,
               linetype = "dashed") +
    ggplot2::annotate("label", x = base::mean(d_grades$grade),
             y = 1,
             label = base::paste0("Mean: ", grade_mean)) +
    ggplot2::labs(title = "Grade distribution",
         caption = base::paste0("n = ", base::nrow(d_grades), "; proportion pass/fail: ", bestehensquote, "/", 1 - bestehensquote),
         x = "grades",
         y = "n") +
    #scale_x_continuous() +
    #scale_y_continuous(breaks = seq(0,15,5)) +
    ggplot2::theme_minimal()

  plot(grades_p_hist)

}
