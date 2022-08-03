

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
    round(nrow(d_grades[d_grades$grade <= 4, ]) / nrow(d_grades), 2)
  grade_mean <-
    round(mean(d_grades$grade, na.rm = TRUE), 2)

  grades_p_hist <-
    d_grades %>%
    ggplot(aes(x = grade)) +
    geom_bar() +
    geom_vline(xintercept = grade_mean,
               linetype = "dashed") +
    annotate("label", x = mean(d_grades$grade),
             y = 1,
             label = paste0("Mean: ", grade_mean)) +
    labs(title = "Grade distribution",
         caption = paste0("n = ", nrow(d_grades), "; proportion pass/fail: ", bestehensquote, " vs.", 1 - bestehensquote),
         x = "grades",
         y = "n") +
    #scale_x_continuous() +
    #scale_y_continuous(breaks = seq(0,15,5)) +
    theme_minimal()

  plot(grades_p_hist)

}
