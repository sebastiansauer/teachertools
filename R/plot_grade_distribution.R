

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
#' d_grades <- data.frame(grade = sample(1:5, size = 30, replace = TRUE))
#' plot_grade_distribution(d_grades)
plot_grade_distribution <- function(d_grades){

  if (is.null(nrow(d_grades[d_grades$grade <= 4, ]))) bestehensquote <- 1
      else bestehensquote <-
      round(nrow(d_grades[d_grades$grade <= 4, ]) / base::nrow(d_grades), 2)
  grade_mean <-
    round(mean(d_grades$grade, na.rm = TRUE), 2)
  grade_sd <-
    round(sd(d_grades$grade, na.rm = TRUE), 2)

  grades_p_hist <-
    d_grades |>
    ggplot2::ggplot(ggplot2::aes(x = grade)) +
    ggplot2::geom_bar() +
    ggplot2::geom_vline(xintercept = grade_mean,
               linetype = "dashed") +
    ggplot2::annotate("label", x = grade_mean,
                      y = Inf,
                      hjust = 0.5, vjust = 1,
                      label = base::paste0("Mean: ", grade_mean)) +
    ggplot2::labs(title = "Grade distribution",
         caption = base::paste0("n = ", base::nrow(d_grades), "; proportion pass/fail: ", bestehensquote, "/", 1 - bestehensquote),
         x = "grades",
         y = "n") +
    ggplot2::annotate("segment",
             x = grade_mean-grade_sd, xend = grade_mean + grade_sd,
             y = 0, yend = 0,
             size = 3,
             color = "grey20",
             #fill = "grey20",
             alpha = .9) +
    ggplot2::annotate("label",
                      x = grade_mean,
                      y = 0,
                      label  = paste0("SD: ", grade_sd)) +
    #scale_x_continuous() +
    #scale_y_continuous(breaks = seq(0,15,5)) +
    ggplot2::theme_minimal()

  plot(grades_p_hist)

}
