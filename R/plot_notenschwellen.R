


#' Plot Notenschwellen
#'
#' Assigns minimal proportion of points needed for a given grade, plot this table then
#'
#' @param scheme grading scheme such as given by `grading_scheme2()`
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_notenschwellen()


plot_notenschwellen <- function(scheme = grading_scheme2()){

  d <-
    data.frame(
      prop = scheme,
      grade = names(scheme)
    )

  ggpubr::ggtexttable(d, rows = NULL)
}
