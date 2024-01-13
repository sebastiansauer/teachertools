
#' Visualize a nominal association of a two-by-two dataset
#'
#' Expects a dataset with two numeric variables,
#' which are median splitted (using `median_split_all`).
#' Next, a bar graph is plotted and returned.
#'
#' @param data dataframe with numeric variables
#' @param var1 character, name of variable going to x axis
#' @param var2 character, name of variable to fill
#' @param label1 label for var1 (character)
#' @param label2 label for var2 (character)
#'
#' @return ggplot object (bar graph of two-by-two assocation)
#' @export
#'
#' @examples
#' plot_nominal_association(mtcars, "mpg", "hp")
#' plot_nominal_assocation(d, "bill_length_mm", "bill_depth_mm")
plot_nominal_association <- function(data, var1, var2, label1 = NULL, label2 = NULL){
  # median split:
  d2 <-
    data |>
    dplyr::select(!!rlang::sym(var1), !!rlang::sym(var2)) |>
    na.omit() |>
    teachertools::median_split_all()

  # plot:
  p <- ggplot2::ggplot(d2) +
    ggplot2::aes(x = !!rlang::sym(names(d2)[1]),
                 fill =  !!rlang::sym(names(d2)[2])) +
    ggplot2::geom_bar(position = "fill") +
    #coord_flip()+
    ggplot2::scale_fill_viridis_d()

  if (!is.null(label1) & !is.null(label2)) {
    p <-
      p + ggplot2::labs(x = label1,
               fill = label2)
  }

  return(p)
}
