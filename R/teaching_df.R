#' providing some data sets suitable for teaching
#'
#' Data sets suitable for (undergraduate) teaching should provide
#' variables at different scale leves (nominal, metric),
#' and should not be too complex or large.
#' Note that internet access is neccessary.
#'
#'
#'
#' @param df name of dataframe to return, leave empty to get overview of available data sets. Enter "random" to get a randomly chosen data set.
#'
#' @return either dataframe of requested name or dataframe with overview of available data
#' @export
#'
#' @examples
#' \dontrun{teaching_df(df == "tips")}





teaching_df <- function(df = NULL, canonicalize = TRUE, verbose = TRUE, ...) {

  if (!is.null(df)) assertthat::assert_that(typeof(df) == "character", msg = "df must be of type character")

  d_df <-
    data.frame(
      datasets_names = c("mtcars",
                         "penguins",
                         "tips",
                         "gtcars",
                         "msleep",
                         "Boston",
                         "TeachingRatings",
                         "trees",
                         "iris",
                         "airquality"
                         ),
      main_var = c("mpg",
                   "bill_length_mm",
                   "tip",
                   "msrp",
                   "awake",
                   "medv",
                   "eval",
                   "Volume",
                   "Sepal.Length",
                   "Ozone"

                   ),
      num_vars_count = c(11,
                         5,
                         3,
                         8,
                         5,
                         12,
                         6,
                         3,
                         4,
                         6
                         ),
      source = c(
        "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/reshape2/tips.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/gt/gtcars.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/msleep.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/MASS/Boston.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/AER/TeachingRatings.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/trees.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/iris.csv",
        "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/airquality.csv"
      ),
      doc = c(
        "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/mtcars.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/palmerpenguins/penguins.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/reshape2/tips.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/gt/gtcars.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/ggplot2/msleep.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/MASS/Boston.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/AER/TeachingRatings.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/trees.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/iris.html",
        "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/airquality.html"
      )
    )
  if (is.null(df)) {
    return(d_df)  # return only overview of data frames
  } else {
    if (df == "random") {
      df <- sample(x = d_df$datasets_names, size = 1)
      message(paste0("Randomly chosen data frame: ", df))
    }
    d <- read.csv(d_df$source[d_df$datasets_names == df])
    attr(d, "df_name") <- df
    output_var_name <- d_df$main_var[d_df$datasets_names == df]
    attr(d, "output_var") <- output_var_name
    attr(d, "d_path") <- d_df$source[d_df$datasets_names == df]
    attr(d, "d_doc") <- d_df$doc[d_df$datasets_names == df]

    if (canonicalize) {
      d <- canonicalize_df(d, output_var_name, ...)
      if (verbose) message(paste0("Data frame ", df, " has been canonalicalized"))
    }

    return(d)
  }
}
