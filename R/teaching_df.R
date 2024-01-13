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
#' \dontrun{teaching_df(df = "tips")}





teaching_df <- function(df = NULL, canonicalize = TRUE, verbose = TRUE, ...) {

  #if (!is.null(df)) assertthat::assert_that(typeof(df) == "character", msg = "df must be of type character")

  data(teaching_data)
  d_df <- teaching_data  # list of data sets for teaching

  if (is.null(df)) {
    return(d_df)  # return only overview of data frames
  } else {
    if (df == "random") {
      df <- sample(x = d_df$datasets_names, size = 1)
      message(paste0("Randomly chosen data frame: ", df))
    }  # if not random:
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
