
#' bonuspoints
#'
#' Gives 1 bonuspoint to exercises which have been *wrongly* answered
#'
#' @param item_vec vector of 0/1 values (numeric)
#' @param return_type type of return vector
#'
#' @return vector indicating wich students get a bonuspoint
#' @export
#'
#' @examples
#' item_vec <- c("a" = 1, "b" = 0, "c" = 0, "d" = 1)
#' bonuspoints(item_vec)
#' bonuspoints(item_vec, return_type = "01")
#' bonuspoints(item_vec, return_type = "name")

bonuspoints <- function(item_vec, return_type = "logical") {

  allowed_return_types <- c("logical", "01", "name")
  stopifnot(return_type %in% allowed_return_types)

  if (return_type == allowed_return_types[1]) out <- !item_vec
  if (return_type == allowed_return_types[2]) out <- 1 - item_vec # was: ifelse(item_vec == 1, 0, 1)
  if (return_type == allowed_return_types[3]) out <- names(which(!item_vec))

  return(out)
}
