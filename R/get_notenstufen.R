#' Notenstufen
#'
#' Returns notenstufen of various types (e.g, "1.0", "1.3",...)
#'
#' @param type type of notenstufen, currently are 4 types supported (integer, defaults to 1)
#'
#' @return numeric vector, can be of different length depending on type
#' @export
#'
#' @examples
#' notenstufen()
#'
notenstufen <- function(type = 1){

  if (type == 1) out <- factor(c("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5"))
  if (type == 2) out <- as.numeric(as.character(factor(c("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5"))))
  if (type == 3) out <- factor(c(1, 2, 3, 4, 5))
  if (type == 4) out <- as.numeric(as.character(factor(c(1, 2, 3, 4, 5))))

  return(out)
}
