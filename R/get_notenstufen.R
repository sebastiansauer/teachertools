#' Notenstufen
#'
#' Returns notenstufen of various types (e.g, "1.0", "1.3",...)
#'
#' The different types support either course grades (1,2,3,...) or more fine grained grades (1.3, 1.7, ...)
#'
#' @param type type of notenstufen, currently are 6 types supported (integer, defaults to 1)
#'
#' @return numeric vector, can be of different length depending on type
#' @export
#'
#' @examples
#' notenstufen()
#'
notenstufen <- function(type = 1){
  fct1 <- factor(c("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5"))
  fct2 <- factor(c(1, 2, 3, 4, 5))
  fct3 <- factor(c("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "4.3", "4.7", "5"))

  return(
    switch(type,
          fct1,  # type = 1
          as.numeric(as.character(fct1)), # type = 2
          fct2,  # type = 3
          as.numeric(as.character(fct2)), # type = 4
          fct3,  # type = 5
          as.numeric(as.character(fct3)), # type = 6
    )
  )

}
