data(teaching_data)

library(tidyverse)


get_names_col <- function(source) {

  out <-
    read_csv(source) |>
    names() |>
    str_flatten(collapse = ",")
}

#out <- get_names_col(teaching_data$source[1])

d2 <-
  teaching_data |>
  mutate(vars = map_chr(.x = source, get_names_col))

teaching_data <- d2

save(teaching_data, file = "data/teaching_data.RData")







