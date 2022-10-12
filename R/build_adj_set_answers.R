#' build adjustment set answers
#'
#' Producing single choice exercises for minimal adjustment sets
#'
#' This function produces a single choice answer list for suitable
#' for R/exam exercises. Use this function to build single choice question asking for the minimal adjustment set
#'
#' @param dag_def dag defintion, based on R package dagitty (string)
#' @param exposure_var which variable is the exposure (string)?
#' @param outcome_var which var. is the outcome variable? (string)
#' @return tibble wit 1 correct and 4 incorrect solutions
#' @export
#'
#' @examples
#' \dontrun{build_adj_set_answers(my_dag, x, y,)}



build_adj_set_answers <- function(dag_def, exposure_var, outcome_var) {





  get_dag_size <- function(dag){
    stringr::str_extract_all(dag, pattern = "\\d") %>% unlist() %>% max()
  }



  dag_size <- get_dag_size(dag_def)

  # compute minimal adjustment set:
  adj <- dagitty::adjustmentSets(dag_def,
                        exposure = exposure_var,
                        outcome = outcome_var)


  # vector to hold all nodes/variables:
  variables <- paste0("x", 1:dag_size)



  # init the solution vector:
  sol <- character()


  no_solution <- "/"
  empty_set <- c("{ }")
  all_sets_size1 <- paste0("{ x", 1:dag_size, " }")
  all_sets_size2 <- outer(variables, variables, FUN = str_c, sep = ", ")

  all_sets_size2_unique <- all_sets_size2[upper.tri(all_sets_size2)]

  all_sets_size2_unique2 <- paste0("{ ",all_sets_size2_unique, " }" )

  all_sets_size0and1and2 <- c(no_solution, empty_set, all_sets_size1, all_sets_size2_unique2)

  # add the empty set:
  variables <- c(variables, " ")


  # find shortest adjustment set (random brake in case of ties):
  shortest_adj_set_nr <- purrr::map_dbl(adj, length) %>% which.min() %>% unname()

  # we continue with the shortest one:
  adj_shortest <- adj[shortest_adj_set_nr]

  # format adj to string:
  adj_string <-
    purrr::map_chr(adj, str_c, collapse= " , ") %>%
    purrr::map_chr(~paste0("{ ",.x, " }"))

  adj_string_shortest <-
    adj_string[shortest_adj_set_nr]

  # but we also need to remenber the other correct solutions,
  # because those should not show up as incorrect ones:
  adj_string_other <- adj_string[-shortest_adj_set_nr]



  incorrect_z_set <- setdiff(all_sets_size0and1and2, adj_string)

  if (length(adj) == 0) sol[1] <- "/"
  if (length(adj) != 0) sol[1] <- adj_string_shortest  # one correct solution

  sol[2:5] <- sample(incorrect_z_set, 4) # incorrect solution
  # more scrambling happens later.


  sol_df <-
    tibble::tibble(
      sol = sol,
      is_correct = c(TRUE, F, F, F, F)
    ) %>%
    dplyr::sample_n(size = nrow(.))  # shuffle it, so that the correct solution comes to a random place
}



