library(testthat)


test_that("Testing exam2yamlrmd",
          {expect_type(
            exam2yamlrmd(
            examfile = "/Users/sebastiansaueruser/github-repos/rexams-exercises/exercises/sebastiansauer/de/Bayes/pigs2.Rmd",
            path_output = "/Users/sebastiansaueruser/Downloads"), "character")
            }
          )



