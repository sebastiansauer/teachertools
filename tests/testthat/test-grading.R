

test_that("basic IO", {


  d <-
    tibble::tribble(
      ~Methodik, ~Formales, ~Inhalt, ~Bonus,
      "3,00",     "3,3",   "3,3", "nein",
      "na",      "na",    "na",   "ja",
      "2,30",     "3,7",   "3,7", "nein",
      "5,00",     "2,7",   "2,7",   "ja"
    )

  expect_type(object = d, type = "list")
  expect_length(d, 4)

  expect_type(grade_essay(d), type = "integer")
  expect_s3_class(grade_essay(d), class = "factor")


})
