build_teaching_dfs <- function() {

  dataset_names <- c("mtcars",
                     "penguins",
                     "tips",
                     "gtcars",
                     "msleep",
                     "Boston",
                     "TeachingRatings",
                     # "trees",
                     "iris",
                     "airquality",
                     "education",
                     "Affairs",
                     "CASchools"
  )

  main_var <- c("mpg",
               "bill_length_mm",
               "tip",
               "msrp",
               "awake",
               "medv",
               "eval",
               #"Volume",
               "Sepal.Length",
               "Ozone",
               "Y",
               "affairs",
               "math"

  )

  num_vars_count <- c(11,
                     5,
                     3,
                     8,
                     5,
                     12,
                     6,
                     # 3,
                     4,
                     6,
                     5,
                     7,
                     11
  )

  source <- c(
    "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/reshape2/tips.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/gt/gtcars.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/msleep.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/MASS/Boston.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/AER/TeachingRatings.csv",
    # "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/trees.csv", DOESNT WORK
    "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/iris.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/datasets/airquality.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/robustbase/education.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv",
    "https://vincentarelbundock.github.io/Rdatasets/csv/AER/CASchools.csv"
  )

  doc <- c(
    "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/mtcars.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/palmerpenguins/penguins.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/reshape2/tips.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/gt/gtcars.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/ggplot2/msleep.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/MASS/Boston.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/AER/TeachingRatings.html",
    #"https://vincentarelbundock.github.io/Rdatasets/doc/datasets/trees.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/iris.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/datasets/airquality.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/robustbase/education.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/AER/Affairs.html",
    "https://vincentarelbundock.github.io/Rdatasets/doc/AER/CASchools.html"
  )



  d_df <- tibble::tibble(
    datasets_names = datasets_names,
    main_var = main_var,
    num_vars_count = num_vars_count,
    source = source,
    doc = doc
  )

  return(d_df)

}

d_df <- build_teaching_dfs()

exclude_col <- c("X")


d_df2 <-
    d_df |>
    dplyr::mutate(Package = stringr::str_remove(source,
                                         "https://vincentarelbundock.github.io/Rdatasets/csv/") |>
             stringr::str_remove("/\\w+\\.csv$")) |>
    dplyr::select(-one_of(exclude_col))

readr::write_csv(d_df2, "data/teaching_df.csv")
