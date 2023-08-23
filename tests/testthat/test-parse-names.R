#library(testthat)

test_that("sub files- returns chr vec", {
  expect_type(get_and_print_sub_files(list(subm_raw = "--")), "character")
})


test_that("matrikelnr - returns chr vec", {

  files <- c(
  submissions_raw_ceb93466 =
  "/Users/sebastiansaueruser/github-repos/Prognose-Wettbewerb-SeS/exams/ds1-2023-sose/subm-raw-ds1/Raphael Josef Balzer_137320_assignsubmission_file/Balzer_Raphael_00163021_Prognose.csv" ,
  submissions_raw_4375172f =
  "/Users/sebastiansaueruser/github-repos/Prognose-Wettbewerb-SeS/exams/ds1-2023-sose/subm-raw-ds1/Simon Wich_137321_assignsubmission_file/Wich_Simon_0161615_Prognose.csv"
  )

  expect_type(get_matrikelnumbers(files), "character")
})


expect_length(get_subm_names(files), > 0)


paths <-
  list(csv_pattern = "\\.csv$|\\.CSV$|\\.Csv$|\\.CSv$|\\.csV$|\\.cSV$|\\.cSV$",
       exam_root = "exams/ds1-2023-sose", subm_raw = "exams/ds1-2023-sose/subm-raw-ds1",
       subm_test = "exams/ds1-2023-sose/submissions-test", results = "Noten",
       subm_proc = "exams/ds1-2023-sose/subm-proc-ds1", solution_df = "exams/ds1-2023-sose/data/d_control.csv",
       train_df = "exams/ds1-2023-sose/data/d_train.csv", notenliste_template = "exams/ds1-2023-sose/Notenliste-leer.xlsx",
       no_shows_file = "exams/ds1-2023-sose/data/no_shows.csv",
       grades_thresholds = "exams/ds1-2023-sose/data/grades_thresholds.csv")


