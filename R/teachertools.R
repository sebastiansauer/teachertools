#' teachertools: A package for helping teachers in some typical tasks
#'
#' The teachertools package provides three categories of functions for teachers:
#' (a) for writing exam/exersises, as an extention to the R package \code{exams},
#' (b) for grading prediction contexts, and (c) for publishing/writing
#' course materials as markdown documents.
#'
#' @section Exam functions:
#' Most importantly, a function is provided that transforms an exercise file
#' in the format of the R/exams package to a Rmd file with a yaml header.
#' The metadata section from the R/exam exercise file will be turned into the
#' yaml header.
#'
#' @section Grading prediction contest:
#' Fuctions are provided that take the CSV file of a (or multiple) students and
#' compute error coefficients (such as RSMSE), and grade this error value.
#'
#'
#' @section Publishing:
#' A framework is provided to control a markdown (or bookdown) website project
#' using a yaml file. In addition, functions to render exercise sections and
#' similar stuff is provided.
#'
#'
#' @docType _PACKAGE
#' @name teachertools
NULL
