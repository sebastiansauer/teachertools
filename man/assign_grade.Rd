% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_grade.R
\name{assign_grade}
\alias{assign_grade}
\title{Assign Grades}
\usage{
assign_grade(
  d,
  grading_scheme = NULL,
  var = "correct_prop",
  failing_grades = c("5", "4.3", "4.7"),
  number_of_grades = 11
)
}
\arguments{
\item{d}{data frame}

\item{grading_scheme}{which grading scheme to use}

\item{var}{variable for which a grade should be assigned}

\item{failing_grades}{grades when student is failing}

\item{number_of_grade}{number of grades that should be in the grading_scheme}
}
\value{
data frame with grades (column) added
}
\description{
Given a variable with points, percentage or other, a grade is
assigned given a grading scheme which acts as a function ascribing the
mapping from proints to grade
}
\examples{
assign_grade(mtcars, var = "am")

}
