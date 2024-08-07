% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_grade_distribution.R
\name{plot_grade_distribution}
\alias{plot_grade_distribution}
\title{plot_grade_distribution}
\usage{
plot_grade_distribution(d_grades, plot_title = "Notenverteilung")
}
\arguments{
\item{d_grades}{df with grades, particularly a column \code{grade} containing the grades}

\item{plot_title}{plot title (string, defaults to "Notenverteilung")}
}
\value{
ggplot2 object
}
\description{
Plots Distribution of Grades
}
\examples{
d_grades <- data.frame(grade = sample(1:5, size = 30, replace = TRUE))
plot_grade_distribution(d_grades)
}
