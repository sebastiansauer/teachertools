% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round-fhan.R
\name{round_grades}
\alias{round_grades}
\title{Round HS Ansbach}
\usage{
round_grades(x, rounding_scheme = ansbach_round_scheme())
}
\arguments{
\item{x}{grade vom 1.0 to 5.0}
}
\value{
vector (factor) with rounded grades
}
\description{
Round grades (1-5) according to HS Ansbach grades
}
\examples{
`round_grades(x = c(1, 1.2, 1.5, 1.9, 4.1, 4.4))`


}
