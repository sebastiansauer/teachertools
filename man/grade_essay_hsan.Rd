% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grading_essay.R
\name{grade_essay_hsan}
\alias{grade_essay_hsan}
\title{grade essay HS Ansbach}
\usage{
grade_essay_hsan(d)
}
\arguments{
\item{d}{data frame with raw grading data}
}
\value{
vector (num) with graded data
}
\description{
takes a dataframe with 4 columns and grades it
}
\details{
Expects (at least) the following columns: Methodik, Formales, Inhalt, Bonus
(num/num/num/chr).
Methodik, Formales, Inhalt are the 3 criteria to which the essay is graded.
Bonus determines whether the student is awarded some bonus.
ATTENTION: This function is hardcoded to suit the HS Ansbach grading method!
}
\examples{
`grade_essay_hsan(d)`

}
