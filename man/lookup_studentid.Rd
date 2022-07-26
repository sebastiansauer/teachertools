% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_studentid.R
\name{lookup_studentid}
\alias{lookup_studentid}
\title{lookup student id}
\usage{
lookup_studentid(
  csv_file,
  lastname,
  firstname = NULL,
  verbose = FALSE,
  fun = rio::import,
  pad_to = 8L,
  strict = TRUE
)
}
\arguments{
\item{csv_file}{Path to file with IDs (not a tibble) and names of students (character)}

\item{lastname}{One word indicating the last name (character)}

\item{verbose}{More output? (lgl)}

\item{fun}{which reading fun should be used? Defaults to rio::import}

\item{pad_to}{to how many digits should the ID be padded with zeros (from the left side)? Defaults to 8 (integer)}

\item{strict}{if TRUE, NAs are returned in case of multiple (ambiguous) results}
}
\value{
student id (chracter)
}
\description{
Look up a student id given the last name
}
\details{
Looks up the student id given a CSV file and a last name.
Beware that double names may exist and hence cause problems.
All students (rows) with the matching last name are returned.
Note that the ID is returned as a character of length one (if it's a number),
in order to retain leading zeros of the ID. Columns are parsed as characters.
To that end, zeros are padded to the left side.
}
\examples{
\dontrun{lookup_studentid(my_students, "Sauer")}


}
