% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_error_submissions.R
\name{comp_error_submissions}
\alias{comp_error_submissions}
\title{comp_error_submission}
\usage{
comp_error_submissions(
  path_to_submissions = "Submissions/",
  verbose = FALSE,
  path_to_train_data,
  path_to_test_data,
  max_row = NULL,
  start_id = 1,
  name_output_var = "y",
  name_id_var = "id",
  name_pred_column = "pred",
  error_fun = yardstick::mae
)
}
\arguments{
\item{path_to_submissions}{path to submission folder with submission files (chr)}

\item{verbose}{Print infos (lgl)?}

\item{path_to_train_data}{path to train data (chr)}

\item{path_to_test_data}{path to test data (Chr)}

\item{max_row}{how many rows should be prepared maximally (int)?}

\item{start_id}{number of the first id (int)}

\item{name_output_var}{name of the variable to be predicted (chr)}

\item{name_id_var}{name of the id variable (chr)?}

\item{name_pred_column}{name of the columns with the predictions (chr)?}

\item{error_fun}{which error fun to use (mae, rmse, ...), possibly from the tidymodels ecoverse}
}
\value{
tibble with prediction error value
}
\description{
Computes prediction error for student submissions
}
\details{
Given a number of csv files with predictions (and id),
this function returns the prediction error (such as MAE or RMSE) for each prediction
}
\examples{
\dontrun{comp_error_submissions(mypath)}
}
