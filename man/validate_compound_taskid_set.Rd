% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_ensemble_inputs.R
\name{validate_compound_taskid_set}
\alias{validate_compound_taskid_set}
\title{Perform validations on the compound task ID set used to calculate an ensemble of
component model outputs for the sample output type, including checks that
(1) \code{compound_taskid_set} is a subset of \code{task_id_cols}, (2) the provided
\code{model_out_tbl} is compatible with the specified \code{compound_taskid_set}, and
(3) all models submit predictions for the same set of non \code{compound_taskid_set}
variables.}
\usage{
validate_compound_taskid_set(
  model_out_tbl,
  task_id_cols,
  compound_taskid_set,
  derived_task_ids = NULL,
  return_missing_combos = FALSE
)
}
\arguments{
\item{model_out_tbl}{an object of class \code{model_out_tbl} with component
model outputs (e.g., predictions).}

\item{task_id_cols}{\code{character} vector with names of columns in
\code{model_out_tbl} that specify modeling tasks. Defaults to \code{NULL}, in which
case all columns in \code{model_out_tbl} other than \code{"model_id"}, \code{"output_type"},
\code{"output_type_id"}, and \code{"value"} are used as task ids.}

\item{compound_taskid_set}{\code{character} vector of the compound task ID variable
set. This argument is only relevant for \code{output_type} \code{"sample"}. Can be one
of three possible values, with the following meanings:
\itemize{
\item \code{NA}: the compound_taskid_set is not relevant for the current modeling task
\item \code{NULL}: samples are from a multivariate joint distribution across all levels
of all task id variables
\item Equality to \code{task_id_cols}: samples are from separate univariate distributions
for each individual prediction task
}

Defaults to NA. Derived task ids must be included if all of the task ids their
values depend on are part of the \code{compound_taskid_set}.}

\item{derived_task_ids}{\code{character} vector of derived task IDs (variables whose
values depend on that of other task ID variables). Defaults to NULL, meaning
there are no derived task IDs.}

\item{return_missing_combos}{\code{boolean} specifying whether to return a \code{data.frame}
summarizing the missing combinations of dependent tasks for each model. If
TRUE, the columns of the \code{data.frame} will be "model_id" and one for each of the
dependent tasks (complement of the \code{compound_taskid_set}). Defaults to FALSE.}
}
\value{
If \code{model_out_tbl} passes the validations, there will be no return value.
Otherwise, the function will either throw an error if \code{return_missing_combos} is
FALSE, or a \code{data.frame} of the missing combinations of dependent tasks will be
returned. See above for more details.
}
\description{
Perform validations on the compound task ID set used to calculate an ensemble of
component model outputs for the sample output type, including checks that
(1) \code{compound_taskid_set} is a subset of \code{task_id_cols}, (2) the provided
\code{model_out_tbl} is compatible with the specified \code{compound_taskid_set}, and
(3) all models submit predictions for the same set of non \code{compound_taskid_set}
variables.
}
