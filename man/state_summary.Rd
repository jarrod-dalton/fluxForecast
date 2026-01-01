\name{state_summary}
\alias{state_summary}
\title{Summarize forecasted state variables over time}
\description{Summarize the distribution of state variables at requested times among runs that are alive and defined at each time point.}
\usage{
state_summary(x, vars = NULL, times = NULL, categorical_max_levels = 50,
              by = c("run", "patient", "patient_draw"))
}
\arguments{
  \item{x}{A \code{ps_forecast}.}
  \item{vars}{Character vector of state/derived variable names. Defaults to all variables stored in the forecast object.}
  \item{times}{Numeric vector of times at which to summarize. Defaults to \code{x$times}.}
  \item{categorical_max_levels}{Maximum number of categorical levels allowed for a single variable.}
  \item{by}{Grouping level for summaries:
    \itemize{
      \item \code{"run"}: pool all eligible runs (default; backward compatible).
      \item \code{"patient"}: summarize within each \code{patient_id} across that patient's runs.
      \item \code{"patient_draw"}: summarize within each \code{patient_id, draw_id} across simulation replicates.
    }}
}
\value{A named list with one element per variable. Each element is a \code{data.frame}.

For continuous variables: columns include \code{time}, \code{n}, \code{mean}, \code{sd}, \code{min}, \code{q1}, \code{median}, \code{q3}, \code{max}. If \code{by != "run"}, the grouping columns (\code{patient_id} and optionally \code{draw_id}) are included.

For categorical variables: columns include \code{time}, \code{level}, \code{n}, \code{p}. If \code{by != "run"}, the grouping columns are included.}
\details{Eligibility is evaluated at each time point using the forecast object's \code{alive} matrix:
\code{!is.na(alive) & alive == TRUE}.}
