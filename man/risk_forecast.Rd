\name{risk_forecast}
\alias{risk_forecast}
\title{Compute event risk curves directly from forward simulations}
\description{
Runs forward simulations and returns a \code{ps_risk} object without retaining a full
\code{ps_forecast} (per-run state matrices). This is designed to be memory-friendly.
}
\usage{
risk_forecast(engine, patients, times, S = 200, param_sets = NULL, event,
  start_time = NULL, terminal_events = NULL, condition_on_events = NULL,
  eligible = NULL, eligible_vars = NULL, ctx = NULL, max_events = 1000,
  seed = NULL, parallel = FALSE, n_workers = NULL, ctx_base = NULL)
}
\arguments{
  \item{engine}{A patientSimCore Engine.}
  \item{patients}{A list of Patient objects (or a single Patient).}
  \item{times}{Numeric vector of forecast times.}
  \item{S}{Integer simulations per (patient, param_set).}
  \item{param_sets}{Optional list of parameter lists.}
  \item{event}{Character vector of event types of interest.}
  \item{start_time}{Scalar time in \code{times} used to define the eligible cohort.}
  \item{terminal_events}{Optional event types that must be absent by \code{start_time}.}
  \item{condition_on_events}{Optional event types that must be absent by \code{start_time}.}
  \item{eligible}{Optional predicate \code{function(snapshot, time, ctx)} evaluated at \code{start_time}.}
  \item{eligible_vars}{Snapshot variables to pass to \code{eligible()}; \code{'alive'} is always included.}
  \item{ctx}{Optional list passed to \code{eligible()}.}
  \item{max_events}{Passed to \code{Engine$run()}.}
  \item{seed}{Optional base seed.}
  \item{parallel}{Logical; if TRUE uses patientSimCore::run_cohort(parallel=TRUE).}
  \item{n_workers}{Optional number of workers.}
  \item{ctx_base}{Optional ctx fields besides params.}
}
\value{A \code{ps_risk} object.}
