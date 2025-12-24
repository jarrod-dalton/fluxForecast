\name{forecast}
\alias{forecast}
\title{Run forward-simulation forecasts for one or more patients}
\description{
Run forward simulations using a patientSimCore Engine and store snapshots at requested times.
}
\usage{
forecast(engine, patients, times, S = 200, param_sets = NULL, vars = NULL,
         max_events = 1000, seed = NULL, parallel = FALSE, n_workers = NULL,
         return = c("object", "summary_stats", "none"), summary_stats = NULL,
         summary_spec = NULL, ctx_base = NULL)
}
\arguments{
  \item{engine}{A patientSimCore Engine.}
  \item{patients}{A list of Patient objects (or a single Patient).}
  \item{times}{Numeric vector of forecast times.}
  \item{S}{Number of simulations per (patient, param_set).}
  \item{param_sets}{Optional list of parameter lists.}
  \item{vars}{Snapshot variables to store; \code{alive} is always included.}
  \item{max_events}{Maximum number of events per run.}
  \item{seed}{Optional base seed.}
  \item{parallel}{Logical; use parallel execution in run_cohort.}
  \item{n_workers}{Number of workers if parallel.}
  \item{return}{One of \code{"object"}, \code{"summary_stats"}, or \code{"none"}.}
  \item{summary_stats}{Summaries to compute when \code{return="summary_stats"}.}
  \item{summary_spec}{List of arguments passed to summary helpers (e.g., for \code{summary_stats="risk"}, provide \code{list(event=..., start_time=..., terminal_events=...)}).}
  \item{ctx_base}{Optional list of ctx fields besides params (reserved).}
}
\value{
A \code{ps_forecast} object when \code{return="object"}.
}
