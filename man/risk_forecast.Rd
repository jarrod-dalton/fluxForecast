\name{risk_forecast}
\alias{risk_forecast}
\title{Memory-light event risk summaries without storing all simulated trajectories}
\description{
Runs forward simulations and computes event risk curves as the simulations run.
This returns a compact \code{ps_risk} object and avoids keeping a large
\code{ps_forecast} (which can be memory-heavy when \code{S} or \code{times} is large).
}
\usage{
risk_forecast(engine, patients, times, S = 200, param_sets = NULL, event,
  start_time = NULL, terminal_events = NULL, condition_on_events = NULL,
  eligible = NULL, ctx = NULL, max_events = 1000, seed = NULL,
  n_workers = NULL)
}
\arguments{
\item{engine}{A patientSimCore Engine.}
\item{patients}{A list of Patient objects (or a single Patient).}
\item{times}{Numeric vector of forecast times.}
\item{S}{Simulations per (patient, param_set).}
\item{param_sets}{Optional list of parameter lists.}
\item{event}{Character vector of event types of interest.}
\item{start_time}{Scalar time in \code{times} defining the fixed eligible cohort.}
\item{terminal_events}{Optional event types that must be absent by \code{start_time}.}
\item{condition_on_events}{Optional event types that must be absent by \code{start_time}.}
\item{eligible}{Optional function(snapshot, time, ctx) -> TRUE/FALSE, evaluated at \code{start_time}.}
\item{ctx}{Optional list passed to \code{eligible()}.}
\item{max_events}{Maximum events per run.}
\item{seed}{Optional base seed.}
\item{backend}{Parallel backend. One of \code{c('none','mclapply','future')}.}
\item{n_workers}{Optional number of workers for \code{mclapply} or an ephemeral \code{future::multisession} plan.}
}
\value{
A \code{ps_risk} object.
}
