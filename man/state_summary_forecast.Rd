\name{state_summary_forecast}
\alias{state_summary_forecast}
\title{Memory-light summaries of simulated state over time}
\description{
Run forward simulations and compute conditional summaries of state (and derived)
variables over a time grid among a fixed eligible cohort defined at
\code{start_time}. Numeric variables return mean and standard deviation.
Binary/categorical variables return level proportions.
}
\usage{
state_summary_forecast(engine, patients, times, vars, S = 200, param_sets = NULL,
  start_time = NULL, terminal_events = NULL, condition_on_events = NULL,
  eligible = NULL, ctx = NULL, max_events = 1000, seed = NULL,
  backend = c("none", "mclapply", "future"), n_workers = NULL)
}
\arguments{
\item{engine}{A patientSimCore Engine.}
\item{patients}{A list of Patient objects (or a single Patient).}
\item{times}{Numeric vector of forecast times.}
\item{vars}{Character vector of state/derived variable names to summarize.}
\item{S}{Simulations per (patient, param_set).}
\item{param_sets}{Optional list of parameter lists.}
\item{start_time}{Time in \code{times} to define the eligible cohort (default \code{min(times)}).}
\item{terminal_events}{Optional event types that must be absent by start_time.}
\item{condition_on_events}{Optional event types that must be absent by start_time.}
\item{eligible}{Optional function(snapshot, time, ctx) -> TRUE/FALSE evaluated at start_time.}
\item{ctx}{Optional list passed to \code{eligible()}.}
\item{max_events}{Maximum events per run.}
\item{seed}{Optional base seed.}
\item{backend}{Parallel backend. One of \code{c('none','mclapply','future')}.}
\item{n_workers}{Optional number of workers for \code{mclapply} or an ephemeral \code{future::multisession} plan.}
}
\value{
A list with elements \code{meta}, \code{numeric} (named list of data frames), and
\code{categorical} (named list of data frames).
}
