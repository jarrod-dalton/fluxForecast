\name{forecast}
\alias{forecast}
\title{Run forward-simulation forecasts for one or more patients}
\description{
Runs forward simulations using a patientSimCore Engine and collects snapshots at the requested
times. By convention, models expose an \code{alive} state variable to indicate vital status
(alive vs dead). This is separate from things like discharge or loss to follow-up, which can be
represented with other state variables (e.g., \code{active_followup}).
}
\usage{
forecast(engine, patients, times, S = 200, param_sets = NULL, vars = NULL,
         max_events = 1000, seed = NULL, backend = c("none", "mclapply", "cluster", "future"), n_workers = NULL,
         return = c("object", "summary_stats", "none"), summary_stats = NULL,
         summary_spec = NULL, ctx = NULL)
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
  \item{backend}{Parallel backend. One of \code{c("none","mclapply","cluster","future")}.
  \itemize{
    \item \code{"none"}: run serially.
    \item \code{"mclapply"}: use \code{parallel::mclapply} (macOS/Linux; not Windows).
    \item \code{"cluster"}: PSOCK cluster via \code{patientSimCore::run_cohort(backend="cluster")} (only for \code{return="object"}).
    \item \code{"future"}: \code{future.apply::future_lapply} for \code{return="summary_stats"} (recommended for cloud/Databricks).
  }}
  \item{n_workers}{Optional number of workers. For \code{backend="future"}, set workers via \code{future::plan()}.}
  \item{return}{One of \code{"object"}, \code{"summary_stats"}, or \code{"none"}.}
  \item{summary_stats}{Summary name when \code{return="summary_stats"} (v1 supports \code{"risk"} or \code{"state"}).}
  \item{summary_spec}{Named list describing the summary to compute when
  \code{return="summary_stats"}. For \code{summary_stats="risk"} this is passed
  to \code{risk_forecast()}. For \code{summary_stats="state"} this is passed to
  \code{state_summary_forecast()}.}
  \item{ctx}{Optional simulation context. Either a single list merged into each run ctx, or a list of per-parameter-set ctx lists. Each per-draw ctx may include its own \code{$params}; if so, that overrides \code{param_sets}.}
}
\value{
A \code{ps_forecast} object when \code{return="object"}.
}
