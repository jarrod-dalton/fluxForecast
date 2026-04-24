\name{forecast}
\alias{forecast}
\title{Run forward-simulation forecasts for one or more entities}
\description{
Runs forward simulations using a fluxCore Engine and collects snapshots at the requested
times. If models expose an \code{alive} state variable, it is used for lifecycle eligibility.
If \code{alive} is omitted, lifecycle status is derived from \code{bundle$terminal_events} when
declared, otherwise it defaults to active wherever runs are defined.
}
\usage{
forecast(engine, entities, times, S = 200, param_sets = NULL, vars = NULL,
         max_events = 1000, seed = NULL,
         backend = c("none", "mclapply", "cluster", "future"), n_workers = NULL,
         return = c("object", "summary_stats", "none"),
         summary_stats = c("both", "event_prob", "state"),
         summary_spec = NULL, ctx = NULL)
}
\arguments{
  \item{engine}{A fluxCore Engine.}
  \item{entities}{A list of Entity objects (or a single Entity).}
  \item{times}{Numeric vector of forecast times.}
  \item{S}{Number of simulations per (entity, param_set).}
  \item{param_sets}{Optional list of parameter lists.}
  \item{vars}{Snapshot variables to store. If schema includes \code{alive}, it is always included.}
  \item{max_events}{Maximum number of events per run.}
  \item{seed}{Optional base seed.}
  \item{backend}{Parallel backend. One of \code{c("none","mclapply","cluster","future")}. 
  \itemize{
    \item \code{"none"}: run serially.
    \item \code{"mclapply"}: use \code{parallel::mclapply} (macOS/Linux; not Windows).
    \item \code{"cluster"}: PSOCK cluster via \code{fluxCore::run_cohort(backend="cluster")}.
    \item \code{"future"}: \code{future.apply::future_lapply} via \code{fluxCore::run_cohort(backend="future")}.
  }}
  \item{n_workers}{Optional number of workers. For \code{backend="future"}, set workers via \code{future::plan()}.}
  \item{return}{One of \code{"object"}, \code{"summary_stats"}, or \code{"none"}.}
  \item{summary_stats}{Summary name when \code{return="summary_stats"} (v1 supports \code{"event_prob"} or \code{"state"}).}
  \item{summary_spec}{Named list describing the summary to compute when
  \code{return="summary_stats"}. For \code{summary_stats="event_prob"} this is passed
  to \code{event_prob_forecast()}. For \code{summary_stats="state"} this is passed to
  \code{state_summary_forecast()}.}
  \item{ctx}{Optional simulation context. Either a single list merged into each run ctx, or a list of per-parameter-set ctx lists. Each per-draw ctx may include its own \code{$params}; if so, that overrides \code{param_sets}.}
}
\value{
A \code{flux_forecast} object when \code{return="object"}.
}
