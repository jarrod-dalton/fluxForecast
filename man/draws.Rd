\name{draws}
\alias{draws}
\title{Extract simulated draws}
\description{Extract per-run draws for one or more variables from a \code{ps_forecast}.}
\usage{
draws(x, var, times = NULL, start_time = NULL, eligible = NULL, ctx = NULL,
      cohort = c("timepoint", "fixed"))
}
\arguments{
  \item{x}{A \code{ps_forecast}.}
  \item{var}{Character vector of variables to extract.}
  \item{times}{Optional subset of forecast times.}
  \item{start_time}{Start time used to define a fixed cohort when \code{cohort="fixed"}.}
  \item{eligible}{Optional function \code{f(snapshot, time, ctx)} evaluated at \code{start_time} only.}
  \item{ctx}{Optional list passed to \code{eligible}.}
  \item{cohort}{"timepoint" or "fixed".}
}
\value{A data.frame when a single variable is requested; otherwise a named list of data.frames.}
