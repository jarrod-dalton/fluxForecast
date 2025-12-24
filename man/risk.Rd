\name{risk}
\alias{risk}
\title{Compute event risk curves from a forecast ensemble}
\description{Compute cumulative event risk by time for a fixed cohort defined at a start time.}
\usage{
risk(x, event, times = NULL, start_time = NULL, terminal_events = NULL,
     condition_on_events = NULL, eligible = NULL, ctx = NULL)
}
\arguments{
  \item{x}{A \code{ps_forecast}.}
  \item{event}{Character vector of event types of interest.}
  \item{times}{Optional subset of forecast times.}
  \item{start_time}{Time in the forecast grid used to define the eligible cohort.}
  \item{terminal_events}{Optional event types that must be absent by \code{start_time}.}
  \item{condition_on_events}{Optional event types that must be absent by \code{start_time}.}
  \item{eligible}{Optional function \code{f(snapshot, time, ctx)} returning TRUE/FALSE. Evaluated at \code{start_time} only.}
  \item{ctx}{Optional list passed to \code{eligible}.}
}
\value{A \code{ps_risk} object.}
