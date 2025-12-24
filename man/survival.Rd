\name{survival}
\alias{survival}
\title{Event-free curve as sugar over risk}
\description{Computes event-free probability for a terminal/composite endpoint specified by the user.}
\usage{survival(x, terminal_events, ...)}
\arguments{
  \item{x}{A \code{ps_forecast}.}
  \item{terminal_events}{Character vector of event types defining the terminal/composite endpoint.}
  \item{...}{Passed to \code{risk}.}
}
\value{A \code{ps_risk} object with an additional column \code{event_free} in \code{result}.}
