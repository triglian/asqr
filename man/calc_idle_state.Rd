% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state.R
\name{calc_idle_state}
\alias{calc_idle_state}
\title{Calculates the idle state.}
\usage{
calc_idle_state(user_events, timeline)
}
\arguments{
\item{user_events}{\code{data.frame} of user events.}

\item{timeline}{\code{data.frame} that described the state in one second intervals.}
}
\value{
An updated timeline \code{data.frame} with the idle state set.
}
\description{
Given a \code{data.frame} of user event and an associated timeline, for each
second of the timeline it calculates if the user was idle or not.
}
