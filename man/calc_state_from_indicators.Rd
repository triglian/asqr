% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state.R
\name{calc_state_from_indicators}
\alias{calc_state_from_indicators}
\title{Calculates the total state from state indicators.}
\usage{
calc_state_from_indicators(hasExercise, connected, visible, focus, idle, input,
  submitted)
}
\arguments{
\item{hasExercise}{\code{Logical} whether there is an exercise or not.}

\item{connected}{\code{Logical} whether the user is connected or not.}

\item{visible}{\code{Logical} whether the ASQ window is visible or not}

\item{focus}{\code{Logical} whether the ASQ window has focus or not}

\item{idle}{\code{Logical} whether the user is idle or not.}

\item{input}{\code{Logical} whether the user is giving input or not.}

\item{submitted}{\code{Logical} whether the user has submitted or not.}
}
\value{
An updated timeline \code{data.frame} with the submitted state set.
}
\description{
Creates a 7 bit number according to the value
of the 7 vars: \code{hasExercise}, \code{visible}, \code{connected},
\code{focus}, \code{idle}, \code{input} and \code{submitted}.
}
