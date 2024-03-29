% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate-by-period.R
\name{aggregate_by_period}
\alias{aggregate_by_period}
\title{Aggregate Effort Totals Across Tags and Time Periods}
\usage{
aggregate_by_period(
  wkls,
  config,
  tags_mapping,
  start_time,
  period,
  directions,
  default = NA_character_
)
}
\arguments{
\item{wkls}{A \code{worklogs} object.}

\item{config}{A \code{worklogs_config} object.}

\item{tags_mapping}{A named character vector providing a mapping between
existing worklog entries tags and new categories. In more detail, if a
given worklog entry with a corresponding set of tags has a tag in one of
the elements of \code{tags_mapping}, then the worklog entry is mapped to a
category with the name given by the corresponding entry in the
\code{tags_mapping} names. In the event that there are tags corresponding to a
given worklogs entry that are elements of multiple entries in
\code{tags_mapping} then the earlier entry in \code{tags_mapping} is given
precedence.}

\item{start_time}{A datetime from which the periods are counted either
forward, backwards, or in both directions, as specified by \code{directions}.}

\item{period}{A duration specifying the duration of the time periods.}

\item{directions}{Either \code{"before"}, \code{"after"}, or \code{"both"}.}

\item{default}{Either \code{NA_character_} or a string. In the case that
\code{NA_character_} is provided, then in the event that the tags associated
with a given worklogs entry don't have a match in \code{tags_mapping} then an
error is thrown, whereas if a string is provided then the tags associated
with a given worklogs entry don't have a match in \code{tags_mapping} then the
worklogs entry is mapped to the value of \code{default}.}
}
\description{
Aggregate Effort Totals Across Tags and Time Periods
}
