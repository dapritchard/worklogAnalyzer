% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/worklogs-filter-time.R
\name{filter_time}
\alias{filter_time}
\alias{filter_time_before}
\alias{filter_time_before,worklogs-method}
\alias{filter_time_after}
\alias{filter_time_after,worklogs-method}
\alias{filter_time_between}
\alias{filter_time_between,worklogs-method}
\alias{filter_this_week}
\alias{filter_this_week,worklogs-method}
\alias{filter_last_week}
\alias{filter_last_week,worklogs-method}
\title{Filter worklogs by timestamp}
\usage{
filter_time_before(wkls, datetime)

\S4method{filter_time_before}{worklogs}(wkls, datetime)

filter_time_after(wkls, datetime)

\S4method{filter_time_after}{worklogs}(wkls, datetime)

filter_time_between(wkls, before_datetime, after_datetime)

\S4method{filter_time_between}{worklogs}(wkls, before_datetime, after_datetime)

filter_this_week(wkls)

\S4method{filter_this_week}{worklogs}(wkls)

filter_last_week(wkls)

\S4method{filter_last_week}{worklogs}(wkls)
}
\arguments{
\item{wkls}{A \code{worklogs} object.}

\item{datetime}{A length-1 datetime.}

\item{before_datetime}{A length-1 datetime.}

\item{after_datetime}{A length-1 datetime.}
}
\description{
TODO: add description
}
