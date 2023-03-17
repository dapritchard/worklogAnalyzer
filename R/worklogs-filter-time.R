#' Filter worklogs by timestamp
#'
#' TODO: add description
#'
#' @param wkls A `worklogs` object.
#' @param datetime A length-1 datetime.
#' @param before_datetime A length-1 datetime.
#' @param after_datetime A length-1 datetime.
#'
#' @name filter_time
NULL

#' @export
#' @rdname filter_time
setGeneric("filter_time_before",
  def       = function(wkls, datetime) standardGeneric("filter_time_before"),
  signature = "wkls"
)

filter_time_before_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs[[start_label]]
    new_wkls_df <- wkls_df[wkls_df[[start_label]] < datetime, ]
    new(
      Class    = "worklogs_leaf",
      worklogs = new_wkls_df,
      name     = wkls@name,
      config   = wkls@config
    )
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_before",
  signature  = "worklogs",
  definition = filter_time_before_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_time_after",
  def       = function(wkls, datetime) standardGeneric("filter_time_after"),
  signature = "wkls"
)

filter_time_after_impl <- function(wkls, datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    new_wkls_df <- wkls_df[wkls_df[[start_label]] >= datetime, ]
    new(
      Class    = "worklogs_leaf",
      worklogs = new_wkls_df,
      name     = wkls@name,
      config   = wkls@config
    )
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_after",
  signature  = "worklogs",
  definition = filter_time_after_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_time_between",
  def = function(wkls, before_datetime, after_datetime)
    standardGeneric("filter_time_between"),
  signature = "wkls"
)

filter_time_between_impl <- function(wkls, before_datetime, after_datetime) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    is_after_start <- before_datetime <= wkls_df[[start_label]]
    is_before_end <- wkls_df[[start_label]] < after_datetime
    new_wkls_df <- wkls_df[is_after_start & is_before_end, ]
    new(
      Class    = "worklogs_leaf",
      worklogs = new_wkls_df,
      name     = wkls@name,
      config   = wkls@config
    )
  }
  stopifnot(
    is(wkls, "worklogs"),
    inherits(before_datetime, "POSIXct"),
    inherits(after_datetime, "POSIXct")
  )
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_time_between",
  signature  = "worklogs",
  definition = filter_time_between_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_this_week",
  def       = function(wkls) standardGeneric("filter_this_week"),
  signature = "wkls"
)

calc_this_week_start <- function(datetime) {
  n_day <- lubridate::wday(datetime)
  datetime - lubridate::days(n_day - 1)
}

# calc_this_week_end <- function(datetime) {
#   n_day <- lubridate::wday(datetime)
#   datetime + days(8 - n_day)
# }

filter_this_week_impl <- function(wkls) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    new_wkls_df <- wkls_df[wkls_df[[start_label]] >= this_week_start, ]
    new(
      Class    = "worklogs_leaf",
      worklogs = new_wkls_df,
      name     = wkls@name,
      config   = wkls@config
    )
  }
  stopifnot(is(wkls, "worklogs"))
  this_week_start <- calc_this_week_start(lubridate::today())
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_this_week",
  signature  = "worklogs",
  definition = filter_this_week_impl
)

#' @rdname filter_time
#' @export
setGeneric("filter_last_week",
  def       = function(wkls) standardGeneric("filter_last_week"),
  signature = "wkls"
)

calc_last_week_start <- function(datetime) {
  n_day <- lubridate::wday(datetime)
  datetime - lubridate::days(n_day + 6)
}

filter_last_week_impl <- function(wkls) {
  filter_fcn <- function(wkls) {
    start_label <- wkls@config@labels@start
    wkls_df <- wkls@worklogs
    is_after_start <- last_week_start <= wkls_df[[start_label]]
    is_before_end <- wkls_df[[start_label]] < this_week_start
    new_wkls_df <- wkls_df[is_after_start & is_before_end, ]
    new(
      Class    = "worklogs_leaf",
      worklogs = new_wkls_df,
      name     = wkls@name,
      config   = wkls@config
    )
  }
  stopifnot(is(wkls, "worklogs"))
  today_datetime <- lubridate::today()
  last_week_start <- calc_last_week_start(today_datetime)
  this_week_start <- calc_this_week_start(today_datetime)
  update_worklogs(wkls, filter_fcn)
}

#' @rdname filter_time
#' @export
setMethod("filter_last_week",
  signature  = "worklogs",
  definition = filter_last_week_impl
)

mk_effort_info_df <- function(tree_components) {
  tibble(
    effort  = map_chr(tree_components, "effort"),
    percent = map_chr(tree_components, "percent"),
    depth   = map_int(tree_components, "depth")
  )
}
