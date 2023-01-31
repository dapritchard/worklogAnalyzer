partition_by_intervals <- function(worklogs_df,
                                   start_time,
                                   period,
                                   directions,
                                   config) {
  filter_worklogs_by_time <- function(before_time, after_time) {
    keep_lgl <- (before_time <= worklogs_start) & (worklogs_start < after_time)
    worklogs_df[keep_lgl, ]
  }
  stopifnot(
    # TODO: other assertions
    directions %in% c("before", "after", "both")
  )
  worklogs_start <- worklogs_df[[config@labels@start]]
  breakpoints <- calc_breakpoints(
    worklogs_start,
    start_time,
    period, directionsi
  )
  breakpoint_pairs <- tibble(
    before_time = head(breakpoints, -1),
    after_time  = breakpoints[-1]
  )
  pmap(breakpoint_pairs, filter_worklogs_by_time)
}

calc_breakpoints <- function(worklogs_start,
                             start_time,
                             period,
                             directions) {
  breakpoints_before <- `if`(
    directions %in% c("before", "both"),
    calc_breakpoints_before(min(worklogs_start), start_time, period),
    parse_date_time(character(0L))
  )
  breakpoints_after <- `if`(
    directions %in% c("after", "both"),
    calc_breakpoints_after(start_time, max(worklogs_start), period),
    start_time
  )
  c(breakpoints_before, breakpoints_after)
}

calc_breakpoints_before <- function(min_time, start_time, period) {
  if (min_time >= start_time) {
    return(parse_date_time(character(0L)))
  }
  interval_val <- interval(min_time, start_time)
  n_periods <- ceiling(interval_val / period)
  sort(start_time - (seq(1, n_periods, 1) * period))
}

calc_breakpoints_after <- function(start_time, time_after, period) {
  if (start_time > time_after) {
    return(start_time)
  }
  interval_val <- interval(start_time, time_after)
  n_periods_orig <- interval_val / period
  n_periods <- `if`(
    floor(n_periods_orig) == n_periods_orig,
    n_periods_orig + 1,
    ceiling(n_periods_orig)
  )
  start_time + ((0 : n_periods) * period)
}

calc_breakpoints(
  parse_date_time(c("20220101", "20220103", "20220105"), "ymd"),
  parse_date_time("2022010406", "ymdh"),
  hours(8),
  "both"
)
