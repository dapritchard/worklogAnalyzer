partition_by_intervals <- function(worklogs,
                                   period,
                                   start,
                                   directions,
                                   config) {
  stopifnot(
    directions %in% c("before", "after", "both")
  )

  is_before <- directions %in% c("before", "both")
  is_after <- directions %in% c("after", "both")

  worklogs_start <- worklogs[[config@labels@start]]
  earliest_start <- min(worklogs_start)
  latest_start <- max(worklogs_start)

  if (is_before) {
    before_dfs <- list()
  }
  else {
    before_dfs <- list()
  }

  if (is_after) {
    after_dfs <- list()
  }
  else {
    after_dfs <- list()
  }

  c(before_dfs, after_dfs)
}

# calc_breakpoints_before <- function(earliest_start, start, period) {
#   n_periods <- 0L
#   current_start
#   while (earliest_start <= start) {

#   }
# }

calc_breakpoints_before <- function(time_before, time_after, period) {
  if (time_before >= time_after) {
    return(time_after)
  }
  interval_val <- interval(time_before, time_after)
  n_periods <- ceiling(interval_val / period)
  sort(time_after - ((0 : n_periods) * period))
}

calc_breakpoints_after <- function(time_before, time_after, period) {
  if (time_before > time_after) {
    return(time_before)
  }
  interval_val <- interval(time_before, time_after)
  n_periods_orig <- interval_val / period
  n_periods <- `if`(
    floor(n_periods_orig) == n_periods_orig,
    n_periods_orig + 1,
    ceiling(n_periods_orig)
  )
  time_before + ((0 : n_periods) * period)
}
