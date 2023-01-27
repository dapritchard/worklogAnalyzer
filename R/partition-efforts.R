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
