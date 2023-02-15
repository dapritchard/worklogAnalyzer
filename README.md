worklogAnalyzer
===============

Analyze your worklog (timesheet) history

A basic example
---------------

``` r
library(lubridate)
library(tibble)

parse_dt <- function(x) {
  parse_date_time(x, "Ymd HM")
}

install_r = tibble(
  description = rep("Install latest version of R", 2L),
  start = parse_dt(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end   = parse_dt(c("2023-02-14 21:24", "2023-02-15 18:33"))
)
```
