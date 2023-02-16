worklogAnalyzer
===============

Analyze your worklog (timesheet) history

A basic example
---------------

``` r
library(lubridate)
library(tibble)
library(worklogAnalyzer)

parse_ymdhm <- function(x) {
  parse_date_time(x, "Ymd HM")
}

install_r <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start 
)

dev_r_packages <- tibble(
  description = "Install devtools and testthat",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("2023-02-15 18:02"),
  duration    = end - start 
)

create_package <- tibble(
  description = "Run 'create_package' and ''use_testthat'",
  start       = parse_ymdhm("2023-02-15 18:02"),
  end         = parse_ymdhm("2023-02-15 18:09"),
  duration    = end - start 
)

source_worklog <- list(
  "Setup development env" = list(
    "Install newest R"             = install_r,
    "Install development packages" = dev_r_packages
  ),
  "Create R package skeleton" = create_package
)

config <- worklogs_config(
  description_label = "description",
  start_label       = "start",
  end_label         = "end",
  duration          = "duration"
)

wkls <- worklogs(source_worklog, FALSE, config)

wkls
```

    ## .
    ## ├── Setup development env
    ## │  ├── Install newest R
    ## │  └── Install development packages
    ## └── Create R package skeleton

``` r
effort_summary(wkls)
```

    ##                                      Effort proportion
    ## .                                    ─────────────────
    ## ├── Setup development env            ┌── 85%
    ## │  ├── Install newest R              │         ┌── 71%
    ## │  └── Install development packages  │         └── 15%
    ## └── Create R package skeleton        └── 15%
