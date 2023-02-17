worklogAnalyzer
===============

Analyze your worklog history.

Definition of a worklog (timesheet)
-----------------------------------

The terms *worklog* and *timesheet* appear to be used interchangeably in
common usage to refer to a record of a contiguous block of time in which
a single task was performed; this project uses the term worklog
throughout.

Representing worklogs
---------------------

Worklog hierarchy
-----------------

This project views worklogs as being naturally represented as a
hierarchical structure. For example, you can imagine your worklogs
history having multiple projects, where each project has its own tasks,
and those tasks possibly having their own subtasks, and so on. Of
course, if you don’t want to represent your worklogs in a hierarchical
structure then you can put all of your worklogs in a single level.

Representing worklogs in a hierarchical manner provides several nice
features. It may be easier to understand the structure of your worklogs
by displaying the relationship between the worklogs in this manner. You
can easily focus on certain subsets of your worklogs history by
selecting a subtree of the history. And you can easily zoom out and zoom
in on the level of detail that you want to view your worklogs in by
considering any worklogs in a given subtree to all belong to the same
level in the hierarchy or vice versa.

Worklog tags
------------

Sometimes relationships between worklogs cut across the hierarchical
structure that you’ve chosen for your worklogs. For example, suppose you
want to add up the amount of time that you’ve spent doing code reviews
across your various projects. For this purpose the project has the
concept of worklogs being tagged, meaning that each worklog has 0 or
identifiers that can be used to filter on or categorize a worklog.

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
