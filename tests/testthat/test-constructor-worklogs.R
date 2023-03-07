parse_ymdhm <- function(x) {
  lubridate::parse_date_time(x, "Ymd HM")
}


# Source data ------------------------------------------------------------------

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
  description = "Run 'create_package' and 'use_testthat'",
  start       = parse_ymdhm("2023-02-15 18:02"),
  end         = parse_ymdhm("2023-02-15 18:09"),
  duration    = end - start
)

source_worklog <- list(
  "Setup development environment" = list(
    "Install latest version of R"   = install_r,
    "Install devtools and testthat" = dev_r_packages
  ),
  "Run 'create_package' and 'use_testthat'" = create_package
)

config <- worklogs_config(
  description_label = "description",
  start_label       = "start",
  end_label         = "end",
  duration          = "duration"
)

empty_list <- structure(list(), names = character(0L))


# Target Objects ---------------------------------------------------------------

prototype <- install_r[integer(0L), ]

wkls_install_r <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r,
  name     = "Install latest version of R",
  config   = config
)

wkls_dev_r_packages <- new(
  Class    = "worklogs_leaf",
  worklogs = dev_r_packages,
  name     = "Install devtools and testthat",
  config   = config
)

wkls_create_package <- new(
  Class    = "worklogs_leaf",
  worklogs = create_package,
  name     = "Run 'create_package' and 'use_testthat'",
  config   = config
)

wkls_setup_dev <- new(
  Class       = "worklogs_node",
  children    = list(
    "Install latest version of R"   = wkls_install_r,
    "Install devtools and testthat" = wkls_dev_r_packages
  ),
  fold_status = "unfolded",
  prototype   = prototype
)

wkls_toplevel <- new(
  Class       = "worklogs_node",
  children    = list(
    "Setup development environment"           = wkls_setup_dev,
    "Run 'create_package' and 'use_testthat'" = wkls_create_package
  ),
  fold_status = "unfolded",
  prototype   = prototype
)


# Begin tests ------------------------------------------------------------------

test_that("`worklogs` creates a multiple level worklogs tree", {
  actual <- worklogs(source_worklog, FALSE, config)
  expect_identical(actual, wkls_toplevel)
})

test_that("`worklogs` throws an error for trees without any leafs", {
  expect_error(worklogs(empty_list, FALSE, config))
  expect_error(worklogs(list(a = empty_list, b = empty_list), FALSE, config))
})

# # TODO: make into test cases
# test_that("remove_worklogs", {
#   remove_worklogs(wkls_install_r, character(0L))
#   remove_worklogs(wkls_toplevel, character(0L))
#   remove_worklogs(wkls_install_r, "Install latest version of R")
#   expect_error(remove_worklogs(wkls_install_r, "bunk name"))
#   remove_worklogs(wkls, "Setup development environment")
#   remove_worklogs(wkls, c("Setup development environment", "Install latest version of R"))
#   remove_worklogs(wkls, c("Setup development environment", "Install devtools and testthat"))
#   remove_worklogs(wkls, "Run 'create_package' and 'use_testthat'")
# })
