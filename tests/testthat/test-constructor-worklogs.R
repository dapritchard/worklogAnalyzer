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


# Derived data -----------------------------------------------------------------

both_install_r_dev_r_packages <- dplyr::bind_rows(install_r, dev_r_packages)
empty_install_r <- install_r[integer(0L), ]

altnames_source <- list(
  "XXXXX Setup development environment" = list(
    "XXXXX Install latest version of R"   = install_r,
    "XXXXX Install devtools and testthat" = dev_r_packages
  ),
  "XXXXX Run 'create_package' and 'use_testthat'" = create_package
)

altnames_install_r <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r,
  name     = "XXXXX Install latest version of R",
  config   = config
)

altnames_dev_r_packages <- new(
  Class    = "worklogs_leaf",
  worklogs = dev_r_packages,
  name     = "XXXXX Install devtools and testthat",
  config   = config
)

altnames_create_package <- new(
  Class    = "worklogs_leaf",
  worklogs = create_package,
  name     = "XXXXX Run 'create_package' and 'use_testthat'",
  config   = config
)

altnames_setup_dev <- new(
  Class       = "worklogs_node",
  children    = list(
    "XXXXX Install latest version of R"   = altnames_install_r,
    "XXXXX Install devtools and testthat" = altnames_dev_r_packages
  ),
  fold_status = "unfolded",
  prototype   = prototype
)

altnames_toplevel <- new(
  Class       = "worklogs_node",
  children    = list(
    "XXXXX Setup development environment"           = altnames_setup_dev,
    "XXXXX Run 'create_package' and 'use_testthat'" = altnames_create_package
  ),
  fold_status = "unfolded",
  prototype   = prototype
)


# Includes a leaf with 0 entries -----------------------------------------------

source_with_emptyleaf <- list("Install latest version of R" = empty_install_r)

wkls_empty_install_r <- new(
  Class    = "worklogs_leaf",
  worklogs = empty_install_r,
  name     = "Install latest version of R",
  config   = config
)

wkls_with_emptyleaf <- new(
  Class       = "worklogs_node",
  children    = list("Install latest version of R" = wkls_empty_install_r),
  fold_status = "unfolded",
  prototype   = prototype
)


# Includes a node with 0 children ----------------------------------------------

source_with_zerochildren <- list(
  "Zero children" = structure(list(), names = character(0L)),
  # "Setup development environment" = list(
  #   "Install latest version of R"   = install_r,
  #   "Install devtools and testthat" = dev_r_packages
  # ),
  "Run 'create_package' and 'use_testthat'" = create_package
)

wkls_zerochildren <- new(
  Class       = "worklogs_node",
  children    = structure(list(), names = character(0L)),
  fold_status = "unfolded",
  prototype   = prototype
)

wkls_with_zerochildren <- new(
  Class       = "worklogs_node",
  children    = list(
    "Zero children" = wkls_zerochildren,
    "Install latest version of R" = wkls_empty_install_r
  ),
  fold_status = "unfolded",
  prototype   = prototype
)



# Begin tests ------------------------------------------------------------------

test_that("`worklogs` data frame input", {

  # A data frame of worklog entries with 0 rows are an error because we can't
  # infer the name
  expect_error(worklogs(empty_install_r, FALSE, config))

  # TODO:
  # A data frame of worklog entries with one row is converted to a
  # `worklogs_node` with a single child

  # A data frame of worklog entries with multiple rows are converted to a
  # `worklogs_node` with a single child
  actual <- worklogs(install_r, FALSE, config)
  expected <- new(
    Class = "worklogs_node",
    children = structure(
      .Data = list(wkls_install_r),
      names = wkls_install_r@worklogs$description[1L]
    ),
    fold_status = "unfolded",
    prototype = prototype
  )
  expect_identical(actual, expected)
})

test_that("`worklogs` with `split_dfs` as `FALSE`", {

  # Basic application of `worklogs`
  actual <- worklogs(source_worklog, FALSE, config)
  expect_identical(actual, wkls_toplevel)

  # The name of a `worklogs_leaf` element is determined by the corresponding
  # element in the children list names in the parent, not by the description
  # field in the worklogs data frame
  actual <- worklogs(altnames_source, FALSE, config)
  expected <- altnames_toplevel
  expect_identical(actual, expected)

  # Lists are required to be named
  expect_error(worklogs(list(), FALSE, config))
  expect_error(worklogs(list(install_r), FALSE, config))

  # A worklogs leaf with 0 entries is allowed
  actual <- worklogs(source_with_emptyleaf, FALSE, config)
  expect_identical(actual, wkls_with_emptyleaf)

  # # FIXME
  # # A worklogs node with 0 children is allowed
  # actual <- worklogs(source_with_zerochildren, FALSE, config)
  # expect_identical(actual, wkls_with_zerochildren)
})

# We need at least one leaf in order to determine the worklog prototype
test_that("`worklogs` throws an error for trees without any leafs", {
  expect_error(worklogs(empty_list, FALSE, config))
  expect_error(worklogs(list(a = empty_list, b = empty_list), FALSE, config))
})

# TODO: need to test `split_dfs = TRUE`

