install_r <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start
)

install_r_withintercept <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start,
  intercept   = 1
)

dev_r_packages <- tibble(
  description = "Install devtools and testthat",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("2023-02-15 18:02"),
  duration    = end - start
)

prototype <- dev_r_packages[integer(0L), ]

wkls_install_r <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r,
  name     = "Install latest version of R",
  config   = config
)

wkls_install_r_withintercept <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r_withintercept,
  name     = "Install latest version of R",
  config   = config
)

wkls_dev_r_packages <- new(
  Class    = "worklogs_leaf",
  worklogs = dev_r_packages,
  name     = "Install devtools and testthat",
  config   = config
)

test_that("`new` for `worklogs_node` throws an error for invalid input", {

  # # FIXME
  # # Invalid `fold_status` inputs for `worklogs_node` construction causes an
  # # error to be thrown
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = character(0L),
  #     prototype = prototype
  #   )
  # )
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = NA_character_,
  #     prototype = prototype
  #   )
  # )
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = c("folded", NA_character_),
  #     prototype = prototype
  #   )
  # )
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = c("folded", "unfolded"),
  #     prototype = prototype
  #   )
  # )
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = "bunkvariant",
  #     prototype = prototype
  #   )
  # )

  # Lists need to be named
  expect_error(
    new(
      Class = "worklogs_node",
      children = list(),
      fold_status = "unfolded",
      prototype = prototype
    )
  )

  # # Prototype must be a data frame with 0 rows
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = structure(list(), names = character(0L)),
  #     fold_status = "unfolded",
  #     prototype = tibble(x = 1)
  #   )
  # )

  # # Prototypes must be consistent for two leafs
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = list(
  #       "Install latest version of R"   = wkls_install_r_withintercept,
  #       "Install devtools and testthat" = wkls_dev_r_packages
  #     ),
  #     fold_status = "unfolded",
  #     prototype = prototype
  #   )
  # )

  # Prototypes must be consistent for a leaf and a node
  expect_error(
    new(
      Class = "worklogs_node",
      children = list(
        "Install latest version of R"   = wkls_install_r,
        "Install devtools and testthat" = wkls_dev_r_packages,
        "Empty node" = new(
          Class = "worklogs_node",
          children = structure(list(), names = character(0L)),
          fold_status = "unfolded",
          prototype = tibble()
        )
      ),
      fold_status = "unfolded",
      prototype = prototype
    )
  )

  # # Prototypes must be consistent with a parent and child
  # expect_error(
  #   new(
  #     Class = "worklogs_node",
  #     children = list("Install latest version of R" = wkls_install_r_withintercept),
  #     fold_status = "unfolded",
  #     prototype = prototype
  #   )
  # )
})

test_that("`new` for `worklogs_leaf` throws an error for invalid input", {

  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = dev_r_packages,
  #     name     = character(0L),
  #     config   = config
  #   )
  # )
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = dev_r_packages,
  #     name     = 1,
  #     config   = config
  #   )
  # )
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = dev_r_packages,
  #     name     = NA_character_,
  #     config   = config
  #   )
  # )
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = dev_r_packages,
  #     name     = c("name1", "name2"),
  #     config   = config
  #   )
  # )

  # TODO: ensure that worklogs are consistent with `config`
})
