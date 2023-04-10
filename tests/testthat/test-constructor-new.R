library(lubridate)

parse_ymdhm <- function(x) {
  lubridate::parse_date_time(x, "Ymd HM")
}


worklogs_config_withdefaults <- function(description_label = NULL,
                                         start_label = NULL,
                                         end_label = NULL,
                                         duration_label = NULL,
                                         tags_label = NULL) {
  if (is.null(description_label)) description_label <- "description"
  if (is.null(start_label)) start_label <- "description"
  if (is.null(end_label)) end_label <- "description"
  if (is.null(duration_label)) duration_label <- "description"
  if (is.null(tags_label)) tags_label <- "description"
  worklogs_config(
    description_label = description_label,
    start_label       = start_label,
    end_label         = end_label,
    duration_label    = duration_label,
    tags_label        = tags_label
  )
}

install_r <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start
)

install_r_withtags <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start,
  tags        = list(character(0L))
)

install_r_withextra <- tibble(
  description = "Install latest version of R",
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start,
  tags        = list(character(0L)),
  extra       = 1,
  baddescription = c("Install latest version of R", "not the same"),
  badstart    = parse_ymdhm(c("2023-02-14 20:12", NA_character_)),
  badend      = parse_ymdhm(c("2023-02-14 20:30", NA_character_)),
  badduration = duration - 1000000,
  badtags     = list("some tag", NA_character_)
)

dev_r_packages <- tibble(
  description = "Install devtools and testthat",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("2023-02-15 18:02"),
  duration    = end - start
)

diff_discriptions <- tibble(
  description = c("Some description", "Another description"),
  start       = parse_ymdhm(c("2023-02-14 20:12", "2023-02-15 18:17")),
  end         = parse_ymdhm(c("2023-02-14 20:30", "2023-02-15 18:33")),
  duration    = end - start,
  tags        = list(character(0L))
)

end_before_start_noduration <- tibble(
  description = "End before start",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("1923-02-15 18:02")
)

end_before_start_withduration <- tibble(
  description = "End before start",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("1923-02-15 18:02"),
  duration    = end - start,
)

not_all_consistent <- tibble(
  description = "End before start",
  start       = parse_ymdhm("2023-02-15 17:55"),
  end         = parse_ymdhm("1923-02-15 18:02"),
  duration    = end - start + 1,
)

config <- worklogs_config(
  description_label = "description",
  start_label       = "start",
  end_label         = "end",
  duration          = "duration"
)

noduration_config <- worklogs_config(
  description_label = "description",
  start_label       = "start",
  end_label         = "end",
  duration          = NULL
)

prototype <- dev_r_packages[integer(0L), ]

wkls_install_r <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r,
  name     = "Install latest version of R",
  config   = config
)

wkls_install_r_withtags <- new(
  Class    = "worklogs_leaf",
  worklogs = install_r_withtags,
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

  # Invalid `fold_status` inputs for `worklogs_node` construction causes an
  # error to be thrown
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = character(0L),
      prototype = prototype
    )
  )
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = NA_character_,
      prototype = prototype
    )
  )
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = c("folded", NA_character_),
      prototype = prototype
    )
  )
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = c("folded", "unfolded"),
      prototype = prototype
    )
  )
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = "bunkvariant",
      prototype = prototype
    )
  )

  # Lists need to be named
  expect_error(
    new(
      Class = "worklogs_node",
      children = list(),
      fold_status = "unfolded",
      prototype = prototype
    )
  )

  # Prototype must be a data frame with 0 rows
  expect_error(
    new(
      Class = "worklogs_node",
      children = structure(list(), names = character(0L)),
      fold_status = "unfolded",
      prototype = tibble(x = 1)
    )
  )

  # Prototypes must be consistent for two leafs
  expect_error(
    new(
      Class = "worklogs_node",
      children = list(
        "Install latest version of R"   = wkls_install_r_withtags,
        "Install devtools and testthat" = wkls_dev_r_packages
      ),
      fold_status = "unfolded",
      prototype = prototype
    )
  )

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

  # Prototypes must be consistent with a parent and child
  expect_error(
    new(
      Class = "worklogs_node",
      children = list("Install latest version of R" = wkls_install_r_withtags),
      fold_status = "unfolded",
      prototype = prototype
    )
  )
})

test_that("`new` for `worklogs_leaf` throws an error for invalid input", {

  expect_error(
    new(
      Class    = "worklogs_leaf",
      worklogs = dev_r_packages,
      name     = character(0L),
      config   = config
    )
  )
  expect_error(
    new(
      Class    = "worklogs_leaf",
      worklogs = dev_r_packages,
      name     = NA_character_,
      config   = config
    )
  )
  expect_error(
    new(
      Class    = "worklogs_leaf",
      worklogs = dev_r_packages,
      name     = c("name1", "name2"),
      config   = config
    )
  )

  # Ensure that columns specified in `config` exist in the worklogs data frame
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = dev_r_packages,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        description_label = "bunkdescription"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = dev_r_packages,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        start_label = "bunkstart"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = dev_r_packages,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        end_label = "bunkend"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = dev_r_packages,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        duration_label = "bunkduration"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = dev_r_packages,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        tags_label = "notagscolumn"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withtags,
      name = "Install latest version of R",
      config = worklogs_config_withdefaults(
        tags_label = "bunktags"
      )
    )
  )

  # Ensure that columns specified in `config` have the right type
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        description_label = "extra"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        start_label = "extra"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        end_label = "extra"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        duration_label = "extra"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install devtools and testthat",
      config = worklogs_config_withdefaults(
        tags_label = "extra"
      )
    )
  )
  expect_error(
    new(
      Class = "worklogs_leaf",
      worklogs = install_r_withextra,
      name = "Install latest version of R",
      config = worklogs_config_withdefaults(
        tags_label = "extra"
      )
    )
  )

  # Ensure that all description elements are equal
  expect_error(
    new(
      Class    = "worklogs_leaf",
      worklogs = diff_discriptions,
      name     = "Who knows?",
      config   = config
    )
  )

  # # Ensure that all starting dates are no later than the corresponding ending
  # # dates
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = end_before_start_noduration,
  #     name     = "End before start no duration",
  #     config   = noduration_config
  #   )
  # )
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = end_before_start_withduration,
  #     name     = "End before start with duration",
  #     config   = withduration_config
  #   )
  # )

  # TODO: if we're given duration paired with only 1 or start or end make sure
  # that duration is nonnegative

  # # Ensure that start, end, and duration are all consistent
  # expect_error(
  #   new(
  #     Class    = "worklogs_leaf",
  #     worklogs = not_all_consistent,
  #     name     = "Not all consistent",
  #     config   = config
  #   )
  # )
})
