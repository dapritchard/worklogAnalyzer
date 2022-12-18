worklogs <- function(x) {
  stopifnot(is.list(x) || is.data.frame(x))
  `if`(
    is.data.frame(x),
    mk_worklogs_leaf(x),
    mk_worklogs_node(x)
  )
}

mk_worklogs_node <- function(worklogs_node) {
  assert_raw_worklogs_leaf(worklogs_node)
  structure(
    .Data = list(
      name     = worklogs_node$name,
      children = map(worklogs_node$children, worklogs)
    ),
    class = "worklogs_node"
  )
}

mk_worklogs_leaf <- function(worklogs_leaf) {
  stopifnot(is.data.frame(worklogs_leaf))
  structure(
    .Data = worklogs_leaf,
    class = c("worklogs_leaf", class(worklogs_leaf))
  )
}

assert_raw_worklogs_node <- function(x) {
  stopifnot(
    is.list(worklogs_node),
    ! is.null(names(x)),
    "name" %in% names(x),
    check_string(x$name),
    "children" %in% names(x),
    is.list(x$children)
  )
}

assert_raw_worklogs_leaf <- function(x) {
  stopifnot(
    is.data.frame(x)
  )
}

check_string <- function(x) {
  is.character(x),
  length(x) == 1L,
  ! is.na(x)
}
