worklogs <- function(x) {
  stopifnot(is.list(x) || is.data.frame(x))
  `if`(
    is.data.frame(x),
    mk_worklogs_leaf(x),
    mk_worklogs_node(x)
  )
}

mk_worklogs_node <- function(worklogs_node) {
  assert_raw_worklogs_node(worklogs_node)
  structure(
    .Data = map(worklogs_node, worklogs),
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
    is.list(x),
    ! is.null(names(x))
  )
}

assert_raw_worklogs_leaf <- function(x) {
  stopifnot(
    is.data.frame(x)
  )
}

check_string <- function(x) {
  stopifnot(
    is.character(x),
    length(x) == 1L,
    ! is.na(x)
  )
}

is_worklogs_node <- function(x) {
  inherits(x, "worklogs_node")
}

is_worklogs_leaf <- function(x) {
  inherits(x, "worklogs_leaf")
}
