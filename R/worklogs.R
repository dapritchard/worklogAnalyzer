setClass(
  Class     = "worklogs_leaf",
  slots     = c(worklogs = "data.frame"),
  prototype = list(data.frame())
)

setClass(
  Class     = "worklogs_node",
  slots     = c(children = "list"),
  prototype = list(data.frame())
)

check_all_worklogs <- function(object) {
  # msgs <- c(
  #   chilren_not_list = "@children is required to be a list",
  #   children_not_named = "@children is required to be a named list"
  # )
  children_names <- names(object@children)
  if (! is.list(object@chilren)) {
    return("@children is required to be a list")
  }
  else if (is.null(children_names)) {
    return("@children is required to be a named list")
  }
  else if (length(unique(children_names)) != length(children_names)) {
    return("@children names must be unique")
  }
  else if (! all(map_lgl(object@children, is, "worklogs"))) {
    return("@children must all be worklogs")
  }
}

setValidity("worklogs_node", check_all_worklogs)

worklogs <- function(x) {
  stopifnot(is.list(x))
  if (is_worklogs(x)) {
    return(x)
  }
  else if (is.data.frame(x)) {
    return(mk_worklogs_leafs(x))
  }
  else {
    return(mk_worklogs_node(x))
  }
}

mk_worklogs_node <- function(worklogs_node) {
  assert_raw_worklogs_node(worklogs_node)
  structure(
    .Data = map(worklogs_node, worklogs),
    class = "worklogs_node"
  )
}

mk_worklogs_leafs <- function(worklogs_leaf) {
  add_class_info <- function(raw_leaf) {
    structure(
      .Data = raw_leaf,
      class = c("worklogs_leaf", class(worklogs_leaf))
    )
  }
  stopifnot(is.data.frame(worklogs_leaf))
  worklogs_leaf_tibble <- as_tibble(worklogs_leaf)
  raw_leafs <- split(worklogs_leaf_tibble, worklogs_leaf_tibble$task)
  structure(
    .Data = map(raw_leafs, add_class_info),
    class = "worklogs_node"
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

assert_string <- function(x) {
  stopifnot(
    is.character(x),
    length(x) == 1L,
    ! is.na(x)
  )
}

is_worklogs <- function(x) {
  is_worklogs_node(x) || is_worklogs_leaf(x)
}

is_worklogs_node <- function(x) {
  inherits(x, "worklogs_node")
}

is_worklogs_leaf <- function(x) {
  inherits(x, "worklogs_leaf")
}
