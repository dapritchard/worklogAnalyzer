worklogs <- function(x) {
  stopifnot(is.list(x) || is.data.frame(x))
  `if`(
    is.data.frame(x),
    mk_worklogs_leaf(x),
    mk_worklogs_node(x)
  )
}

mk_worklogs_leaf <- function(worklogs_leaf) {
  stopifnot(is.data.frame(worklogs_leaf))
  structure(
    .Data = worklogs_leaf,
    class = c("worklogs_leaf", class(worklogs_leaf))
  )
}

mk_worklogs_node <- function(worklogs_node) {
  stopifnot(is.list(worklogs_node))
  structure(
    .Data = map(worklogs_node, worklogs),
    class = "worklogs_node"
  )
}
