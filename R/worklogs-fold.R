setGeneric("fold_this",
  def       = function(wkls) standardGeneric("fold_this"),
  signature = "wkls"
)

fold_this_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@fold_status <- "folded"
  validObject(wkls)
  wkls
}

setMethod("fold_this",
  signature  = "worklogs_node",
  definition = fold_this_node
)

fold_this_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls
}

setMethod("fold_this",
  signature  = "worklogs_leaf",
  definition = fold_this_leaf
)

#' Fold Worklogs Below a Given Location
#'
#' Nodes in a worklogs tree can be flagged as being _folded_, which has the
#' effect of letting functions that operate on worklogs know to consider all
#' descendents of that node to belong to that node.
#'
#' @param wkls A `worklogs` object.
#' @param path A character vector such that each element in vector corresponds
#'   to the name of an element in the `worklogs` tree.
#'
#' @return A `worklogs` object.
#' @export
setGeneric("fold",
  # TODO: consider calling this `fold_element` for better symmetry with `fold_children`
  def       = function(wkls, path) standardGeneric("fold"),
  signature = "wkls"
)

fold_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_this)
}

#' @rdname fold
#' @export
setMethod("fold",
  signature  = "worklogs",
  definition = fold_impl
)

setGeneric("fold_these_children",
  def       = function(wkls) standardGeneric("fold_these_children"),
  signature = "wkls"
)

fold_these_children_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@children <- map(wkls@children, fold_this)
  validObject(wkls)
  wkls
}

setMethod("fold_these_children",
  signature  = "worklogs_node",
  definition = fold_these_children_node
)

fold_these_children_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls
}
setMethod("fold_these_children",
  signature  = "worklogs_leaf",
  definition = fold_these_children_leaf
)

#' @rdname fold
#' @export
setGeneric("fold_children",
  def       = function(wkls, path) standardGeneric("fold_children"),
  signature = "wkls"
)

fold_children_impl <- function(wkls, path) {
  stopifnot(is(wkls, "worklogs"))
  update_child(wkls, path, character(0L), fold_these_children)
}

#' @rdname fold
#' @export
setMethod("fold_children",
  signature  = "worklogs",
  definition = fold_children_impl
)
