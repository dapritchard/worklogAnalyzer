setGeneric("extract_prototype",
  def       = function(wkls) standardGeneric("extract_prototype"),
  signature = "wkls"
)

extract_prototype_worklogs_node <- function(wkls) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@prototype
}

setMethod("extract_prototype",
  signature  = "worklogs_node",
  definition = extract_prototype_worklogs_node
)

extract_prototype_worklogs_leaf <- function(wkls) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls@worklogs[integer(0L), ]
}

setMethod("extract_prototype",
  signature  = "worklogs_leaf",
  definition = extract_prototype_worklogs_leaf
)

setGeneric("add_prototype",
  def       = function(wkls, prototype) standardGeneric("add_prototype"),
  signature = "wkls"
)

add_prototype_no_prototype_node <- function(wkls, prototype) {
  stopifnot(is(wkls, "no_prototype_node"))
  new_children <- map(wkls@children, add_prototype, prototype = prototype)
  new("worklogs_node", new_children, wkls@fold_status, prototype)
}

setMethod("add_prototype",
  signature  = "no_prototype_node",
  definition = add_prototype_no_prototype_node
)

add_prototype_worklogs_node <- function(wkls, prototype) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls
}

setMethod("add_prototype",
  signature  = "worklogs_node",
  definition = add_prototype_worklogs_node
)

add_prototype_worklogs_leaf <- function(wkls, prototype) {
  stopifnot(is(wkls, "worklogs_leaf"))
  wkls
}

setMethod("add_prototype",
  signature  = "worklogs_leaf",
  definition = add_prototype_worklogs_leaf
)
