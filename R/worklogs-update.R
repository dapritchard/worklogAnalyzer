setGeneric("update_child",
  def       = function(wkls, path, parents, f) standardGeneric("update_child"),
  signature = "wkls"
)

update_child_node <- function(wkls, path, parents, f) {

  # Update the appropriate child of `wkls`
  update_child <- function() {

    # Extract children
    children <- wkls@children

    # Update `path` and `parents` information for upcoming call
    stopifnot(length(path) >= 1L)
    child_name <- path[1L]
    new_path <- path[-1L]
    new_parents <- c(parents, child_name)

    # Update the appropriate child
    child <- chuck(children, child_name)
    new_child <- update_child_node(child, new_path, new_parents, f)
    stopifnot(is(new_child, "worklogs"))
    validObject(new_child)

    # Update the `children` slot and return `wkls`
    pluck(children, child_name) <- new_child
    wkls@children <- children
    validObject(wkls)
    wkls
  }

  `if`(
    length(path) == 0L,
    f(wkls),
    update_child()
  )
}

setMethod("update_child",
  signature  = "worklogs_node",
  definition = update_child_node
)

update_child_leaf <- function(wkls, path, parents, f) {
  if (length(path) >= 1L) {
    msg <- sprintf(
      "%s\n%s%s\n%s",
      "Can't get the child of a leaf node with the following parents:",
      sprintf("    %s\n", path),
      "The following children were asked for: ",
      sprintf("    %s\n", path)
    )
    stop(msg)
  }
  f(wkls)
}

setMethod("update_child",
  signature  = "worklogs_leaf",
  definition = update_child_leaf
)

# TODO: how does `update_worklogs` compare to `update_child`? Is there a
# consistent naming scheme that can be used for the two?

setGeneric("update_worklogs",
  def       = function(wkls, f) standardGeneric("update_worklogs"),
  signature = "wkls"
)

update_worklogs_node <- function(wkls, f) {
  stopifnot(is(wkls, "worklogs_node"))
  wkls@children <- map(wkls@children, update_worklogs, f = f)
  wkls
}

setMethod("update_worklogs",
  signature  = "worklogs_node",
  definition = update_worklogs_node
)

update_worklogs_leaf <- function(wkls, f) {
  stopifnot(is(wkls, "worklogs_leaf"))
  f(wkls)
}

setMethod("update_worklogs",
  signature  = "worklogs_leaf",
  definition = update_worklogs_leaf
)
