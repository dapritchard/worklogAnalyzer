setGeneric("extract_worklogs_impl",
  def       = function(wkls, path, parents) standardGeneric("extract_worklogs_impl"),
  signature = "wkls"
)

extract_worklogs_impl_node <- function(wkls, path, parents) {

  # Update the appropriate child of `wkls`
  extract_worklogs_impl <- function() {

    # Extract children
    children <- wkls@children

    # Update `path` and `parents` information for upcoming call
    stopifnot(length(path) >= 1L)
    child_name <- path[1L]
    new_path <- path[-1L]
    new_parents <- c(parents, child_name)

    # Extract the appropriate child
    child <- chuck(children, child_name)
    extract_worklogs_impl_node(child, new_path, new_parents)
  }

  `if`(
    length(path) == 0L,
    wkls,
    extract_worklogs_impl()
  )
}

setMethod("extract_worklogs_impl",
  signature  = "worklogs_node",
  definition = extract_worklogs_impl_node
)

extract_worklogs_impl_leaf <- function(wkls, path, parents) {
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
  wkls
}

setMethod("extract_worklogs_impl",
  signature  = "worklogs_leaf",
  definition = extract_worklogs_impl_leaf
)

#' Extract a Subtree From a Worklogs Tree
#'
#' @param wkls A `worklogs` object.
#' @param path A character vector such that each element in vector corresponds
#'
#' @export
setGeneric("extract_worklogs",
  def       = function(wkls, path) standardGeneric("extract_worklogs"),
  signature = "wkls"
)

#' @rdname extract_worklogs
#' @export
setMethod("extract_worklogs",
  signature  = "worklogs",
  definition = function(wkls, path) extract_worklogs_impl(wkls, path, "")
)
