subtree_extract <- function(worklogs_node, path) {
  stopifnot(
    is_worklogs_node(worklogs_node),
    is_chr_nomiss(path)
  )
  if (length(path) == 0L) {
    return(worklogs_node)
  }
  else if (length(path) == 1L) {
    child <- chuck(worklogs_node, path[1L])
    if (is_worklogs_node(child)) {
      return(child)
    }
    else if (is_worklogs_leaf(child)) {
      return(worklogs_node[path[1L]])
    }
    else {
      msg <- sprintf(
        "Invalid input data. The %s child is not a worklogs type",
        path[1L]
      )
      stop(msg, call. = FALSE)
    }
  }
  else {
    child <- chuck(worklogs_node, path[1L])
    if (is_worklogs_node(child)) {
      return(subtree_extract(child, path[-1L]))
    }
    else if (is_worklogs_leaf(child)) {
      stop("Cannot take the child of a leaf", call. = FALSE)
    }
    else {
      msg <- sprintf(
        "Invalid input data. The %s child is not a worklogs type",
        path[1L]
      )
      stop(msg, call. = FALSE)
    }
  }
}
