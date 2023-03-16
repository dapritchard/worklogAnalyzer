setGeneric("remove_worklogs_impl",
  def       = function(wkls, path, parents) standardGeneric("remove_worklogs_impl"),
  signature = "wkls"
)

remove_worklogs_impl_node <- function(wkls, path, parents) {

  stopifnot(
    is(wkls, "worklogs_node"),
    is_chr_nomiss(path),
    length(path) >= 1L,
    is_chr_nomiss(parents)
  )

  # Extract children
  children <- wkls@children
  children_names <- names(children)
  stopifnot(is_chr_nomiss_norepeat(children_names))

  # Update `path` and `parents` information for upcoming call
  stopifnot(length(path) >= 1L)
  child_name <- path[1L]
  new_path <- path[-1L]
  new_parents <- c(parents, child_name)

  # TODO comment
  path_match_lgl <- (child_name == children_names)
  n_path_matches <- sum(path_match_lgl)

  # No matches signifies a user error. Two or more matches is impossible to
  # reach under a well-formed `worklogs_node` object
  if (n_path_matches == 0L) {
    msg <- sprintf(
      paste0(
        "\nThe child '%s' with the following parents does not exist:\n",
        "%s\n\n",
        "The following children were asked for:\n",
        "%s\n"
      ),
      child_name,
      `if`(
        length(parents) >= 1L,
        paste0("    ", parents, collapse = "\n"),
        "    <top level>"
      ),
      paste0("    ", c(parents, path), collapse = "\n")
    )
    stop(msg)
  }
  stopifnot(n_path_matches == 1L)

  # TODO comment
  if (length(path) == 1L) {
    children <- children[! path_match_lgl]
  }
  else {
    child <- remove_worklogs_impl(
      wkls    = children[[child_name]],
      path    = new_path,
      parents = new_parents
    )
    children[[child_name]] <- child
  }

  # TODO comment
  new(
    Class       = "worklogs_node",
    children    = children,
    fold_status = wkls@fold_status,
    prototype   = wkls@prototype
  )
}

setMethod("remove_worklogs_impl",
  signature  = "worklogs_node",
  definition = remove_worklogs_impl_node
)

remove_worklogs_impl_leaf <- function(wkls, path, parents) {
  msg <- sprintf(
    paste0(
      "\nCan't get the child of a leaf node with the following path:\n",
      "%s\n\n",
      "The path of the worklog that was asked for was the following:\n",
      "%s\n"
    ),
    paste0("    ", parents, collapse = "\n"),
    paste0("    ", c(parents, path), collapse = "\n")
  )
  stop(msg)
}

setMethod("remove_worklogs_impl",
  signature  = "worklogs_leaf",
  definition = remove_worklogs_impl_leaf
)

#' Remove a Subtree From a Worklogs Tree
#'
#' @param wkls A `worklogs` object.
#' @param path A character vector such that each element in vector corresponds
#'
#' @export
setGeneric("remove_worklogs",
  def       = function(wkls, path) standardGeneric("remove_worklogs"),
  signature = "wkls"
)

remove_worklogs_node <- function(wkls, path) {
  stopifnot(
    is(wkls, "worklogs_node"),
    is_chr_nomiss(path)
  )
  if (length(path) == 0L) {
    wkls@children <- structure(list(), names = character(0L))
  }
  else {
    wkls <- remove_worklogs_impl(wkls, path, character(0L))
  }
  wkls
}

#' @rdname remove_worklogs
#' @export
setMethod("remove_worklogs",
  signature  = "worklogs_node",
  definition = remove_worklogs_node
)

remove_worklogs_leaf <- function(wkls, path) {
  stopifnot(
    is(wkls, "worklogs_leaf"),
    is_chr_nomiss(path)
  )
  if (length(path) == 0L) {
    wkls <- new(
      Class       = "worklogs_node",
      children    = structure(list(), names = character(0L)),
      fold_status = "unfolded",
      prototype   = wkls@worklogs[numeric(0L), ]
    )
  }
  else {
    wkls_node <- create_node_from_leaf(wkls)
    wkls <- remove_worklogs_impl_node(wkls_node, path, character(0L))
  }
  wkls
}

#' @rdname remove_worklogs
#' @export
setMethod("remove_worklogs",
  signature  = "worklogs_leaf",
  definition = remove_worklogs_leaf
)
