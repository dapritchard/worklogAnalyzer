add_hierarchy <- function(x, hierarchy) {
  stopifnot(is.list(x) || is.data.frame(x))
  `if`(
    is.data.frame(x),
    add_hierarchy_leaf(x, hierarchy),
    add_hierarchy_node(x, hierarchy)
  )
}

add_hierarchy_node <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_node(worklogs_node),
    is.list(hierarchy) || is.character(hierarchy)
  )
  `if`(
    is.list(hierarchy),
    add_hierarchy_node_list(worklogs_node, hierarchy),
    add_hierarchy_node_char(worklogs_node, hierarchy)
  )
}

add_hierarchy_node_char <- function(worklogs_node, hierarchy) {
  stopifnot(
    is_worklogs_node(worklogs_node),
    is.character(hierarchy),
    ! is.na(hierarchy),
    length(hierarchy) == length(worklogs_node)
  )
  split(worklogs_node, hierarchy)
}

add_hierarchy_node_list <- function(worklogs_node, hierarchy) {
  stop("add_hierarchy_node_list is not yet implemented")
}

add_hierarchy_leaf <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_leaf(worklogs_leaf),
    is.list(hierarchy) || is.character(hierarchy)
  )
  `if`(
    is.list(hierarchy),
    add_hierarchy_leaf_list(worklogs_leaf, hierarchy),
    add_hierarchy_leaf_char(worklogs_leaf, hierarchy)
  )
}

add_hierarchy_leaf_char <- function(worklogs_leaf, hierarchy) {
  stopifnot(
    is_worklogs_leaf(worklogs_leaf),
    is.character(hierarchy),
    ! is.na(hierarchy),
    length(hierarchy) == nrow(worklogs_leaf)
  )
  worklogs(split(worklogs_leaf, hierarchy))
}

add_hierarchy_leaf_list <- function(worklogs_leaf, hierarchy) {
  stop("add_hierarchy_leaf_list is not yet implemented")
}

hierarchy_from_elements <- function(worklogs_leaf) {
  split_children <- function(worklogs_leaf) {
    parent <- map_chr(worklogs_leaf$parents, chuck, 1L)
    worklogs_leaf$parents <- map(worklogs_leaf$parents, tail, n = -1L)
    children <- add_hierarchy(worklogs_leaf, parent)
    map(children, hierarchy_from_elements)
  }
  stopifnot(
    is_worklogs_leaf(worklogs_leaf),
    "parents" %in% names(worklogs_leaf),
    is.list(worklogs_leaf$parents),
    map_lgl(worklogs_leaf$parents, is.character)
  )
  n_no_parents <- sum(map_int(worklogs_leaf$parents, length) == 0L)
  if ((n_no_parents != 0L) && (n_no_parents < nrow(worklogs_leaf))) {
    print(worklogs_leaf$parents)
    stop("Either none or all worklogs must have a parent")
  }
  `if`(
    n_no_parents == 0L,
    split_children(worklogs_leaf),
    select(worklogs_leaf, -parents)
  )
}
