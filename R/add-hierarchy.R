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
